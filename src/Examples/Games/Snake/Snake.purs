module Examples.Games.Snake.Snake where

import Prelude

import Control.Alt ((<|>))
import Control.Bind (join)
import Data.Array (all, cons, filter, foldl, length, range, replicate, uncons, updateAt, (!!))
import Data.Tuple (Tuple(..))
import Data.Foldable (any, traverse_)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Traversable (traverse)
import Data.Int (floor, toNumber)
import Effect (Effect)

import Effect.Aff (delay, Milliseconds(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.Event (eventListener)
import Halogen.VDom.Driver (runUI)
import Web.HTML (window)
import Web.HTML.Window (document)
import Web.HTML.Window as Window
import Web.HTML.HTMLDocument (toDocument, toNonElementParentNode)
import Web.DOM.NonElementParentNode (getElementById)
import Web.DOM.Element (toEventTarget)
import Web.Event.Event (EventType(..))
import Web.UIEvent.MouseEvent as ME
import Web.UIEvent.MouseEvent.EventTypes as MET
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET
import Graphics.Canvas (Context2D, CanvasElement, getContext2D, getCanvasElementById, TextAlign(..))
import Graphics.Canvas as Canvas
import Web.HTML.HTMLElement (offsetLeft, offsetTop)
import Web.HTML.HTMLCanvasElement (toHTMLElement, fromElement)

-- | Direction type
data Direction = Up | Down | Left | Right

derive instance eqDirection :: Eq Direction

instance showDirection :: Show Direction where
  show Up = "Up"
  show Down = "Down"
  show Left = "Left"
  show Right = "Right"

-- | Position type
type Position = { x :: Int, y :: Int }

-- | Snake type
type Snake = Array Position

-- | Game state
type State =
  { snake :: Snake
  , direction :: Direction
  , food :: Position
  , gameOver :: Boolean
  , score :: Int
  , canvas :: Maybe { element :: CanvasElement, context :: Context2D }
  , gameSpeed :: Int  -- milliseconds between moves
  }

-- | Actions
data Action
  = Initialize
  | HandleClick ME.MouseEvent
  | HandleKeyPress KE.KeyboardEvent
  | UpdateGame
  | Reset

-- | Initial state
initialState :: forall i. i -> State
initialState _ =
  { snake: [ { x: 10, y: 10 } ]
  , direction: Right
  , food: { x: 15, y: 15 }
  , gameOver: false
  , score: 0
  , canvas: Nothing
  , gameSpeed: 200
  }

-- | Component
component :: forall query input output m. MonadAff m => H.Component query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

-- | Render the UI
render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div_
    [ HH.canvas
        [ HP.id "game-canvas"
        , HP.attr (HH.AttrName "width") "600"
        , HP.attr (HH.AttrName "height") "600"
        , HP.attr (HH.AttrName "style") "border: 2px solid #333; cursor: pointer;"
        ]
    , HH.div
        [ HP.attr (HH.AttrName "style") "margin-top: 20px; text-align: center;" ]
        [ HH.text $ "Score: " <> show state.score
        , if state.gameOver
            then HH.div_
              [ HH.text "Game Over! "
              , HH.button
                  [ HE.onClick \_ -> Reset ]
                  [ HH.text "Play Again" ]
              ]
            else HH.text ""
        ]
    ]

-- | Handle actions
handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Initialize -> do
    maybeCanvas <- H.liftEffect $ getCanvasElementById "game-canvas"
    case maybeCanvas of
      Just canvasElement -> do
        ctx <- H.liftEffect $ getContext2D canvasElement
        H.modify_ _ { canvas = Just { element: canvasElement, context: ctx } }
        drawGame
      Nothing -> pure unit

    -- Start game loop
    void $ H.fork do
      let
        loop = do
          handleAction UpdateGame
          s <- H.get
          H.liftAff $ delay (Milliseconds (toNumber s.gameSpeed * 1.0))
          loop
      loop

    -- Subscribe to canvas clicks
    doc <- H.liftEffect $ window >>= document
    maybeElement <- H.liftEffect $ getElementById "game-canvas" (toNonElementParentNode doc)
    case maybeElement of
      Just element -> do
        void $ H.subscribe $
          eventListener
            MET.click
            (toEventTarget element)
            (map HandleClick <<< ME.fromEvent)
      Nothing -> pure unit

    -- Subscribe keyboard on window (canvas doesn't receive keyboard events without focus)
    win <- H.liftEffect window
    void $ H.subscribe $
      eventListener
        KET.keydown
        (Window.toEventTarget win)
        (map HandleKeyPress <<< KE.fromEvent)

  HandleClick _ -> do
    state <- H.get
    when (not state.gameOver) do
      -- In Snake game, clicking should not affect gameplay directly
      pure unit

  HandleKeyPress keyEvent -> do
    let key = KE.key keyEvent
    case key of
      "r" -> handleAction Reset
      "R" -> handleAction Reset
      _ -> do
        state <- H.get
        when (not state.gameOver) do
          case key of
            "ArrowUp" -> H.modify_ \s -> s { direction = Up }
            "ArrowDown" -> H.modify_ \s -> s { direction = Down }
            "ArrowLeft" -> H.modify_ \s -> s { direction = Left }
            "ArrowRight" -> H.modify_ \s -> s { direction = Right }
            _ -> pure unit

  UpdateGame -> do
    state <- H.get
    when (not state.gameOver) do
      -- Update game state
      H.modify_ updateState
      drawGame

  Reset -> do
    H.modify_ \s -> s
      { snake = [ { x: 10, y: 10 } ]
      , direction = Right
      , food = { x: 15, y: 15 }
      , gameOver = false
      , score = 0
      }
    drawGame

-- | Update game state
updateState :: State -> State
updateState state =
  let
    -- Get head of snake
    head = fromMaybe { x: 0, y: 0 } (state.snake !! 0)
    newHead = case state.direction of
      Up -> { x: head.x, y: head.y - 1 }
      Down -> { x: head.x, y: head.y + 1 }
      Left -> { x: head.x - 1, y: head.y }
      Right -> { x: head.x + 1, y: head.y }

    -- Check if game over (collision with walls or self)
    gameOver =
      -- Check wall collision
      newHead.x < 0 || newHead.x >= 30 || newHead.y < 0 || newHead.y >= 30
      -- Check self collision (simplified - check if new head position matches any body segment)
      || any (\pos -> pos.x == newHead.x && pos.y == newHead.y) (tail state.snake)

    -- Check if food eaten
    foodEaten = newHead.x == state.food.x && newHead.y == state.food.y

    -- Create new snake
    newSnake = if foodEaten
      then cons newHead state.snake
      else cons newHead (tail state.snake)

    -- Generate new food position (fixed for now)
    newFood = if foodEaten
      then { x: 15, y: 15 }
      else state.food

    -- Calculate new score
    newScore = if foodEaten then state.score + 1 else state.score
  in
    state
      { snake = newSnake
      , food = newFood
      , gameOver = gameOver
      , score = newScore
      }

-- | Get tail of array
tail :: forall a. Array a -> Array a
tail arr = case uncons arr of
  Just { tail: xs } -> xs
  Nothing -> []

-- | Draw the game
drawGame :: forall output m. MonadEffect m => H.HalogenM State Action () output m Unit
drawGame = do
  state <- H.get
  case state.canvas of
    Just { context: ctx } -> H.liftEffect do
      -- Clear canvas
      Canvas.setFillStyle ctx "#f0f0f0"
      Canvas.fillRect ctx { x: 0.0, y: 0.0, width: 600.0, height: 600.0 }

      -- Draw grid
      Canvas.setStrokeStyle ctx "#ddd"
      Canvas.setLineWidth ctx 0.5
      Canvas.beginPath ctx

      -- Draw vertical lines
      let drawVerticalLine x = do
            Canvas.moveTo ctx (toNumber x * 20.0) 0.0
            Canvas.lineTo ctx (toNumber x * 20.0) 600.0
            Canvas.stroke ctx

      -- Draw horizontal lines
      let drawHorizontalLine y = do
            Canvas.moveTo ctx 0.0 (toNumber y * 20.0)
            Canvas.lineTo ctx 600.0 (toNumber y * 20.0)
            Canvas.stroke ctx

      -- Draw vertical lines
      traverse_ drawVerticalLine (range 0 29)
      -- Draw horizontal lines
      traverse_ drawHorizontalLine (range 0 29)
      -- Draw snake
      drawSnake ctx state.snake
      -- Draw food
      drawFood ctx state.food

      -- Draw game over message if needed
      when state.gameOver do
        Canvas.setFillStyle ctx "#000"
        Canvas.setFont ctx "30px Arial"
        Canvas.setTextAlign ctx AlignCenter
        Canvas.fillText ctx "Game Over!" 300.0 300.0
    Nothing -> pure unit

-- | Draw the snake
drawSnake :: Context2D -> Snake -> Effect Unit
drawSnake ctx snake = do
  let cellSize = 20.0
  -- Draw snake head in red
  let head = fromMaybe { x: 0, y: 0 } (snake !! 0)
  Canvas.setFillStyle ctx "#e74c3c"
  Canvas.fillRect ctx { x: toNumber head.x * cellSize, y: toNumber head.y * cellSize, width: cellSize, height: cellSize }

  -- Draw snake body in green
  let drawBodySegment segment = do
        Canvas.setFillStyle ctx "#27ae60"
        Canvas.fillRect ctx { x: toNumber segment.x * cellSize, y: toNumber segment.y * cellSize, width: cellSize, height: cellSize }

  traverse_ drawBodySegment (tail snake)

-- | Draw the food
drawFood :: Context2D -> Position -> Effect Unit
drawFood ctx food = do
  let cellSize = 20.0
  Canvas.setFillStyle ctx "#f39c12"
  Canvas.beginPath ctx
  Canvas.arc ctx { x: toNumber food.x * cellSize + cellSize / 2.0, y: toNumber food.y * cellSize + cellSize / 2.0, radius: cellSize / 2.0, start: 0.0, end: 6.28318, useCounterClockwise: false }
  Canvas.fill ctx

-- | Main entry point
main :: Effect Unit
main = runHalogenAff do
  body <- awaitBody
  runUI component unit body
