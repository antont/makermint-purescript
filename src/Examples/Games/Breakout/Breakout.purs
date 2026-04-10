module Examples.Games.Breakout.Breakout where

import Prelude

import Control.Alt ((<|>))
import Control.Monad (void, when)
import Data.Array (foldl, range, filter, length, (!!), uncons)
import Data.Tuple (Tuple(..))
import Data.Foldable (all, any, minimum)
import Data.Int (floor, toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (traverse_)

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
import Graphics.Canvas (Context2D, CanvasElement, getContext2D, getCanvasElementById, TextAlign(..), TextBaseline(..))
import Graphics.Canvas as Canvas
import Web.HTML.HTMLElement (offsetLeft)
import Web.HTML.HTMLCanvasElement (toHTMLElement, fromElement)

-- | Game state enumeration
data GameState = Playing | Won | Lost

derive instance eqGameState :: Eq GameState

instance showGameState :: Show GameState where
  show Playing = "Playing"
  show Won = "You Win!"
  show Lost = "Game Over"

-- | Ball entity
type Ball =
  { x :: Number
  , y :: Number
  , dx :: Number
  , dy :: Number
  , radius :: Number
  , speed :: Number
  }

-- | Paddle entity
type Paddle =
  { x :: Number
  , y :: Number
  , width :: Number
  , height :: Number
  , speed :: Number
  }

-- | Brick entity
type Brick =
  { x :: Number
  , y :: Number
  , width :: Number
  , height :: Number
  , active :: Boolean
  , color :: String
  }

-- | Game state
type State =
  { ball :: Ball
  , paddle :: Paddle
  , bricks :: Array Brick
  , score :: Int
  , lives :: Int
  , gameState :: GameState
  , canvas :: Maybe { element :: CanvasElement, context :: Context2D }
  }

-- | Actions
data Action
  = Initialize
  | MouseMove ME.MouseEvent
  | HandleKeyPress KE.KeyboardEvent
  | UpdateGame
  | Reset

-- | Constants
canvasWidth :: Number
canvasWidth = 800.0

canvasHeight :: Number
canvasHeight = 600.0

-- | Initial ball state
initialBall :: Ball
initialBall =
  { x: 400.0
  , y: 500.0
  , dx: 4.0
  , dy: -4.0
  , radius: 8.0
  , speed: 5.0
  }

-- | Initial paddle state
initialPaddle :: Paddle
initialPaddle =
  { x: 350.0
  , y: 550.0
  , width: 100.0
  , height: 15.0
  , speed: 8.0
  }

-- | Generate bricks
generateBricks :: Int -> Int -> Array Brick
generateBricks rows cols =
  let
    brickWidth = 75.0
    brickHeight = 25.0
    padding = 10.0
    offsetLeft = 50.0
    offsetTop = 50.0
    colors = ["#e74c3c", "#e67e22", "#f1c40f", "#2ecc71", "#3498db"]
  in
    foldl (\acc row -> acc <> foldl (\acc2 col ->
      acc2 <> [
        { x: offsetLeft + toNumber col * (brickWidth + padding)
        , y: offsetTop + toNumber row * (brickHeight + padding)
        , width: brickWidth
        , height: brickHeight
        , active: true
        , color: fromMaybe "#ffffff" (colors !! (row `mod` length colors))
        }
      ]) [] (range 0 (cols - 1))
    ) [] (range 0 (rows - 1))

-- | Initial state
initialState :: forall i. i -> State
initialState _ =
  { ball: initialBall
  , paddle: initialPaddle
  , bricks: generateBricks 5 8
  , score: 0
  , lives: 3
  , gameState: Playing
  , canvas: Nothing
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
        , HP.attr (HH.AttrName "width") "800"
        , HP.attr (HH.AttrName "height") "600"
        , HP.attr (HH.AttrName "style") "border: 2px solid #333;"
        ]
    , HH.div
        [ HP.attr (HH.AttrName "style") "margin-top: 10px; display: flex; justify-content: space-between; padding: 0 20px;" ]
        [ HH.span [] [ HH.text $ "Score: " <> show state.score ]
        , HH.span [] [ HH.text $ "Lives: " <> show state.lives ]
        , HH.span [] [ HH.text $ "State: " <> show state.gameState ]
        ]
    , HH.div
        [ HP.attr (HH.AttrName "style") "text-align: center; margin-top: 10px;" ]
        [ HH.button
            [ HE.onClick \_ -> Reset ]
            [ HH.text "Reset Game" ]
        ]
    , HH.div
        [ HP.attr (HH.AttrName "style") "text-align: center; margin-top: 10px; color: #666;" ]
        [ HH.text "Use mouse or arrow keys to move. Press Space to reset when game ends." ]
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

    -- Subscribe to mouse events
    doc <- H.liftEffect $ window >>= document
    maybeElement <- H.liftEffect $ getElementById "game-canvas" (toNonElementParentNode doc)
    case maybeElement of
      Just element -> do
        -- Subscribe to mouse move events for paddle control
        void $ H.subscribe $
          eventListener
            MET.mousemove
            (toEventTarget element)
            (map MouseMove <<< ME.fromEvent)
      Nothing -> pure unit

    -- Subscribe keyboard on window (canvas doesn't receive keyboard events without focus)
    win <- H.liftEffect window
    void $ H.subscribe $
      eventListener
        KET.keydown
        (Window.toEventTarget win)
        (map HandleKeyPress <<< KE.fromEvent)

    -- Start game loop
    void $ H.fork do
      let
        loop = do
          handleAction UpdateGame
          H.liftAff $ delay (Milliseconds 16.0)
          loop
      loop


  MouseMove mouseEvent -> do
    state <- H.get
    when (state.gameState == Playing) do
      doc <- H.liftEffect $ window >>= document
      maybeElement <- H.liftEffect $ getElementById "game-canvas" (toNonElementParentNode doc)
      case maybeElement >>= fromElement of
        Just canvasElement -> do
          let htmlElement = toHTMLElement canvasElement
          canvasOffsetX <- H.liftEffect $ offsetLeft htmlElement
          mouseX <- H.liftEffect $ pure $ toNumber $ ME.clientX mouseEvent
          let
            canvasX = mouseX - canvasOffsetX
            newX = canvasX - initialPaddle.width / 2.0
            clampedX = if newX < 0.0 then 0.0 else if newX > canvasWidth - initialPaddle.width then canvasWidth - initialPaddle.width else newX
          H.modify_ _ { paddle = state.paddle { x = clampedX } }
          drawGame
        Nothing -> pure unit

  HandleKeyPress keyEvent -> do
    state <- H.get
    let key = KE.key keyEvent
    case key of
      "ArrowLeft" -> do
        when (state.gameState == Playing) do
          let newPaddleX = state.paddle.x - state.paddle.speed
          H.modify_ _ { paddle = state.paddle { x = newPaddleX } }
          drawGame
      "ArrowRight" -> do
        when (state.gameState == Playing) do
          let newPaddleX = state.paddle.x + state.paddle.speed
          H.modify_ _ { paddle = state.paddle { x = newPaddleX } }
          drawGame
      " " -> do
        when (state.gameState /= Playing) $ handleAction Reset
      _ -> pure unit

  UpdateGame -> do
    state <- H.get
    when (state.gameState == Playing) $ do
      let
        ball = state.ball
        paddle = state.paddle
        canvasW = canvasWidth
        canvasH = canvasHeight

        -- Calculate new position
        newBallX = ball.x + ball.dx
        newBallY = ball.y + ball.dy

        -- Check wall collisions
        ballAfterWalls =
          let
            bouncedX = newBallX < ball.radius || newBallX > canvasW - ball.radius
            bouncedY = newBallY < ball.radius || newBallY > canvasH - ball.radius
            clampedX = if newBallX < ball.radius then ball.radius else if newBallX > canvasW - ball.radius then canvasW - ball.radius else newBallX
            clampedY = if newBallY < ball.radius then ball.radius else if newBallY > canvasH - ball.radius then canvasH - ball.radius else newBallY
          in
            ball { x = clampedX
                 , y = clampedY
                 , dx = if bouncedX then -ball.dx else ball.dx
                 , dy = if bouncedY then if newBallY < ball.radius then -ball.dy else ball.dy else ball.dy
                 }

        -- Check paddle collision (only when moving downward)
        ballAfterPaddle =
          if ballAfterWalls.y + ball.radius > paddle.y &&
             ballAfterWalls.y - ball.radius < paddle.y + paddle.height &&
             ballAfterWalls.x > paddle.x &&
             ballAfterWalls.x < paddle.x + paddle.width &&
             ballAfterWalls.dy > 0.0
            then ballAfterWalls { dy = -ballAfterWalls.dy * 1.05, y = paddle.y - ball.radius }
            else ballAfterWalls

        -- Check if ball went below paddle
        ballLost = ballAfterPaddle.y - ball.radius > canvasH

        -- Update lives and game state
        newLives = if ballLost then state.lives - 1 else state.lives
        newGameState =
          if newLives < 0 then Lost
          else if all (\b -> not b.active) state.bricks then Won
          else Playing

        -- Reset ball if lost a life
        finalBall =
          if ballLost && newLives >= 0
            then initialBall { x = paddle.x + paddle.width / 2.0, y = paddle.y - ball.radius - 5.0 }
            else ballAfterPaddle

        -- Check brick collisions
        brickCollisionResult = checkBrickCollision finalBall state.bricks
        finalBricks = brickCollisionResult.bricks
        scoreIncrease = brickCollisionResult.scoreIncrease
        finalBallWithBrickBounce = brickCollisionResult.ball
        newScore = state.score + scoreIncrease

      H.modify_ _
        { ball = finalBallWithBrickBounce
        , score = newScore
        , bricks = finalBricks
        , lives = newLives
        , gameState = newGameState
        }
      drawGame

  Reset -> do
    H.modify_ _
      { ball = initialBall
      , paddle = initialPaddle
      , bricks = generateBricks 5 8
      , score = 0
      , lives = 3
      , gameState = Playing
      }
    drawGame

-- | Brick collision result
type BrickCollisionResult =
  { ball :: Ball
  , bricks :: Array Brick
  , scoreIncrease :: Int
  }

-- | Check collision with bricks
checkBrickCollision :: Ball -> Array Brick -> BrickCollisionResult
checkBrickCollision ball bricks =
  go bricks { ball: ball, bricks: bricks, scoreIncrease: 0 }
  where
    go :: Array Brick -> BrickCollisionResult -> BrickCollisionResult
    go bs acc = case uncons bs of
      Nothing -> acc
      Just { head: b, tail: bs' } ->
        if b.active &&
           acc.ball.x + acc.ball.radius > b.x &&
           acc.ball.x - acc.ball.radius < b.x + b.width &&
           acc.ball.y + acc.ball.radius > b.y &&
           acc.ball.y - acc.ball.radius < b.y + b.height
          then
            let
              overlapLeft = acc.ball.x + acc.ball.radius - b.x
              overlapRight = b.x + b.width - (acc.ball.x - acc.ball.radius)
              overlapTop = acc.ball.y + acc.ball.radius - b.y
              overlapBottom = b.y + b.height - (acc.ball.y - acc.ball.radius)
              minOverlap = fromMaybe 0.0 (minimum [overlapLeft, overlapRight, overlapTop, overlapBottom])
              newBall = if minOverlap == overlapLeft || minOverlap == overlapRight
                        then acc.ball { dx = -acc.ball.dx }
                        else acc.ball { dy = -acc.ball.dy }
              newBricks = map (\brick -> if brick.x == b.x && brick.y == b.y then brick { active = false } else brick) acc.bricks
            in go bs' { ball: newBall, bricks: newBricks, scoreIncrease: acc.scoreIncrease + 10 }
          else go bs' acc


-- | Draw the game
drawGame :: forall output m. MonadEffect m => H.HalogenM State Action () output m Unit
drawGame = do
  state <- H.get
  case state.canvas of
    Just { context: ctx } -> H.liftEffect do
      -- Clear canvas
      Canvas.setFillStyle ctx "#1a1a2e"
      Canvas.fillRect ctx { x: 0.0, y: 0.0, width: canvasWidth, height: canvasHeight }

      -- Draw bricks
      traverse_ (\brick ->
        when brick.active $ do
          Canvas.setFillStyle ctx brick.color
          Canvas.fillRect ctx { x: brick.x, y: brick.y, width: brick.width, height: brick.height }
          Canvas.setStrokeStyle ctx "#ffffff"
          Canvas.setLineWidth ctx 1.0
          Canvas.strokeRect ctx { x: brick.x, y: brick.y, width: brick.width, height: brick.height }
      ) state.bricks

      -- Draw paddle
      Canvas.setFillStyle ctx "#00ff88"
      Canvas.fillRect ctx { x: state.paddle.x, y: state.paddle.y, width: state.paddle.width, height: state.paddle.height }

      -- Draw ball
      Canvas.beginPath ctx
      Canvas.arc ctx { x: state.ball.x, y: state.ball.y, radius: state.ball.radius, start: 0.0, end: 6.28318, useCounterClockwise: false }
      Canvas.setFillStyle ctx "#ff6b9d"
      Canvas.fill ctx

      -- Draw game over/win message
      when (state.gameState /= Playing) do
        Canvas.beginPath ctx
        Canvas.arc ctx { x: 400.0, y: 300.0, radius: 150.0, start: 0.0, end: 6.28318, useCounterClockwise: false }
        Canvas.setFillStyle ctx "#000000cc"
        Canvas.fill ctx
        Canvas.setStrokeStyle ctx "#ffffff"
        Canvas.setLineWidth ctx 3.0
        Canvas.stroke ctx
        Canvas.setLineWidth ctx 1.0
        Canvas.setFillStyle ctx "#ffffff"
        Canvas.setFont ctx "30px Arial"
        Canvas.setTextAlign ctx AlignCenter
        Canvas.setTextBaseline ctx BaselineMiddle
        Canvas.fillText ctx (show state.gameState) 400.0 280.0
        Canvas.setFont ctx "20px Arial"
        Canvas.fillText ctx "Press Space or Click Reset" 400.0 320.0
    Nothing -> pure unit

-- | Main entry point
main :: Effect Unit
main = runHalogenAff do
  body <- awaitBody
  runUI component unit body
