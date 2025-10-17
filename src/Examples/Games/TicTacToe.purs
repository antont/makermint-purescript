module Examples.Games.TicTacToe where

import Prelude

import Control.Alt ((<|>))
import Control.Bind (join)
import Data.Array (all, findMap, foldl, range, replicate, uncons, updateAt, (!!))
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Traversable (traverse)
import Data.Int (floor, toNumber)
import Effect (Effect)
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
import Web.HTML.HTMLDocument (toDocument, toNonElementParentNode)
import Web.DOM.NonElementParentNode (getElementById)
import Web.DOM.Element (toEventTarget)
import Web.Event.Event (EventType(..))
import Web.UIEvent.MouseEvent as ME
import Web.UIEvent.MouseEvent.EventTypes as MET
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET
import Graphics.Canvas (Context2D, CanvasElement, getContext2D, getCanvasElementById)
import Graphics.Canvas as Canvas
import Web.HTML.HTMLElement (offsetLeft, offsetTop)
import Web.HTML.HTMLCanvasElement (toHTMLElement, fromElement)

-- | Player type
data Player = X | O

derive instance eqPlayer :: Eq Player

instance showPlayer :: Show Player where
  show X = "X"
  show O = "O"

-- | Game state
type State =
  { board :: Array (Maybe Player)
  , currentPlayer :: Player
  , winner :: Maybe Player
  , gameOver :: Boolean
  , canvas :: Maybe { element :: CanvasElement, context :: Context2D }
  }

-- | Actions
data Action
  = Initialize
  | HandleClick ME.MouseEvent
  | HandleKeyPress KE.KeyboardEvent
  | Reset

-- | Initial state
initialState :: forall i. i -> State
initialState _ =
  { board: replicate 9 Nothing
  , currentPlayer: X
  , winner: Nothing
  , gameOver: false
  , canvas: Nothing
  }

-- | Component
component :: forall query input output m. MonadEffect m => H.Component query input output m
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
        [ HH.text $ if state.gameOver
            then case state.winner of
              Just player -> show player <> " wins! "
              Nothing -> "Draw! "
            else "Current player: " <> show state.currentPlayer
        , if state.gameOver
            then HH.button
              [ HE.onClick \_ -> Reset ]
              [ HH.text "Reset" ]
            else HH.text ""
        ]
    ]

-- | Handle actions
handleAction :: forall output m. MonadEffect m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Initialize -> do
    H.liftEffect do
      maybeCanvas <- getCanvasElementById "game-canvas"
      void $ traverse (\c -> getContext2D c) maybeCanvas
    
    -- Small delay to ensure canvas is ready
    void $ H.fork do
      H.liftEffect $ Canvas.tryLoadImage "data:image/gif;base64,R0lGODlhAQABAAAAACw=" $ \_ -> pure unit
      maybeCanvas <- H.liftEffect $ getCanvasElementById "game-canvas"
      case maybeCanvas of
        Just canvasElement -> do
          ctx <- H.liftEffect $ getContext2D canvasElement
          H.modify_ _ { canvas = Just { element: canvasElement, context: ctx } }
          drawBoard
        Nothing -> pure unit
    
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
        
        -- Subscribe to keyboard events
        void $ H.subscribe $
          eventListener
            KET.keydown
            (toEventTarget element)
            (map HandleKeyPress <<< KE.fromEvent)
      Nothing -> pure unit
  
  HandleClick mouseEvent -> do
    state <- H.get
    when (not state.gameOver) do
      doc <- H.liftEffect $ window >>= document
      maybeElement <- H.liftEffect $ getElementById "game-canvas" (toNonElementParentNode doc)
      case maybeElement >>= fromElement of
        Just canvasElement -> do
          -- Get canvas position and calculate relative mouse position
          let htmlElement = toHTMLElement canvasElement
          canvasOffsetX <- H.liftEffect $ offsetLeft htmlElement
          canvasOffsetY <- H.liftEffect $ offsetTop htmlElement
          let
            mouseX = toNumber $ ME.clientX mouseEvent
            mouseY = toNumber $ ME.clientY mouseEvent
            canvasX = mouseX - canvasOffsetX
            canvasY = mouseY - canvasOffsetY
            cellIndex = getCellIndex canvasX canvasY
          
          case cellIndex of
            Just idx ->
              case state.board !! idx of
                Just Nothing -> do
                  let
                    newBoard = fromMaybe state.board (updateAt idx (Just state.currentPlayer) state.board)
                    newWinner = checkWinner newBoard
                    newGameOver = isJust newWinner || isBoardFull newBoard
                  
                  H.modify_ _ 
                    { board = newBoard
                    , currentPlayer = if state.currentPlayer == X then O else X
                    , winner = newWinner
                    , gameOver = newGameOver
                    }
                  
                  drawBoard
                _ -> pure unit
            Nothing -> pure unit
        Nothing -> pure unit
  
  HandleKeyPress keyEvent -> do
    let key = KE.key keyEvent
    when (key == "r" || key == "R") do
      handleAction Reset
  
  Reset -> do
    H.modify_ _ 
      { board = replicate 9 Nothing
      , currentPlayer = X
      , winner = Nothing
      , gameOver = false
      }
    drawBoard

-- | Get cell index from canvas coordinates
getCellIndex :: Number -> Number -> Maybe Int
getCellIndex x y =
  let
    cellSize = 200.0
    col = floor (x / cellSize)
    row = floor (y / cellSize)
  in
    if col >= 0 && col < 3 && row >= 0 && row < 3
      then Just (row * 3 + col)
      else Nothing

-- | Check for winner by checking rows, columns, and diagonals
-- Returns Just Player if that player won, Nothing otherwise
checkWinner :: Array (Maybe Player) -> Maybe Player
checkWinner board =
  checkAllLines (allRows <> allColumns <> allDiagonals)
  where
    -- Get the player at a board position
    getPlayer :: Int -> Maybe Player
    getPlayer idx = join (board !! idx)
    
    -- Check if a line of three cells all contain the same player
    checkLine :: Array Int -> Maybe Player
    checkLine indices = do
      players <- traverse getPlayer indices
      case players of
        [p1, p2, p3] | p1 == p2 && p2 == p3 -> Just p1
        _ -> Nothing
    
    -- Check all lines and return the first winner found (if any)
    checkAllLines :: Array (Array Int) -> Maybe Player
    checkAllLines lines = findMap checkLine lines
    
    -- Generate all row indices: [[0,1,2], [3,4,5], [6,7,8]]
    allRows :: Array (Array Int)
    allRows = map makeRow (range 0 2)
      where makeRow r = map (\c -> r * 3 + c) (range 0 2)
    
    -- Generate all column indices: [[0,3,6], [1,4,7], [2,5,8]]
    allColumns :: Array (Array Int)
    allColumns = map makeColumn (range 0 2)
      where makeColumn c = map (\r -> r * 3 + c) (range 0 2)
    
    -- Generate diagonal indices: [[0,4,8], [2,4,6]]
    allDiagonals :: Array (Array Int)
    allDiagonals = 
      [ map (\i -> i * 4) (range 0 2)      -- main diagonal: 0, 4, 8
      , map (\i -> (2 - i) * 3 + i) (range 0 2)  -- anti-diagonal: 2, 4, 6
      ]

-- | Check if board is full
isBoardFull :: Array (Maybe Player) -> Boolean
isBoardFull = all isJust

-- | Draw the board
drawBoard :: forall output m. MonadEffect m => H.HalogenM State Action () output m Unit
drawBoard = do
  state <- H.get
  case state.canvas of
    Just { context: ctx } -> H.liftEffect do
      -- Clear
      Canvas.setFillStyle ctx "#f0f0f0"
      Canvas.fillRect ctx { x: 0.0, y: 0.0, width: 600.0, height: 600.0 }
      
      -- Draw grid
      Canvas.setStrokeStyle ctx "#333"
      Canvas.setLineWidth ctx 4.0
      Canvas.beginPath ctx
      Canvas.moveTo ctx 200.0 0.0
      Canvas.lineTo ctx 200.0 600.0
      Canvas.stroke ctx
      Canvas.beginPath ctx
      Canvas.moveTo ctx 400.0 0.0
      Canvas.lineTo ctx 400.0 600.0
      Canvas.stroke ctx
      Canvas.beginPath ctx
      Canvas.moveTo ctx 0.0 200.0
      Canvas.lineTo ctx 600.0 200.0
      Canvas.stroke ctx
      Canvas.beginPath ctx
      Canvas.moveTo ctx 0.0 400.0
      Canvas.lineTo ctx 600.0 400.0
      Canvas.stroke ctx
      
      -- Draw pieces
      foldl (\acc idx -> acc *> drawPiece ctx idx (state.board !! idx)) (pure unit) (range 0 8)
    Nothing -> pure unit

-- | Draw a piece
drawPiece :: Context2D -> Int -> Maybe (Maybe Player) -> Effect Unit
drawPiece ctx idx cell =
  case cell of
    Just (Just player) ->
      let
        row = idx / 3
        col = idx `mod` 3
        centerX = toNumber col * 200.0 + 100.0
        centerY = toNumber row * 200.0 + 100.0
      in
        case player of
          X -> do
            Canvas.setStrokeStyle ctx "#e74c3c"
            Canvas.setLineWidth ctx 8.0
            Canvas.beginPath ctx
            Canvas.moveTo ctx (centerX - 60.0) (centerY - 60.0)
            Canvas.lineTo ctx (centerX + 60.0) (centerY + 60.0)
            Canvas.stroke ctx
            Canvas.beginPath ctx
            Canvas.moveTo ctx (centerX + 60.0) (centerY - 60.0)
            Canvas.lineTo ctx (centerX - 60.0) (centerY + 60.0)
            Canvas.stroke ctx
          O -> do
            Canvas.setStrokeStyle ctx "#3498db"
            Canvas.setLineWidth ctx 8.0
            Canvas.beginPath ctx
            Canvas.arc ctx { x: centerX, y: centerY, radius: 60.0, start: 0.0, end: 6.28318, useCounterClockwise: false }
            Canvas.stroke ctx
    _ -> pure unit

-- | Main entry point
main :: Effect Unit
main = runHalogenAff do
  body <- awaitBody
  runUI component unit body
