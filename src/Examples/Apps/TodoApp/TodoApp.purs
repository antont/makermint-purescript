module Examples.Apps.TodoApp.TodoApp where

import Prelude

import Data.Array (filter, snoc, deleteAt, length, index)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)

-- | Todo item type
type Todo =
  { id :: Int
  , text :: String
  , completed :: Boolean
  }

-- | Application state
type State =
  { todos :: Array Todo
  , nextId :: Int
  , inputValue :: String
  }

-- | Actions that can modify the state
data Action
  = AddTodo
  | ToggleTodo Int
  | DeleteTodo Int
  | UpdateInput String

-- | Initial state
initialState :: State
initialState =
  { todos: []
  , nextId: 1
  , inputValue: ""
  }

-- | The main component
component :: forall query input output m. MonadEffect m => H.Component query input output m
component =
  H.mkComponent
    { initialState: \_ -> initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

-- | Render the UI
render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div
    [ HP.class_ (HH.ClassName "todo-app") ]
    [ HH.h1_ [ HH.text "📝 Todo App" ]
    , HH.div
        [ HP.class_ (HH.ClassName "add-todo") ]
        [ HH.input
            [ HP.type_ HP.InputText
            , HP.placeholder "What needs to be done?"
            , HP.value state.inputValue
            , HE.onValueInput UpdateInput
            , HP.class_ (HH.ClassName "todo-input")
            ]
        , HH.button
            [ HE.onClick \_ -> AddTodo
            , HP.class_ (HH.ClassName "add-button")
            , HP.disabled (state.inputValue == "")
            ]
            [ HH.text "Add" ]
        ]
    , HH.div
        [ HP.class_ (HH.ClassName "todo-list") ]
        (map renderTodo state.todos)
    , HH.div
        [ HP.class_ (HH.ClassName "todo-stats") ]
        [ HH.text $ show (countActive state.todos) <> " items left" ]
    ]

-- | Render a single todo item
renderTodo :: forall m. Todo -> H.ComponentHTML Action () m
renderTodo todo =
  HH.div
    [ HP.class_ (HH.ClassName $ "todo-item" <> if todo.completed then " completed" else "")
    ]
    [ HH.input
        [ HP.type_ HP.InputCheckbox
        , HP.checked todo.completed
        , HE.onClick \_ -> ToggleTodo todo.id
        , HP.class_ (HH.ClassName "todo-checkbox")
        ]
    , HH.span
        [ HP.class_ (HH.ClassName "todo-text") ]
        [ HH.text todo.text ]
    , HH.button
        [ HE.onClick \_ -> DeleteTodo todo.id
        , HP.class_ (HH.ClassName "delete-button")
        ]
        [ HH.text "×" ]
    ]

-- | Handle actions
handleAction :: forall output m. MonadEffect m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  AddTodo -> do
    state <- H.get
    when (state.inputValue /= "") do
      let
        newTodo =
          { id: state.nextId
          , text: state.inputValue
          , completed: false
          }
      H.modify_ \s -> s
        { todos = snoc s.todos newTodo
        , nextId = s.nextId + 1
        , inputValue = ""
        }
  
  ToggleTodo id -> do
    H.modify_ \s -> s
      { todos = map (\todo -> if todo.id == id then todo { completed = not todo.completed } else todo) s.todos
      }
  
  DeleteTodo id -> do
    state <- H.get
    let
      maybeIndex = findIndex (\todo -> todo.id == id) state.todos
    case maybeIndex of
      Just idx -> H.modify_ \s -> s { todos = fromMaybe s.todos (deleteAt idx s.todos) }
      Nothing -> pure unit
  
  UpdateInput value -> do
    H.modify_ _ { inputValue = value }

-- | Helper function to find index
findIndex :: forall a. (a -> Boolean) -> Array a -> Maybe Int
findIndex pred arr = go 0
  where
  go idx
    | idx >= length arr = Nothing
    | otherwise = case index arr idx of
        Just x | pred x -> Just idx
        _ -> go (idx + 1)

-- | Count active (non-completed) todos
countActive :: Array Todo -> Int
countActive = length <<< filter (not _.completed)

-- | Main entry point
main :: Effect Unit
main = runHalogenAff do
  body <- awaitBody
  runUI component unit body

