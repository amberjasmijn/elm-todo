import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Html.Keyed as Keyed

-- Add Todo
-- Remove Todo

main =
  Browser.sandbox 
    { init = init
    , update = update
    , view = view
    }

-- Model

type alias Model = 
  { input : String
  , todos : List Todo
  , countTodos : Int 
  , uid : Int
  }

type alias Todo = 
  { id : Int
  , description : String 
  , completed : Bool
  }

init : Model
init = 
  { input = ""
  , todos = [] 
  , countTodos = 0
  , uid = 0
  }

-- Update

type Msg 
  = Add 
  | Delete Int
  | UpdateInput String
  | ToggleComplete Int Bool

update : Msg -> Model -> Model
update msg model = 
  case msg of 
    Add ->
      { model 
        | uid = model.uid + 1
        , todos = model.todos ++ [ newTodo model.input model.uid ]
      }
    Delete id ->
      { model | todos = List.filter (\t -> t.id /= id) model.todos }
    UpdateInput newInput -> 
      { model | input = newInput }
    ToggleComplete id isCompleted ->
      let
        updateTodo t =
          if t.id == id then
            { t | completed = isCompleted }
          else
            t
      in ( { model | todos = List.map updateTodo model.todos } )

-- View

view : Model -> Html Msg
view model =
  div []
    [ h1 [] [ text "Todos" ]
    , input [ placeholder "What do you want to do?", value model.input, onInput UpdateInput ] []
    , button [ onClick Add ] [ text "Add" ]    
    , Keyed.ul []
        (List.map (
          \todo -> viewKeyedTodo todo
          ) model.todos)
    ]

viewKeyedTodo : Todo -> (String, Html Msg)
viewKeyedTodo todo =
  (String.fromInt todo.id, viewTodo todo)

viewTodo : Todo -> Html Msg
viewTodo todo =
  li 
    []
    [ div 
        [] 
        [ input 
          [ type_ "checkbox"
          , onClick (ToggleComplete todo.id (not todo.completed))
          , checked todo.completed ] []
        , text todo.description 
        ]
      , button [ onClick (Delete todo.id) ] [ text "Delete" ]
    ]

newTodo : String -> Int -> Todo
newTodo description id =
  { id = id
  , description = description 
  , completed = False
  }
