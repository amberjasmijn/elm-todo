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

update : Msg -> Model -> Model
update msg model = 
  case msg of 
    Add ->
      { model 
        | todos = model.todos ++ [ newTodo model.input model.uid ]
        , uid = model.uid + 1
      }
    Delete id ->
      { model | todos = List.filter (\t -> t.id /= id) model.todos }
    UpdateInput newInput -> 
      { model | input = newInput }

-- View

view : Model -> Html Msg
view model =
  div []
    [ input [ placeholder "What do you want to do?", value model.input, onInput UpdateInput ] []
    , Keyed.ul []
        (List.map (
          \todo -> viewKeyedTodo todo
          ) model.todos)
    , button [ onClick Add ] [ text "Add" ]    
    ]

viewKeyedTodo : Todo -> (String, Html Msg)
viewKeyedTodo todo =
  (String.fromInt todo.id, viewTodo todo)

viewTodo : Todo -> Html Msg
viewTodo todo =
  li []
    [ div [] [ text todo.description ],
      button [ onClick (Delete todo.id) ] [ text "Remove" ]
    ]

newTodo : String -> Int -> Todo
newTodo description id =
  { id = id
  , description = description 
  }
