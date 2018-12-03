import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)

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
  , todos : List String
  , countTodos : Int }

init : Model
init = 
  { input = ""
  , todos = [ "Learn Elm" ] 
  , countTodos = 0
  }

-- Update

type Msg = AddTodo String | RemoveTodo String

update : Msg -> Model -> Model
update msg model = 
  case msg of 
    AddTodo input ->
      { model | todos = List.append model.todos [input] }
    RemoveTodo input ->
      { model | todos = model.todos }

-- View

view : Model -> Html Msg
view model =
  div []
    [ input [ placeholder "Text to reverse", value model.input, onInput AddTodo ] [], 
      ul []
        (List.map (\l -> li [] [ text l ]) model.todos)    
    ]