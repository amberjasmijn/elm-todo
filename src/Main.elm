import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Html.Keyed as Keyed

main =
  Browser.document 
    { init = init
    , update = update
    , view = \model -> { title = "Todos by Amber", body = [view model] }
    , subscriptions = \model -> Sub.none
    }

-- Model

type alias Model = 
  { input : String
  , todos : List Todo
  , countTodos : Int 
  , uid : Int
  }

init : () -> ( Model, Cmd Msg )
init _ = 
  ({ input = ""
  , todos = [] 
  , countTodos = 0
  , uid = 0
  }
  , Cmd.none )

-- Update

type Msg 
  = Add 
  | Delete Int
  | UpdateInput String
  | ToggleComplete Int Bool

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = 
  case msg of 
    Add ->
      ( { model 
        | uid = model.uid + 1
        , todos = model.todos ++ [ newTodo model.input model.uid ]
      }
      , Cmd.none
      )
    Delete id ->
      ( { model | todos = List.filter (\t -> t.id /= id) model.todos }
      , Cmd.none
      )
    UpdateInput newInput -> 
      ( { model | input = newInput }
      , Cmd.none
      )
    ToggleComplete id isCompleted ->
      let
        updateTodo t =
          if t.id == id then
            { t | completed = isCompleted }
          else
            t
      in ( { model | todos = List.map updateTodo model.todos }
      , Cmd.none )
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

type alias Todo = 
  { id : Int
  , description : String 
  , completed : Bool
  }

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
