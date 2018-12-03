import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Html.Keyed as Keyed

main =
  Browser.document 
    { init = init
    , update = update
    , view = \model -> { title = "Tasks by Amber", body = [view model] }
    , subscriptions = \model -> Sub.none
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

emptyModel : Model
emptyModel = 
  { input = ""
  , todos = 
    [ { id = 0
      , description = "This is an example"
      , completed = False }
    ] 
  , countTodos = 0
  , uid = 0
  }

init : () -> ( Model, Cmd Msg )
init _ = 
  ( emptyModel
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
        , input = ""
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
  div [ class "container" ]
    [ viewHeader
    , div [ class "content" ]
      [ div [ class "input-group" ] 
        [ input 
          [ placeholder "What do you want to do?"
          , value model.input
          , onInput UpdateInput 
          , type_ "text"
          ] []
          , button [ onClick Add, class "btn-add" ] [ text "Add" ] 
        ]
        , viewTodos model.todos 
      ]
    ]

viewTodos : List Todo -> Html Msg
viewTodos items =
  Keyed.ul [ class "todos" ]
    ( List.map (\todo -> viewKeyedTodo todo) items )

viewKeyedTodo : Todo -> (String, Html Msg)
viewKeyedTodo todo =
  (String.fromInt todo.id, viewTodo todo)

viewTodo : Todo -> Html Msg
viewTodo todo =
  li 
    [ classList 
      [ ( "todo-item", True )
      , ( "todo-completed", todo.completed ) 
      ] 
    ]
    [ div 
      [ class "todo-text"] 
      [ label [] 
        [ input 
          [ type_ "checkbox"
          , onClick (ToggleComplete todo.id (not todo.completed))
          , checked todo.completed 
          ] []
        , text todo.description ]
      ]
      , button [ onClick (Delete todo.id), class "btn-delete" ] [ text "Delete" ]
    ]

newTodo : String -> Int -> Todo
newTodo description id =
  { id = id
  , description = description 
  , completed = False
  }

viewHeader : Html msg
viewHeader = 
  header [ class "header" ]
    [ p [ class "subtitle" ] [ text "Build with Elm" ]
    , h1 [ class "title" ] [ text "Your tasks" ]
  ]