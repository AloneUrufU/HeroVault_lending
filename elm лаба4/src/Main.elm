module Main exposing (main)

import Browser
import Html exposing (Html, text, div, input, button, span)
import Html.Attributes exposing (class, value, autofocus, placeholder, type_)
import Html.Events exposing (onInput, onClick)
import Dict exposing (Dict)
import List

-- Model
type alias Id = Int

type alias Todo =
    { text : String
    , priority : Int
    }

type alias Model =
    { inputText : String
    , inputPriority : String
    , todos : Dict Id Todo
    }

init : Model
init =
    { inputText = ""
    , inputPriority = "1"
    , todos = Dict.empty
    }

type Msg
    = UpdateInputText String
    | UpdatePriority String
    | AddTodo

-- Update
update msg model =
    case msg of
        UpdateInputText newInputText ->
            { model | inputText = newInputText }
            
        UpdatePriority newPriority ->
            { model | inputPriority = newPriority }
            
        AddTodo ->
            let
                priority = String.toInt model.inputPriority
                    |> Maybe.withDefault 1
                newid = (+) 1
                    <| Maybe.withDefault 0
                    <| List.maximum
                    <| Dict.keys model.todos
                newTodo =
                    { text = model.inputText
                    , priority = priority
                    }
            in
                { model
                    | inputText = ""
                    , todos = Dict.insert newid newTodo model.todos
                }

-- View
view model =
    div []
        [ div []
            [ input
                [ placeholder "Enter Todo"
                , onInput UpdateInputText
                , value model.inputText
                ]
                []
            , input
                [ placeholder "Priority"
                , type_ "number"
                , onInput UpdatePriority
                , value model.inputPriority
                ]
                []
            , button
                [ onClick AddTodo ]
                [ text "Add Todo" ]
            ]
        , div []
            (model.todos
                |> Dict.toList
                |> List.sortBy (\(_, todo) -> -todo.priority)
                |> List.map (\(id, todo) ->
                    div []
                        [ text (String.fromInt id ++ ". [" ++
                            String.fromInt todo.priority ++ "] " ++
                            todo.text)
                        ]
                    )
            )
        ]

-- Main
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }
