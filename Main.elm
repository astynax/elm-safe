module Main exposing (main)

import Color
import Graphics.Render exposing (..)
import Html exposing (Html)
import Html.Events as Events
import List
import Random


type alias List2d =
    List (List Bool)


type alias Config =
    { rows : Int
    , cols : Int
    , size : Float
    }


type alias Model =
    { field : List2d
    , moves : Int
    , config : Config
    }


type Click
    = Click Int Int


type Msg
    = Clicked Click
    | GotClicks (List Click)
    | NewGame


main : Program Never Model Msg
main =
    Html.program
        { init =
            init
                { rows = 7
                , cols = 7
                , size = 600
                }
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : Config -> ( Model, Cmd Msg )
init c =
    { field = init2d c.rows c.cols
    , moves = 0
    , config = c
    }
        ! [ Random.generate GotClicks <|
                genClicks c.rows c.cols
          ]


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.button [ Events.onClick NewGame ]
            [ Html.text "New game" ]
        , Html.text "Moves:"
        , Html.text <| toString <| model.moves
        , if gameIsOver model.field then
            renderGameOver model.config.size
          else
            render model
        ]


render : Model -> Html Msg
render model =
    let
        rows =
            model.field

        size =
            model.config.size

        s =
            size / toFloat (List.length rows)

        viewRow y row =
            position ( s / 2, s / 2 + toFloat y * s ) <|
                group <|
                    List.indexedMap
                        (viewCell y)
                        row

        viewCell y x st =
            position ( toFloat x * s, 0 ) <|
                onClick (Clicked <| Click x y) <|
                    vault s st
    in
        svg 0 0 size size <|
            group <|
                List.indexedMap viewRow rows


renderGameOver : Float -> Html a
renderGameOver size =
    svg 0 0 size size <|
        position ( size / 2, size / 2 ) <|
            group
                [ filled (solid Color.orange) <|
                    rectangle
                        size
                        size
                , centered <|
                    fontColor Color.yellow <|
                        text 72 "You win!"
                ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Clicked c ->
            { model
                | field = toggle2d c model.field
                , moves = model.moves + 1
            }
                ! []

        GotClicks cs ->
            { model
                | field = List.foldl toggle2d model.field cs
                , moves = 0
            }
                ! []

        NewGame ->
            init model.config


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


vault : Float -> Bool -> Form a
vault size st =
    let
        bar w h =
            if st then
                rectangle w h
            else
                rectangle h w

        shadow =
            group
                [ filled (solid Color.darkGray) <|
                    bar (size / 10) (size * 0.8)
                , filled (solid Color.darkGray) <|
                    circle (size / 6)
                ]

        knob =
            group
                [ filled (solid Color.black) <|
                    bar (size / 10) (size * 0.8)
                , filled (solid Color.darkGray) <|
                    bar (size / 15) (size * 0.75)
                , filled (solid Color.black) <|
                    circle (size / 6)
                , filled (solid Color.darkGray) <|
                    circle (size / 7)
                ]
    in
        position ( size / 2, size / 2 ) <|
            group
                [ filled (solid Color.gray) <|
                    rectangle size size
                , position ( size / 9, size / 9 ) <|
                    shadow
                , knob
                ]


init2d : Int -> Int -> List2d
init2d w h =
    List.repeat h <| List.repeat w False


toggle2d : Click -> List2d -> List2d
toggle2d (Click x y) =
    List.indexedMap
        (\yy ->
            if yy == y then
                List.map not
            else
                List.indexedMap
                    (\xx v ->
                        if xx == x then
                            not v
                        else
                            v
                    )
        )


genClicks : Int -> Int -> Random.Generator (List Click)
genClicks w h =
    let
        s =
            toFloat <| max w h
    in
        Random.float (0.5 * s) (1.5 * s)
            |> Random.andThen
                (\n ->
                    Random.list (truncate n) <|
                        Random.map2 Click
                            (Random.int 0 (w - 1))
                            (Random.int 0 (h - 1))
                )


gameIsOver : List2d -> Bool
gameIsOver =
    List.all (List.all not)
