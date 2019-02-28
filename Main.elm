module Main exposing (main)

import Browser
import Collage exposing (..)
import Collage.Events exposing (onClick)
import Collage.Render exposing (svg)
import Collage.Text as Text
import Color
import Debug exposing (toString)
import Html exposing (Html)
import Html.Events as Events
import List
import Random


type alias List2d =
    List (List Bool)


type alias Config =
    { cells : Int
    , cellSize : Float
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


main : Program () Model Msg
main =
    Browser.element
        { init =
            init
                { cells = 7
                , cellSize = 80
                }
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : Config -> flags -> ( Model, Cmd Msg )
init c _ =
    ( { field = init2d c.cells c.cells
      , moves = 0
      , config = c
      }
    , genClicks c.cells c.cells
        |> Random.generate GotClicks
    )


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.button [ Events.onClick NewGame ]
            [ Html.text "New game" ]
        , Html.text "Moves:"
        , model.moves
            |> toString
            |> Html.text
        , if gameIsOver model.field then
            renderGameOver (model.config.cellSize * toFloat model.config.cells)

          else
            render model
        ]


render : Model -> Html Msg
render model =
    let
        rows =
            model.field

        s =
            model.config.cellSize

        viewRow y row =
            List.indexedMap (viewCell y) row
                |> group
                |> shift ( s / 2, s / 2 + toFloat y * s )

        viewCell y x st =
            vault s st
                |> onClick (Clicked <| Click x y)
                |> shift ( toFloat x * s, 0 )
    in
    List.indexedMap viewRow rows
        |> group
        |> svg


renderGameOver : Float -> Html a
renderGameOver size =
    group
        [ Text.fromString "You win!"
            |> Text.size 72
            |> Text.color Color.yellow
            |> rendered
        , square size
            |> filled (uniform Color.orange)
        ]
        |> shift ( size / 2, size / 2 )
        |> svg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Clicked c ->
            ( { model
                | field = toggle2d c model.field
                , moves = model.moves + 1
              }
            , Cmd.none
            )

        GotClicks cs ->
            ( { model
                | field = List.foldl toggle2d model.field cs
                , moves = 0
              }
            , Cmd.none
            )

        NewGame ->
            init model.config ()


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


vault : Float -> Bool -> Collage a
vault size st =
    let
        bar w h =
            if st then
                rectangle w h

            else
                rectangle h w

        shadow =
            group
                [ bar (size / 10) (size * 0.8)
                    |> filled (uniform Color.darkGray)
                , circle (size / 6)
                    |> filled (uniform Color.darkGray)
                ]

        knob =
            group
                [ circle (size / 7)
                    |> filled (uniform Color.darkGray)
                , circle (size / 6)
                    |> filled (uniform Color.black)
                , bar (size / 15) (size * 0.75)
                    |> filled (uniform Color.darkGray)
                , bar (size / 10) (size * 0.8)
                    |> filled (uniform Color.black)
                ]
    in
    shift ( size / 2, size / 2 ) <|
        group
            [ knob
            , shadow
                |> shift ( size / 10, size / 10 )
            , square size
                |> filled (uniform Color.gray)
            ]


init2d : Int -> Int -> List2d
init2d w h =
    List.repeat h <| List.repeat w False


toggle2d : Click -> List2d -> List2d
toggle2d (Click x y) =
    List.indexedMap <|
        \yy ->
            if yy == y then
                List.map not

            else
                List.indexedMap <|
                    \xx v ->
                        if xx == x then
                            not v

                        else
                            v


genClicks : Int -> Int -> Random.Generator (List Click)
genClicks w h =
    let
        s =
            max w h |> toFloat
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
