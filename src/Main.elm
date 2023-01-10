module Main exposing (main)

import Browser exposing (Document)
import Browser.Dom exposing (Viewport, getViewport)
import Browser.Events exposing (onKeyDown, onResize)
import Html exposing (div)
import Html.Attributes exposing (class, style)
import Json.Decode as Decode exposing (Decoder)
import Task


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    { player : Object
    , scene : Scene
    }


type alias Object =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    }


type alias Scene =
    { width : Float
    , height : Float
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { player = { x = 0, y = 0, width = 100, height = 30 }
      , scene = { width = 0, height = 0 }
      }
    , Task.perform GotViewPort getViewport
    )


type Msg
    = NoOp
    | GotViewPort Viewport
    | SceneResize Int Int
    | MovePlayer Direction


padding : Float
padding =
    100


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GotViewPort { scene } ->
            let
                player =
                    model.player
            in
            ( { model
                | scene = scene
                , player = { player | x = padding, y = scene.height - padding - player.height }
              }
            , Cmd.none
            )

        SceneResize w h ->
            let
                newWidth =
                    toFloat w

                newHeight =
                    toFloat h

                player =
                    model.player
            in
            ( { model
                | scene = { width = newWidth, height = newHeight }
                , player = { player | x = padding, y = newHeight - padding - player.height }
              }
            , Cmd.none
            )

        MovePlayer direction ->
            let
                player =
                    model.player
            in
            case direction of
                Left ->
                    let
                        calculatedX =
                            player.x - 10

                        newX =
                            if calculatedX < padding then
                                padding

                            else
                                calculatedX
                    in
                    ( { model | player = { player | x = newX } }, Cmd.none )

                Right ->
                    let
                        calculatedX =
                            player.x + 10

                        newX =
                            if calculatedX > model.scene.width - padding - player.width then
                                model.scene.width - padding - player.width

                            else
                                calculatedX
                    in
                    ( { model | player = { player | x = newX } }, Cmd.none )

                Other ->
                    ( model, Cmd.none )


view : Model -> Document Msg
view model =
    { title = "Lang Invaders"
    , body =
        [ div
            [ class "player"
            , style "position" "absolute"
            , style "top" (String.fromFloat model.player.y ++ "px")
            , style "left" (String.fromFloat model.player.x ++ "px")
            , style "width" (String.fromFloat model.player.width ++ "px")
            , style "height" (String.fromFloat model.player.height ++ "px")
            ]
            []
        ]
    }


type Direction
    = Left
    | Right
    | Other


keyDecoder : Decoder Direction
keyDecoder =
    Decode.map toDirection (Decode.field "key" Decode.string)


toDirection : String -> Direction
toDirection string =
    case string of
        "ArrowLeft" ->
            Left

        "ArrowRight" ->
            Right

        _ ->
            Other


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ onResize SceneResize
        , onKeyDown keyDecoder |> Sub.map MovePlayer
        ]
