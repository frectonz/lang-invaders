module Main exposing (main)

import Browser exposing (Document)
import Browser.Dom exposing (Viewport, getViewport)
import Browser.Events exposing (onKeyDown, onResize)
import Canvas exposing (clear, rect, shapes)
import Canvas.Settings exposing (fill)
import Color
import Json.Decode as Decode exposing (Decoder)
import Task
import Time


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
    , enemies : List Object
    , enemiesPos : { x : Float, y : Float }
    , enemiesDelta : { x : Float, y : Float }
    , missile : Maybe Object
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


enemiesGap : Float
enemiesGap =
    10


enemySize : Float
enemySize =
    50


rows : Int
rows =
    5


cols : Int
cols =
    11


enemiesGroupWidth : Float
enemiesGroupWidth =
    toFloat cols * (enemySize + enemiesGap)


init : () -> ( Model, Cmd Msg )
init _ =
    ( { player = { x = 0, y = 0, width = 100, height = 30 }
      , scene = { width = 0, height = 0 }
      , enemies =
            List.repeat (rows * cols) 0
                |> List.indexedMap
                    (\i _ ->
                        let
                            row =
                                i // cols

                            col =
                                i - (row * cols)
                        in
                        { x = toFloat col * (enemySize + enemiesGap)
                        , y = toFloat row * (enemySize + enemiesGap)
                        , width = enemySize
                        , height = enemySize
                        }
                    )
      , enemiesPos = { x = 0, y = 0 }
      , enemiesDelta = { x = 5, y = 5 }
      , missile = Nothing
      }
    , Task.perform GotViewPort getViewport
    )


type Msg
    = NoOp
    | GotViewPort Viewport
    | SceneResize Int Int
    | MovePlayer Direction
    | MoveEnemies Time.Posix


scenePadding : Float
scenePadding =
    100


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GotViewPort { scene } ->
            let
                newScene =
                    { width = scene.width - scenePadding
                    , height = scene.height - scenePadding
                    }

                player =
                    model.player

                newPlayer =
                    { player
                        | y = newScene.height - player.height
                    }
            in
            ( { model
                | scene = newScene
                , player = newPlayer
              }
            , Cmd.none
            )

        SceneResize w h ->
            let
                scene =
                    model.scene

                newScene =
                    { scene
                        | width = toFloat w - scenePadding
                        , height = toFloat h - scenePadding
                    }

                player =
                    model.player

                newPlayer =
                    { player
                        | y = newScene.height - player.height
                        , x = 0
                    }
            in
            ( { model
                | scene = newScene
                , player = newPlayer
              }
            , Cmd.none
            )

        MovePlayer direction ->
            let
                player =
                    model.player

                scene =
                    model.scene
            in
            case direction of
                Left ->
                    let
                        calculatedX =
                            player.x - 10

                        newX =
                            if calculatedX <= 0 then
                                0

                            else
                                calculatedX
                    in
                    ( { model | player = { player | x = newX } }, Cmd.none )

                Right ->
                    let
                        calculatedX =
                            player.x + 10

                        newX =
                            if calculatedX > scene.width - player.width then
                                scene.width - player.width

                            else
                                calculatedX
                    in
                    ( { model | player = { player | x = newX } }, Cmd.none )

                Up ->
                    ( model, Cmd.none )

                Other ->
                    ( model, Cmd.none )

        MoveEnemies _ ->
            let
                enemiesX =
                    model.enemiesPos.x

                enemiesY =
                    model.enemiesPos.y

                enemiesDeltaX =
                    model.enemiesDelta.x

                enemiesDeltaY =
                    model.enemiesDelta.y

                scene =
                    model.scene

                newEnemiesX =
                    enemiesX + model.enemiesDelta.x

                newEnemiesDeltaX =
                    if newEnemiesX > scene.width - enemiesGroupWidth || newEnemiesX < 0 then
                        enemiesDeltaX * -1

                    else
                        enemiesDeltaX

                correctedNewEnemiesX =
                    if newEnemiesX > scene.width - enemiesGroupWidth then
                        scene.width - enemiesGroupWidth

                    else if newEnemiesX < 0 then
                        0

                    else
                        newEnemiesX
            in
            ( { model
                | enemiesPos =
                    { x = correctedNewEnemiesX
                    , y = enemiesY
                    }
                , enemiesDelta =
                    { x = newEnemiesDeltaX
                    , y = enemiesDeltaY
                    }
              }
            , Cmd.none
            )


view : Model -> Document Msg
view { scene, player, enemies, enemiesPos } =
    let
        enemiesRect =
            enemies
                |> List.map
                    (\e ->
                        shapes [ fill Color.red ]
                            [ rect
                                ( e.x + enemiesPos.x
                                , e.y + enemiesPos.y
                                )
                                e.width
                                e.height
                            ]
                    )

        playerRect =
            shapes [ fill Color.white ]
                [ rect
                    ( player.x
                    , player.y
                    )
                    player.width
                    player.height
                ]

        clearRect =
            clear ( 0, 0 ) scene.width scene.height
    in
    { title = "Lang Invaders"
    , body =
        [ Canvas.toHtml ( scene.width |> floor, scene.height |> floor ) [] (clearRect :: playerRect :: enemiesRect)
        ]
    }


type Direction
    = Left
    | Right
    | Other
    | Up


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

        "ArrowUp" ->
            Up

        _ ->
            Other


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ onResize SceneResize
        , onKeyDown keyDecoder |> Sub.map MovePlayer
        , Time.every 100 MoveEnemies
        ]
