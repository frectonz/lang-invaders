module Main exposing (main)

import Browser exposing (Document)
import Browser.Dom exposing (Viewport, getViewport)
import Browser.Events exposing (onAnimationFrame, onKeyDown, onResize)
import Canvas exposing (clear, shapes)
import Color
import Config
import Json.Decode as Decode exposing (Decoder)
import Object exposing (Object)
import Task
import Vector


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
    , missile : Maybe Object
    }


type alias Scene =
    { width : Float
    , height : Float
    }


initEnemies : List Object
initEnemies =
    List.repeat (Config.rows * Config.cols) 0
        |> List.indexedMap
            (\i _ ->
                let
                    row =
                        i // Config.cols

                    col =
                        i - (row * Config.cols)

                    x =
                        toFloat col * (Config.enemySize + Config.enemiesGap)

                    y =
                        toFloat row * (Config.enemySize + Config.enemiesGap)
                in
                Object.make
                    (Vector.make x y)
                    Config.enemyVelocity
                    Config.enemySize
                    Config.enemySize
            )


initPlayer : Object
initPlayer =
    Object.withWidthAndHeight Config.playerVelocity Config.playerWidth Config.playerHeight


initScene : Scene
initScene =
    { width = 0, height = 0 }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { player = initPlayer
      , scene = initScene
      , enemies = initEnemies
      , missile = Nothing
      }
    , Task.perform GotViewPort getViewport
    )


type Msg
    = GotViewPort Viewport
    | SceneResize Int Int
    | MovePlayer Direction
    | Tick


updateWidthAndHeight : Float -> Float -> Model -> Model
updateWidthAndHeight width height model =
    let
        newScene =
            { width = width - Config.scenePadding
            , height = height - Config.scenePadding
            }

        newPlayerPos =
            Vector.make 0 (newScene.height - Config.playerHeight)
    in
    { model
        | scene = newScene
        , player = Object.withPos newPlayerPos model.player
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotViewPort { scene } ->
            ( updateWidthAndHeight scene.width scene.height model
            , Cmd.none
            )

        SceneResize w h ->
            ( updateWidthAndHeight (toFloat w) (toFloat h) model
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
                    ( { model
                        | player =
                            player
                                |> Object.moveLeft
                                |> Object.clampX 0 scene.width
                      }
                    , Cmd.none
                    )

                Right ->
                    ( { model
                        | player =
                            player
                                |> Object.moveRight
                                |> Object.clampX 0 scene.width
                      }
                    , Cmd.none
                    )

                Up ->
                    let
                        missleX =
                            Object.midpointX player

                        missleY =
                            Object.getY player

                        missle =
                            Object.make (Vector.make missleX missleY) Config.missleVelocity Config.missleWidth Config.missleHeight

                        newMissile =
                            model.missile |> Maybe.withDefault missle
                    in
                    ( { model | missile = Just newMissile }, Cmd.none )

                Other ->
                    ( model, Cmd.none )

        Tick ->
            let
                scene =
                    model.scene

                newEnemies =
                    model.enemies
                        |> List.map Object.update
                        |> List.indexedMap
                            (\i e ->
                                let
                                    row =
                                        i // Config.cols

                                    col =
                                        i - (row * Config.cols)

                                    diffToSceneEnd =
                                        (Config.cols - col) * (Config.enemySize + Config.enemiesGap) |> toFloat

                                    diffToSceneStart =
                                        col * (Config.enemySize + Config.enemiesGap) |> toFloat
                                in
                                if Object.getX e + diffToSceneEnd > scene.width || Object.getX e - diffToSceneStart < 0 then
                                    Object.flipX e

                                else
                                    e
                            )

                missile =
                    model.missile

                newMissile =
                    missile
                        |> Maybe.map Object.moveUp

                sceneObject =
                    Object.withWidthAndHeight Vector.zero scene.width scene.height

                -- collion detection with scene
                inScene =
                    newMissile
                        |> Maybe.map (Object.contains sceneObject)
                        |> Maybe.withDefault False

                -- collision detection with enemies
                colliededEnemy =
                    newMissile
                        |> Maybe.andThen
                            (\m ->
                                newEnemies
                                    |> List.indexedMap
                                        (\i e ->
                                            if Object.overlaps e m && Object.isVisible e then
                                                Just i

                                            else
                                                Nothing
                                        )
                                    |> List.filterMap identity
                                    |> List.head
                            )

                newEnemiesFiltered =
                    case colliededEnemy of
                        Just i ->
                            let
                                _ =
                                    Debug.log "collison" i
                            in
                            newEnemies
                                |> List.indexedMap
                                    (\j e ->
                                        if i == j then
                                            Object.kill e

                                        else
                                            e
                                    )

                        Nothing ->
                            newEnemies

                checkedMissile =
                    if inScene && colliededEnemy == Nothing then
                        newMissile

                    else
                        Nothing
            in
            ( { model
                | missile = checkedMissile
                , enemies = newEnemiesFiltered
              }
            , Cmd.none
            )


view : Model -> Document Msg
view { enemies, player, missile, scene } =
    let
        enemiesRect =
            enemies
                |> List.map (Object.view Color.red)

        playerRect =
            Object.view Color.blue player

        missileRect =
            missile
                |> Maybe.map (Object.view Color.green)
                |> Maybe.withDefault (shapes [] [])

        clearRect =
            clear ( 0, 0 ) scene.width scene.height
    in
    { title = "Lang Invaders"
    , body =
        [ Canvas.toHtml ( scene.width |> floor, scene.height |> floor )
            []
            (clearRect :: missileRect :: playerRect :: enemiesRect)
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
        , onAnimationFrame (\_ -> Tick)
        ]
