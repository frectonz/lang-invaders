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
    { scene : Scene
    , player : Object
    , enemies : List Object
    , missile : Maybe Object
    }


type alias Scene =
    { width : Float
    , height : Float
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { scene = initScene
      , player = initPlayer
      , enemies = initEnemies
      , missile = Nothing
      }
    , Task.perform GotViewPort getViewport
    )


initScene : Scene
initScene =
    { width = 0, height = 0 }


initPlayer : Object
initPlayer =
    Object.withWidthAndHeight
        Config.playerVelocity
        Config.playerWidth
        Config.playerHeight


initEnemies : List Object
initEnemies =
    List.repeat (Config.rows * Config.cols) 0
        |> List.indexedMap
            (\i _ ->
                let
                    x =
                        toFloat (getCol i) * (Config.enemySize + Config.enemiesGap)

                    y =
                        toFloat (getRow i) * (Config.enemySize + Config.enemiesGap)
                in
                Object.make
                    (Vector.make x y)
                    Config.enemyVelocity
                    Config.enemySize
                    Config.enemySize
            )


getRow : Int -> Int
getRow i =
    i // Config.cols


getCol : Int -> Int
getCol i =
    i - (getRow i * Config.cols)


type Msg
    = GotViewPort Viewport
    | SceneResize Int Int
    | MovePlayer Direction
    | Tick


handleEnemyOverflow : Float -> Int -> Object -> Object
handleEnemyOverflow sceneWidth i e =
    let
        diffToSceneEnd =
            (Config.cols - getCol i) * (Config.enemySize + Config.enemiesGap) |> toFloat

        diffToSceneStart =
            getCol i * (Config.enemySize + Config.enemiesGap) |> toFloat

        rightOverflow =
            Object.getX e + diffToSceneEnd > sceneWidth

        leftOverflow =
            Object.getX e - diffToSceneStart < 0
    in
    if rightOverflow || leftOverflow then
        Object.flipX e

    else
        e


findOverlapingEnemy : Object -> Int -> Object -> Maybe Int
findOverlapingEnemy m i e =
    if Object.overlaps m e && Object.isVisible e then
        Just i

    else
        Nothing


killEnemyAtIndex : List Object -> Int -> List Object
killEnemyAtIndex enemies i =
    enemies
        |> List.indexedMap
            (\j e ->
                if i == j then
                    Object.kill e

                else
                    e
            )


getFirstOveralpingEnemy : List Object -> Object -> Maybe Int
getFirstOveralpingEnemy enemies m =
    enemies
        |> List.indexedMap (findOverlapingEnemy m)
        |> List.filterMap identity
        |> List.head


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotViewPort { scene } ->
            ( updateScene scene.width scene.height model
            , Cmd.none
            )

        SceneResize w h ->
            ( updateScene (toFloat w) (toFloat h) model
            , Cmd.none
            )

        MovePlayer direction ->
            ( movePlayer direction model, Cmd.none )

        Tick ->
            ( tick model, Cmd.none )


updateScene : Float -> Float -> Model -> Model
updateScene width height model =
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


movePlayer : Direction -> Model -> Model
movePlayer direction model =
    case direction of
        Left ->
            { model
                | player =
                    model.player
                        |> Object.moveLeft
                        |> Object.clampX 0 model.scene.width
            }

        Right ->
            { model
                | player =
                    model.player
                        |> Object.moveRight
                        |> Object.clampX 0 model.scene.width
            }

        Up ->
            let
                missleY =
                    Object.getY model.player

                missleX =
                    Object.midpointX model.player

                missle =
                    Object.make
                        (Vector.make missleX missleY)
                        Config.missleVelocity
                        Config.missleWidth
                        Config.missleHeight

                newMissile =
                    model.missile |> Maybe.withDefault missle
            in
            { model | missile = Just newMissile }

        Other ->
            model


tick : Model -> Model
tick model =
    let
        sceneObject =
            Object.withWidthAndHeight
                Vector.zero
                model.scene.width
                model.scene.height

        newMissile =
            model.missile
                |> Maybe.map Object.moveUp

        inScene =
            newMissile
                |> Maybe.map (Object.contains sceneObject)
                |> Maybe.withDefault False

        newEnemies =
            model.enemies
                |> List.map Object.update
                |> List.indexedMap (handleEnemyOverflow model.scene.width)

        colliededEnemy =
            newMissile
                |> Maybe.andThen (getFirstOveralpingEnemy newEnemies)

        newEnemiesFiltered =
            colliededEnemy
                |> Maybe.map (killEnemyAtIndex newEnemies)
                |> Maybe.withDefault newEnemies

        checkedMissile =
            if inScene && colliededEnemy == Nothing then
                newMissile

            else
                Nothing
    in
    { model
        | missile = checkedMissile
        , enemies = newEnemiesFiltered
    }


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
