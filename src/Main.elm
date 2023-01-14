module Main exposing (main)

import Browser exposing (Document)
import Browser.Dom exposing (Viewport, getViewport)
import Browser.Events exposing (onAnimationFrame, onResize)
import Canvas exposing (Renderable, clear, shapes)
import Color
import Config
import KeyDecoder exposing (Direction(..), handleKeyDown)
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
        |> List.indexedMap (first initEnemy)


first : (a -> b) -> a -> c -> b
first f a _ =
    f a


initEnemy : Int -> Object
initEnemy i =
    Object.make
        (Vector.make (getX i) (getY i))
        Config.enemyVelocity
        Config.enemySize
        Config.enemySize


getX : Int -> Float
getX i =
    toFloat (getCol i) * (Config.enemySize + Config.enemiesGap)


getY : Int -> Float
getY i =
    toFloat (getRow i) * (Config.enemySize + Config.enemiesGap)


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


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ onResize SceneResize
        , handleKeyDown |> Sub.map MovePlayer
        , onAnimationFrame (\_ -> Tick)
        ]


view : Model -> Document Msg
view { enemies, player, missile, scene } =
    { title = "Lang Invaders"
    , body =
        [ Canvas.toHtml ( scene.width |> floor, scene.height |> floor )
            []
            (clearScene scene.width scene.height
                :: viewMissle missile
                :: viewPlayer player
                :: viewEnemies enemies
            )
        ]
    }


clearScene : Float -> Float -> Renderable
clearScene w h =
    clear ( 0, 0 ) w h


viewMissle : Maybe Object -> Renderable
viewMissle missile =
    missile
        |> Maybe.map (Object.view Color.green)
        |> Maybe.withDefault (shapes [] [])


viewEnemies : List Object -> List Renderable
viewEnemies enemies =
    enemies
        |> List.map (Object.view Color.red)


viewPlayer : Object -> Renderable
viewPlayer player =
    Object.view Color.blue player
