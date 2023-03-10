module Main exposing (main)

import Browser exposing (Document)
import Browser.Dom exposing (Viewport, getViewport)
import Browser.Events exposing (onAnimationFrame, onResize)
import Canvas exposing (Renderable, clear, shapes)
import Canvas.Texture exposing (Texture, loadFromImageUrl)
import Color
import Config
import Html exposing (Html, button, div, h1, img, p, text)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)
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


type Model
    = Start Language
    | Playing Game
    | Paused


type Language
    = Python
    | JavaScript


type alias Game =
    { scene : Scene
    , player : Object
    , enemies : List Object
    , missle : Maybe Object
    , sprites : Sprites
    , enemyType : Language
    }


type alias Scene =
    { width : Float
    , height : Float
    }


type alias Sprites =
    { javascript : Load Texture
    , python : Load Texture
    , missle : Load Texture
    }


type Load a
    = Loading
    | Failure
    | Success a


init : () -> ( Model, Cmd Msg )
init _ =
    ( Start JavaScript
    , Cmd.none
    )


initGame : Language -> Game
initGame lang =
    { scene = initScene
    , player = initPlayer
    , enemies = initEnemies
    , missle = Nothing
    , sprites = initSprites
    , enemyType = lang
    }


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


initSprites : Sprites
initSprites =
    { javascript = Loading
    , python = Loading
    , missle = Loading
    }


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
    = TextureLoaded TextureType (Maybe Texture)
    | GotViewPort Viewport
    | SceneResize Int Int
    | MovePlayer Direction
    | SelectLanguage Language
    | StartGame
    | Tick


type TextureType
    = JavaScriptTexture
    | PythonTexture
    | MissleTexture


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
    case model of
        Start lang ->
            case msg of
                SelectLanguage newLang ->
                    ( Start newLang, Cmd.none )

                StartGame ->
                    ( Playing (initGame lang), Task.perform GotViewPort getViewport )

                _ ->
                    ( model, Cmd.none )

        Paused ->
            ( model, Cmd.none )

        Playing game ->
            updateGame msg game |> Tuple.mapFirst Playing


updateGame : Msg -> Game -> ( Game, Cmd Msg )
updateGame msg model =
    case msg of
        TextureLoaded textureType maybeTexture ->
            ( updateTexture textureType maybeTexture model
            , Cmd.none
            )

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

        _ ->
            ( model, Cmd.none )


updateTexture : TextureType -> Maybe Texture -> Game -> Game
updateTexture textureType maybeTexture model =
    let
        sprites =
            model.sprites
    in
    case textureType of
        JavaScriptTexture ->
            { model | sprites = { sprites | javascript = handleTextureLoad maybeTexture } }

        PythonTexture ->
            { model | sprites = { sprites | python = handleTextureLoad maybeTexture } }

        MissleTexture ->
            { model | sprites = { sprites | missle = handleTextureLoad maybeTexture } }


handleTextureLoad : Maybe Texture -> Load Texture
handleTextureLoad maybeTexture =
    case maybeTexture of
        Just texture ->
            Success texture

        Nothing ->
            Failure


updateScene : Float -> Float -> Game -> Game
updateScene width height model =
    let
        newScene =
            { width = width - Config.scenePadding.x
            , height = height - Config.scenePadding.y
            }

        newPlayerPos =
            Vector.make 0 (newScene.height - Config.playerHeight)
    in
    { model
        | scene = newScene
        , player = Object.withPos newPlayerPos model.player
    }


movePlayer : Direction -> Game -> Game
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
                    Object.midpointX model.player - Config.missleWidth / 2

                missle =
                    Object.make
                        (Vector.make missleX missleY)
                        Config.missleVelocity
                        Config.missleWidth
                        Config.missleHeight

                newMissle =
                    model.missle |> Maybe.withDefault missle
            in
            { model | missle = Just newMissle }

        Other ->
            model


tick : Game -> Game
tick model =
    let
        sceneObject =
            Object.withWidthAndHeight
                Vector.zero
                model.scene.width
                model.scene.height

        newMissle =
            model.missle
                |> Maybe.map Object.moveUp

        inScene =
            newMissle
                |> Maybe.map (Object.contains sceneObject)
                |> Maybe.withDefault False

        newEnemies =
            model.enemies
                |> List.map Object.update
                |> List.indexedMap (handleEnemyOverflow model.scene.width)

        colliededEnemy =
            newMissle
                |> Maybe.andThen (getFirstOveralpingEnemy newEnemies)

        newEnemiesFiltered =
            colliededEnemy
                |> Maybe.map (killEnemyAtIndex newEnemies)
                |> Maybe.withDefault newEnemies

        checkedMissle =
            if inScene && colliededEnemy == Nothing then
                newMissle

            else
                Nothing
    in
    { model
        | missle = checkedMissle
        , enemies = newEnemiesFiltered
    }


subscriptionsGame : Game -> Sub Msg
subscriptionsGame _ =
    Sub.batch
        [ onResize SceneResize
        , handleKeyDown |> Sub.map MovePlayer
        , onAnimationFrame (\_ -> Tick)
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Start _ ->
            Sub.none

        Paused ->
            Sub.none

        Playing game ->
            subscriptionsGame game


view : Model -> Document Msg
view model =
    { title = "Lang Invaders"
    , body =
        case model of
            Start lang ->
                [ viewStart lang ]

            Paused ->
                []

            Playing game ->
                [ viewGame game ]
    }


activeIf : Language -> Language -> String
activeIf lang1 lang2 =
    if lang1 == lang2 then
        "active"

    else
        ""


viewStart : Language -> Html Msg
viewStart lang =
    div [ class "container" ]
        [ h1 [ class "title" ] [ text "Lang Invaders" ]
        , p [ class "detail" ] [ text "Choose a language to Kill" ]
        , div [ class "buttons" ]
            [ button
                [ class ("button " ++ activeIf lang JavaScript)
                , onClick (SelectLanguage JavaScript)
                ]
                [ img [ src "./js.jpg" ] [] ]
            , button
                [ class ("button " ++ activeIf lang Python)
                , onClick (SelectLanguage Python)
                ]
                [ img [ src "./python.png" ] [] ]
            ]
        , button [ class "start", onClick StartGame ] [ text "START" ]
        ]


viewGame : Game -> Html Msg
viewGame { enemies, player, missle, scene, sprites, enemyType } =
    Canvas.toHtmlWith
        { width = floor scene.width
        , height = floor scene.height
        , textures =
            [ loadFromImageUrl "./js.jpg" (TextureLoaded JavaScriptTexture)
            , loadFromImageUrl "./python.png" (TextureLoaded PythonTexture)
            , loadFromImageUrl "./missle.png" (TextureLoaded MissleTexture)
            ]
        }
        []
        (clearScene scene.width scene.height
            :: viewMissle sprites.missle missle
            :: viewPlayer player
            :: viewEnemies (getSprite enemyType sprites) enemies
        )


getSprite : Language -> Sprites -> Load Texture
getSprite lang sprites =
    case lang of
        JavaScript ->
            sprites.javascript

        Python ->
            sprites.python


clearScene : Float -> Float -> Renderable
clearScene w h =
    clear ( 0, 0 ) w h


viewMissle : Load Texture -> Maybe Object -> Renderable
viewMissle s missle =
    case s of
        Success sprite ->
            missle
                |> Maybe.map (Object.viewSprite sprite)
                |> Maybe.withDefault (shapes [] [])

        _ ->
            missle
                |> Maybe.map (Object.view Color.green)
                |> Maybe.withDefault (shapes [] [])


viewEnemies : Load Texture -> List Object -> List Renderable
viewEnemies s enemies =
    case s of
        Success sprite ->
            enemies
                |> List.map (Object.viewSprite sprite)

        _ ->
            []


viewPlayer : Object -> Renderable
viewPlayer player =
    Object.view Color.blue player
