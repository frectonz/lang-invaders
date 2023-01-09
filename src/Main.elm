module Main exposing (main)

import Browser exposing (Document)
import Html exposing (h1, text)


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    {}


init : () -> ( Model, Cmd Msg )
init _ =
    ( {}, Cmd.none )


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


view : Model -> Document Msg
view _ =
    { title = "Lang Invaders"
    , body =
        [ h1 [] [ text "Hello, World!" ]
        ]
    }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
