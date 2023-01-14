module KeyDecoder exposing (Direction(..), handleKeyDown)

import Browser.Events exposing (onKeyDown)
import Json.Decode as Decode exposing (Decoder)


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


handleKeyDown : Sub Direction
handleKeyDown =
    onKeyDown keyDecoder
