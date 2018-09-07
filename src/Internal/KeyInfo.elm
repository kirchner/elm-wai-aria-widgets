module Internal.KeyInfo
    exposing
        ( KeyInfo
        , decoder
        )

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode


type alias KeyInfo =
    { code : String
    , altDown : Bool
    , controlDown : Bool
    , metaDown : Bool
    , shiftDown : Bool
    }


decoder : Decoder KeyInfo
decoder =
    Decode.succeed KeyInfo
        |> Decode.required "key" Decode.string
        |> Decode.optional "altDown" Decode.bool False
        |> Decode.optional "ctrlKey" Decode.bool False
        |> Decode.optional "metaKey" Decode.bool False
        |> Decode.optional "shiftKey" Decode.bool False
