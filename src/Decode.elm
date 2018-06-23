module Decode
    exposing
        ( required
        , requiredAt
        )

import Json.Decode as Decode exposing (Decoder)


required field decoder previousDecoder =
    Decode.map2 apply previousDecoder (Decode.field field decoder)


requiredAt path decoder previousDecoder =
    Decode.map2 apply previousDecoder (Decode.at path decoder)


apply f a =
    f a
