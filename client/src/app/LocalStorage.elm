port module LocalStorage exposing (..)

import Json.Decode as Decode
import Json.Encode as Encode


type alias LocalStorageItem =
    { key : String
    , value : Maybe String
    }


setItem : String -> a -> (a -> Encode.Value) -> Cmd msg
setItem key value encoder =
    let
        encodedValue =
            encoder value
    in
        setItemRaw (LocalStorageItem key (Just (Encode.encode 0 encodedValue)))


decodeToType : LocalStorageItem -> Decode.Decoder a -> Maybe a
decodeToType item decoder =
    case item.value of
        Just value ->
            Result.toMaybe (Decode.decodeString decoder value)

        Nothing ->
            Nothing


port setItemRaw : LocalStorageItem -> Cmd msg


port getItem : String -> Cmd msg


port receiveItem : (LocalStorageItem -> msg) -> Sub msg
