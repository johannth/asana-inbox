port module LocalStorage exposing (..)

import Json.Decode as Decode
import Json.Encode as Encode


type alias LocalStorageItem =
    { key : String
    , value : Maybe String
    }


encodeToItem : String -> Encode.Value -> LocalStorageItem
encodeToItem key value =
    LocalStorageItem key (Just (Encode.encode 0 value))


decodeToType : LocalStorageItem -> Decode.Decoder a -> Maybe a
decodeToType item decoder =
    case item.value of
        Just value ->
            Result.toMaybe (Decode.decodeString decoder value)

        Nothing ->
            Nothing


port setItem : LocalStorageItem -> Cmd msg


port getItem : String -> Cmd msg


port receiveItem : (LocalStorageItem -> msg) -> Sub msg
