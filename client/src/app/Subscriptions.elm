module Subscriptions exposing (..)

import Types exposing (..)
import LocalStorage


subscriptions : Model -> Sub Msg
subscriptions =
    \_ -> LocalStorage.receiveItem ReceiveItem
