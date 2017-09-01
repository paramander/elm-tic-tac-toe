port module Ports exposing (persistModel, updateModel)

import Json.Encode


port persistModel : Json.Encode.Value -> Cmd msg


port updateModel : (Json.Encode.Value -> msg) -> Sub msg
