module Encoders exposing (..)

import Json.Encode as JE
import Types exposing (..)


encode : Model -> JE.Value
encode model =
    JE.object
        [ ( "board", encodeBoard model.board )
        , ( "currentPlayer", encodePlayer <| otherPlayer model.currentPlayer )
        ]


encodeBoard : Board -> JE.Value
encodeBoard (Board a b c d e f g h i) =
    JE.list
        []


encodeTile : Tile -> JE.Value
encodeTile tile =
    tile
        |> Maybe.map encodePlayer
        |> Maybe.withDefault JE.null


encodePlayer : Player -> JE.Value
encodePlayer p =
    case p of
        Cross ->
            JE.string "x"

        Circle ->
            JE.string "o"


otherPlayer : Player -> Player
otherPlayer p =
    case p of
        Cross ->
            Circle

        Circle ->
            Cross
