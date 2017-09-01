module Encoders exposing (..)

import Json.Encode as JE
import Types exposing (..)


encode : Model -> JE.Value
encode model =
    JE.object
        [ ( "board", encodeBoard model.board )
        , ( "currentPlayer", encodePlayer model.currentPlayer )
        ]


encodeBoard : Board -> JE.Value
encodeBoard (Board a b c d e f g h i) =
    JE.list
        [ encodeTile a
        , encodeTile b
        , encodeTile c
        , encodeTile d
        , encodeTile e
        , encodeTile f
        , encodeTile g
        , encodeTile h
        , encodeTile i
        ]


encodeTile : Tile -> JE.Value
encodeTile tile =
    tile
        |> Maybe.map encodePlayer
        |> Maybe.withDefault (JE.string "_")


encodePlayer : Player -> JE.Value
encodePlayer p =
    case p of
        Cross ->
            JE.string "x"

        Circle ->
            JE.string "o"
