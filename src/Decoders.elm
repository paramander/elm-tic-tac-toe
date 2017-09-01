module Decoders exposing (..)

import Json.Decode as JD exposing (Decoder)
import Types exposing (..)


decode : Decoder Model
decode =
    JD.map2 Model
        (JD.field "board" decodeBoard)
        (JD.field "player" (JD.andThen decodePlayer JD.string))


decodeBoard : Decoder Board
decodeBoard =
    JD.list decodeTile
        |> JD.andThen decodeBoardFromList


decodeBoardFromList : List Tile -> Decoder Board
decodeBoardFromList tileList =
    case tileList of
        a :: b :: c :: d :: e :: f :: g :: h :: i :: [] ->
            Board a b c d e f g h i
                |> JD.succeed

        _ ->
            JD.fail "No board with 9 tiles supplied to Board decoder"


decodeTile : Decoder Tile
decodeTile =
    JD.maybe (JD.andThen decodePlayer JD.string)


decodePlayer : String -> Decoder Player
decodePlayer p =
    case p of
        "x" ->
            JD.succeed Cross

        "o" ->
            JD.succeed Circle

        _ ->
            JD.fail "No 'x' or 'o' given in JSON structure for a Tile/Player"
