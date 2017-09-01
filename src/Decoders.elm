module Decoders exposing (..)

import Json.Decode as JD exposing (Decoder)
import Types exposing (..)


decode : Decoder Model
decode =
    JD.map2 Model
        (JD.field "board" decodeBoard)
        (JD.field "currentPlayer" (JD.andThen decodePlayer JD.string))


decodeBoard : Decoder Board
decodeBoard =
    JD.list (JD.andThen decodeTile JD.string)
        |> JD.andThen decodeBoardFromList


decodeBoardFromList : List Tile -> Decoder Board
decodeBoardFromList tileList =
    case tileList of
        a :: b :: c :: d :: e :: f :: g :: h :: i :: [] ->
            Board a b c d e f g h i
                |> JD.succeed

        _ ->
            JD.fail "No board with 9 tiles supplied to Board decoder"


decodeTile : String -> Decoder Tile
decodeTile p =
    case p of
        "_" ->
            JD.succeed Nothing

        _ ->
            JD.maybe (decodePlayer p)


decodePlayer : String -> Decoder Player
decodePlayer p =
    case p of
        "x" ->
            JD.succeed Cross

        "o" ->
            JD.succeed Circle

        _ ->
            JD.fail "No 'x' or 'o' given in JSON structure for a Tile/Player"
