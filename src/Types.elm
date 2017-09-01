module Types exposing (..)

import Json.Encode


type Player
    = Cross
    | Circle


type alias Tile =
    Maybe Player


type Board
    = Board Tile Tile Tile Tile Tile Tile Tile Tile Tile


type alias Model =
    { board : Board
    , currentPlayer : Player
    }


type Msg
    = None
    | Click Int
    | UpdateModel Json.Encode.Value
