module Main exposing (..)


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


empty : Board
empty =
    Board Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing


init =
    { board = empty
    , currentPlayer = Cross
    }


winner : Board -> Maybe Player
winner board =
    let
        checkTiles a b c =
            if a == b && b == c then
                Just a
            else
                Nothing
    in
    case board of
        Board (Just a) (Just b) (Just c) _ _ _ _ _ _ ->
            checkTiles a b c

        Board _ _ _ (Just a) (Just b) (Just c) _ _ _ ->
            checkTiles a b c

        Board _ _ _ _ _ _ (Just a) (Just b) (Just c) ->
            checkTiles a b c

        Board (Just a) _ _ _ (Just b) _ _ _ (Just c) ->
            checkTiles a b c

        Board _ _ (Just a) _ (Just b) _ (Just c) _ _ ->
            checkTiles a b c

        _ ->
            Nothing
