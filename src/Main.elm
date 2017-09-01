module Main exposing (..)

import Html exposing (Html)
import Ports
import Types exposing (..)


empty : Board
empty =
    Board Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing


init =
    { board = empty
    , currentPlayer = Cross
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Click position ->
            ( { model
                | board = checkPosition position model.currentPlayer model.board
                , currentPlayer = otherPlayer model.currentPlayer
              }
            , Cmd.none
            )

        None ->
            ( model, Cmd.none )


otherPlayer : Player -> Player
otherPlayer player =
    if player == Cross then
        Circle
    else
        Cross


checkPosition : Int -> Player -> Board -> Board
checkPosition position player (Board a b c d e f g h i) =
    case position of
        1 ->
            Board (Just player) b c d e f g h i

        2 ->
            Board a (Just player) c d e f g h i

        3 ->
            Board a b (Just player) d e f g h i

        4 ->
            Board a b c (Just player) e f g h i

        5 ->
            Board a b c d (Just player) f g h i

        6 ->
            Board a b c d e (Just player) g h i

        7 ->
            Board a b c d e f (Just player) h i

        8 ->
            Board a b c d e f g (Just player) i

        9 ->
            Board a b c d e f g h (Just player)

        _ ->
            Debug.crash "Fix it; This shouldn't be possible"


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

        Board _ (Just a) _ _ (Just b) _ _ (Just c) _ ->
            checkTiles a b c

        _ ->
            Nothing


view : Model -> Html Msg
view model =
    case winner model.board of
        Just player ->
            Html.text ""

        _ ->
            Html.text ""
