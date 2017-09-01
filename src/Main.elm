module Main exposing (..)

import Html exposing (Html)
import Html.Attributes exposing (style)
import Html.Events as Events
import List
import List.Extra as List
import Ports
import Types exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { init = ( init, Cmd.none )
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }


empty : Board
empty =
    Board Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing


init : Model
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
winner (Board a b c d e f g h i) =
    let
        options =
            [ ( a, b, c )
            , ( d, e, f )
            , ( g, h, i )
            , ( a, d, g )
            , ( b, e, h )
            , ( c, f, i )
            , ( a, e, i )
            , ( c, e, g )
            ]

        checkTiles ( a, b, c ) =
            if a == b && b == c then
                a
            else
                Nothing

        isJust v =
            v |> (==) Nothing |> not
    in
    options
        |> List.map checkTiles
        |> List.find isJust
        |> Maybe.withDefault Nothing


view : Model -> Html Msg
view model =
    case ( winner model.board, model.board ) of
        ( Just player, _ ) ->
            Html.text ""

        ( _, Board a b c d e f g h i ) ->
            [ [ a, b, c ], [ d, e, f ], [ g, h, i ] ]
                |> List.indexedMap viewRow
                |> Html.div []


viewRow : Int -> List Tile -> Html Msg
viewRow row tiles =
    tiles
        |> List.indexedMap (\column tile -> viewTile row column tile)
        |> Html.div []


viewTile : Int -> Int -> Tile -> Html Msg
viewTile row column tile =
    let
        action =
            row
                |> (*) 3
                |> (+) column
                |> (+) 1
                |> Click

        tileStyle =
            [ ( "width", "96px" )
            , ( "height", "96px" )
            , ( "display", "inline-block" )
            , ( "background", "lightgray" )
            , ( "margin", "8px" )
            , ( "overflow", "hidden" )
            , ( "text-align", "center" )
            , ( "line-height", "96px" )
            ]
    in
    case tile of
        Just Cross ->
            Html.div [ style tileStyle ] [ Html.text "X" ]

        Just Circle ->
            Html.div [ style tileStyle ] [ Html.text "O" ]

        Nothing ->
            Html.div
                [ Events.onClick action
                , style tileStyle
                ]
                []
