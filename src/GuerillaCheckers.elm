module GuerillaCheckers exposing (..)

import GuerillaCheckersBoard exposing (..)
import Browser
import Html exposing (..)
import Html.Events exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)

-- MAIN

main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

-- MODEL

type alias Model =
  { board : BoardState
  , log : List String
  }

initBoard : Model
initBoard =
  Model
    (BoardState
        [ (2, 3)
        , (3, 4)
        , (4, 5)
        , (3, 2)
        , (4, 3)
        , (5, 4)
        ]
        []
        66
        Nothing
        Guerilla
        [])
    []

init : () -> (Model, Cmd Msg)
init _ = (initBoard, Cmd.none)

-- UPDATE

type Msg
  = SelectCoin (Int, Int)
  | DeselectCoin
  | MoveCoin (Int, Int)
  | PlaceGuerilla (Int, Int)
  | ResetBoard

-- TODO: Boundary check on these guys on both ends

stringFromTuple : (Int, Int) -> String
stringFromTuple (x,y) = "(" ++ (String.fromInt x) ++ "," ++ (String.fromInt y) ++ ")"

stringFromMessage : BoardState -> Msg -> String
stringFromMessage { selectedCoin, guerillasRemaining } msg =
  case msg of
    MoveCoin dst ->
      case selectedCoin of
        Just src -> "COIN: " ++ stringFromTuple src ++ " -> " ++ stringFromTuple dst
        Nothing  -> ""
    PlaceGuerilla pos ->
      "GUERILLA: " ++ stringFromTuple pos ++
      " (left: " ++ String.fromInt guerillasRemaining ++ ")"
    _ ->
      ""

stringFromMaybe : (a -> String) -> Maybe a -> String
stringFromMaybe f ma =
  Maybe.map f ma
  |> Maybe.withDefault ""

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let
    logMessage = stringFromMessage model.board msg
  in
  case msg of
    SelectCoin pos ->
      ( { model | board = selectCoin pos model.board }
      , Cmd.none
      )
    DeselectCoin ->
      ( { model | board = deselectCoin model.board }
      , Cmd.none
      )
    MoveCoin newpos ->
      let
        (newboard, captured) = moveSelectedCoin newpos model.board
        fullMessage =
          logMessage
          ++ stringFromMaybe (\pos -> " (captured: " ++ stringFromTuple pos ++ ")") captured
      in
      ( { model
        | board = newboard
        , log = if String.isEmpty fullMessage then model.log else fullMessage :: model.log
        }
      , Cmd.none
      )
    PlaceGuerilla pos ->
      let
          (newboard, captured) = placeGuerilla pos model.board
          captureMessage =
            if List.isEmpty captured then
              ""
            else
              " (captured: "
              ++ (List.map stringFromTuple captured |> String.join ",")
              ++ ")"
          fullMessage = logMessage ++ captureMessage
      in
      ( { model
        | board = newboard
        , log = if String.isEmpty fullMessage then model.log else fullMessage :: model.log
        }
      , Cmd.none
      )
    ResetBoard ->
      init ()

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- VIEW

type Color = White | Black

type alias ColorConfig =
  { border : String
  , coin : String
  , selectedCoin : String
  , guerilla : String
  }

colorConfig : ColorConfig
colorConfig =
  { border = "white"
  , coin = "red"
  , selectedCoin = "yellow"
  , guerilla = "black"
  }

color : Color -> String
color c = case c of
  White -> "lightgray"
  Black -> "gray"

newBoardSq : Color -> (Int, Int) -> Svg Msg
newBoardSq c (xcoord,ycoord) =
  let
    (xcoord2,ycoord2) = fromSquareCoords (xcoord,ycoord)
  in
  rect
    [ x <| String.fromInt xcoord2
    , y <| String.fromInt ycoord2
    , width "50"
    , height "50"
    , stroke colorConfig.border
    , strokeWidth "4"
    , fill <| color c
    ]
    []

newBoardNode : (Int, Int) -> Svg Msg
newBoardNode pos =
  let
    (bx,by) = fromNodeCoords pos
  in
  circle
  [ cx <| String.fromInt bx
  , cy <| String.fromInt by
  , r "5"
  , fill colorConfig.border
  , stroke colorConfig.border
  ]
  []

newGuerilla : (Int, Int) -> Svg Msg
newGuerilla pos =
  let
    (bx,by) = fromNodeCoords pos
  in
  circle
  [ cx <| String.fromInt bx
  , cy <| String.fromInt by
  , r "10"
  , fill colorConfig.guerilla
  , stroke colorConfig.guerilla
  ]
  []

newGuerillaPlacement : (Int, Int) -> Svg Msg
newGuerillaPlacement pos =
  let
    (bx,by) = fromNodeCoords pos
  in
  circle
  [ cx <| String.fromInt bx
  , cy <| String.fromInt by
  , r "10"
  , stroke "transparent"
  --, fill "yellow"
  , fill "transparent"
  , fillOpacity "0.3"
  , onClick (PlaceGuerilla pos)
  ]
  []

newCoin : Bool -> Turn -> (Int, Int) -> Svg Msg
newCoin selected turn pos =
  let
    (bx,by) = fromCoinCoords pos
    c = if selected then colorConfig.selectedCoin else colorConfig.coin
    eh = if turn == Coin then [ onClick (if selected then DeselectCoin else SelectCoin pos) ] else []
  in
  circle
  ( List.append
    [ cx <| String.fromInt bx
    , cy <| String.fromInt by
    , r "20"
    , fill c
    , stroke c
    ]
    eh
  )
  []

newClickableSq : (Int, Int) -> Svg Msg
newClickableSq (xcoord,ycoord) =
  let
    (xcoordr,ycoordr) = fromSquareCoords (xcoord,ycoord)
  in
  rect
  [ x <| String.fromInt xcoordr
  , y <| String.fromInt ycoordr
  , width "50"
  , height "50"
  , stroke "transparent"
  , strokeWidth "0"
  , fill colorConfig.coin
  , fillOpacity "0.2"
  , onClick (MoveCoin (xcoord,ycoord))
  ]
  []

newSelectedCoin : Turn -> (Int, Int) -> Svg Msg
newSelectedCoin = newCoin True

newUnselectedCoin : Turn -> (Int, Int) -> Svg Msg
newUnselectedCoin = newCoin False

fromNodeCoords : (Int, Int) -> (Int, Int)
fromNodeCoords (x, y) =
  ( (x+1) * 50
  , (y+1) * 50
  )

fromSquareCoords : (Int, Int) -> (Int, Int)
fromSquareCoords (x, y) =
  ( x * 50
  , y * 50
  )

fromCoinCoords : (Int, Int) -> (Int, Int)
fromCoinCoords (x, y) =
  ( x * 50 + 25
  , y * 50 + 25
  )

baseBoard : List (Svg Msg)
baseBoard =
  let
    coord2Color (x,y) = if modBy 2 (x+y) == 0 then Black else White
    makeSq pos = newBoardSq (coord2Color pos) pos
    board = List.map makeSq squareCoords
    nodes = List.map newBoardNode nodeCoords
  in
    List.append board nodes

generateBoard : BoardState -> List (Svg Msg)
generateBoard
  { coins, selectedCoin, guerillas, turn } =
  let
    selected =
      case selectedCoin of
        Nothing  -> []
        Just pos -> List.filter ((==) pos) coins |> List.map (newSelectedCoin turn)
    unselected =
      case selectedCoin of
        Nothing  -> coins |> List.map (newUnselectedCoin turn)
        Just pos -> List.filter ((/=) pos) coins |> List.map (newUnselectedCoin turn)
  in
  [ baseBoard
  , selected
  , unselected
  , List.map newGuerilla guerillas
  ] |> List.concat

generateCoinTargets : BoardState -> List (Svg Msg)
generateCoinTargets { coins, selectedCoin } =
  let
    flip f b a = f a b
  in
  case selectedCoin of
    Just spos ->
      coinNeighbors spos
      |> List.filter (not << flip List.member coins)
      |> List.map newClickableSq
    Nothing ->
      []

generateGuerillaPlacements : BoardState -> List (Svg Msg)
generateGuerillaPlacements =
  availableGuerillaPlacements >> List.map newGuerillaPlacement

fromTurn : Turn -> String
fromTurn t =
  case t of
    Guerilla -> "Guerilla"
    Coin -> "Coin"

view : Model -> Html Msg
view model =
  div []
    [ div []
      [ svg
        [ width "400"
        , height "400"
        ]
        (List.concat <|
          [ (generateBoard model.board)
          , (generateCoinTargets model.board)
          , (generateGuerillaPlacements model.board)
          ]
        )
      ]
    , div [] [ button [ onClick ResetBoard ] [ Html.text "Reset" ] ]
    , div [] [ Html.text <| "Turn: " ++ fromTurn model.board.turn ]
    , div [] [ Html.text <| "Coins remaining: " ++ String.fromInt (List.length model.board.coins) ]
    , div [] [ Html.text <| "Guerillas remaining: " ++ String.fromInt model.board.guerillasRemaining ]
    , div [] [ Html.text <| "Log:" ]
    , div [] [ ul [] <| List.map (\l -> li [] [ Html.text l ]) model.log ]
    ]
