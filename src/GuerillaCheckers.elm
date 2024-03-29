module GuerillaCheckers exposing (..)

import GuerillaCheckersBoard exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (rows, cols, readonly)
import Html.Events exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)

import Json.Decode as Decode

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
  , mousedGuerillaPlacement : Maybe (Int, Int)
  }

init : () -> (Model, Cmd Msg)
init _ = (Model initBoard [] Nothing, Cmd.none)

-- UPDATE

type Msg
  = SelectCoin (Int, Int)
  | DeselectCoin
  | MoveCoin (Int, Int)
  | PlaceGuerilla (Int, Int)
  | MouseOverGuerillaPlacement (Int, Int)
  | MouseLeaveGuerillaPlacement (Int, Int)
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
      " (left: " ++ String.fromInt (guerillasRemaining-1) ++ ")"
    _ ->
      ""

stringFromMaybe : (a -> String) -> Maybe a -> String
stringFromMaybe f m =
  Maybe.map f m
  |> Maybe.withDefault ""

stringFromPlayer : Player -> String
stringFromPlayer t =
  case t of
    Coin     -> "Coin"
    Guerilla -> "Guerilla"

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let
    newmodel =
      case msg of
        ResetBoard -> updateBoard msg model -- still want this to restart game
        _          ->
          case winner model.board of
            Just _  -> model
            Nothing -> updateBoard msg model
  in
  (newmodel, Cmd.none)

updateBoard : Msg -> Model -> Model
updateBoard msg model =
  let
    logMessage = stringFromMessage model.board msg
  in
  case msg of
    SelectCoin pos ->
      { model | board = selectCoin pos model.board }
    DeselectCoin ->
      { model | board = deselectCoin model.board }
    MoveCoin newpos ->
      let
        (newboard, captured) = moveSelectedCoin newpos model.board
        fullMessage =
          logMessage
          ++ stringFromMaybe (\pos -> " (captured: " ++ stringFromTuple pos ++ ")") captured
      in
      { model
      | board = newboard
      , log = if String.isEmpty fullMessage then model.log else fullMessage :: model.log
      }
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
      { model
      | board = newboard
      , log = if String.isEmpty fullMessage then model.log else fullMessage :: model.log
      , mousedGuerillaPlacement = Nothing
      }
    MouseOverGuerillaPlacement pos ->
      { model | mousedGuerillaPlacement = Just pos }
    MouseLeaveGuerillaPlacement _ ->
      { model | mousedGuerillaPlacement = Nothing }
    ResetBoard ->
      Model initBoard [] Nothing

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
    [ x <| String.fromFloat xcoord2 ++ "em"
    , y <| String.fromFloat ycoord2 ++ "em"
    , width "5em"
    , height "5em"
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
  [ cx <| String.fromFloat bx ++ "em"
  , cy <| String.fromFloat by ++ "em"
  , r "0.5em"
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
  [ cx <| String.fromFloat bx ++ "em"
  , cy <| String.fromFloat by ++ "em"
  , r "1em"
  , fill colorConfig.guerilla
  , stroke colorConfig.guerilla
  ]
  []

newGuerillaPlacement : Maybe (Int, Int) -> (Int, Int) -> Svg Msg
newGuerillaPlacement mousedOver pos =
  let
    c = case mousedOver of
      Nothing -> "transparent"
      Just pos2 ->
        if pos == pos2 then "grey"
        else                       "transparent"
    (bx,by) = fromNodeCoords pos
  in
  circle
  [ cx <| String.fromFloat bx ++ "em"
  , cy <| String.fromFloat by ++ "em"
  , r "1em"
  , stroke "transparent"
  , fill c
  , fillOpacity "0.5"
  , Svg.Events.onClick (PlaceGuerilla pos)
  , Svg.Events.on "mouseover"  (Decode.map MouseOverGuerillaPlacement (Decode.succeed pos))
  , Svg.Events.on "mouseenter" (Decode.map MouseOverGuerillaPlacement (Decode.succeed pos))
  , Svg.Events.on "mouseleave" (Decode.map MouseLeaveGuerillaPlacement (Decode.succeed pos))
  ]
  []

newCoin : Bool -> Player -> (Int, Int) -> Svg Msg
newCoin selected turn pos =
  let
    (bx,by) = fromCoinCoords pos
    c = if selected then colorConfig.selectedCoin else colorConfig.coin
    eh = if turn == Coin then [ Svg.Events.onClick (if selected then DeselectCoin else SelectCoin pos) ] else []
  in
  circle
  ( List.append
    [ cx <| String.fromFloat bx ++ "em"
    , cy <| String.fromFloat by ++ "em"
    , r "2em"
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
  [ x <| String.fromFloat xcoordr ++ "em"
  , y <| String.fromFloat ycoordr ++ "em"
  , width "5em"
  , height "5em"
  , stroke "transparent"
  , strokeWidth "0em"
  , fill colorConfig.coin
  , fillOpacity "0.2"
  , Svg.Events.onClick (MoveCoin (xcoord,ycoord))
  ]
  []

newSelectedCoin : Player -> (Int, Int) -> Svg Msg
newSelectedCoin = newCoin True

newUnselectedCoin : Player -> (Int, Int) -> Svg Msg
newUnselectedCoin = newCoin False

fromNodeCoords : (Int, Int) -> (Float, Float)
fromNodeCoords (x, y) =
  ( toFloat (x+1) * 5.0
  , toFloat (y+1) * 5.0
  )

fromSquareCoords : (Int, Int) -> (Float, Float)
fromSquareCoords (x, y) =
  ( toFloat x * 5.0
  , toFloat y * 5.0
  )

fromCoinCoords : (Int, Int) -> (Float, Float)
fromCoinCoords (x, y) =
  ( toFloat x * 5.0 + 2.5
  , toFloat y * 5.0 + 2.5
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

generateGuerillaPlacements : Maybe (Int, Int) -> BoardState -> List (Svg Msg)
generateGuerillaPlacements mousedOver =
  availableGuerillaPlacements >> List.map (newGuerillaPlacement mousedOver)

view : Model -> Html Msg
view model =
  let
    winnerDivs =
      Maybe.map
        (\p -> [ div [] [ Html.text <| "WINNER: " ++ stringFromPlayer p ] ])
        (winner model.board)
      |> Maybe.withDefault []
  in
  div [ class "elm" ]
    [ div [ class "board" ]
      (
        [ div []
          [ svg
            [ width "40em"
            , height "40em"
            ]
            (
              List.concat <|
                [ (generateBoard model.board)
                , (generateCoinTargets model.board)
                , (generateGuerillaPlacements model.mousedGuerillaPlacement model.board)
                ]
            )
          ]
        , div [ id "turn-indicator" ] [ Html.b [] [ Html.text "Turn: " ], Html.text <| stringFromPlayer model.board.turn ]
        ]
        ++ winnerDivs
      )
    , div [ class "sidebar" ]
      [ div [ class "sidebar-section" ]
        [ table [ class "state" ]
          [ tr []
            [ td [ class "state-key" ] [ Html.text "Turn" ]
            , td [ class "state-value" ] [ Html.text <| stringFromPlayer model.board.turn ]
            ]
          , tr []
            [ td [ class "state-key" ] [ Html.text "Coins remaining" ]
            , td [ class "state-value" ] [ Html.text <| String.fromInt (List.length model.board.coins) ]
            ]
          , tr []
            [ td [ class "state-key" ] [ Html.text "Guerillas remaining" ]
            , td [ class "state-value" ] [ Html.text <| String.fromInt model.board.guerillasRemaining ]
            ]
          ]
        ]
      , div [ class "sidebar-section"]
        [ div [ class "log" ]
          [ div [] [ Html.textarea [ Html.Attributes.style "resize" "none", rows 20, cols 50, readonly True ] [ Html.text <| String.join "\n" model.log ] ]
          ]
        ]
      , div [ class "sidebar-section" ]
        [ div [ class "controls" ] [ button [ id "reset-button", Svg.Events.onClick ResetBoard ] [ Html.text "Reset" ] ]
        ]
      ]
    ]
