-- Press a button to generate a random number between 1 and 6.
--
-- Read how it works:
--   https://guide.elm-lang.org/effects/random.html
--

module GuerillaCheckers exposing (..)

import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Random

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

type Turn = Coin | Guerilla

type alias Model =
  { coins : List (Int, Int)
  , guerillas : List (Int, Int)
  , guerillasRemaining: Int
  , selectedCoin : Maybe (Int, Int)
  , turn : Turn
  , log : List String
  }

initBoard : Model
initBoard =
  Model
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

moveCoin : List (Int, Int) -> (Int, Int) -> (Int, Int) -> List (Int, Int)
moveCoin coins pos1 pos2 =
  if not <| List.member pos1 coins then
    coins
  else if List.member pos2 coins then
    coins
  else if not <| List.member pos2 (coinNeighbors pos1) then
    coins
  else
    List.filter ((/=) pos1) coins |> (::) pos2

find : a -> List a -> Maybe a
find x = List.filter ((==) x) >> List.head

stringFromTuple : (Int, Int) -> String
stringFromTuple (x,y) = "(" ++ (String.fromInt x) ++ "," ++ (String.fromInt y) ++ ")"

stringFromMessage : Model -> Msg -> String
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

findCapture : List (Int, Int) -> (Int, Int) -> (Int, Int) -> Maybe (Int, Int)
findCapture guerillas (x1,y1) (x2,y2) =
  let
    potential =
      ( if x1 < x2 then x1 else x2
      , if y1 < y2 then y1 else y2
      )
  in
  find potential guerillas

stringFromMaybe : (a -> String) -> Maybe a -> String
stringFromMaybe f ma =
  Maybe.map f ma
  |> Maybe.withDefault ""

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let
    logMessage = stringFromMessage model msg
  in
  case msg of
    SelectCoin pos ->
      ( { model
        | selectedCoin = find pos model.coins
        }
      , Cmd.none
      )
    DeselectCoin ->
      ( { model | selectedCoin = Nothing }
      , Cmd.none
      )
    MoveCoin newpos ->
      let
        newCoins =
          case model.selectedCoin of
            Nothing  -> model.coins
            Just pos -> moveCoin model.coins pos newpos
        captured = model.selectedCoin |> Maybe.andThen (findCapture model.guerillas newpos)
        newGuerillas =
          case captured of
            Nothing  -> model.guerillas
            Just pos -> List.filter (\g -> g /= pos) model.guerillas
        fullMessage =
          logMessage
          ++ stringFromMaybe (\pos -> " (captured: " ++ stringFromTuple pos ++ ")") captured
      in
      ( { model
        | coins = newCoins
        , guerillas = newGuerillas
        , selectedCoin = Nothing
        , turn = Guerilla
        , log = if String.isEmpty fullMessage then model.log else fullMessage :: model.log
        }
      , Cmd.none
      )
    PlaceGuerilla pos ->
      let
        newGuerillas = List.sort <| pos :: model.guerillas

        potentialCapturableSquares (x,y) = [ (x,y), (x+1,y), (x,y+1), (x+1,y+1) ]

        isCapturableSquare (x,y) =
          let
            capturingGuerillas =
              if x == 0 && y == 0      then [ (0,0) ]
              else if x == 0 && y == 7 then [ (0,6) ]
              else if x == 7 && y == 0 then [ (6,0) ]
              else if x == 7 && y == 7 then [ (6,6) ]
              else if x == 0           then [ (0,y-1), (0,y) ]
              else if y == 0           then [ (x-1,0), (x,0) ]
              else if x == 7           then [ (6,y-1), (6,y) ]
              else if y == 7           then [ (x-1,6), (x,6) ]
              else                          [ (x-1,y-1), (x,y-1), (x-1,y), (x,y) ]
          in
          List.all (\g -> List.member g newGuerillas) capturingGuerillas

        captured =
          potentialCapturableSquares pos
          |> List.filter (\sq -> List.member sq model.coins && isCapturableSquare sq)
          |> List.sort

        newCoins = seqDiff model.coins captured
      in
      ( { model
        | coins = newCoins
        , guerillas = newGuerillas
        , guerillasRemaining = model.guerillasRemaining - 1
        , turn = Coin
        , log = if String.isEmpty logMessage then model.log else logMessage :: model.log
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

combinations : List a -> List b -> List (a,b)
combinations xs ys =
  case (xs, ys) of
    ([], _)       -> []
    (_, [])       -> []
    (x::xs2, ys2) ->
      List.append
        (List.map (Tuple.pair x) ys2)
        (combinations xs2 ys2)

squareCoords : List (Int, Int)
squareCoords = combinations (List.range 0 7) (List.range 0 7)

nodeCoords : List (Int, Int)
nodeCoords = combinations (List.range 0 6) (List.range 0 6)

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

coinNeighbors : (Int, Int) -> List (Int, Int)
coinNeighbors (x,y) =
  let
    isInRange (x2,y2) = x2 >= 0 && y2 >= 0 && x2 < 8 && y2 < 8
  in
    List.filter isInRange [(x-1,y-1), (x-1,y+1), (x+1,y-1), (x+1,y+1)]

generateBoard : Model -> List (Svg Msg)
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

generateCoinTargets : Model -> List (Svg Msg)
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

-- NB: Assumes both lists are sorted
seqDiff : List a -> List a -> List a
seqDiff =
  let
      seqDiffInner acc xs ys =
        case (xs,ys) of
          ([],_)          -> List.reverse acc
          (xs2,[])        -> List.append (List.reverse acc) xs2
          (x::xs2,y::ys2) ->
            if x == y then
              seqDiffInner acc xs2 ys2
            else
              seqDiffInner (x::acc) xs2 (y::ys2)
  in
    seqDiffInner []

availableGuerillaPlacements : Model -> List (Int,Int)
availableGuerillaPlacements { guerillas, guerillasRemaining, turn } =
  if guerillasRemaining > 0 && turn == Guerilla
  then seqDiff nodeCoords guerillas
  else []

generateGuerillaPlacements : Model -> List (Svg Msg)
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
          [ (generateBoard model)
          , (generateCoinTargets model)
          , (generateGuerillaPlacements model)
          ]
        )
      ]
    , div [] [ button [ onClick ResetBoard ] [ Html.text "Reset" ] ]
    , div [] [ Html.text <| "Turn: " ++ fromTurn model.turn ]
    , div [] [ Html.text <| "Coins remaining: " ++ String.fromInt (List.length model.coins) ]
    , div [] [ Html.text <| "Guerillas remaining: " ++ String.fromInt model.guerillasRemaining ]
    , div [] [ Html.text <| "Log:" ]
    , div [] [ ul [] <| List.map (\l -> li [] [ Html.text l ]) model.log ]
    ]
