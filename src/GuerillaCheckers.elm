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

type alias Model =
  { coins : List (Int, Int)
  , guerillas : List (Int, Int)
  , guerillasRemaining: Int
  , selectedCoin : Maybe (Int, Int)
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

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
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
      in
      ( { model
        | coins = newCoins
        , selectedCoin = Nothing
        }
      , Cmd.none
      )
    PlaceGuerilla pos ->
      ( { model
        | guerillas = List.sort <| pos :: model.guerillas
        , guerillasRemaining = model.guerillasRemaining - 1
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

newCoin : Bool -> (Int, Int) -> Svg Msg
newCoin selected pos =
  let
    (bx,by) = fromCoinCoords pos
    c = if selected then colorConfig.selectedCoin else colorConfig.coin
  in
  circle
  [ cx <| String.fromInt bx
  , cy <| String.fromInt by
  , r "20"
  , fill c
  , stroke c
  , onClick (if selected then DeselectCoin else SelectCoin pos)
  ]
  []

newClickableSq : (Int, Int) -> Svg Msg
newClickableSq (xcoord,ycoord) =
  let
    (xcoord2,ycoord2) = fromSquareCoords (xcoord,ycoord)
  in
  rect
    [ x <| String.fromInt xcoord2
    , y <| String.fromInt ycoord2
    , width "50"
    , height "50"
    , stroke "transparent"
    , strokeWidth "0"
    , fill "transparent"
    , fillOpacity "0.0"
    , onClick (MoveCoin (xcoord,ycoord))
    ]
    []

newSelectedCoin : (Int, Int) -> Svg Msg
newSelectedCoin = newCoin True

newUnselectedCoin : (Int, Int) -> Svg Msg
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
  { coins, selectedCoin, guerillas } =
  let
    selected =
      case selectedCoin of
        Nothing  -> []
        Just pos -> List.filter ((==) pos) coins |> List.map newSelectedCoin
    unselected =
      case selectedCoin of
        Nothing  -> coins |> List.map newUnselectedCoin
        Just pos -> List.filter ((/=) pos) coins |> List.map newUnselectedCoin
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
    Just spos -> coinNeighbors spos |> List.filter (not << flip List.member coins) |> List.map newClickableSq
    Nothing   -> []

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
availableGuerillaPlacements { guerillas, guerillasRemaining } =
  if guerillasRemaining > 0
  then seqDiff nodeCoords guerillas
  else []

generateGuerillaPlacements : Model -> List (Svg Msg)
generateGuerillaPlacements =
  availableGuerillaPlacements >> List.map newGuerillaPlacement

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
    , div []
      [ button [ onClick ResetBoard ] [ Html.text "Reset" ] ]
    ]
