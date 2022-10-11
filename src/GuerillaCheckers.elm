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

init : () -> (Model, Cmd Msg)
init _ =
  ( Model
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
  , Cmd.none
  )

-- UPDATE

type Msg
  = SelectCoin (Int, Int)
  | DeselectCoin
  | MoveCoin (Int, Int)
  | PlaceGuerilla (Int, Int)

-- TODO: Boundary check on these guys on both ends

moveCoin : List (Int, Int) -> (Int, Int) -> (Int, Int) -> List (Int, Int)
moveCoin coins pos1 pos2 =
  if not <| List.member pos1 coins then
    coins
  else if List.member pos2 coins then
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
        | guerillas = pos :: model.guerillas
        , guerillasRemaining = model.guerillasRemaining - 1
        }
      , Cmd.none
      )

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

initBoard : List (Svg Msg)
initBoard =
  let
    mkSq idx c =
      let
        x = modBy 8 idx
        y = idx // 8
      in
        newBoardSq c (x, y)

    board =
      List.repeat 4
      [ List.repeat 4 [ Black, White ]
      , List.repeat 4 [ White, Black ]
      ]
      |> List.concat
      |> List.concat
      |> List.concat
      |> List.indexedMap mkSq

    mkBoardNode (idx,sq) =
      let
        x = modBy 8 idx
        y = idx // 8
      in
        if modBy 8 idx == 7 || idx // 8 == 7 then
          []
        else
          [ newBoardNode (x,y) ]

    nodes = 
      board
      |> List.indexedMap (\idx sq -> (idx, sq))
      |> List.concatMap mkBoardNode
  in
    List.append board nodes

generateBoard : Model -> List (Svg Msg)
generateBoard
  { coins, selectedCoin, guerillas, guerillasRemaining } =
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
  [ initBoard
  , selected
  , unselected
  , List.map newGuerilla guerillas
  ] |> List.concat

view : Model -> Html Msg
view model =
  div []
    [ svg
      [ width "400"
      , height "400"
      ]
      (generateBoard model)
    ]
