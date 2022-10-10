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
  { color : Color
  }

init : () -> (Model, Cmd Msg)
init _ =
  ( Model White
  , Cmd.none
  )

-- UPDATE

type Msg = NewColor Color

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NewColor c ->
      ( Model c
      , Cmd.none
      )

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- VIEW

type alias Board =
  { coins : List (Int, Int)
  , guerillas : List (Int, Int)
  , guerillasRemaining: Int
  }

type Color = White | Black

type alias ColorConfig =
  { border : String
  , coin : String
  , guerilla : String
  }

colorConfig : ColorConfig
colorConfig =
  { border = "white"
  , coin = "red"
  , guerilla = "black"
  }

color : Color -> String
color c = case c of
  White -> "lightgray"
  Black -> "gray"

newSquare : Color -> (Int, Int) -> Svg Msg
newSquare c (xcoord,ycoord) =
  rect
    [ x <| String.fromInt xcoord
    , y <| String.fromInt ycoord
    , width "50"
    , height "50"
    , stroke colorConfig.border
    , strokeWidth "4"
    , fill <| color c
    ]
    []

newNode : (Int, String) -> (Int, Int) -> Svg Msg
newNode (rad, c) (xcoord,ycoord) =
  circle
  [ cx <| String.fromInt xcoord
  , cy <| String.fromInt ycoord
  , r <| String.fromInt rad
  , fill c
  , stroke c
  ]
  []

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

newBoardNode : (Int, Int) -> Svg Msg
newBoardNode = fromNodeCoords >> newNode (5, colorConfig.border)

newGuerilla : (Int, Int) -> Svg Msg
newGuerilla = fromNodeCoords >> newNode (10, colorConfig.guerilla)

newCoin : (Int, Int) -> Svg Msg
newCoin = fromCoinCoords >> newNode (20, colorConfig.coin)

newBoardSq : Color -> (Int, Int) -> Svg Msg
newBoardSq c = fromSquareCoords >> newSquare c

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

view : Model -> Html Msg
view model =
  div []
    [ svg
      [ width "400"
      , height "400"
      ]
      (
        List.append 
          initBoard
          -- Sample data
          [ newCoin (2, 3)
          , newCoin (3, 4)
          , newCoin (4, 5)
          , newCoin (3, 2)
          , newCoin (4, 3)
          , newCoin (5, 4)

          , newGuerilla (0, 0)
          ]
      )
    ]
