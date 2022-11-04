module GuerillaCheckersBoard exposing (..)

type Turn = Coin | Guerilla

type alias BoardState =
  { coins : List (Int, Int)
  , guerillas : List (Int, Int)
  , guerillasRemaining: Int
  , selectedCoin : Maybe (Int, Int)
  , turn : Turn
  }

coinNeighbors : (Int, Int) -> List (Int, Int)
coinNeighbors (x,y) =
  let
    isInRange (x2,y2) = x2 >= 0 && y2 >= 0 && x2 < 8 && y2 < 8
  in
    List.filter isInRange [(x-1,y-1), (x-1,y+1), (x+1,y-1), (x+1,y+1)]

squareCoords : List (Int, Int)
squareCoords = combinations (List.range 0 7) (List.range 0 7)

nodeCoords : List (Int, Int)
nodeCoords = combinations (List.range 0 6) (List.range 0 6)

combinations : List a -> List b -> List (a,b)
combinations xs ys =
  case (xs, ys) of
    ([], _)       -> []
    (_, [])       -> []
    (x::xs2, ys2) ->
      List.append
        (List.map (Tuple.pair x) ys2)
        (combinations xs2 ys2)

find : a -> List a -> Maybe a
find x = List.filter ((==) x) >> List.head

findCapture : List (Int, Int) -> (Int, Int) -> (Int, Int) -> Maybe (Int, Int)
findCapture guerillas (x1,y1) (x2,y2) =
  let
    potential =
      ( if x1 < x2 then x1 else x2
      , if y1 < y2 then y1 else y2
      )
  in
  find potential guerillas

availableGuerillaPlacements : BoardState -> List (Int,Int)
availableGuerillaPlacements { guerillas, guerillasRemaining, turn } =
  if guerillasRemaining > 0 && turn == Guerilla
  then seqDiff nodeCoords guerillas
  else []

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

selectCoin : (Int, Int) -> BoardState -> BoardState
selectCoin pos board =
  { board | selectedCoin = find pos board.coins }

deselectCoin : BoardState -> BoardState
deselectCoin board = { board | selectedCoin = Nothing }

moveSelectedCoin : (Int, Int) -> BoardState -> (BoardState, Maybe (Int, Int))
moveSelectedCoin newpos board =
  let
    newCoins =
      case board.selectedCoin of
        Nothing  -> board.coins
        Just pos -> moveCoin board.coins pos newpos
    captured = board.selectedCoin |> Maybe.andThen (findCapture board.guerillas newpos)
    newGuerillas =
      case captured of
        Nothing  -> board.guerillas
        Just pos -> List.filter (\g -> g /= pos) board.guerillas
  in
  ( { board
    | coins = newCoins
    , guerillas = newGuerillas
    , selectedCoin = Nothing
    , turn = Guerilla
    }
  , captured
  )

placeGuerilla : (Int, Int) -> BoardState -> (BoardState, List (Int, Int))
placeGuerilla pos board =
  let
    newGuerillas = List.sort <| pos :: board.guerillas

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
      |> List.filter (\sq -> List.member sq board.coins && isCapturableSquare sq)
      |> List.sort

    newCoins = seqDiff board.coins captured
  in
  ( { board
    | coins = newCoins
    , guerillas = newGuerillas
    , guerillasRemaining = board.guerillasRemaining - 1
    , turn = Coin
    }
  , captured
  )
