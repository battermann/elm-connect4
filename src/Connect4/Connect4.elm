module Connect4.Connect4 exposing (Connect4, GameState(..), Player(..), empty, legalMoves, tryMakeMove, winningPositions)

import Array exposing (Array)
import Array.Extra
import Maybe.Extra


type alias Connect4 =
    { moves : List Int
    , board : Array (Array Player)
    }


type GameState
    = InProgress Connect4
    | P1Won Connect4
    | P2Won Connect4
    | Draw Connect4


tryMakeMove : Int -> GameState -> GameState
tryMakeMove col gameState =
    case gameState of
        InProgress connect4 ->
            let
                maybeRow =
                    connect4.board
                        |> Array.get col
                        |> Maybe.map Array.length
                        |> Maybe.Extra.filter (\r -> r < 6 && r >= 0)
            in
            case maybeRow of
                Just row ->
                    let
                        newBoard =
                            makeMove col connect4
                    in
                    if (List.length newBoard.moves |> modBy 2) == 1 && checkIfMoveWins P1 col row newBoard.board then
                        P1Won newBoard

                    else if checkIfMoveWins P2 col row newBoard.board then
                        P2Won newBoard

                    else if List.length newBoard.moves == 7 * 6 then
                        Draw newBoard

                    else
                        InProgress newBoard

                Nothing ->
                    gameState

        _ ->
            gameState


type Player
    = None
    | P1
    | P2


playerAt : Int -> Int -> Array (Array Player) -> Player
playerAt x y =
    Array.get x >> Maybe.andThen (Array.get y) >> Maybe.withDefault None


checkIfMoveWins : Player -> Int -> Int -> Array (Array Player) -> Bool
checkIfMoveWins p x y board =
    winningPositions x y |> List.any (List.all (\( x1, y1 ) -> (board |> playerAt x1 y1) == p))


winningPositions : Int -> Int -> List (List ( Int, Int ))
winningPositions x y =
    List.concat
        [ List.range (x - 3) x |> List.map (\col -> List.range col (col + 3) |> List.map (\c -> ( c, y )))
        , List.range (y - 3) y |> List.map (\row -> List.range row (row + 3) |> List.map (\r -> ( x, r )))
        , List.range -3 0 |> List.map (\a -> List.range 0 3 |> List.map (\b -> ( x + a + b, y + a + b )))
        , List.range -3 0 |> List.map (\a -> List.range 0 3 |> List.map (\b -> ( x + a + b, y - a - b )))
        ]


empty : GameState
empty =
    InProgress
        { moves = []
        , board = Array.repeat 7 Array.empty
        }


legalMoves : { a | moves : List Int } -> List Int
legalMoves { moves } =
    List.range 0 6
        |> List.filter (\col -> moves |> List.filter ((==) col) |> List.length |> (\l -> l < 6))


makeMove : Int -> Connect4 -> Connect4
makeMove col { moves, board } =
    let
        player =
            if (List.length moves |> modBy 2) == 0 then
                P1

            else
                P2
    in
    { moves = moves ++ [ col ]
    , board = board |> Array.Extra.update col (Array.push player)
    }
