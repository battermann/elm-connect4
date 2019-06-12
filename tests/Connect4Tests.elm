module Connect4Tests exposing (all)

import Connect4.Connect4 as C4 exposing (GameState(..))
import Expect
import Fuzz
import List.Extra
import Test exposing (..)


all : Test
all =
    describe "Connect4"
        [ test "game in progress" <|
            \_ ->
                let
                    game =
                        [ 0, 1, 2, 3, 4, 5, 6 ] |> List.foldl (\col gameState -> C4.tryMakeMove col gameState) C4.empty

                    isInProgress =
                        case game of
                            InProgress _ ->
                                True

                            _ ->
                                False
                in
                Expect.true "game is in progress" isInProgress
        , test "player 1 wins vertical" <|
            \_ ->
                let
                    game =
                        [ 0, 1, 0, 1, 0, 1, 0 ] |> List.foldl (\col gameState -> C4.tryMakeMove col gameState) C4.empty

                    player1Wins =
                        case game of
                            P1Won _ ->
                                True

                            _ ->
                                False
                in
                Expect.true "player 1 wins" player1Wins
        , test "player 2 wins verical" <|
            \_ ->
                let
                    game =
                        [ 0, 1, 0, 1, 0, 1, 2, 1 ] |> List.foldl (\col gameState -> C4.tryMakeMove col gameState) C4.empty

                    player2Wins =
                        case game of
                            P2Won _ ->
                                True

                            _ ->
                                False
                in
                Expect.true "player 2 wins vertical" player2Wins
        , test "player 1 wins horizontal" <|
            \_ ->
                let
                    game =
                        [ 0, 0, 1, 1, 2, 2, 3 ] |> List.foldl (\col gameState -> C4.tryMakeMove col gameState) C4.empty

                    player1Wins =
                        case game of
                            P1Won _ ->
                                True

                            _ ->
                                False
                in
                Expect.true "player 1 wins horizontal" player1Wins
        , test "player 2 wins horizontal" <|
            \_ ->
                let
                    game =
                        [ 0, 1, 1, 2, 2, 3, 3, 4 ] |> List.foldl (\col gameState -> C4.tryMakeMove col gameState) C4.empty

                    player2Wins =
                        case game of
                            P2Won _ ->
                                True

                            _ ->
                                False
                in
                Expect.true "player 2 wins horizontal" player2Wins
        , test "player 1 wins diagonal" <|
            \_ ->
                let
                    game =
                        [ 0, 1, 1, 2, 2, 3, 2, 3, 3, 5, 3 ] |> List.foldl (\col gameState -> C4.tryMakeMove col gameState) C4.empty

                    player1Wins =
                        case game of
                            P1Won _ ->
                                True

                            _ ->
                                False
                in
                Expect.true "player 1 wins diagonal" player1Wins
        , test "legaMoves empty" <|
            \_ ->
                Expect.equal (C4.legalMoves { moves = [] }) [ 0, 1, 2, 3, 4, 5, 6 ]
        , test "legaMoves one move" <|
            \_ ->
                Expect.equal (C4.legalMoves { moves = [ 0 ] }) [ 0, 1, 2, 3, 4, 5, 6 ]
        , test "legaMoves 5 moves" <|
            \_ ->
                Expect.equal (C4.legalMoves { moves = [ 0, 0, 0, 0, 0 ] }) [ 0, 1, 2, 3, 4, 5, 6 ]
        , test "legaMoves 6 moves" <|
            \_ ->
                Expect.equal (C4.legalMoves { moves = [ 0, 0, 0, 0, 0, 0 ] }) [ 1, 2, 3, 4, 5, 6 ]
        , test "legaMoves 6 moves in 2 columns" <|
            \_ ->
                Expect.equal (C4.legalMoves { moves = [ 0, 0, 0, 0, 0, 0, 4, 4, 4, 4, 4, 4 ] }) [ 1, 2, 3, 5, 6 ]
        , test "winningPositions" <|
            \_ ->
                Expect.equal
                    (C4.winningPositions 3 3)
                    [ [ ( 0, 3 ), ( 1, 3 ), ( 2, 3 ), ( 3, 3 ) ]
                    , [ ( 1, 3 ), ( 2, 3 ), ( 3, 3 ), ( 4, 3 ) ]
                    , [ ( 2, 3 ), ( 3, 3 ), ( 4, 3 ), ( 5, 3 ) ]
                    , [ ( 3, 3 ), ( 4, 3 ), ( 5, 3 ), ( 6, 3 ) ]
                    , [ ( 3, 0 ), ( 3, 1 ), ( 3, 2 ), ( 3, 3 ) ]
                    , [ ( 3, 1 ), ( 3, 2 ), ( 3, 3 ), ( 3, 4 ) ]
                    , [ ( 3, 2 ), ( 3, 3 ), ( 3, 4 ), ( 3, 5 ) ]
                    , [ ( 3, 3 ), ( 3, 4 ), ( 3, 5 ), ( 3, 6 ) ]
                    , [ ( 0, 0 ), ( 1, 1 ), ( 2, 2 ), ( 3, 3 ) ]
                    , [ ( 1, 1 ), ( 2, 2 ), ( 3, 3 ), ( 4, 4 ) ]
                    , [ ( 2, 2 ), ( 3, 3 ), ( 4, 4 ), ( 5, 5 ) ]
                    , [ ( 3, 3 ), ( 4, 4 ), ( 5, 5 ), ( 6, 6 ) ]
                    , [ ( 0, 6 ), ( 1, 5 ), ( 2, 4 ), ( 3, 3 ) ]
                    , [ ( 1, 5 ), ( 2, 4 ), ( 3, 3 ), ( 4, 2 ) ]
                    , [ ( 2, 4 ), ( 3, 3 ), ( 4, 2 ), ( 5, 1 ) ]
                    , [ ( 3, 3 ), ( 4, 2 ), ( 5, 1 ), ( 6, 0 ) ]
                    ]
        , fuzz2 (Fuzz.intRange 0 6) (Fuzz.intRange 0 5) "winningPositions properties" <|
            \x y ->
                C4.winningPositions x y
                    |> Expect.all
                        [ List.length >> Expect.equal 16
                        , List.all (List.Extra.unique >> List.length >> (==) 4) >> Expect.true "all sublists have 4 unique elements"
                        , List.all (List.length >> (==) 4) >> Expect.true "all sublists have 4 elements"
                        , List.all (List.member ( x, y )) >> Expect.true "all sublists contain the original position"
                        ]
        ]
