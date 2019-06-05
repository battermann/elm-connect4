module Connect4Tests exposing (all)

import Connect4.Connect4 as C4
import Expect
import List.Extra
import Test exposing (..)


all : Test
all =
    describe "Connect4"
        [ test "legaMoves empty" <|
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
        ]
