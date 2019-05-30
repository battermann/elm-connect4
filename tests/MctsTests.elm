module MctsTests exposing (all)

import Expect
import Mcts.Mcts as Mcts exposing (..)
import Test exposing (..)
import Tree
import Tree.Zipper as Zipper


all : Test
all =
    describe "Mcts"
        [ test "Selection policy applied to singleton tree should select root" <|
            \_ ->
                let
                    problem i =
                        List.repeat i () |> List.map (\x -> \() -> i - 1)

                    actual =
                        Mcts.selection Mcts.selectionPolicyAvg (Mcts.mkNode problem 9)
                            |> Zipper.label
                            |> .state

                    expected =
                        9
                in
                Expect.equal actual expected
        , test "Selection policy applied to singleton tree which is terminal should select root" <|
            \_ ->
                let
                    actual =
                        Mcts.selection Mcts.selectionPolicyAvg (Mcts.mkNode (\_ -> []) 0)
                            |> Zipper.label
                            |> .state

                    expected =
                        0
                in
                Expect.equal actual expected
        , test "Selection policy applied to tree with no moves and one child should select child" <|
            \_ ->
                let
                    treeNoMovesOneChild =
                        Tree.tree { visits = 2, wins = 1, state = 9, moves = [] }
                            [ Tree.singleton { visits = 10, wins = 5, state = 8, moves = [] } ]

                    actual =
                        Mcts.selection Mcts.selectionPolicyAvg treeNoMovesOneChild
                            |> Zipper.label
                            |> .state

                    expected =
                        8
                in
                Expect.equal actual expected
        , test "Selection policy applied to tree with moves and one child should select root" <|
            \_ ->
                let
                    treeWithMovesOneChild =
                        Tree.tree { visits = 2, wins = 1, state = 2, moves = [ \() -> 7 ] }
                            [ Tree.singleton { visits = 10, wins = 5, state = 0, moves = [] }
                            ]

                    actual =
                        Mcts.selection Mcts.selectionPolicyAvg treeWithMovesOneChild
                            |> Zipper.label
                            |> .state

                    expected =
                        2
                in
                Expect.equal actual expected
        , test "Selection policy applied to tree no moves and multiple children should select best child" <|
            \_ ->
                let
                    treeNoMovesMultipleChildren =
                        Tree.tree { visits = 2, wins = 1, state = 9, moves = [] }
                            [ Tree.singleton { visits = 10, wins = 5, state = 5, moves = [] }
                            , Tree.tree { visits = 10, wins = 6, state = 6, moves = [ \() -> 7 ] }
                                [ Tree.singleton { visits = 10, wins = 5, state = 0, moves = [] } ]
                            , Tree.singleton { visits = 10, wins = 3, state = 3, moves = [] }
                            ]

                    actual =
                        Mcts.selection Mcts.selectionPolicyAvg treeNoMovesMultipleChildren
                            |> Zipper.label
                            |> .state

                    expected =
                        6
                in
                Expect.equal actual expected
        ]
