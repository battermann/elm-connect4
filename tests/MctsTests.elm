module MctsTests exposing (all)

import Expect
import Mcts.Mcts as Mcts exposing (..)
import Mcts.MctsTree as MctsTree
import Random
import Test exposing (..)
import Tree as Tree
import Tree.Zipper as Zipper


all : Test
all =
    describe "Mcts"
        [ test "Simulation" <|
            \_ ->
                let
                    problem i =
                        List.repeat i () |> List.map (\x -> \() -> i - 1)

                    ( actual, _ ) =
                        Mcts.simulation (Random.initialSeed 0) problem 9

                    expected =
                        0
                in
                Expect.equal actual expected
        , test "Selection policy applied to singleton tree should select root" <|
            \_ ->
                let
                    problem i =
                        List.repeat i () |> List.map (\x -> \() -> i - 1)

                    actual =
                        Mcts.selection Mcts.selectionPolicyAvg (MctsTree.tree problem 9)
                            |> MctsTree.nodeStats
                            |> .state

                    expected =
                        9
                in
                Expect.equal actual expected
        , test "Selection policy applied to singleton tree which is terminal should select root" <|
            \_ ->
                let
                    actual =
                        Mcts.selection Mcts.selectionPolicyAvg (MctsTree.tree (\_ -> []) 0)
                            |> MctsTree.nodeStats
                            |> .state

                    expected =
                        0
                in
                Expect.equal actual expected
        , test "Selection policy applied to tree with no moves and one child should select child" <|
            \_ ->
                let
                    tree =
                        Tree.tree { visits = 2, wins = 1, state = 9, unvisitedPaths = [] }
                            [ Tree.singleton { visits = 10, wins = 5, state = 8, unvisitedPaths = [] } ]
                            |> MctsTree.mctsTree

                    actual =
                        Mcts.selection Mcts.selectionPolicyAvg tree
                            |> MctsTree.nodeStats
                            |> .state

                    expected =
                        8
                in
                Expect.equal actual expected
        , test "Selection policy applied to tree with moves and one child should select root" <|
            \_ ->
                let
                    tree =
                        Tree.tree { visits = 2, wins = 1, state = 2, unvisitedPaths = [ \() -> 7 ] }
                            [ Tree.singleton { visits = 10, wins = 5, state = 0, unvisitedPaths = [] }
                            ]
                            |> MctsTree.mctsTree

                    actual =
                        Mcts.selection Mcts.selectionPolicyAvg tree
                            |> MctsTree.nodeStats
                            |> .state

                    expected =
                        2
                in
                Expect.equal actual expected
        , test "Selection policy applied to tree no moves and multiple children should select best child" <|
            \_ ->
                let
                    tree =
                        Tree.tree { visits = 2, wins = 1, state = 9, unvisitedPaths = [] }
                            [ Tree.singleton { visits = 10, wins = 5, state = 5, unvisitedPaths = [] }
                            , Tree.tree { visits = 10, wins = 6, state = 6, unvisitedPaths = [ \() -> 7 ] }
                                [ Tree.singleton { visits = 10, wins = 5, state = 0, unvisitedPaths = [] } ]
                            , Tree.singleton { visits = 10, wins = 3, state = 3, unvisitedPaths = [] }
                            ]
                            |> MctsTree.mctsTree

                    actual =
                        Mcts.selection Mcts.selectionPolicyAvg tree
                            |> MctsTree.nodeStats
                            |> .state

                    expected =
                        6
                in
                Expect.equal actual expected
        , test "UCT Selection policy applied to tree should select root with highest UCT" <|
            \_ ->
                let
                    tree =
                        Tree.tree { visits = 100, wins = 60, state = 0, unvisitedPaths = [] }
                            [ Tree.singleton { visits = 40, wins = 20, state = 1, unvisitedPaths = [] } -- UCT ~ 1.85
                            , Tree.singleton { visits = 40, wins = 19, state = 2, unvisitedPaths = [] } -- UCT ~ 1.82
                            , Tree.singleton { visits = 20, wins = 9, state = 3, unvisitedPaths = [] } -- UCT ~ 2.24
                            ]
                            |> MctsTree.mctsTree

                    actual =
                        Mcts.selection (Mcts.selectionPolicyUct defaultExplorationParameter) tree
                            |> MctsTree.nodeStats
                            |> .state

                    expected =
                        3
                in
                Expect.equal actual expected
        , test "selection and expansion should return the expanded node" <|
            \_ ->
                let
                    tree =
                        Tree.tree { visits = 2, wins = 1, state = 0, unvisitedPaths = [ \() -> 2, \() -> 3 ] }
                            [ Tree.singleton { visits = 1, wins = 1, state = 1, unvisitedPaths = [] }
                            ]
                            |> MctsTree.mctsTree

                    actual =
                        Mcts.selection Mcts.selectionPolicyAvg tree
                            |> Mcts.expansion (\_ -> [])
                            |> Maybe.map (MctsTree.nodeStats >> .state)

                    expected =
                        Just 2
                in
                Expect.equal actual expected
        ]
