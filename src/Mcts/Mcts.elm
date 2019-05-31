module Mcts.Mcts exposing
    ( SelectionPolicy
    , avg
    , defaultExplorationParameter
    , expansion
    , selection
    , selectionPolicyAvg
    , selectionPolicyUct
    , simulation
    )

import Mcts.MctsTree as Tree exposing (MctsTree, Node, NodeStats, Problem)
import Random exposing (Seed)


type alias SelectionPolicy a =
    Node a -> Maybe (Node a)


avg : NodeStats a -> Float
avg =
    uct 0 1


uct : Float -> Int -> NodeStats a -> Float
uct c numVisitsParent { wins, visits } =
    toFloat wins / toFloat visits + c * sqrt (logBase e (toFloat numVisitsParent) / toFloat visits)


selectionPolicyAvg : SelectionPolicy a
selectionPolicyAvg =
    Tree.selectionPolicy avg


defaultExplorationParameter : Float
defaultExplorationParameter =
    sqrt 2


selectionPolicyUct : Float -> SelectionPolicy a
selectionPolicyUct c node =
    node |> Tree.selectionPolicy (uct c (Tree.nodeStats node |> .visits))


selection : SelectionPolicy a -> MctsTree a -> Node a
selection policy tree =
    let
        select : Node a -> Node a
        select node =
            if Tree.nodeStats node |> .unvisitedPaths |> List.isEmpty then
                policy node
                    |> Maybe.map select
                    |> Maybe.withDefault node

            else
                node
    in
    select (Tree.fromTree tree)


expansion : Problem a -> Node a -> Maybe (Node a)
expansion p node =
    case Tree.nodeStats node |> .unvisitedPaths of
        [] ->
            Nothing

        head :: tail ->
            Tree.prependSelectChild (Tree.tree p (head ())) (node |> Tree.update (\ns -> { ns | unvisitedPaths = tail }))


simulation : Seed -> Problem a -> a -> ( a, Seed )
simulation seed p state =
    case p state of
        [] ->
            ( state, seed )

        head :: tail ->
            let
                ( nextState, nextSeed ) =
                    Random.step (Random.uniform head tail) seed
            in
            simulation nextSeed p (nextState ())
