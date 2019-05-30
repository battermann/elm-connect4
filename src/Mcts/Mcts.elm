module Mcts.Mcts exposing (MctsTree, Node, NodeStats, Problem, SelectionPolicy, avg, mkNode, selection, selectionPolicyAvg)

import Tree
import Tree.Zipper as Zipper


type alias Problem a =
    a -> List (() -> a)


type alias NodeStats a =
    { state : a
    , visits : Int
    , wins : Int
    , moves : List (() -> a)
    }


type alias Node a =
    Zipper.Zipper (NodeStats a)


type alias MctsTree a =
    Tree.Tree (NodeStats a)


mkNode : Problem a -> a -> MctsTree a
mkNode p state =
    Tree.singleton
        { state = state
        , visits = 0
        , wins = 0
        , moves = p state
        }


type alias SelectionPolicy a =
    Node a -> Maybe (Node a)


avg : Node a -> Float
avg node =
    let
        wins =
            Zipper.label node |> .wins

        visits =
            Zipper.label node |> .visits
    in
    toFloat wins / toFloat visits


selectionPolicyAvg : SelectionPolicy a
selectionPolicyAvg node =
    let
        findBest : Maybe (Node a) -> Maybe (Node a) -> Maybe (Node a)
        findBest maybeBest maybeNext =
            case ( maybeBest, maybeNext ) of
                ( Just best, Just next ) ->
                    if avg best > avg next then
                        findBest (Just best) (Zipper.nextSibling next)

                    else
                        findBest (Just next) (Zipper.nextSibling next)

                ( Nothing, Just next ) ->
                    findBest (Just next) (Zipper.nextSibling next)

                ( best, Nothing ) ->
                    best

        maybeChild =
            Zipper.firstChild node

        maybe2ndChild =
            maybeChild |> Maybe.andThen Zipper.nextSibling
    in
    findBest maybeChild maybe2ndChild


selection : SelectionPolicy a -> MctsTree a -> Node a
selection policy tree =
    let
        select : Node a -> Node a
        select node =
            if Zipper.label node |> .moves |> List.isEmpty then
                policy node
                    |> Maybe.map select
                    |> Maybe.withDefault node

            else
                node
    in
    select (Zipper.fromTree tree)
