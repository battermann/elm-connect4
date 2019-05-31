module Mcts.MctsTree exposing (MctsTree, Node, NodeStats, Problem, fromTree, mctsTree, nodeStats, prependSelectChild, selectionPolicy, tree, update)

import Tree
import Tree.Zipper as Zipper
import Zipper.Extra


type alias Problem a =
    a -> List (() -> a)


type alias NodeStats a =
    { state : a
    , visits : Int
    , wins : Int
    , unvisitedPaths : List (() -> a)
    }


type Node a
    = Node (Zipper.Zipper (NodeStats a))


type MctsTree a
    = MctsTree (Tree.Tree (NodeStats a))


mctsTree : Tree.Tree (NodeStats a) -> MctsTree a
mctsTree =
    MctsTree


tree : Problem a -> a -> MctsTree a
tree p state =
    Tree.singleton
        { state = state
        , visits = 0
        , wins = 0
        , unvisitedPaths = p state
        }
        |> MctsTree


selectionPolicy : (NodeStats a -> comparable) -> Node a -> Maybe (Node a)
selectionPolicy f (Node n) =
    Zipper.Extra.maxChildBy f n |> Maybe.map Node


nodeStats : Node a -> NodeStats a
nodeStats (Node n) =
    Zipper.label n


fromTree : MctsTree a -> Node a
fromTree (MctsTree tr) =
    Zipper.fromTree tr |> Node


prependSelectChild : MctsTree a -> Node a -> Maybe (Node a)
prependSelectChild (MctsTree tr) (Node node) =
    node
        |> Zipper.replaceTree (node |> Zipper.tree |> Tree.prependChild tr)
        |> Zipper.firstChild
        |> Maybe.map Node


update : (NodeStats a -> NodeStats a) -> Node a -> Node a
update f (Node node) =
    Zipper.mapLabel f node |> Node
