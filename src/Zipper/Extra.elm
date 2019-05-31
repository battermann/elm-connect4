module Zipper.Extra exposing (foldlOverChildren, maxChildBy)

import Tree.Zipper as Zipper exposing (Zipper)


foldlOverChildren : (Zipper a -> b -> b) -> b -> Zipper a -> b
foldlOverChildren func baseCase zipper =
    let
        f : Maybe (Zipper a) -> b -> b
        f maybeNext acc =
            case maybeNext of
                Just next ->
                    f (Zipper.nextSibling next) (func next acc)

                Nothing ->
                    acc
    in
    f (Zipper.firstChild zipper) baseCase


maxChildBy : (a -> comparable) -> Zipper a -> Maybe (Zipper a)
maxChildBy toComp zipper =
    foldlOverChildren
        (\this maybeOther ->
            case maybeOther of
                Just other ->
                    if toComp (Zipper.label this) >= toComp (Zipper.label other) then
                        Just this

                    else
                        Just other

                Nothing ->
                    Just this
        )
        Nothing
        zipper
