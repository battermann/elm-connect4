module Main exposing (Model, Msg(..), init, main, update, view)

import Array
import Bootstrap.Alert as Alert
import Bootstrap.Button as Button
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Utilities.Flex as Flex
import Bootstrap.Utilities.Size as Size
import Bootstrap.Utilities.Spacing as Spacing
import Browser
import Connect4.Connect4 as C4 exposing (GameState(..), Player(..))
import Html exposing (Attribute, Html)
import Html.Attributes
import Html.Events
import Maybe.Extra
import Svg
import Svg.Attributes



---- MODEL ----


type alias Model =
    { game : GameState
    , selectedColumn : Maybe Int
    }


init : ( Model, Cmd Msg )
init =
    ( { game = C4.empty, selectedColumn = Nothing }, Cmd.none )



---- UPDATE ----


type Msg
    = Played Int
    | NewGame
    | ColumnHover (Maybe Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Played col ->
            ( { model | game = C4.tryMakeMove col model.game }, Cmd.none )

        NewGame ->
            ( { model | game = C4.empty }, Cmd.none )

        ColumnHover col ->
            ( { model | selectedColumn = col }, Cmd.none )



---- VIEW ----


viewCircle : Attribute Msg -> String -> String -> Html Msg
viewCircle corner backgroundColor color =
    Svg.svg
        [ Svg.Attributes.width "100%"
        , Svg.Attributes.height "100%"
        , Svg.Attributes.viewBox "0 0 42 42"
        , Html.Attributes.style "background" backgroundColor
        , corner
        ]
        [ Svg.circle
            [ Svg.Attributes.cx "21"
            , Svg.Attributes.cy "21"
            , Svg.Attributes.r "16"
            , Svg.Attributes.fill color
            ]
            []
        ]


viewCell : Attribute Msg -> Bool -> Player -> Html Msg
viewCell corner selected player =
    let
        backgroundColor =
            if selected then
                "rgba(0,0,255,0.75)"

            else
                "rgba(0,0,255,1)"
    in
    case player of
        P1 ->
            viewCircle corner backgroundColor "red"

        P2 ->
            viewCircle corner backgroundColor "yellow"

        None ->
            viewCircle corner backgroundColor "lightgray"


cornersAttr : Int -> Int -> Attribute Msg
cornersAttr col row =
    let
        corner =
            case ( col, row ) of
                ( 0, 0 ) ->
                    Just "border-top-left-radius"

                ( 0, 5 ) ->
                    Just "border-bottom-left-radius"

                ( 6, 0 ) ->
                    Just "border-top-right-radius"

                ( 6, 5 ) ->
                    Just "border-bottom-right-radius"

                _ ->
                    Nothing
    in
    corner
        |> Maybe.map (\key -> Html.Attributes.style key "15px")
        |> Maybe.withDefault (Html.Attributes.style "" "")


view : Model -> Html Msg
view { game, selectedColumn } =
    let
        ( maybeMsg, board ) =
            case game of
                InProgress c4 ->
                    ( Nothing, c4.board )

                P1Won c4 ->
                    ( Just "Red wins!", c4.board )

                P2Won c4 ->
                    ( Just "Yellow wins!", c4.board )

                Draw c4 ->
                    ( Just "Draw", c4.board )
    in
    Grid.container []
        [ CDN.stylesheet -- creates an inline style node with the Bootstrap CSS
        , Grid.row []
            [ Grid.col []
                [ maybeMsg |> Maybe.Extra.unwrap (Html.text "") (\msg -> Alert.simplePrimary [ Spacing.mt3 ] [ Html.text msg ])
                , Html.div [ Spacing.mb3, Spacing.mt3, Flex.block, Flex.row, Flex.justifyCenter, Flex.alignItemsCenter ]
                    (board
                        |> Array.map Array.toList
                        |> Array.toList
                        |> List.indexedMap
                            (\x col ->
                                Html.div
                                    [ Flex.block
                                    , Flex.col
                                    , Flex.justifyCenter
                                    , Flex.alignItemsCenter
                                    , Size.h100
                                    , Html.Events.onClick (Played x)
                                    , Html.Events.onMouseEnter (ColumnHover (Just x))
                                    , Html.Events.onMouseLeave (ColumnHover Nothing)
                                    , Html.Attributes.style "cursor" "pointer"
                                    ]
                                    (col
                                        |> List.indexedMap
                                            (\y cell ->
                                                Html.div
                                                    [ Html.Attributes.style "width" "12vmin"
                                                    , Html.Attributes.style "height" "12vmin"
                                                    , Html.Attributes.style "user-select" "none"
                                                    , Flex.block
                                                    , Flex.col
                                                    , Flex.justifyAround
                                                    , Flex.alignItemsCenter
                                                    ]
                                                    [ viewCell
                                                        (cornersAttr x y)
                                                        (selectedColumn
                                                            |> Maybe.Extra.filter ((==) x)
                                                            |> Maybe.Extra.unwrap
                                                                False
                                                                (always True)
                                                        )
                                                        cell
                                                    ]
                                            )
                                    )
                            )
                    )
                , Html.div [ Flex.block, Flex.col, Flex.alignItemsCenter ]
                    [ Button.button [ Button.outlinePrimary, Button.onClick NewGame ] [ Html.text "Reset" ]
                    , Button.linkButton
                        [ Button.roleLink, Button.attrs [ Spacing.mt3, Html.Attributes.href "https://github.com/battermann/elm-connect4" ] ]
                        [ Html.i [ Html.Attributes.class "fab fa-github" ] [], Html.text " Source Code" ]
                    ]
                ]
            ]
        ]


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
