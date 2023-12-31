module View.Board exposing (..)

import Block exposing (Block(..))
import Config
import Dict
import Game exposing (Board, Game)
import Html exposing (Html)
import Html.Attributes
import Layout
import Pixel
import Structure exposing (Structure)
import View.Structure


selection : { onSelect : ( Int, Int ) -> msg } -> Game -> Html msg
selection args game =
    let
        zoom =
            Config.normalZoom
    in
    List.range 0 (Config.boardSize - 1)
        |> List.concatMap
            (\y ->
                List.range 0 (Config.boardSize - 1)
                    |> List.map (Tuple.pair y)
            )
        |> List.map
            (\( x, y ) ->
                let
                    ( left, top ) =
                        View.Structure.toIsomatricPos zoom ( x, y, 0 )

                    active =
                        game |> Game.canPlace ( x, y )
                in
                Layout.textButton
                    [ String.fromInt top
                        ++ "px"
                        |> Html.Attributes.style "top"
                    , String.fromInt left
                        ++ "px"
                        |> Html.Attributes.style "left"
                    , Html.Attributes.style "position" "absolute"
                    , Pixel.pixelated
                    , Html.Attributes.class "as-square"
                    , Html.Attributes.disabled (not active)
                    ]
                    { label = "place"
                    , onPress =
                        if active then
                            args.onSelect ( x, y ) |> Just

                        else
                            Nothing
                    }
            )
        |> Html.div
            [ Html.Attributes.style "position" "relative"
            , (Config.boardSize * Config.spriteHeight zoom - Config.boardSize * Config.squareHeight zoom |> String.fromInt)
                ++ "px"
                |> Html.Attributes.style "height"
            ]


toHtml : Board -> Html msg
toHtml board =
    let
        zoom =
            Config.normalZoom
    in
    List.range 0 (Config.boardSize - 1)
        |> List.concatMap
            (\y ->
                List.range 0 (Config.boardSize - 1)
                    |> List.map (Tuple.pair y)
            )
        |> List.concatMap
            (\( x, y ) ->
                List.range 1 board.maxZ
                    |> List.map (\z -> ( x, y, z ))
            )
        |> List.filterMap
            (\pos ->
                Game.get pos board
                    |> Maybe.map (Tuple.pair pos)
            )
        |> Structure.fromBlocks
        |> View.Structure.toHtml zoom []
