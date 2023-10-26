module View.Board exposing (..)

import Block exposing (Block(..))
import Config
import Dict
import Game exposing (Board)
import Html exposing (Html)
import Html.Attributes
import Layout
import Pixel
import View.Block


toIsomatricPos : ( Int, Int, Int ) -> ( Int, Int )
toIsomatricPos ( x, y, z ) =
    ( y
        * Config.spriteWidth
        // 2
        - x
        * Config.spriteWidth
        // 2
        + 140
    , y
        * (Config.spriteHeight - Config.squareHeight)
        // 2
        + x
        * (Config.spriteHeight - Config.squareHeight)
        // 2
        - z
        * (Config.squareHeight - Config.zoom)
        + Config.spriteHeight
        * 2
    )


selection : { onSelect : ( Int, Int ) -> msg } -> Board -> Html msg
selection args board =
    List.range 0 3
        |> List.concatMap
            (\y ->
                List.range 0 3
                    |> List.map (Tuple.pair y)
            )
        |> List.map
            (\( x, y ) ->
                let
                    ( left, top ) =
                        toIsomatricPos ( x, y, 4 )

                    active =
                        board.dict
                            |> Dict.get ( x, y )
                            |> Maybe.andThen (\{ maxZ, blocks } -> blocks |> Dict.get maxZ)
                            |> Maybe.map Block.isSolid
                            |> Maybe.withDefault True
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
        |> Html.div [ Html.Attributes.style "position" "relative" ]


toHtml : Board -> Html msg
toHtml board =
    (List.range 0 3
        |> List.concatMap
            (\y ->
                List.range 0 3
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
        |> List.map
            (\( ( x, y, z ), block ) ->
                let
                    ( left, top ) =
                        toIsomatricPos ( x, y, z - board.maxZ )
                in
                View.Block.toHtml
                    [ String.fromInt top
                        ++ "px"
                        |> Html.Attributes.style "top"
                    , String.fromInt left
                        ++ "px"
                        |> Html.Attributes.style "left"
                    , Html.Attributes.style "position" "absolute"
                    ]
                    block
            )
    )
        |> Html.div [ Html.Attributes.style "position" "relative" ]
