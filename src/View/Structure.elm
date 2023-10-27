module View.Structure exposing (..)

import Config
import Html exposing (Attribute, Html)
import Html.Attributes
import Structure exposing (Structure)
import View.Block


toIsomatricPos : { zoom : Int } -> ( Int, Int, Int ) -> ( Int, Int )
toIsomatricPos zoom ( x, y, z ) =
    ( y
        * Config.spriteWidth zoom
        // 2
        - x
        * Config.spriteWidth zoom
        // 2
        - Config.spriteWidth zoom
        // 2
    , y
        * (Config.spriteHeight zoom - Config.squareHeight zoom)
        // 2
        + x
        * (Config.spriteHeight zoom - Config.squareHeight zoom)
        // 2
        - z
        * (Config.squareHeight zoom - zoom.zoom)
    )


toHtml : { zoom : Int } -> List (Attribute msg) -> Structure -> Html msg
toHtml zoom attrs board =
    let
        ( dimX, dimY, dimZ ) =
            board.dimensions

        ( width, height ) =
            toIsomatricPos zoom ( dimZ, dimY, -dimZ + 1 )
    in
    (board.blocks
        |> List.map
            (\( ( x, y, z ), block ) ->
                let
                    ( left, top ) =
                        toIsomatricPos zoom ( x, y, z - dimZ + 1 )
                in
                View.Block.toHtml zoom
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
        |> Html.div
            (attrs
                ++ [ Html.Attributes.style "position" "relative"
                   , ((Config.spriteHeight zoom
                        + (dimZ - 1)
                        * Config.squareHeight zoom
                        |> String.fromInt
                      )
                        ++ "px"
                     )
                        |> Html.Attributes.style "height"
                   ]
            )
