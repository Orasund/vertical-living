module View.Board exposing (..)

import Config
import Html exposing (Html)
import Html.Attributes
import Pixel
import View.Square


toHtml : Html msg
toHtml =
    List.range 0 3
        |> List.concatMap
            (\y ->
                List.range 0 3
                    |> List.map (Tuple.pair y)
            )
        |> List.concatMap
            (\( x, y ) ->
                List.range 0 3
                    |> List.map (\z -> ( x, y, z ))
            )
        |> List.map
            (\( x, y, z ) ->
                View.Square.toHtml
                    [ String.fromInt
                        (y
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
                        ++ "px"
                        |> Html.Attributes.style "top"
                    , (((y
                            * Config.spriteWidth
                            // 2
                            - x
                            * Config.spriteWidth
                            // 2
                            + 2
                            * Config.spriteWidth
                        )
                            |> String.fromInt
                       )
                        ++ "px"
                      )
                        |> Html.Attributes.style "left"
                    , Html.Attributes.style "position" "absolute"
                    , Pixel.pixelated
                    ]
            )
        |> Html.div [ Html.Attributes.style "position" "relative" ]
