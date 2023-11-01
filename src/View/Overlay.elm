module View.Overlay exposing (..)

import Config
import Html exposing (Attribute, Html)
import Html.Attributes
import Layout
import Structure exposing (Structure)
import View.Structure


shop : { buy : Structure -> msg, cart : List Structure, onRandom : msg } -> Html msg
shop args =
    let
        zoom =
            { zoom = 3 }
    in
    [ "Shop" |> Layout.text [ Html.Attributes.class "font-size-title" ]
    , args.cart
        |> List.map
            (\structure ->
                View.Structure.toHtml zoom
                    (Layout.asEl :: Layout.centered)
                    structure
                    |> Layout.el
                        ([ Html.Attributes.style "width" "80px"
                         , Html.Attributes.style "height" "80px"
                         ]
                            ++ Layout.centered
                        )
            )
        |> Layout.row []
    , "Choose " ++ String.fromInt Config.maxCartSize ++ " items to add to your house" |> Layout.text []
    , Layout.textButton [] { label = "RANDOM", onPress = args.onRandom |> Just }
    , Structure.sets
        |> List.map
            (\structure ->
                View.Structure.toHtml zoom
                    (Layout.asEl :: Layout.centered)
                    structure
                    |> Layout.button
                        ([ Html.Attributes.style "width" "80px"
                         , Html.Attributes.style "height" "80px"
                         ]
                            ++ Layout.centered
                        )
                        { label = "Buy"
                        , onPress = Just (args.buy structure)
                        }
            )
        |> Layout.row []
    ]
        |> Layout.column
            (Html.Attributes.style "gap" "var(--big-space)"
                :: Layout.centered
            )
        |> asFullScreenOverlay
            ([ Html.Attributes.style "background-color" "var(--secondary-color)"
             , Html.Attributes.style "color" "white"
             ]
                ++ Layout.centered
            )


asFullScreenOverlay : List (Attribute msg) -> Html msg -> Html msg
asFullScreenOverlay attrs =
    Layout.el
        ([ Html.Attributes.style "position" "absolute"
         , Html.Attributes.style "inset" "0 0"
         , Html.Attributes.style "height" "100%"
         , Html.Attributes.style "width" "100%"
         ]
            ++ attrs
        )
