module View.Overlay exposing (..)

import Config
import Html exposing (Attribute, Html)
import Html.Attributes
import Layout
import Structure exposing (Structure)
import View.Structure


shop : { buy : List Structure -> msg, cart : List Structure, onRandom : msg } -> Html msg
shop args =
    let
        zoom =
            { zoom = 2 }
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
        |> List.concatMap
            (\( name, list ) ->
                [ Layout.textButton []
                    { label = name
                    , onPress = Just (args.buy list)
                    }
                , list
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
                ]
            )
        |> Layout.column []
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


gameMenu : { onPlay : msg } -> Html msg
gameMenu args =
    [ [ "Vertial" |> Layout.text [ Html.Attributes.class "font-size-title" ]
      , "Living" |> Layout.text [ Html.Attributes.class "font-size-title" ]
      ]
        |> Layout.column
            (Html.Attributes.style "gap" "var(--space)"
                :: Layout.centered
            )
    , "Stack structures on top of eachother. Most structures placed wins."
        |> Layout.text [ Html.Attributes.style "padding" "var(--big-space)" ]
    , Layout.textButton []
        { label = "Play"
        , onPress = args.onPlay |> Just
        }
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
