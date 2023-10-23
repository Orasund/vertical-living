module View.Square exposing (..)

import Config
import Html exposing (Attribute, Html)
import Pixel


toHtml : List (Attribute msg) -> Html msg
toHtml attrs =
    Pixel.spriteImage attrs
        { url = "assets/spritesheet.png"
        , pos = ( 0, 0 )
        , width = Config.spriteWidth
        , height = Config.spriteHeight
        , sheetColumns = 2
        , sheetRows = 2
        }
