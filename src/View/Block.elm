module View.Block exposing (..)

import Block exposing (Block)
import Config
import Html exposing (Attribute, Html)
import Pixel


fromPos : { zoom : Int } -> ( Int, Int ) -> List (Attribute msg) -> Html msg
fromPos zoom pos attrs =
    Pixel.spriteImage (Pixel.pixelated :: attrs)
        { url = "assets/spritesheet.png"
        , pos = pos
        , width = Config.spriteWidth zoom |> toFloat
        , height = Config.spriteHeight zoom |> toFloat
        , sheetColumns = 8
        , sheetRows = 8
        }


toHtml : { zoom : Int } -> List (Attribute msg) -> Block -> Html msg
toHtml zoom attrs block =
    fromPos zoom
        (Block.toSprite block)
        attrs
