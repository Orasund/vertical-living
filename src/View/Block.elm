module View.Block exposing (..)

import Block exposing (Block(..))
import Config
import Html exposing (Attribute, Html)
import Pixel


fromPos : ( Int, Int ) -> List (Attribute msg) -> Html msg
fromPos pos attrs =
    Pixel.spriteImage (Pixel.pixelated :: attrs)
        { url = "assets/spritesheet.png"
        , pos = pos
        , width = Config.spriteWidth
        , height = Config.spriteHeight
        , sheetColumns = 2
        , sheetRows = 2
        }


toHtml : List (Attribute msg) -> Block -> Html msg
toHtml attrs block =
    fromPos (Block.toSprite block)
        attrs
