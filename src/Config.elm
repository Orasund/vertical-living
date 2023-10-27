module Config exposing (..)

{-| -}


{-| I experienced odd behaviour with dynamic screen-sizes on itch.io

That's why i tend to require width of 400-500px. Bigger screens just effect the margins.

This has also the added benefit, that the games are easier to support on mobile.

-}
screenMinWidth : number
screenMinWidth =
    400


screenMinHeight : number
screenMinHeight =
    700


title : String
title =
    "Game Template"


normalZoom : { zoom : Int }
normalZoom =
    { zoom = 6 }


spriteHeight : { zoom : Int } -> Int
spriteHeight args =
    18 * args.zoom


spriteWidth : { zoom : Int } -> Int
spriteWidth args =
    16 * args.zoom


squareHeight : { zoom : Int } -> Int
squareHeight args =
    10 * args.zoom


maxCartSize : Int
maxCartSize =
    10
