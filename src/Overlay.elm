module Overlay exposing (..)

import Structure exposing (Structure)


type Overlay
    = Shop { cart : List Structure }
    | GameMenu
