module Block exposing (..)


type Block
    = BrickWall
    | WoodWall
    | Flower


asList : List Block
asList =
    [ BrickWall, WoodWall, Flower ]


isSolid : Block -> Bool
isSolid block =
    case block of
        Flower ->
            False

        _ ->
            True


toSprite : Block -> ( Int, Int )
toSprite block =
    case block of
        BrickWall ->
            ( 0, 1 )

        WoodWall ->
            ( 0, 0 )

        Flower ->
            ( 1, 0 )
