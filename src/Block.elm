module Block exposing (..)


type Block
    = BrickWall
    | WoodWall
    | WoodTable
    | WoodChairLeft
    | WoodChairRight
    | BrickStairsLeft
    | BrickStairsRight
    | Flower
    | BrickFloor
    | WoodCabinetTopLeft
    | WoodCabinetTopRight
    | WoodCabinetBottomLeft
    | WoodCabinetBottomRight


flip : Block -> Block
flip block =
    case block of
        WoodChairLeft ->
            WoodChairRight

        WoodChairRight ->
            WoodChairLeft

        BrickStairsLeft ->
            BrickStairsRight

        BrickStairsRight ->
            BrickStairsLeft

        _ ->
            block


isSolid : Block -> Bool
isSolid block =
    case block of
        Flower ->
            False

        WoodChairLeft ->
            False

        WoodChairRight ->
            False

        BrickStairsLeft ->
            False

        BrickStairsRight ->
            False

        _ ->
            True


toSprite : Block -> ( Int, Int )
toSprite block =
    case block of
        WoodWall ->
            ( 0, 1 )

        WoodTable ->
            ( 1, 1 )

        WoodChairRight ->
            ( 2, 1 )

        WoodChairLeft ->
            ( 3, 1 )

        Flower ->
            ( 1, 0 )

        BrickWall ->
            ( 0, 2 )

        BrickFloor ->
            ( 1, 2 )

        BrickStairsRight ->
            ( 2, 2 )

        BrickStairsLeft ->
            ( 3, 2 )

        WoodCabinetTopLeft ->
            ( 4, 0 )

        WoodCabinetTopRight ->
            ( 5, 0 )

        WoodCabinetBottomLeft ->
            ( 4, 1 )

        WoodCabinetBottomRight ->
            ( 5, 1 )
