module Block exposing (..)


type Block
    = ConcreteWall
    | BrickStairsLeft
    | BrickStairsRight
    | BrickFloor
    | WoodWall
    | WoodTable
    | WoodChairLeft
    | WoodChairRight
    | Flower
    | WoodFloor
    | WoodCabinetLeftTopLeft
    | WoodCabinetLeftTopRight
    | WoodCabinetLeftBottomLeft
    | WoodCabinetLeftBottomRight
    | WoodCabinetRightTopLeft
    | WoodCabinetRightTopRight
    | WoodCabinetRightBottomLeft
    | WoodCabinetRightBottomRight
    | LongTableLeftFront
    | LongTableLeftBack
    | LongTableRightFront
    | LongTableRightBack
    | BigPlant Bool Bool Bool
    | Gras
    | Vines
    | GrasPillar Bool
    | Pillar Bool
    | RightShelf Bool
    | LeftShelf Bool
    | CoatHanger Bool
    | WallPaneling { left : Bool }
    | DoorMat
    | Coat { left : Bool } Bool


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

        WoodCabinetLeftTopLeft ->
            WoodCabinetRightTopRight

        WoodCabinetLeftTopRight ->
            WoodCabinetRightTopLeft

        WoodCabinetLeftBottomLeft ->
            WoodCabinetRightBottomRight

        WoodCabinetLeftBottomRight ->
            WoodCabinetRightBottomLeft

        WoodCabinetRightTopRight ->
            WoodCabinetLeftTopLeft

        WoodCabinetRightTopLeft ->
            WoodCabinetLeftTopRight

        WoodCabinetRightBottomRight ->
            WoodCabinetLeftBottomLeft

        WoodCabinetRightBottomLeft ->
            WoodCabinetLeftBottomRight

        LongTableLeftBack ->
            LongTableRightBack

        LongTableRightBack ->
            LongTableLeftBack

        LongTableLeftFront ->
            LongTableRightFront

        LongTableRightFront ->
            LongTableLeftFront

        BigPlant a b c ->
            BigPlant b a c

        RightShelf b ->
            LeftShelf b

        LeftShelf b ->
            RightShelf b

        WallPaneling { left } ->
            WallPaneling { left = not left }

        Coat { left } b ->
            Coat { left = not left } b

        _ ->
            block


needsGround : Block -> Bool
needsGround block =
    case block of
        BrickFloor ->
            False

        WoodFloor ->
            False

        Vines ->
            False

        Coat _ _ ->
            False

        _ ->
            True


isSolid : Block -> Bool
isSolid block =
    case block of
        Flower ->
            False

        Gras ->
            False

        WoodChairLeft ->
            False

        WoodChairRight ->
            False

        BrickStairsLeft ->
            False

        BrickStairsRight ->
            False

        BigPlant _ _ _ ->
            False

        WallPaneling _ ->
            False

        CoatHanger _ ->
            False

        LongTableLeftBack ->
            False

        LongTableLeftFront ->
            False

        LongTableRightBack ->
            False

        LongTableRightFront ->
            False

        DoorMat ->
            False

        Coat _ _ ->
            False

        _ ->
            True


toSprite : Block -> ( Int, Int )
toSprite block =
    case block of
        WoodWall ->
            ( 0, 1 )

        WoodTable ->
            ( 3, 0 )

        WoodChairRight ->
            ( 2, 1 )

        WoodChairLeft ->
            ( 3, 1 )

        Flower ->
            ( 3, 4 )

        ConcreteWall ->
            ( 0, 2 )

        BrickFloor ->
            ( 1, 2 )

        WoodFloor ->
            ( 1, 1 )

        BrickStairsRight ->
            ( 2, 2 )

        BrickStairsLeft ->
            ( 3, 2 )

        WoodCabinetLeftTopLeft ->
            ( 4, 0 )

        WoodCabinetLeftTopRight ->
            ( 5, 0 )

        WoodCabinetLeftBottomLeft ->
            ( 4, 1 )

        WoodCabinetLeftBottomRight ->
            ( 5, 1 )

        WoodCabinetRightTopLeft ->
            ( 4, 2 )

        WoodCabinetRightTopRight ->
            ( 5, 2 )

        WoodCabinetRightBottomLeft ->
            ( 4, 3 )

        WoodCabinetRightBottomRight ->
            ( 5, 3 )

        LongTableLeftBack ->
            ( 7, 0 )

        LongTableLeftFront ->
            ( 6, 0 )

        LongTableRightBack ->
            ( 6, 1 )

        LongTableRightFront ->
            ( 7, 1 )

        BigPlant False False False ->
            ( 0, 0 )

        BigPlant True False False ->
            ( 0, 7 )

        BigPlant False True False ->
            ( 1, 5 )

        BigPlant False False True ->
            ( 0, 4 )

        BigPlant True True False ->
            ( 1, 7 )

        BigPlant False True True ->
            ( 1, 4 )

        BigPlant True False True ->
            ( 0, 6 )

        BigPlant True True True ->
            ( 1, 6 )

        GrasPillar True ->
            ( 2, 4 )

        GrasPillar False ->
            ( 3, 5 )

        Gras ->
            ( 2, 6 )

        Vines ->
            ( 2, 5 )

        Pillar False ->
            ( 4, 5 )

        Pillar True ->
            ( 4, 4 )

        RightShelf False ->
            ( 6, 3 )

        RightShelf True ->
            ( 6, 2 )

        LeftShelf False ->
            ( 7, 3 )

        LeftShelf True ->
            ( 7, 2 )

        CoatHanger False ->
            ( 6, 5 )

        CoatHanger True ->
            ( 6, 4 )

        WallPaneling { left } ->
            if left then
                ( 5, 5 )

            else
                ( 5, 4 )

        DoorMat ->
            ( 7, 4 )

        Coat { left } b ->
            case ( left, b ) of
                ( True, True ) ->
                    ( 6, 6 )

                ( True, False ) ->
                    ( 6, 7 )

                ( False, True ) ->
                    ( 7, 6 )

                ( False, False ) ->
                    ( 7, 7 )
