module Structure exposing (..)

import Block exposing (Block(..))


type alias Structure =
    { dimensions : ( Int, Int, Int )
    , blocks : List ( ( Int, Int, Int ), Block )
    }


flipAround : ( Int, Int ) -> List ( ( Int, Int, Int ), Block ) -> List ( ( Int, Int, Int ), Block )
flipAround ( x, y ) =
    let
        flipPos ( x2, y2, z2 ) =
            ( y2 - y + x, (x2 - x) + y, z2 )
    in
    List.map (\( pos, block ) -> ( flipPos pos, Block.flip block ))


flip : Structure -> Structure
flip structure =
    let
        flipPos ( x, y, z ) =
            ( y, x, z )
    in
    { dimensions = structure.dimensions |> flipPos
    , blocks = structure.blocks |> flipAround ( 0, 0 )
    }


fromBlocks : List ( ( Int, Int, Int ), Block ) -> Structure
fromBlocks list =
    let
        maxBy fun =
            list
                |> List.map fun
                |> List.maximum
                |> Maybe.withDefault 0
                |> (+) 1
    in
    { dimensions =
        ( maxBy (\( ( x, _, _ ), _ ) -> x)
        , maxBy (\( ( _, y, _ ), _ ) -> y)
        , maxBy (\( ( _, _, z ), _ ) -> z)
        )
    , blocks = list
    }


singleton : Block -> Structure
singleton block =
    [ ( ( 0, 0, 0 ), block ) ] |> fromBlocks


shelf : ( Int, Int, Int ) -> List ( ( Int, Int, Int ), Block )
shelf ( x, y, z ) =
    [ ( ( x, y, z ), RightShelf False )
    , ( ( x, y, z + 1 ), RightShelf True )
    ]


cabinet : ( Int, Int, Int ) -> List ( ( Int, Int, Int ), Block )
cabinet ( x, y, z ) =
    [ ( ( x, y, z ), WoodCabinetLeftBottomLeft )
    , ( ( x, y + 1, z ), WoodCabinetLeftBottomRight )
    , ( ( x, y, z + 1 ), WoodCabinetLeftTopLeft )
    , ( ( x, y + 1, z + 1 ), WoodCabinetLeftTopRight )
    ]


verticalShelf : ( Int, Int, Int ) -> List ( ( Int, Int, Int ), Block )
verticalShelf ( x, y, z ) =
    [ longTable ( x, y, z ) |> flipAround ( x, y )
    , [ ( ( x, y, z + 1 ), WallPaneling { left = True } )
      , ( ( x, y + 1, z + 1 ), WallPaneling { left = True } )
      ]
    ]
        |> List.concat


longTable : ( Int, Int, Int ) -> List ( ( Int, Int, Int ), Block )
longTable ( x, y, z ) =
    [ ( ( x, y, z ), LongTableLeftBack )
    , ( ( x + 1, y, z ), LongTableLeftFront )
    ]


stairs : ( Int, Int, Int ) -> List ( ( Int, Int, Int ), Block )
stairs ( x, y, z ) =
    [ ( ( x, y, z ), BrickFloor )
    , ( ( x + 1, y, z ), BrickStairsLeft )
    ]


longStairs : ( Int, Int, Int ) -> List ( ( Int, Int, Int ), Block )
longStairs ( x, y, z ) =
    [ ( ( x, y, z ), BrickFloor )
    , ( ( x + 1, y, z ), BrickFloor )
    , ( ( x + 2, y, z ), BrickStairsLeft )
    ]


floor : ( Int, Int, Int ) -> List ( ( Int, Int, Int ), Block )
floor ( x, y, z ) =
    [ ( ( x, y, z ), ConcreteWall )
    , ( ( x + 1, y, z ), BrickFloor )
    , ( ( x + 2, y, z ), BrickFloor )
    ]


vinePillar : ( Int, Int, Int ) -> List ( ( Int, Int, Int ), Block )
vinePillar ( x, y, z ) =
    [ ( ( x, y, z + 1 ), Vines )
    , ( ( x + 1, y, z ), GrasPillar False )
    , ( ( x + 1, y, z + 1 ), GrasPillar True )
    ]


bigVinePillar : ( Int, Int, Int ) -> List ( ( Int, Int, Int ), Block )
bigVinePillar ( x, y, z ) =
    [ ( ( x, y, z + 1 ), Vines )
    , ( ( x + 1, y, z ), GrasPillar False )
    , ( ( x + 1, y, z + 1 ), GrasPillar True )
    , ( ( x, y + 1, z + 1 ), Vines )
    , ( ( x + 1, y + 1, z + 1 ), Vines )
    ]


pillar : ( Int, Int, Int ) -> List ( ( Int, Int, Int ), Block )
pillar ( x, y, z ) =
    [ ( ( x, y, z + 1 ), BrickFloor )
    , ( ( x + 1, y, z ), Pillar False )
    , ( ( x + 1, y, z + 1 ), Pillar True )
    ]


bigPillar : ( Int, Int, Int ) -> List ( ( Int, Int, Int ), Block )
bigPillar ( x, y, z ) =
    [ ( ( x, y, z + 1 ), BrickFloor )
    , ( ( x + 1, y, z ), Pillar False )
    , ( ( x + 1, y, z + 1 ), Pillar True )
    , ( ( x, y + 1, z + 1 ), BrickFloor )
    , ( ( x + 1, y + 1, z + 1 ), BrickFloor )
    ]


gras : ( Int, Int, Int ) -> List ( ( Int, Int, Int ), Block )
gras ( x, y, z ) =
    [ ( ( x, y, z ), Gras )
    , ( ( x + 1, y, z ), Gras )
    ]


bigPlant : ( Int, Int, Int ) -> List ( ( Int, Int, Int ), Block )
bigPlant ( x, y, z ) =
    [ ( ( x, y, z ), BigPlant False False False )
    , ( ( x + 1, y, z ), BigPlant True False False )
    , ( ( x, y + 1, z ), BigPlant False True False )
    , ( ( x + 1, y + 1, z ), BigPlant True True False )
    , ( ( x, y, z + 1 ), BigPlant False False True )
    , ( ( x + 1, y, z + 1 ), BigPlant True False True )
    , ( ( x, y + 1, z + 1 ), BigPlant False True True )
    , ( ( x + 1, y + 1, z + 1 ), BigPlant True True True )
    ]


coatHanger : ( Int, Int, Int ) -> List ( ( Int, Int, Int ), Block )
coatHanger ( x, y, z ) =
    [ ( ( 0, 0, 0 ), CoatHanger False )
    , ( ( 0, 0, 1 ), CoatHanger True )
    , ( ( 1, 0, 0 ), Coat { left = True } False )
    , ( ( 1, 0, 1 ), Coat { left = True } True )
    ]


sets : List ( String, List Structure )
sets =
    [ ( "Dinning Room"
      , [ shelf ( 0, 0, 0 ) |> fromBlocks
        , cabinet ( 0, 0, 0 ) |> fromBlocks
        , cabinet ( 0, 0, 0 ) |> fromBlocks
        , cabinet ( 0, 0, 0 ) |> fromBlocks
        , [ [ ( ( 0, 0, 0 ), WoodChairRight )
            , ( ( 1, 0, 0 ), WoodChairRight )
            ]
          , longTable ( 0, 1, 0 )
          ]
            |> List.concat
            |> flipAround ( 0, 0 )
            |> fromBlocks
        , floor ( 0, 0, 0 ) |> fromBlocks
        , floor ( 0, 0, 0 ) |> fromBlocks
        , stairs ( 0, 0, 0 ) |> fromBlocks
        , pillar ( 0, 0, 0 ) |> fromBlocks
        , singleton Flower

        {--, [ ( ( 0, 0, 0 ), LongTableLeftBack )
        , ( ( 1, 0, 0 ), LongTableRightBack )
        , ( ( 0, 1, 0 ), LongTableLeftBack )
        , ( ( 1, 1, 0 ), LongTableLeftFront )
        ]
            |> fromBlocks--}
        {--, [ ( ( 0, 0, 0 ), WoodWall )
          , ( ( 1, 0, 0 ), WoodFloor )
          , ( ( 0, 1, 0 ), WoodWall )
          , ( ( 1, 1, 0 ), WoodFloor )
          ]
            |> fromBlocks--}
        ]
      )
    , ( "Entrance Hall"
      , [ stairs ( 0, 0, 0 ) |> fromBlocks
        , stairs ( 0, 0, 0 ) |> fromBlocks
        , [ floor ( 0, 0, 0 )
          , floor ( 0, 1, 0 )
          ]
            |> List.concat
            |> fromBlocks
        , [ floor ( 0, 0, 0 )
          , floor ( 0, 1, 0 )
          ]
            |> List.concat
            |> fromBlocks
        , [ ( ( 0, 0, 0 ), DoorMat )
          , ( ( 1, 0, 0 ), DoorMat )
          ]
            |> fromBlocks
        , singleton WoodChairLeft
        , coatHanger ( 0, 0, 0 ) |> fromBlocks
        , shelf ( 0, 0, 0 ) |> fromBlocks
        , cabinet ( 0, 0, 0 ) |> fromBlocks
        , verticalShelf ( 0, 0, 0 ) |> fromBlocks
        ]
      )
    , ( "Garden"
      , [ vinePillar ( 0, 0, 0 ) |> fromBlocks
        , vinePillar ( 0, 0, 0 ) |> fromBlocks
        , [ gras ( 0, 0, 0 )
          , gras ( 0, 1, 0 )
          ]
            |> List.concat
            |> fromBlocks
        , gras ( 0, 0, 0 ) |> fromBlocks
        , stairs ( 0, 0, 0 ) |> fromBlocks
        , stairs ( 0, 0, 0 ) |> fromBlocks
        , [ ( ( 0, 0, 0 ), BrickFloor )
          , ( ( 1, 0, 0 ), ConcreteWall )
          ]
            |> fromBlocks
        , [ ( ( 0, 0, 0 ), BrickFloor )
          , ( ( 1, 0, 0 ), ConcreteWall )
          ]
            |> fromBlocks
        , [ ( ( 0, 0, 0 ), BrickFloor )
          , ( ( 1, 0, 0 ), ConcreteWall )
          ]
            |> fromBlocks
        , bigPillar ( 0, 0, 0 ) |> fromBlocks
        ]
      )
    ]
