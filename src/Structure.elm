module Structure exposing (..)

import Block exposing (Block(..))


type alias Structure =
    { dimensions : ( Int, Int, Int )
    , blocks : List ( ( Int, Int, Int ), Block )
    }


flip : Structure -> Structure
flip structure =
    let
        flipPos ( x, y, z ) =
            ( y, x, z )
    in
    { dimensions = structure.dimensions |> flipPos
    , blocks = structure.blocks |> List.map (\( pos, block ) -> ( flipPos pos, Block.flip block ))
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


sets : List Structure
sets =
    [ [ Flower
      , WoodChairLeft
      ]
        |> List.map singleton
    , [ [ ( ( 0, 0, 0 ), WoodCabinetLeftBottomLeft )
        , ( ( 0, 1, 0 ), WoodCabinetLeftBottomRight )
        , ( ( 0, 0, 1 ), WoodCabinetLeftTopLeft )
        , ( ( 0, 1, 1 ), WoodCabinetLeftTopRight )
        ]
            |> fromBlocks
      , [ ( ( 0, 0, 0 ), LongTableLeftBack )
        , ( ( 1, 0, 0 ), LongTableLeftFront )
        ]
            |> fromBlocks

      {--, [ ( ( 0, 0, 0 ), LongTableLeftBack )
        , ( ( 1, 0, 0 ), LongTableRightBack )
        , ( ( 0, 1, 0 ), LongTableLeftBack )
        , ( ( 1, 1, 0 ), LongTableLeftFront )
        ]
            |> fromBlocks--}
      , singleton BrickStairsLeft
      , [ ( ( 0, 0, 0 ), BrickWall )
        , ( ( 1, 0, 0 ), BrickFloor )
        , ( ( 2, 0, 0 ), BrickFloor )
        ]
            |> fromBlocks
      , [ ( ( 0, 0, 0 ), WoodWall )
        , ( ( 1, 0, 0 ), WoodFloor )
        , ( ( 0, 1, 0 ), WoodWall )
        , ( ( 1, 1, 0 ), WoodFloor )
        ]
            |> fromBlocks

      {--, [ ( ( 0, 0, 0 ), BrickWall )
        , ( ( 1, 0, 0 ), BrickWall )
        , ( ( 0, 1, 0 ), BrickWall )
        , ( ( 1, 1, 0 ), BrickWall )
        ]
            |> fromBlocks--}
      , [ ( ( 0, 0, 0 ), BigPlant False False False )
        , ( ( 1, 0, 0 ), BigPlant True False False )
        , ( ( 0, 1, 0 ), BigPlant False True False )
        , ( ( 1, 1, 0 ), BigPlant True True False )
        , ( ( 0, 0, 1 ), BigPlant False False True )
        , ( ( 1, 0, 1 ), BigPlant True False True )
        , ( ( 0, 1, 1 ), BigPlant False True True )
        , ( ( 1, 1, 1 ), BigPlant True True True )
        ]
            |> fromBlocks
      ]
    ]
        |> List.concat
