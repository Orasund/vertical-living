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
    [ [ WoodWall
      , Flower
      , WoodTable
      , WoodChairLeft
      , BrickWall
      , BrickStairsLeft
      ]
        |> List.map singleton
    , [ [ ( ( 0, 0, 0 ), BrickWall )
        , ( ( 1, 0, 0 ), BrickFloor )
        ]
            |> fromBlocks
      , [ ( ( 0, 0, 0 ), WoodCabinetBottomLeft )
        , ( ( 0, 1, 0 ), WoodCabinetBottomRight )
        , ( ( 0, 0, 1 ), WoodCabinetTopLeft )
        , ( ( 0, 1, 1 ), WoodCabinetTopRight )
        ]
            |> fromBlocks
      , [ ( ( 0, 0, 0 ), WoodWall )
        , ( ( 1, 0, 0 ), WoodWall )
        ]
            |> fromBlocks
      ]
    ]
        |> List.concat
