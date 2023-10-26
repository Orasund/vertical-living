module Game exposing (..)

import Block exposing (Block(..))
import Dict exposing (Dict)
import Random exposing (Generator, Seed)


type alias Board =
    { maxZ : Int
    , dict : Dict ( Int, Int ) { maxZ : Int, blocks : Dict Int Block }
    }


type alias Game =
    { board : Board
    , nextBlock : Block
    }


new : Game
new =
    { board = { maxZ = 0, dict = Dict.empty }
    , nextBlock = Block.BrickWall
    }


get : ( Int, Int, Int ) -> Board -> Maybe Block
get ( x, y, z ) board =
    board.dict
        |> Dict.get ( x, y )
        |> Maybe.andThen (\{ blocks } -> blocks |> Dict.get z)


place : ( Int, Int ) -> Game -> Generator Game
place pos game =
    (case Block.asList of
        head :: tail ->
            Random.uniform head tail

        [] ->
            Random.constant BrickWall
    )
        |> Random.map
            (\block ->
                let
                    z =
                        game.board.dict
                            |> Dict.get pos
                            |> Maybe.map .maxZ
                            |> Maybe.withDefault 0
                            |> (+) 1
                in
                { game
                    | board =
                        { maxZ = max game.board.maxZ z
                        , dict =
                            game.board.dict
                                |> Dict.update pos
                                    (\maybe ->
                                        maybe
                                            |> Maybe.withDefault { maxZ = 0, blocks = Dict.empty }
                                            |> (\elem ->
                                                    { maxZ = z
                                                    , blocks = Dict.insert z game.nextBlock elem.blocks
                                                    }
                                               )
                                            |> Just
                                    )
                        }
                    , nextBlock = block
                }
            )
