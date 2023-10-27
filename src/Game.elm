module Game exposing (..)

import Block exposing (Block(..))
import Dict exposing (Dict)
import Random exposing (Generator, Seed)
import Structure exposing (Structure)


type alias Board =
    { maxZ : Int
    , dict : Dict ( Int, Int ) { maxZ : Int, blocks : Dict Int Block }
    }


type alias Game =
    { board : Board
    , next : List Structure
    }


new : Game
new =
    { board = { maxZ = 0, dict = Dict.empty }
    , next = []
    }


setNext : List Structure -> Game -> Generator Game
setNext list game =
    list
        |> Random.constant
        |> Random.map (\next -> { game | next = next })


get : ( Int, Int, Int ) -> Board -> Maybe Block
get ( x, y, z ) board =
    board.dict
        |> Dict.get ( x, y )
        |> Maybe.andThen (\{ blocks } -> blocks |> Dict.get z)


place : ( Int, Int ) -> Game -> Game
place pos game =
    case game.next of
        head :: tail ->
            let
                ( x, y ) =
                    pos

                z =
                    head.blocks
                        |> List.filterMap
                            (\( ( relX, relY, relZ ), _ ) ->
                                if relZ == 0 then
                                    game.board.dict
                                        |> Dict.get ( x + relX, y + relY )
                                        |> Maybe.map .maxZ
                                        |> Maybe.withDefault 0
                                        |> (+) 1
                                        |> Just

                                else
                                    Nothing
                            )
                        |> List.maximum
                        |> Maybe.withDefault 0

                ( _, _, dimZ ) =
                    head.dimensions
            in
            { game
                | board =
                    { maxZ = max game.board.maxZ (z + dimZ)
                    , dict =
                        head.blocks
                            |> List.foldl
                                (\( ( relX, relY, relZ ), block ) ->
                                    Dict.update ( x + relX, y + relY )
                                        (\maybe ->
                                            maybe
                                                |> Maybe.withDefault { maxZ = 0, blocks = Dict.empty }
                                                |> (\elem ->
                                                        { maxZ = z + relZ
                                                        , blocks = Dict.insert (z + relZ) block elem.blocks
                                                        }
                                                   )
                                                |> Just
                                        )
                                )
                                game.board.dict
                    }
                , next = tail
            }

        [] ->
            game
