module Game exposing (..)

import Block exposing (Block(..))
import Config
import Dict exposing (Dict)
import Random exposing (Generator)
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
    let
        board : Board
        board =
            List.range 0 (Config.boardSize - 1)
                |> List.concatMap (\y -> List.range 0 (Config.boardSize - 1) |> List.map (Tuple.pair y))
                |> List.map (\pos -> ( pos, { maxZ = 1, blocks = Dict.singleton 1 BrickWall } ))
                |> Dict.fromList
                |> (\dict -> { maxZ = 1, dict = dict })
    in
    { board = board
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


canPlace : ( Int, Int ) -> Game -> Bool
canPlace ( x, y ) game =
    case game.next of
        head :: _ ->
            let
                isValid v =
                    0 <= v && v < Config.boardSize

                z =
                    head.blocks
                        |> List.filterMap
                            (\( ( relX, relY, relZ ), _ ) ->
                                if relZ == 0 then
                                    game.board.dict
                                        |> Dict.get ( x + relX, y + relY )
                                        |> Maybe.map .maxZ
                                        |> Maybe.withDefault 0
                                        |> Just

                                else
                                    Nothing
                            )
                        |> List.maximum
                        |> Maybe.withDefault 0
            in
            head.blocks
                |> List.all
                    (\( ( relX, relY, relZ ), block ) ->
                        if not (isValid (relX + x)) || not (isValid (relY + y)) || not (0 <= (relZ + z)) then
                            False

                        else
                            (game.board
                                |> get ( x + relX, y + relY, z + relZ + 1 )
                                |> (==) Nothing
                            )
                                && (if relZ == 0 && Block.needsGround block then
                                        game.board
                                            |> get ( x + relX, y + relY, z )
                                            |> Maybe.map Block.isSolid
                                            |> Maybe.withDefault False

                                    else
                                        True
                                   )
                    )

        [] ->
            False


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
