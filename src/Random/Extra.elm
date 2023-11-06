module Random.Extra exposing (..)

import Array
import Random exposing (Generator)


{-| Sample without replacement: produce a randomly selected element of the
list, and the list with that element omitted. If the list is empty, the
selected element will be `Nothing`.
-}
choose : List a -> Generator ( Maybe a, List a )
choose list =
    if List.isEmpty list then
        Random.constant ( Nothing, list )

    else
        let
            lastIndex =
                List.length list - 1

            front i =
                List.take i list

            back i =
                List.drop (i + 1) list

            gen =
                Random.int 0 lastIndex

            array =
                Array.fromList list
        in
        Random.map
            (\index ->
                ( Array.get index array, List.append (front index) (back index) )
            )
            gen


{-| Repeated sample without replacement: produce a list of randomly
selected elements of some list, and the list of unselected elements.
-}
choices : Int -> List a -> Generator ( List a, List a )
choices count list =
    if count < 1 then
        Random.constant ( [], list )

    else
        choose list
            |> Random.andThen
                (\( choice, remaining ) ->
                    let
                        genRest =
                            Random.lazy (\_ -> choices (count - 1) remaining)

                        addToChoices =
                            \elem ( chosen, unchosen ) -> ( elem :: chosen, unchosen )
                    in
                    case choice of
                        Nothing ->
                            Random.constant ( [], list )

                        Just elem ->
                            Random.map (addToChoices elem) genRest
                )


{-| Shuffle the list. Takes O(_n_ log _n_) time and no extra space.
-}
shuffle : List a -> Generator (List a)
shuffle list =
    let
        anyInt : Generator Int
        anyInt =
            Random.int Random.minInt Random.maxInt
    in
    Random.map
        (\independentSeed ->
            list
                |> List.foldl
                    (\item ( acc, seed ) ->
                        let
                            ( tag, nextSeed ) =
                                Random.step anyInt seed
                        in
                        ( ( item, tag ) :: acc, nextSeed )
                    )
                    ( [], independentSeed )
                |> Tuple.first
                |> List.sortBy Tuple.second
                |> List.map Tuple.first
        )
        Random.independentSeed
