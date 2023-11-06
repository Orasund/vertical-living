module Main exposing (..)

import Browser
import Config
import Game exposing (Game)
import Gen.Sound exposing (Sound(..))
import Html exposing (Html)
import Html.Attributes
import Json.Decode
import Layout
import Overlay exposing (Overlay(..))
import Port
import PortDefinition exposing (FromElm(..), ToElm(..))
import Random exposing (Generator, Seed)
import Random.Extra
import Structure exposing (Structure)
import View
import View.Board
import View.Overlay
import View.Structure


type alias Model =
    { game : Game
    , undo : Maybe Game
    , overlay : Maybe Overlay
    , seed : Seed
    }


type Msg
    = NewGame
    | SetOverlay (Maybe Overlay)
    | SoundRequested
    | Received (Result Json.Decode.Error ToElm)
    | GotSeed Seed
    | Place ( Int, Int )
    | AddToCart (List Structure)
    | RandomCart
    | FlipStructure
    | Undo


apply : Model -> Generator Model -> Model
apply { seed } generator =
    let
        ( model, newSeed ) =
            Random.step generator seed
    in
    { model | seed = newSeed }


init : () -> ( Model, Cmd Msg )
init () =
    ( { game = Game.new
      , undo = Nothing
      , seed = Random.initialSeed 42
      , overlay = Just GameMenu
      }
    , [ Gen.Sound.asList |> RegisterSounds |> Port.fromElm
      , Random.generate GotSeed Random.independentSeed
      ]
        |> Cmd.batch
    )


newGame : Model -> ( Model, Cmd msg )
newGame model =
    ( { model
        | game = Game.new
        , overlay = Just (Shop { cart = [] })
      }
    , PlaySound { looping = True, sound = Theme }
        |> Port.fromElm
    )


gotSeed : Seed -> Model -> Model
gotSeed seed model =
    { model | seed = seed }


setOverlay : Maybe Overlay -> Model -> Model
setOverlay maybeOverlay model =
    { model | overlay = maybeOverlay }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        withNoCmd a =
            ( a, Cmd.none )
    in
    case msg of
        NewGame ->
            newGame model

        GotSeed seed ->
            model |> gotSeed seed |> withNoCmd

        SoundRequested ->
            ( model
            , PlaySound { sound = ClickButton, looping = False }
                |> Port.fromElm
            )

        Received result ->
            case result of
                Ok (SoundEnded _) ->
                    model |> withNoCmd

                Err error ->
                    let
                        _ =
                            Debug.log "received invalid json" error
                    in
                    model |> withNoCmd

        SetOverlay maybeOverlay ->
            model |> setOverlay maybeOverlay |> withNoCmd

        Place pos ->
            Game.place pos model.game
                |> (\game ->
                        { model
                            | game = game
                            , undo = model.game |> Just
                            , overlay =
                                if game.next == [] then
                                    Just (Shop { cart = [] })

                                else
                                    Nothing
                        }
                   )
                |> withNoCmd

        AddToCart list ->
            case model.overlay of
                Just (Shop shop) ->
                    list
                        ++ shop.cart
                        |> (\cart ->
                                if List.length cart >= Config.maxCartSize then
                                    model.game
                                        |> Game.setNext cart
                                        |> Random.map (\game -> { model | game = game, overlay = Nothing })
                                        |> apply model

                                else
                                    { model | overlay = Just (Shop { cart = cart }) }
                           )
                        |> withNoCmd

                _ ->
                    model |> withNoCmd

        RandomCart ->
            Structure.sets
                |> Random.Extra.choices 2
                |> Random.andThen
                    (\( list, _ ) ->
                        (case list of
                            head :: tail ->
                                Random.uniform head tail
                                    |> Random.andThen
                                        (\( _, l ) ->
                                            case l of
                                                h :: t ->
                                                    Random.uniform h t

                                                [] ->
                                                    Random.constant (Structure.fromBlocks [])
                                        )

                            [] ->
                                Random.constant (Structure.fromBlocks [])
                        )
                            |> Random.list Config.maxCartSize
                    )
                |> Random.andThen
                    (\cart ->
                        model.game
                            |> Game.setNext cart
                            |> Random.map (\game -> { model | game = game, overlay = Nothing })
                    )
                |> apply model
                |> withNoCmd

        FlipStructure ->
            case model.game.next of
                head :: tail ->
                    model.game
                        |> (\game ->
                                { game | next = Structure.flip head :: tail }
                           )
                        |> (\game -> { model | game = game })
                        |> withNoCmd

                [] ->
                    model |> withNoCmd

        Undo ->
            case model.undo of
                Just game ->
                    { model | game = game, undo = Nothing }
                        |> withNoCmd

                Nothing ->
                    model |> withNoCmd


viewOverlay : Model -> Overlay -> Html Msg
viewOverlay _ overlay =
    case overlay of
        Shop { cart } ->
            View.Overlay.shop
                { buy = AddToCart
                , cart = cart
                , onRandom = RandomCart
                }

        GameMenu ->
            View.Overlay.gameMenu
                { onPlay = NewGame }


view :
    Model
    ->
        { title : String
        , body : List (Html Msg)
        }
view model =
    let
        content =
            model.game.next
                |> List.head
                |> Maybe.map
                    (\structure ->
                        [ View.Structure.toHtml Config.normalZoom
                            (Layout.asEl :: Layout.centered)
                            structure
                        , Layout.textButton [] { label = "Flip", onPress = Just FlipStructure }
                        , if model.undo /= Nothing then
                            Layout.textButton [] { label = "Undo", onPress = Just Undo }

                          else
                            Layout.none
                        , View.Board.selection
                            { onSelect = Place }
                            model.game
                        , View.Board.toHtml
                            model.game.board
                        ]
                            |> Layout.column
                                [ Html.Attributes.style "width" "100%"
                                , Layout.alignAtCenter
                                ]
                    )
                |> Maybe.withDefault Layout.none
    in
    { title = Config.title
    , body =
        [ View.viewportMeta

        --, View.stylesheet
        , model.overlay
            |> Maybe.map (viewOverlay model)
            |> Maybe.map List.singleton
            |> Maybe.withDefault []
            |> (::) (content |> Layout.el [ Html.Attributes.class "content" ])
            |> Html.div
                [ Html.Attributes.style "width" (String.fromFloat Config.screenMinWidth ++ "px")
                , Html.Attributes.style "height" (String.fromFloat Config.screenMinHeight ++ "px")
                , Html.Attributes.class "container"
                ]
        ]
    }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Port.toElm |> Sub.map Received


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
