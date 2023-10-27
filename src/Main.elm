module Main exposing (..)

import Browser
import Config
import Game exposing (Game)
import Gen.Sound exposing (Sound(..))
import Html exposing (Html)
import Html.Attributes
import Json.Decode exposing (Value)
import Layout
import Overlay exposing (Overlay(..))
import Port
import PortDefinition exposing (FromElm(..), ToElm(..))
import Random exposing (Generator, Seed)
import Structure exposing (Structure)
import View
import View.Block
import View.Board
import View.Overlay
import View.Structure


type alias Model =
    { game : Game
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
    | AddToCart Structure
    | FlipStructure


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
      , seed = Random.initialSeed 42
      , overlay = Just (Shop { cart = [] })
      }
    , [ Gen.Sound.asList |> RegisterSounds |> Port.fromElm
      , Random.generate GotSeed Random.independentSeed
      ]
        |> Cmd.batch
    )


newGame : Model -> Model
newGame model =
    { model | game = Game.new }
        |> setOverlay Nothing


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
            newGame model |> withNoCmd

        GotSeed seed ->
            model |> gotSeed seed |> withNoCmd

        SoundRequested ->
            ( model
            , PlaySound { sound = ClickButton, looping = False }
                |> Port.fromElm
            )

        Received result ->
            case result of
                Ok (SoundEnded sound) ->
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
                            , overlay =
                                if game.next == [] then
                                    Just (Shop { cart = [] })

                                else
                                    Nothing
                        }
                   )
                |> withNoCmd

        AddToCart structure ->
            case model.overlay of
                Just (Shop shop) ->
                    structure
                        :: shop.cart
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

                Nothing ->
                    model |> withNoCmd

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


viewOverlay : Model -> Overlay -> Html Msg
viewOverlay _ overlay =
    case overlay of
        Shop { cart } ->
            View.Overlay.shop
                { buy = AddToCart
                , cart = cart
                }


view :
    Model
    ->
        { title : String
        , body : List (Html Msg)
        }
view model =
    let
        content =
            [ model.game.next
                |> List.head
                |> Maybe.map
                    (View.Structure.toHtml Config.normalZoom
                        ([ Layout.asEl
                         ]
                            ++ Layout.centered
                        )
                    )
                |> Maybe.withDefault Layout.none
            , Layout.textButton [] { label = "Flip", onPress = Just FlipStructure }
            , View.Board.selection
                { onSelect = Place }
                model.game.board
            , View.Board.toHtml
                model.game.board
            ]
                |> Layout.column
                    [ Html.Attributes.style "width" "100%"
                    , Layout.alignAtCenter
                    ]
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
