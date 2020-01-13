module Main exposing (FromServer(..), FromUi(..), Model, Msg(..), fromServer, init, main, update, view, viewItem)

import Api exposing (..)
import Browser
import Debug exposing (log)
import Dict exposing (Dict)
import File exposing (File)
import File.Select as Select
import Html exposing (Html, button, div, input, li, text, ul)
import Html.Attributes exposing (style, type_, value)
import Html.Events exposing (on, onClick, onInput)
import Http
import Json.Decode as D
import Task


main : Program () Model Msg
main =
    Browser.element
        { init = \() -> init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }



-- MODEL


type alias Model =
    { items : Dict Int Item
    , addItemInput : String
    , pic : String
    , files : List File
    , previews : List String
    , error : Maybe String
    }


init : ( Model, Cmd Msg )
init =
    ( Model Dict.empty "" "" [] [] Nothing
    , Api.getApiItem (fromServer Initial)
    )



-- UPDATE


type Msg
    = FromServer FromServer
    | FromUi FromUi
    | Error String


type FromServer
    = Initial (List ItemId)
    | NewItem Item
    | Delete ItemId


type FromUi
    = AddItemInputChange String
    | AddItemButton
    | GotImages ItemId (List File)
    | PutImages ItemId (List String)
    | GotPreviews (List String)
    | Done ItemId


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FromServer fromServerMsg ->
            case fromServerMsg of
                Initial itemIds ->
                    ( model
                    , itemIds
                        |> List.map (\id -> getApiItemByItemId id (fromServer NewItem))
                        |> Cmd.batch
                    )

                NewItem item ->
                    ( { model | items = Dict.insert item.id item model.items }
                    , Cmd.none
                    )

                Delete id ->
                    ( { model | items = Dict.remove id model.items }
                    , Cmd.none
                    )

        FromUi fromUi ->
            case fromUi of
                AddItemButton ->
                    let
                        itemName =
                            model.addItemInput
                    in
                    if itemName == "" then
                        update (Error "empty field") model

                    else
                        ( { model | addItemInput = "" }
                        , postApiItem itemName (fromServer (\id -> NewItem (Item id itemName "")))
                        )

                AddItemInputChange t ->
                    ( { model | addItemInput = t, error = Nothing }
                    , Cmd.none
                    )

                GotImages itemId files ->
                     (model,
                     Task.perform (FromUi << (PutImages itemId))
                         <| Task.sequence
                         <| List.map File.toUrl (files))

                PutImages itemId urls ->
                    let api url = Api.putApiItemImageByItemId
                            (log "itemId" itemId)
                            (log "url" url)
                            (fromServer (\() -> Initial itemIds))
                        itemIds = Dict.keys model.items
                    in (model, List.map api urls |> Cmd.batch)

                GotPreviews urls -> ({model | previews = urls ++ model.previews}, Cmd.none)

                Done id ->
                    ( model
                    , deleteApiItemByItemId id (fromServer (\() -> Delete id))
                    )

        Error error ->
            ( { model | error = Just error }, Cmd.none )


fromServer : (a -> FromServer) -> Result Http.Error a -> Msg
fromServer msgConstructor result =
    case result of
        Ok content ->
            FromServer <| msgConstructor content

        Err error ->
            Error <| httpErrorToString error


httpErrorToString : Http.Error -> String
httpErrorToString error =
    case error of
        Http.BadUrl s ->
            "bad url: " ++ s

        Http.Timeout ->
            "timeout"

        Http.NetworkError ->
            "network error"

        Http.BadStatus status ->
            "bad status: " ++ String.fromInt status

        Http.BadBody response ->
            "bad payload: " ++ response



-- VIEW


view : Model -> Html Msg
view model =
    let
        items =
            List.map (viewItem << Tuple.second) (Dict.toList model.items)

        error =
            model.error
                |> Maybe.map viewError
                |> Maybe.withDefault (Html.text "")

        previews = List.map viewPreview model.previews

    in
    div []
        [ ul [] items
        , input [ onInput (FromUi << AddItemInputChange), value model.addItemInput ] []
        , button [ onClick (FromUi AddItemButton) ] [ text "add item" ]
        , div [] previews
        , error
        ]

filesDecoder : D.Decoder (List File)
filesDecoder =
  D.at ["target","files"] (D.list File.decoder)

viewPreview : String -> Html Msg
viewPreview url = div
    [ style "width" "60px"
    , style "height" "60px"
    , style "background-image" ("url('" ++ (log "url" url) ++ "')")
    , style "background-position" "center"
    , style "background-repeat" "no-repeat"
    , style "background-size" "contain"
    ]
    []

viewItem : Item -> Html Msg
viewItem item =
    li []
        [ text item.text
        , viewPreview item.pic
        , input [
            type_ "file"
          , on "change" (D.map (FromUi << (GotImages item.id)) filesDecoder)
          ] []
        , text " - "
        , button [ onClick (FromUi <| Done item.id) ] [ text "done" ]
        ]


viewError : String -> Html msg
viewError error =
    div
        []
        [ text <| "Error: " ++ error ]
