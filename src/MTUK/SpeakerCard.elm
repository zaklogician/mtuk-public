module MTUK.SpeakerCard exposing (model, update, view, Model, Msg (Init))

import Html exposing (..)
import Html.Attributes exposing (id, class)
import Material.Grid exposing (..)
import Material.Button as Button
import Material.Options as Options
import Material.Color as Color
import Material.Card as Card
import Material.Elevation as Elevation
import Material.Icon as Icon
import Material.Spinner as Spinner
import Material

import Markdown

import MTUK.Util exposing (andThen, batched, white, spinnerGrid)
import MTUK.API as API

-- MODEL

type alias Key = String
type alias Title = String

type alias Speaker =
    { key  : String
    , icon : String
    , name : String
    , blob : Maybe String
    }

-- TODO: replace hard-coded cards with list
speaker1 : Speaker
speaker1 =
    { key  = "speaker1"
    , icon = "speaker1-icon"
    , name = "speaker1-name"
    , blob = Nothing
    }
speaker2 : Speaker
speaker2 =
    { key  = "speaker2"
    , icon = "speaker2-icon"
    , name = "speaker2-name"
    , blob = Nothing
    }
speaker3 : Speaker
speaker3 =
    { key  = "speaker3"
    , icon = "speaker3-icon"
    , name = "speaker3-name"
    , blob = Nothing
    }
speaker4 : Speaker
speaker4 =
    { key  = "speaker4"
    , icon = "speaker4-icon"
    , name = "speaker4-name"
    , blob = Nothing
    }


type alias Model =
    { speakers : List Speaker
    , error  : String
    , mdl : Material.Model
    }

model :  Model
model =
    { speakers = [ speaker1, speaker2, speaker3, speaker4 ]
    , error = ""
    , mdl = Material.model
    }

-- ACTION/UPDATE

type Msg
    = Mdl (Material.Msg Msg)
    | ApiRequest String
    | ApiResponse API.Msg
    | Init

update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        Mdl action_ ->
            Material.update Mdl action_ model
        ApiRequest key ->
            (model, Cmd.map ApiResponse (API.read key) )
        ApiResponse (API.ReadSuccess s) ->
            let
                toggle speaker =
                    if speaker.key == s.key
                        then { speaker | blob = Just s.value }
                        else speaker
            in ({ model | speakers = List.map toggle model.speakers }, Cmd.none)
        ApiResponse (API.ReadFailure x) ->
            ({ model | error = "Error: " ++ toString x }, Cmd.none)
        ApiResponse _ ->
            (model, Cmd.none)
        Init ->
            let
                request speaker = ApiRequest (speaker.key)
            in
                batched update (List.map request model.speakers) model

-- VIEW

options =
    [ Options.css "padding-left" "10px"
    , Options.cs "mld-cell mdl-cell--12col contentcard"
    , Color.background Color.white
    , Color.text Color.black
    , Elevation.e4
    ]

editBtn : Model -> Html Msg
editBtn model =
    Button.render Mdl [0,0] model.mdl
        [ Button.icon, Button.ripple, white, Options.onClick (ApiRequest "speaker1")]
        [ Icon.i "edit" ]


viewSpeaker : Speaker -> Html Msg
viewSpeaker speaker =
    let blob = case speaker.blob of
        Nothing -> spinnerGrid
        Just blob_ -> text blob_
    in
        li [class "mdl-list__item mdl-list__item--three-line"]
            [ span [class "mdl-list__item-primary-content"]
                 [ i [ id speaker.icon
                     , class "material-icons mdl-list__item-avatar"
                     ] []
                 , span [] [text speaker.name]
                 , span [class "mdl-list__item-text-body"] [ blob ]
                 ]
            ]

view : Model -> Html Msg
view model =
    Card.view
        [ Options.css "padding-left" "10px"
        , Options.cs "mld-cell mdl-cell--12col contentcard"
        , Color.background Color.white 
        , Color.text Color.black
        , Elevation.e4
        ]
        [ Card.title [] [ Card.head [] [ text "Invited Speakers" ] ]
        , Card.text []
             [ ul [class "mdl-list"] (List.map viewSpeaker model.speakers)
             , text model.error
             ]
        , Card.menu []
              [ Button.render Mdl [0,0] model.mdl
                  [ Button.icon, Button.ripple, white ]
                  [ Icon.i "edit" ]
              ]
        ]
