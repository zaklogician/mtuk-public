module MTUK.Overview exposing (model, update, view, Model, Msg(Init) )

import Html exposing (..)
import Html.Attributes exposing (id, class)
import Material.Grid exposing (..)
import Material.Button as Button
import Material.Options as Options
import Material.Color as Color
import Material.Card as Card
import Material.Elevation as Elevation
import Material.Icon as Icon
import Material
import Material.Helpers exposing (pure, lift, map1st, map2nd)

import MTUK.Util exposing (andThen)
import MTUK.ContentCard as ContentCard
import MTUK.SpeakerCard as SpeakerCard

-- MODEL


type alias Model =
    { introCard : ContentCard.Model
    , speakerCard : SpeakerCard.Model
    , supportCard : ContentCard.Model
    , mdl : Material.Model
    }

introCard : ContentCard.Model
introCard =
    ContentCard.model
        "intro"
        "British Postgraduate Model Theory Conference 2019"

supportCard : ContentCard.Model
supportCard =
    ContentCard.model
        "support"
        ""

model : Model
model =
    { introCard = introCard
    , speakerCard = SpeakerCard.model
    , supportCard = supportCard
    , mdl = Material.model
    }



-- ACTION/UPDATE


type Msg
    = Mdl (Material.Msg Msg)
    | IntroCardMsg ContentCard.Msg
    | SpeakerCardMsg SpeakerCard.Msg
    | SupportCardMsg ContentCard.Msg
    | Init

update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        Mdl action_ ->
            Material.update Mdl action_ model
        IntroCardMsg a ->
            lift .introCard
                (\m x -> { m | introCard = x })
                IntroCardMsg
                ContentCard.update a model
        SpeakerCardMsg a ->
            lift .speakerCard
                (\m x -> { m | speakerCard = x })
                SpeakerCardMsg
                SpeakerCard.update a model
        SupportCardMsg a ->
            lift .supportCard
                (\m x -> { m | supportCard = x })
                SupportCardMsg
                ContentCard.update a model
        Init ->
            update (IntroCardMsg ContentCard.Init) model
            |> andThen update (SpeakerCardMsg SpeakerCard.Init)
            |> andThen update (SupportCardMsg ContentCard.Init)

-- VIEW
separator : Html Msg
separator = Options.div [ Options.css "margin-bottom" "48px"] [text " "]

view : Model -> Html Msg
view model =
    Options.div
        [ Options.cs "page-content mdl-color--grey-100" ]
        [ separator
        , Html.map IntroCardMsg <| ContentCard.view model.introCard
        , separator
        , Html.map SpeakerCardMsg <| SpeakerCard.view model.speakerCard
        , separator
        , Html.map SupportCardMsg <| ContentCard.view model.supportCard
        , separator
        ]
