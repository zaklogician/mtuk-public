module MTUK.Page exposing (model, update, view, Model, Msg(Init) )

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

-- MODEL


type alias Model =
    { contentCard : ContentCard.Model
    , mdl : Material.Model
    }

model : String -> String -> Model
model key title =
    { contentCard = ContentCard.model key title
    , mdl = Material.model
    }

-- ACTION/UPDATE

type Msg
    = Mdl (Material.Msg Msg)
    | ContentCardMsg ContentCard.Msg
    | Init

update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        Mdl action_ ->
            Material.update Mdl action_ model
        ContentCardMsg a ->
            lift .contentCard
                (\m x -> { m | contentCard = x })
                ContentCardMsg
                ContentCard.update a model
        Init ->
            update (ContentCardMsg ContentCard.Init) model

-- VIEW
separator : Html Msg
separator = Options.div [ Options.css "margin-bottom" "48px"] [text " "]

view : Model -> Html Msg
view model =
    Options.div
        [ Options.cs "page-content mdl-color--grey-100" ]
        [ separator
        , Html.map ContentCardMsg <| ContentCard.view model.contentCard
        , separator
        ]
