module MTUK.Schedule exposing (model, update, view, Model, Msg(Init) )

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
import MTUK.ScheduleCard as ScheduleCard

-- MODEL


type alias Model =
    { day1Card : ScheduleCard.Model
    , day2Card : ScheduleCard.Model
    , day3Card : ScheduleCard.Model
    , abstractsCard : ContentCard.Model
    , mdl : Material.Model
    }

dayCard : Int -> ScheduleCard.Model
dayCard n =
    ScheduleCard.model
        ("day" ++ toString n)
        ("Day " ++ toString n)

abstractsCard : ContentCard.Model
abstractsCard =
    ContentCard.model
        "abstracts"
        "Abstracts"

model : Model
model =
    { day1Card = dayCard 1
    , day2Card = dayCard 2
    , day3Card = dayCard 3
    , abstractsCard = abstractsCard
    , mdl = Material.model
    }



-- ACTION/UPDATE


type Msg
    = Mdl (Material.Msg Msg)
    | Day1CardMsg ScheduleCard.Msg
    | Day2CardMsg ScheduleCard.Msg
    | Day3CardMsg ScheduleCard.Msg
    | AbstractsCardMsg ContentCard.Msg
    | Init

update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        Mdl action_ ->
            Material.update Mdl action_ model
        Day1CardMsg a ->
            lift .day1Card
                (\m x -> { m | day1Card = x })
                Day1CardMsg
                ScheduleCard.update a model
        Day2CardMsg a ->
            lift .day2Card
                (\m x -> { m | day2Card = x })
                Day2CardMsg
                ScheduleCard.update a model
        Day3CardMsg a ->
            lift .day3Card
                (\m x -> { m | day3Card = x })
                Day3CardMsg
                ScheduleCard.update a model
        AbstractsCardMsg a ->
            lift .abstractsCard
                (\m x -> { m | abstractsCard = x })
                AbstractsCardMsg
                ContentCard.update a model
        Init ->
            update (AbstractsCardMsg ContentCard.Init) model
            |> andThen update (Day1CardMsg ScheduleCard.Init)
            |> andThen update (Day2CardMsg ScheduleCard.Init)
            |> andThen update (Day3CardMsg ScheduleCard.Init)

-- VIEW
separator : Html Msg
separator = Options.div [ Options.css "margin-bottom" "48px"] [text " "]

view : Model -> Html Msg
view model =
    Options.div
        [ Options.cs "page-content mdl-color--grey-100" ]
        [ separator
        , Material.Grid.grid []
              [ Material.Grid.cell [ Material.Grid.size All 4 ]
                    [ Html.map Day1CardMsg <| ScheduleCard.view model.day1Card ]
              , Material.Grid.cell [ Material.Grid.size All 4 ]
                    [ Html.map Day2CardMsg <| ScheduleCard.view model.day2Card ]
              , Material.Grid.cell [ Material.Grid.size All 4 ]
                    [ Html.map Day3CardMsg <| ScheduleCard.view model.day3Card ]
              ]
        , separator
        , Html.map AbstractsCardMsg <| ContentCard.view model.abstractsCard
        , separator
        ]
