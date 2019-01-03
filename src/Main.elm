module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (href, class, style, src, alt)
import Html.Lazy
import Platform.Cmd exposing (..)
import Array exposing (Array)
import Dict exposing (Dict)
import String
import Navigation
import RouteUrl as Routing

import Material
import Material.Color as Color
import Material.Layout as Layout
import Material.Helpers exposing (pure, lift, map1st, map2nd)
import Material.Options as Options exposing (when)
import Material.Scheme as Scheme
import Material.Icon as Icon
import Material.Typography as Typography
import Material.Menu as Menu
import Material.Toggles as Toggles
import Material.Button as Button

import MTUK.Util exposing (nth, andThen)
import MTUK.Overview
import MTUK.Schedule
import MTUK.Page
import MTUK.ContentCard
import MTUK.API

-- MODEL


type alias Model =
    { mdl : Material.Model
    , overview : MTUK.Overview.Model
    , schedule : MTUK.Schedule.Model
    , venue : MTUK.Page.Model
    , information : MTUK.Page.Model
    , selectedTab : Int
    , logMessages : Bool
    }


model : Model
model =
    { mdl = Material.model
    , overview = MTUK.Overview.model
    , schedule = MTUK.Schedule.model
    , venue = MTUK.Page.model "venue" "Venue"
    , information = MTUK.Page.model "information" "Information"
    , selectedTab = 0
    , logMessages = False
    }


-- ACTION, UPDATE


type Msg
    = SelectTab Int
    | OverviewMsg MTUK.Overview.Msg
    | ScheduleMsg MTUK.Schedule.Msg
    | VenueMsg MTUK.Page.Msg
    | InformationMsg MTUK.Page.Msg
    | ApiMsg MTUK.API.Msg
    | ToggleLog
    | Init
    | Mdl (Material.Msg Msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let 
        log msg = 
            if model.logMessages then Debug.log "Msg" else identity
    in
      case log "Msg" msg of
          ApiMsg a -> ( model, Cmd.none )

          SelectTab k ->
              ( { model | selectedTab = k }, Cmd.none )
          ToggleLog -> 
              ( { model | logMessages = not model.logMessages }, Cmd.none )

          OverviewMsg a ->
              lift .overview (\m x -> { m | overview = x }) OverviewMsg MTUK.Overview.update a model

          ScheduleMsg a ->
              lift .schedule (\m x -> { m | schedule = x }) ScheduleMsg MTUK.Schedule.update a model
              
          VenueMsg a ->
              lift .venue (\m x -> { m | venue = x }) VenueMsg MTUK.Page.update a model
              
          InformationMsg a ->
              lift .information (\m x -> { m | information = x }) InformationMsg MTUK.Page.update a model
              
          Mdl msg ->
              Material.update Mdl msg model
              |> andThen update Init
          Init ->
              update (OverviewMsg MTUK.Overview.Init) model
              |> andThen update (ScheduleMsg MTUK.Schedule.Init)
              |> andThen update (VenueMsg MTUK.Page.Init)
              |> andThen update (InformationMsg MTUK.Page.Init)

-- VIEW


tabs : List ( String, String, Model -> Html Msg )
tabs =
    [ ( "Overview", "overview", .overview >> MTUK.Overview.view >> Html.map OverviewMsg )
    , ( "Schedule", "schedule", .schedule >> MTUK.Schedule.view >> Html.map ScheduleMsg )
    , ( "Venue", "venue", .venue >> MTUK.Page.view >> Html.map VenueMsg )
    , ( "Info", "info", .information >> MTUK.Page.view >> Html.map InformationMsg )
    ]


tabTitles : List (Html a)
tabTitles =
    List.map (\( x, _, _ ) -> text x) tabs


tabViews : Array (Model -> Html Msg)
tabViews =
    List.map (\( _, _, v ) -> v) tabs |> Array.fromList


tabUrls : Array String
tabUrls =
    List.map (\( _, x, _ ) -> x) tabs |> Array.fromList


urlTabs : Dict String Int
urlTabs =
    List.indexedMap (\idx ( _, x, _ ) -> ( x, idx )) tabs |> Dict.fromList


e404 : Model -> Html Msg
e404 _ =
    div
        []
        [ Options.styled Html.h1
            [ Options.cs "mdl-typography--display-4"
            , Typography.center
            ]
            [ text "404" ]
        ]


drawerLink : (String, String, Model -> Html Msg) -> Html Msg
drawerLink (name,link,_) =
    Layout.link
        [ Layout.href <| "#" ++ link
        , Options.onClick (Layout.toggleDrawer Mdl)
        ] [ text name ]

drawer : Model -> List (Html Msg)
drawer model =
    [ Layout.title [] [ img [src "./images/logo-mobile.png"] [] ]
    , Layout.navigation
        []
        (List.map drawerLink tabs)
    ]

registerBtn : Html Msg
registerBtn =
  Button.render Mdl [9, 0, 0, 1] model.mdl
      [ Button.ripple
      , Button.accent
      , Button.raised
      , Button.link "#info"
      ]
      [ text "Register" ]
              

header : Model -> List (Html Msg)
header model =
    [ Layout.row
        [ Options.css "transition" "height 333ms ease-in-out 0s"
        , Options.cs "mdl-layout__header-animated"
        ]
        [ Layout.title [Options.cs "mdl-layout--small-screen-only"]
              [ text "BPGMTC 2019" ]
        , Layout.title [Options.cs "mdl-layout--large-screen-only"]
              [ img [src "./images/title-web-main.png", alt "BPGMTC 2019"] [] ]
        , Layout.spacer
        , Layout.navigation []
              [ registerBtn ]
        ]
    ]


view : Model -> Html Msg
view =
    Html.Lazy.lazy view_


view_ : Model -> Html Msg
view_ model =
    let
        top =
            (Array.get model.selectedTab tabViews |> Maybe.withDefault e404) model
    in
        Layout.render Mdl
            model.mdl
            [ Layout.selectedTab model.selectedTab
            , Layout.onSelectTab SelectTab
            , Layout.fixedHeader
            ]
            { header = header model
            , drawer = drawer model
            , tabs = ( tabTitles, [ Color.background (Color.color Color.DeepPurple Color.S400) ] )
            , main = [ top ]
            }



{- ** End -}
-- ROUTING


urlOf : Model -> String
urlOf model =
    "#" ++ (Array.get model.selectedTab tabUrls |> Maybe.withDefault "")


delta2url : Model -> Model -> Maybe Routing.UrlChange
delta2url model1 model2 =
    if model1.selectedTab /= model2.selectedTab then
        { entry = Routing.NewEntry
        , url = urlOf model2
        }
            |> Just
    else
        Nothing


location2messages : Navigation.Location -> List Msg
location2messages location =
    [ case String.dropLeft 1 location.hash of
        "" ->
            SelectTab 0

        x ->
            Dict.get x urlTabs
                |> Maybe.withDefault -1
                |> SelectTab
    ]



-- APP


main : Routing.RouteUrlProgram Never Model Msg
main =
    Routing.program
        { delta2url = delta2url
        , location2messages = location2messages
        , init =
            ( { model | mdl = Layout.setTabsWidth 2124 model.mdl }
            , Material.init Mdl
            )
        , view = view
        , subscriptions =
            \model ->
                Sub.batch
                    [ Material.subscriptions Mdl model
                    ]
        , update = update
        }

