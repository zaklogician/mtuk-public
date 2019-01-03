module MTUK.ScheduleCard exposing (model, update, view, Model, Msg (Init))

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
import Material.Textfield as Textfield
import Material

import Markdown

import MTUK.API as API
import MTUK.Util exposing (white, spinnerGrid, spacer)

-- MODEL

type alias Key = String
type alias Title = String

type alias EditorModel =
    { body : String
    , passwd : String
    }

type alias Model =
    { key : Key
    , title : String
    , body : Maybe String
    , editor : Maybe EditorModel
    , mdl : Material.Model
    }

model : String -> String -> Model
model key_ title_ =
    { key = key_
    , title = title_
    , body = Nothing
    , editor = Nothing
    , mdl = Material.model
    }

-- ACTION/UPDATE

type EditorMsg
    = ChangeBody String
    | ChangePasswd String
    | ClickSave
    | ClickCancel

type Msg
    = Mdl (Material.Msg Msg)
    | ClickEdit
    | EditorMsg EditorMsg
    | ApiRequestLoad
    | ApiResponse API.Msg
    | Init

update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        Mdl action_ ->
            Material.update Mdl action_ model
        ClickEdit ->
            case model.body of
                Nothing -> (model, Cmd.none)
                Just body_ ->
                    let
                        editor_ = { body = body_, passwd = "" }
                    in
                        ( { model | editor = Just editor_ }, Cmd.none )
        EditorMsg a ->
            case model.editor of
                Nothing -> (model, Cmd.none)
                Just editor_ -> case a of
                    ChangeBody str ->
                        ( { model | editor = Just { editor_ | body = str } }
                        , Cmd.none )
                    ChangePasswd str ->
                        ( { model | editor = Just { editor_ | passwd = str } }
                        , Cmd.none )
                    ClickCancel ->
                        ( { model | editor = Nothing }
                        , Cmd.none )
                    ClickSave ->
                        let
                            content_ = { key = model.key, value = editor_.body }
                        in
                            ( model
                            , Cmd.map ApiResponse
                                  (API.update editor_.passwd content_)
                            )
        ApiRequestLoad ->
          (model, Cmd.map ApiResponse (API.read model.key) )
        ApiResponse (API.ReadSuccess s) ->
            ({ model | body = Just s.value }, Cmd.none)
        ApiResponse (API.ReadFailure x) ->
            (model, Cmd.none)
        ApiResponse (API.UpdateSuccess s) ->
            ({ model | body = Just s.value, editor = Nothing }, Cmd.none)
        ApiResponse (API.UpdateFailure x) ->
            (model, Cmd.none) -- TODO: report errors, low priority
        Init ->
            update ApiRequestLoad model

-- VIEW

options =
    [ Options.cs "mld-cell mdl-cell--4col schedulecard"
    , Color.background Color.white
    , Color.text Color.black
    , Elevation.e4
    ]

editBtn : Model -> Html Msg
editBtn model =
    Button.render Mdl [0,0] model.mdl
        [ Button.icon, Button.ripple, white, Options.onClick ClickEdit]
        [ Icon.i "edit" ]

saveBtn : Model -> Html Msg
saveBtn model =
    Button.render Mdl [1] model.mdl
        [ Options.onClick (EditorMsg ClickSave) ]
        [ text "Save" ]

cancelBtn : Model -> Html Msg
cancelBtn model =
    Button.render Mdl [2] model.mdl
        [ Options.onClick (EditorMsg ClickCancel) ]
        [ text "Cancel" ]

titlebar : Model -> Card.Block Msg
titlebar model =
    Card.title
        [ Color.background Color.primary
        , Color.text Color.white
        ]
        [ Card.head [] [ text model.title ] ]

view : Model -> Html Msg
view model = case model.body of
    Nothing ->
        Card.view options
            [ titlebar model
            , Card.text [] [ spinnerGrid ]
            , Card.menu [] [ editBtn model ]
            ]
      
    Just body ->
        case model.editor of
            Nothing ->
                Card.view options
                    [ titlebar model
                    , Card.text [] [ Markdown.toHtml [class "markdown-content"] body ]
                    , Card.menu [] [ editBtn model ]
                    ]
            Just editor_ ->
                Card.view options
                    [ titlebar model
                    , Card.text []
                          [ Textfield.render Mdl [4] model.mdl
                                [ Textfield.textarea
                                , Textfield.rows 6
                                , Textfield.value editor_.body
                                , Options.cs "markdown-content"
                                , Options.onInput (\x -> EditorMsg <| ChangeBody x)
                                ] []
                          ]
                    , Card.actions [Card.border, white]
                          [ Textfield.render Mdl [5] model.mdl
                                [ Textfield.password
                                , Textfield.label "Password"
                                , Textfield.value editor_.passwd
                                , Options.cs "small-textfield"
                                , Options.onInput (\x -> EditorMsg <| ChangePasswd x)
                                ] []
                          , spacer
                          , saveBtn model
                          , cancelBtn model
                          ]
                    ]
