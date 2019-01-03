module MTUK.Util exposing (..)

import Material.Options as Options
import Material.Color as Color
import Material.Spinner as Spinner
import Material

import Html exposing (..)
import Html.Attributes exposing (class)

nth : Int -> List a -> Maybe a
nth k xs =
    List.drop k xs |> List.head

andThen : (msg -> model -> (model, Cmd a)) -> msg -> (model, Cmd a) -> (model, Cmd a)
andThen update msg (model1, cmd1) =
    let (model2, cmd2) = update msg model1 in
        (model2, Cmd.batch [cmd1,cmd2])

batched : (msg -> model -> (model, Cmd a)) -> List msg -> model -> (model, Cmd a)
batched update xs model =
    case xs of
        [] ->
            (model, Cmd.none)
        (x :: xs) ->
            batched update xs model  |> andThen update x


white : Options.Property c m
white =
    Color.text Color.white

spacer : Html msg
spacer = div [class "mdl-layout-spacer"] []

spinner : Html msg
spinner =
    Spinner.spinner
        [ Spinner.active True
        , Spinner.singleColor True
        ]

spinnerGrid : Html msg
spinnerGrid =
    div [class "mdl-grid loadlate"] [spacer, spinner, spacer]
