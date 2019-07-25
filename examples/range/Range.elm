module Range exposing (main)

import Compat.Date as Date exposing (Date, day, month, weekday, year)
import Compat.Time as Time exposing (Weekday(..))
import DatePicker exposing (DateEvent(..), defaultSettings)
import Html exposing (Html, div, h1, text)


type Msg
    = ToStartDatePicker DatePicker.Msg
    | ToEndDatePicker DatePicker.Msg


type alias Model =
    { startDate : Maybe Date
    , endDate : Maybe Date
    , startDatePicker : DatePicker.DatePicker
    , endDatePicker : DatePicker.DatePicker
    }



-- Could be used to customize common settings for both date pickers. Like for
-- example disabling weekends from them.


commonSettings : DatePicker.Settings
commonSettings =
    defaultSettings



-- Extend commonSettings with isDisabled function which would disable dates
-- after already selected end date because range start should come before end.


startSettings : Maybe Date -> DatePicker.Settings
startSettings endDate =
    let
        isDisabled =
            case endDate of
                Nothing ->
                    commonSettings.isDisabled

                Just date ->
                    \d ->
                        Date.toRataDie d
                            > Date.toRataDie date
                            || commonSettings.isDisabled d
    in
    { commonSettings
        | placeholder = "Pick a start date"
        , isDisabled = isDisabled
    }



-- Extend commonSettings with isDisabled function which would disable dates
-- before already selected start date because range end should come after start.


endSettings : Maybe Date -> DatePicker.Settings
endSettings startDate =
    let
        isDisabled =
            case startDate of
                Nothing ->
                    commonSettings.isDisabled

                Just date ->
                    \d ->
                        Date.toRataDie d
                            < Date.toRataDie date
                            || commonSettings.isDisabled d
    in
    { commonSettings
        | placeholder = "Pick an end date"
        , isDisabled = isDisabled
    }


init : ( Model, Cmd Msg )
init =
    let
        ( startDatePicker, startDatePickerFx ) =
            DatePicker.init

        ( endDatePicker, endDatePickerFx ) =
            DatePicker.init
    in
    ( { startDate = Nothing
      , startDatePicker = startDatePicker
      , endDate = Nothing
      , endDatePicker = endDatePicker
      }
    , Cmd.batch
        [ Cmd.map ToStartDatePicker startDatePickerFx
        , Cmd.map ToEndDatePicker endDatePickerFx
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToStartDatePicker subMsg ->
            let
                ( newDatePicker, dateEvent ) =
                    DatePicker.update (startSettings model.endDate) subMsg model.startDatePicker

                newDate =
                    case dateEvent of
                        Picked changedDate ->
                            Just changedDate

                        _ ->
                            model.startDate
            in
            ( { model
                | startDate = newDate
                , startDatePicker = newDatePicker
              }
            , Cmd.none
            )

        ToEndDatePicker subMsg ->
            let
                ( newDatePicker, dateEvent ) =
                    DatePicker.update (endSettings model.startDate) subMsg model.endDatePicker

                newDate =
                    case dateEvent of
                        Picked changedDate ->
                            Just changedDate

                        _ ->
                            model.endDate
            in
            ( { model
                | endDate = newDate
                , endDatePicker = newDatePicker
              }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    div []
        [ viewRange model.startDate model.endDate
        , DatePicker.view model.startDate (startSettings model.endDate) model.startDatePicker
            |> Html.map ToStartDatePicker
        , DatePicker.view model.endDate (endSettings model.startDate) model.endDatePicker
            |> Html.map ToEndDatePicker
        ]


viewRange : Maybe Date -> Maybe Date -> Html Msg
viewRange start end =
    case ( start, end ) of
        ( Nothing, Nothing ) ->
            h1 [] [ text "Pick dates" ]

        ( Just s, Nothing ) ->
            h1 [] [ text <| formatDate s ++ " – Pick end date" ]

        ( Nothing, Just e ) ->
            h1 [] [ text <| "Pick start date – " ++ formatDate e ]

        ( Just s, Just e ) ->
            h1 [] [ text <| formatDate s ++ " – " ++ formatDate e ]


formatDate : Date -> String
formatDate d =
    Date.format "MMM dd, yyyy" d


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
