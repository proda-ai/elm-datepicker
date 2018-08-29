module SimpleNightwatch exposing (main)

{-| This is a simple test suitable for automated browser testing (such as with nightwatch.js)
-}

import Date exposing (Date, Weekday(..), day, weekday, month, year)
import DatePicker exposing (defaultSettings, DateEvent(..))
import Html exposing (Html, div, h1, text, button)
import Process
import Task
import Time
import Browser


type Msg
    = ToDatePicker DatePicker.Msg
    | NoOp


type alias Model =
    { date : Maybe Date
    , datePicker : DatePicker.DatePicker
    }


settings : DatePicker.Settings
settings =
    defaultSettings


init : ( Model, Cmd Msg )
init =
    let
        moonLandingDate =
            Date.fromCalendarDate 1969 Date.Jul 20
    in
        ( { date = Nothing
          , datePicker = DatePicker.initFromDate moonLandingDate
          }
          -- trigger a NoOp command after two seconds. This is used to test
          -- that re-renders of the app do not cause things to dissapear.
        , delayedNoOpCmd { seconds = 2 }
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ date, datePicker } as model) =
    case msg of
        ToDatePicker subMsg ->
            let
                ( newDatePicker, datePickerFx, dateEvent ) =
                    DatePicker.update settings subMsg datePicker

                newDate =
                    case dateEvent of
                        Changed changedDate ->
                            changedDate

                        _ ->
                            date
            in
                ( { model
                    | date = newDate
                    , datePicker = newDatePicker
                  }
                , Cmd.map ToDatePicker datePickerFx
                )

        NoOp ->
            ( model, Cmd.none )


view : Model -> Html Msg
view ({ date, datePicker } as model) =
    div []
        [ case date of
            Nothing ->
                h1 [] [ text "Pick a date" ]

            Just d ->
                h1 [] [ text <| Date.toFormattedString "MMM dd, yyyy" d ]
        , DatePicker.view date settings datePicker
            |> Html.map ToDatePicker
        ]

delayedNoOpCmd : { seconds : Float } -> Cmd Msg
delayedNoOpCmd { seconds } =
    Process.sleep (seconds * 1000)
        |> Task.perform (\_ -> NoOp)


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
