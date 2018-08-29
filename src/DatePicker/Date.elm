module DatePicker.Date
    exposing
        ( YearRange(..)
        , initDate
        , formatDay
        , formatMonth
        , fromPosix
        , weekdayToInterval
        , changeYear
        , yearRange
        )

import Date exposing (Date, Unit(..), Interval(..), Weekday(..), Month(..), year, month, day)
import Time


fromPosix : Time.Zone -> Time.Posix -> Date
fromPosix zone posix =
    Date.fromCalendarDate
        (Time.toYear zone posix)
        (Time.toMonth zone posix |> timeMonthToDateMonth)
        (Time.toDay zone posix)

timeMonthToDateMonth : Time.Month -> Month
timeMonthToDateMonth month =
    case month of
        Time.Jan -> Jan
        Time.Feb -> Feb
        Time.Mar -> Mar
        Time.Apr -> Apr
        Time.May -> May
        Time.Jun -> Jun
        Time.Jul -> Jul
        Time.Aug -> Aug
        Time.Sep -> Sep
        Time.Oct -> Oct
        Time.Nov -> Nov
        Time.Dec -> Dec


type alias Year =
    Int


type alias Day =
    Int


type YearRange
    = Off
    | MoreOrLess Int
    | Between Year Year
    | From Year
    | To Year


initDate : Date
initDate =
    Date.fromCalendarDate 1992 May 31


formatDay : Date.Weekday -> String
formatDay day =
    case day of
        Mon ->
            "Mo"

        Tue ->
            "Tu"

        Wed ->
            "We"

        Thu ->
            "Th"

        Fri ->
            "Fr"

        Sat ->
            "Sa"

        Sun ->
            "Su"


formatMonth : Month -> String
formatMonth month =
    case month of
        Jan ->
            "January"

        Feb ->
            "February"

        Mar ->
            "March"

        Apr ->
            "April"

        May ->
            "May"

        Jun ->
            "June"

        Jul ->
            "July"

        Aug ->
            "August"

        Sep ->
            "September"

        Oct ->
            "October"

        Nov ->
            "November"

        Dec ->
            "December"


weekdayToInterval : Date.Weekday -> Date.Interval
weekdayToInterval weekday =
    case weekday of
        Mon ->
            Monday

        Tue ->
            Tuesday

        Wed ->
            Wednesday

        Thu ->
            Thursday

        Fri ->
            Friday

        Sat ->
            Saturday

        Sun ->
            Sunday

changeYear : Date -> String -> Date
changeYear current newYear =
    case String.toInt newYear of
        Just year ->
            Date.fromCalendarDate year (month current) (day current)

        Nothing ->
            Debug.todo ("Unknown year " ++ newYear)


yearRange : { currentMonth : Date, today : Date } -> YearRange -> List Int
yearRange { currentMonth, today } range =
    case range of
        MoreOrLess num ->
            List.range ((year currentMonth) - num) ((year currentMonth) + num)

        Between start end ->
            List.range start end

        From year_ ->
            List.range year_ (year today)

        To year_ ->
            List.range (year today) year_

        Off ->
            []
