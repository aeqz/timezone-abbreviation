module Internal exposing (maxOffsetMinutes, toTime)

import Time exposing (Posix, Weekday(..), Zone)


{-| For simplicity, when computing the `Time.Posix` offset, functions in this module only work
properly with `Time.Zone`s that have minute offsets in the range `[-4320, 4320]`, which should be
enough for dealing with usual time zones.
-}
maxOffsetMinutes : Int
maxOffsetMinutes =
    4320


{-| Helper function to expose this package abbreviation functions with a nicer interface based on
`Time` types.

The wrapped function takes an offset in minutes and a POSIX time in seconds, and the returned
version takes a `Time.Zone` and a `Time.Posix` instead.

-}
toTime : ({ offsetMinutes : Int, posixSeconds : Int } -> a) -> Zone -> Posix -> a
toTime f zone posix =
    f { offsetMinutes = toOffset zone posix, posixSeconds = toSeconds posix }


toSeconds : Posix -> Int
toSeconds posix =
    Time.posixToMillis posix // 1000


toOffset : Zone -> Posix -> Int
toOffset zone posix =
    let
        utcWeekday =
            Time.toWeekday Time.utc posix

        localWeekday =
            Time.toWeekday zone posix

        thursdayOffset =
            modBy 7 (weekdayCode Thu - weekdayCode utcWeekday)

        daysOffset =
            modBy 7 (weekdayCode localWeekday + thursdayOffset) - weekdayCode Thu
    in
    (daysOffset * 1440)
        + (Time.toHour zone posix * 60 + Time.toMinute zone posix)
        - (Time.toHour Time.utc posix * 60 + Time.toMinute Time.utc posix)


weekdayCode : Weekday -> Int
weekdayCode weekday =
    case weekday of
        Mon ->
            0

        Tue ->
            1

        Wed ->
            2

        Thu ->
            3

        Fri ->
            4

        Sat ->
            5

        Sun ->
            6
