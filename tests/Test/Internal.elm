module Test.Internal exposing (suite)

import Dict
import Expect
import Fuzz
import Fuzz.Extra as Fuzz
import Internal
import Test exposing (Test)
import Time
import TimeZone


suite : Test
suite =
    Test.concat
        [ Test.describe "toTime" <|
            [ Test.fuzz Fuzz.posix "provides the posix value in seconds" <|
                \posix ->
                    Internal.toTime .posixSeconds Time.utc posix
                        |> Expect.equal (Time.posixToMillis posix // 1000)
            , Test.describe "provides the zone offset"
                [ Test.fuzz2 (Fuzz.fixedOffsetZone Internal.maxOffsetMinutes) Fuzz.posix "for an up to 3 day fixed-offset zone at any time" <|
                    \{ offset, zone } posix ->
                        Internal.toTime .offsetMinutes zone posix
                            |> Expect.equal offset
                , Test.concat <|
                    List.map
                        (\( name, lazyZone ) ->
                            Test.fuzz2 (Fuzz.constant (lazyZone ())) Fuzz.posix ("consistently with " ++ name ++ "'s timezone-data at a given time") <|
                                \zone posix ->
                                    let
                                        same component otherZone =
                                            component zone posix
                                                |> Expect.equal (component otherZone posix)
                                    in
                                    Internal.toTime (\{ offsetMinutes } -> Time.customZone offsetMinutes []) zone posix
                                        |> Expect.all
                                            [ same Time.toYear
                                            , same Time.toMonth
                                            , same Time.toWeekday
                                            , same Time.toDay
                                            , same Time.toHour
                                            , same Time.toMinute
                                            ]
                        )
                        (Dict.toList TimeZone.zones)
                ]
            ]
        ]
