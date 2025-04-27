module Test.TimeZone.Abbreviation exposing (suite)

import Dict
import Expect
import Expect.Extra as Expect
import Fuzz
import Fuzz.Extra as Fuzz
import Internal
import Test exposing (Test)
import Time
import TimeZone
import TimeZone.Abbreviation exposing (Abbreviation(..), Error(..))


suite : Test
suite =
    Test.concat
        [ Test.describe "forName"
            [ Test.describe "provides the zone offset"
                [ Test.fuzz2 (Fuzz.fixedOffsetZone Internal.maxOffsetMinutes) Fuzz.posix "for an offset zone name" <|
                    \{ offset, zone } ->
                        TimeZone.Abbreviation.forZone
                            ( Time.Offset offset
                            , zone
                            )
                            >> Expect.okWith (Expect.offsetAbbreviation (Expect.equal offset))
                , Test.fuzz2 (Fuzz.constant (TimeZone.pacific__kanton ())) Fuzz.posix "if there's no abbreviated name for that zone offset" <|
                    \zone ->
                        TimeZone.Abbreviation.forZone
                            ( Time.Name "Pacific/Kanton"
                            , zone
                            )
                            >> Expect.okWith (Expect.offsetAbbreviation (always Expect.pass))
                ]
            , Test.test "can indicate uninhabited times" <|
                \_ ->
                    Time.millisToPosix 0
                        |> TimeZone.Abbreviation.forZone
                            ( Time.Name "Antarctica/Rothera"
                            , TimeZone.antarctica__rothera ()
                            )
                        |> Expect.okWith (Expect.equal Uninhabited)
            , Test.describe "can indicate Local Mean Times"
                [ Test.test "if the offset gets truncated when converting to minutes" <|
                    \_ ->
                        Time.millisToPosix -631152000000
                            |> TimeZone.Abbreviation.forZone
                                ( Time.Name "Pacific/Niue"
                                , Time.customZone (-40780 // 60) []
                                )
                            |> Expect.okWith (Expect.equal Lmt)
                , Test.test "if the offset gets rounded when converting to minutes" <|
                    \_ ->
                        Time.millisToPosix -631152000000
                            |> TimeZone.Abbreviation.forZone
                                ( Time.Name "Pacific/Niue"
                                , Time.customZone (round (toFloat -40780 / toFloat 60)) []
                                )
                            |> Expect.okWith (Expect.equal Lmt)
                ]
            , Test.describe "provides abbreviated names"
                [ Test.fuzz2 (Fuzz.constant (TimeZone.europe__madrid ())) Fuzz.posix "for a zone with positive offsets" <|
                    \zone ->
                        TimeZone.Abbreviation.forZone
                            ( Time.Name "Europe/Madrid"
                            , zone
                            )
                            >> Expect.okWith (Expect.shortNameAbbreviation (Expect.member [ "CET", "CEST" ]))
                , Test.fuzz2 (Fuzz.constant (TimeZone.america__chicago ())) Fuzz.posix "for a zone with negative offsets" <|
                    \zone ->
                        TimeZone.Abbreviation.forZone
                            ( Time.Name "America/Chicago"
                            , zone
                            )
                            >> Expect.okWith (Expect.shortNameAbbreviation (Expect.member [ "CST", "CDT" ]))
                , Test.fuzz2 (Fuzz.constant (TimeZone.europe__london ())) Fuzz.posix "for an alias zone name" <|
                    \zone ->
                        TimeZone.Abbreviation.forZone
                            ( Time.Name "Europe/Jersey"
                            , zone
                            )
                            >> Expect.okWith (Expect.shortNameAbbreviation (Expect.member [ "GMT", "BST" ]))
                , Test.test "before 1970" <|
                    \_ ->
                        Expect.all
                            [ \_ ->
                                Time.millisToPosix -769395660000
                                    |> TimeZone.Abbreviation.forZone
                                        ( Time.Name "Pacific/Honolulu"
                                        , Time.customZone -570 []
                                        )
                                    |> Expect.okWith (Expect.shortNameAbbreviation (Expect.equal "HWT"))
                            , \_ ->
                                Time.millisToPosix -769395600000
                                    |> TimeZone.Abbreviation.forZone
                                        ( Time.Name "Pacific/Honolulu"
                                        , Time.customZone -570 []
                                        )
                                    |> Expect.okWith (Expect.shortNameAbbreviation (Expect.equal "HPT"))
                            , \_ ->
                                Time.millisToPosix -902102400000
                                    |> TimeZone.Abbreviation.forZone
                                        ( Time.Name "Europe/London"
                                        , Time.customZone 120 []
                                        )
                                    |> Expect.okWith (Expect.shortNameAbbreviation (Expect.equal "BDST"))
                            ]
                            ()
                ]
            , Test.describe "fails"
                [ Test.fuzz2 (Fuzz.fixedOffsetZone Internal.maxOffsetMinutes) Fuzz.posix "for an unknown zone name" <|
                    \{ zone } ->
                        TimeZone.Abbreviation.forZone
                            ( Time.Name "Nowere/Nowere"
                            , zone
                            )
                            >> Expect.errWith (Expect.equal (UnknownZoneName "Nowere/Nowere"))
                , Test.fuzz2 (Fuzz.constant (Time.customZone -120 [])) Fuzz.posix "for an unknown zone offset" <|
                    \zone ->
                        TimeZone.Abbreviation.forZone
                            ( Time.Name "Europe/Madrid"
                            , zone
                            )
                            >> Expect.errWith (Expect.equal (UnknownZoneOffset (Time.Name "Europe/Madrid") -120))
                , Test.fuzz2 (Fuzz.constant (Time.customZone 60 [])) Fuzz.posix "if an offset zone name doesn't match the offset" <|
                    \zone ->
                        TimeZone.Abbreviation.forZone
                            ( Time.Offset -180
                            , zone
                            )
                            >> Expect.errWith (Expect.equal (UnknownZoneOffset (Time.Offset -180) 60))
                , Test.fuzz2 (Fuzz.constant (Internal.maxOffsetMinutes + 1)) Fuzz.posix "if the offset is unsupported" <|
                    \offset ->
                        TimeZone.Abbreviation.forZone
                            ( Time.Offset offset
                            , Time.customZone offset []
                            )
                            >> Expect.errWith (Expect.equal (UnsupportedOffsetZone offset))
                ]
            ]
        , Test.describe "forNameUnsafe"
            [ Test.fuzz2 (Fuzz.fixedOffsetZone Internal.maxOffsetMinutes) Fuzz.posix "suppresses unknown zone name error" <|
                \{ offset, zone } ->
                    TimeZone.Abbreviation.forZoneUnsafe
                        ( Time.Name "Nowere/Nowere"
                        , zone
                        )
                        >> Expect.offsetAbbreviation (Expect.equal offset)
            , Test.fuzz2 (Fuzz.constant (Time.customZone -120 [])) Fuzz.posix "suppresses unknown zone offset error" <|
                \zone ->
                    TimeZone.Abbreviation.forZoneUnsafe
                        ( Time.Name "Europe/Madrid"
                        , zone
                        )
                        >> Expect.offsetAbbreviation (Expect.equal -120)
            , Test.fuzz2 (Fuzz.constant (Internal.maxOffsetMinutes + 1)) Fuzz.posix "suppresses unsupported offset error" <|
                \offset ->
                    TimeZone.Abbreviation.forZoneUnsafe
                        ( Time.Offset offset
                        , Time.customZone offset []
                        )
                        >> Expect.offsetAbbreviation (always Expect.pass)
            , Test.concat <|
                List.map
                    (\( name, lazyZone ) ->
                        Test.fuzz2 (Fuzz.constant (lazyZone ())) Fuzz.posix ("is safe to use with " ++ name ++ " in timezone-data") <|
                            \zone posix ->
                                TimeZone.Abbreviation.forZoneUnsafe
                                    ( Time.Name name
                                    , zone
                                    )
                                    posix
                                    |> Ok
                                    |> Expect.equal
                                        (TimeZone.Abbreviation.forZone
                                            ( Time.Name name
                                            , zone
                                            )
                                            posix
                                        )
                    )
                    (Dict.toList TimeZone.zones)
            ]
        ]
