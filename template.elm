module TimeZone.Abbreviation exposing
    ( version
    , Abbreviation(..), toString
    , forZone, Error(..), forZoneUnsafe
    , ZONE_FUNCTIONS
    )

{-| This module provides time zone name abbreviations from the IANA Time Zone Database.

@docs version


## Abbreviation

@docs Abbreviation, toString


## Obtaining abbreviations

@docs forZone, Error, forZoneUnsafe

---

Using `forZone` or `forZoneUnsafe` will include all the zones abbreviation data in your optimized
build. If you only need to support a few time zones, then you can use the following individual
functions directly instead, which may return an `Error` with the offset in minutes if the
`Time.Zone` data doesn't contain valid offsets for the function zone name.

Each of these functions is named after its zone name (e.g. `America/New_York`), where a `for__`
prefix is added, slashes are replaced by `__`, dashes are replaced by `_`, and the name is
lowercased (e.g. `for__america__new_york`). Functions for alias zone names (e.g. `Europe/Jersey`)
are not provided.

@docs ZONE_FUNCTIONS

-}

import Internal
import Time exposing (Posix, Zone, ZoneName)


{-| What release of the IANA Time Zone Database is this data from?

    version
    --> "DB_VERSION"

-}
version : String
version =
    "DB_VERSION"


{-| A time zone abbreviation, which can be a `ShortName`, an `Offset` in minutes, an `Uninhabited`
indicator or a `Lmt` (i.e. local mean time).
-}
type Abbreviation
    = ShortName String
    | Offset Int
    | Uninhabited
    | Lmt


{-| Convert an `Abbreviation` into a `String` by formatting `Offset` and `Uninhabited` values
using the IANA Time Zone Database style:

    Offset 120
        |> TimeZone.Abbreviation.toString
    --> "+02"

    Offset -615
        |> TimeZone.Abbreviation.toString
    --> "-1015"

    Offset 0
        |> TimeZone.Abbreviation.toString
    --> "+00"

    Uninhabited
        |> TimeZone.Abbreviation.toString
    --> "-00"

    Lmt
        |> TimeZone.Abbreviation.toString
    --> "LMT"

    ShortName "BST"
        |> TimeZone.Abbreviation.toString
    --> "BST"

-}
toString : Abbreviation -> String
toString abbreviation =
    case abbreviation of
        ShortName name ->
            name

        Offset offset ->
            let
                sign =
                    if offset < 0 then
                        "-"

                    else
                        "+"

                hours =
                    String.padLeft 2 '0' (String.fromInt (abs offset // 60))

                minutes =
                    case modBy 60 (abs offset) of
                        0 ->
                            ""

                        nonZero ->
                            String.padLeft 2 '0' (String.fromInt nonZero)
            in
            sign ++ hours ++ minutes

        Uninhabited ->
            "-00"

        Lmt ->
            "LMT"


{-| Obtain the `Abbreviation` for a `Time.ZoneName` at a given `Time.Posix` time:

    import Time

    Time.millisToPosix 1743465600000
        |> TimeZone.Abbreviation.forZone
            ( Time.Name "Europe/London"
            , Time.customZone 60 []
            )
    --> Ok (ShortName "BST")

Alias zone names are also accepted (e.g. `"Europe/Jersey"` is treated as `"Europe/London"`), and
abbreviations are returned as present in the IANA Time Zone Database even for negative timestamps
(i.e. earlier than 1970):

    Time.millisToPosix -902102400000
        |> TimeZone.Abbreviation.forZone
            ( Time.Name "Europe/Jersey"
            , Time.customZone 120 []
            )
    --> Ok (ShortName "BDST")

The time `Offset` in minutes is returned if there's no abbreviation for that zone offset:

    Time.millisToPosix 1743465600000
        |> TimeZone.Abbreviation.forZone
            ( Time.Name "Asia/Dubai"
            , Time.customZone 240 []
            )
    --> Ok (Offset 240)

**Note:** You may have expected `"GST"` being returned in the previous example, but the IANA Time
Zone Database is
[removing invented abbreviations](https://mm.icann.org/pipermail/tz-announce/2017-February/000045.html).

Some example usages for this function could be:

  - Using `Time.getZoneName`, `Time.here` and `Time.now` for obtaining the current local time zone
    abbreviation.
  - Using [`timezone-data`'s `getZone`](https://package.elm-lang.org/packages/justinmimbs/timezone-data/latest/TimeZone#getZone)
    for obtaining the local time abbreviation at any time between 1970 and 2037.
  - Using [`timezone-data`'s zones](https://package.elm-lang.org/packages/justinmimbs/timezone-data/latest/TimeZone#zones)
    for obtaining the time abbreviation for any of the provided time zones at any time
    between 1970 and 2037.

-}
forZone : ( ZoneName, Zone ) -> Posix -> Result Error Abbreviation
forZone ( zoneName, zone ) =
    case zoneName of
        Time.Name name ->
            forZoneName name zone

        Time.Offset offset ->
            if offset > Internal.maxOffsetMinutes then
                \_ -> Err (UnsupportedOffsetZone offset)

            else
                Internal.toTime
                    (\offsetMinutes _ ->
                        if offsetMinutes /= offset then
                            Err (UnknownZoneOffset zoneName offsetMinutes)

                        else
                            Ok (Offset offset)
                    )
                    zone


{-| The `forZone` abbreviation function relies on receiving a valid zone name according to the IANA
Time Zone Database, a `Time.Zone` containing meaningful offsets for that zone name and, in the case
of `Time.Offset` zone names, doesn't accept values outside of the `[-4320, 4320]` range. These are
the errors that will be returned in each corresponding case.
-}
type Error
    = UnknownZoneName String
    | UnknownZoneOffset ZoneName Int
    | UnsupportedOffsetZone Int


{-| Sometimes, if you trust your input data, you may want to ignore `Error`s and treat them as
`Offset`s:

    import Time

    Time.millisToPosix 1743465600000
        |> TimeZone.Abbreviation.forZoneUnsafe
            ( Time.Name "Europe/Nowere"
            , Time.customZone 60 []
            )
    --> Offset 60

Which would have returned an `UnknownZoneName "Europe/Nowere"` error if using
`forZone`.

-}
forZoneUnsafe : ( ZoneName, Zone ) -> Posix -> Abbreviation
forZoneUnsafe zone posix =
    case forZone zone posix of
        Ok abbreviation ->
            abbreviation

        Err (UnknownZoneOffset _ offset) ->
            Offset offset

        Err _ ->
            Internal.toTime
                (\offsetMinutes _ -> Offset offsetMinutes)
                (Tuple.second zone)
                posix



-- GENERATED
