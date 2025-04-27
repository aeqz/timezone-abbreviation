module TimeZone.Abbreviation exposing
    ( version
    , Abbreviation(..), toString
    , forZone, Error(..), forZoneUnsafe
    , for__africa__abidjan, for__africa__algiers, for__africa__bissau, for__africa__cairo, for__africa__casablanca, for__africa__ceuta, for__africa__el_aaiun, for__africa__johannesburg, for__africa__juba, for__africa__khartoum, for__africa__lagos, for__africa__maputo, for__africa__monrovia, for__africa__nairobi, for__africa__ndjamena, for__africa__sao_tome, for__africa__tripoli, for__africa__tunis, for__africa__windhoek, for__america__adak, for__america__anchorage, for__america__araguaina, for__america__argentina__buenos_aires, for__america__argentina__catamarca, for__america__argentina__cordoba, for__america__argentina__jujuy, for__america__argentina__la_rioja, for__america__argentina__mendoza, for__america__argentina__rio_gallegos, for__america__argentina__salta, for__america__argentina__san_juan, for__america__argentina__san_luis, for__america__argentina__tucuman, for__america__argentina__ushuaia, for__america__asuncion, for__america__bahia, for__america__bahia_banderas, for__america__barbados, for__america__belem, for__america__belize, for__america__boa_vista, for__america__bogota, for__america__boise, for__america__cambridge_bay, for__america__campo_grande, for__america__cancun, for__america__caracas, for__america__cayenne, for__america__chicago, for__america__chihuahua, for__america__ciudad_juarez, for__america__costa_rica, for__america__cuiaba, for__america__danmarkshavn, for__america__dawson, for__america__dawson_creek, for__america__denver, for__america__detroit, for__america__edmonton, for__america__eirunepe, for__america__el_salvador, for__america__fort_nelson, for__america__fortaleza, for__america__glace_bay, for__america__goose_bay, for__america__grand_turk, for__america__guatemala, for__america__guayaquil, for__america__guyana, for__america__halifax, for__america__havana, for__america__hermosillo, for__america__indiana__indianapolis, for__america__indiana__knox, for__america__indiana__marengo, for__america__indiana__petersburg, for__america__indiana__tell_city, for__america__indiana__vevay, for__america__indiana__vincennes, for__america__indiana__winamac, for__america__inuvik, for__america__iqaluit, for__america__jamaica, for__america__juneau, for__america__kentucky__louisville, for__america__kentucky__monticello, for__america__la_paz, for__america__lima, for__america__los_angeles, for__america__maceio, for__america__managua, for__america__manaus, for__america__martinique, for__america__matamoros, for__america__mazatlan, for__america__menominee, for__america__merida, for__america__metlakatla, for__america__mexico_city, for__america__miquelon, for__america__moncton, for__america__monterrey, for__america__montevideo, for__america__new_york, for__america__nome, for__america__noronha, for__america__north_dakota__beulah, for__america__north_dakota__center, for__america__north_dakota__new_salem, for__america__nuuk, for__america__ojinaga, for__america__panama, for__america__paramaribo, for__america__phoenix, for__america__port_au_prince, for__america__porto_velho, for__america__puerto_rico, for__america__punta_arenas, for__america__rankin_inlet, for__america__recife, for__america__regina, for__america__resolute, for__america__rio_branco, for__america__santarem, for__america__santiago, for__america__santo_domingo, for__america__sao_paulo, for__america__scoresbysund, for__america__sitka, for__america__st_johns, for__america__swift_current, for__america__tegucigalpa, for__america__thule, for__america__tijuana, for__america__toronto, for__america__vancouver, for__america__whitehorse, for__america__winnipeg, for__america__yakutat, for__antarctica__casey, for__antarctica__davis, for__antarctica__macquarie, for__antarctica__mawson, for__antarctica__palmer, for__antarctica__rothera, for__antarctica__troll, for__antarctica__vostok, for__asia__almaty, for__asia__amman, for__asia__anadyr, for__asia__aqtau, for__asia__aqtobe, for__asia__ashgabat, for__asia__atyrau, for__asia__baghdad, for__asia__baku, for__asia__bangkok, for__asia__barnaul, for__asia__beirut, for__asia__bishkek, for__asia__chita, for__asia__colombo, for__asia__damascus, for__asia__dhaka, for__asia__dili, for__asia__dubai, for__asia__dushanbe, for__asia__famagusta, for__asia__gaza, for__asia__hebron, for__asia__ho_chi_minh, for__asia__hong_kong, for__asia__hovd, for__asia__irkutsk, for__asia__jakarta, for__asia__jayapura, for__asia__jerusalem, for__asia__kabul, for__asia__kamchatka, for__asia__karachi, for__asia__kathmandu, for__asia__khandyga, for__asia__kolkata, for__asia__krasnoyarsk, for__asia__kuching, for__asia__macau, for__asia__magadan, for__asia__makassar, for__asia__manila, for__asia__nicosia, for__asia__novokuznetsk, for__asia__novosibirsk, for__asia__omsk, for__asia__oral, for__asia__pontianak, for__asia__pyongyang, for__asia__qatar, for__asia__qostanay, for__asia__qyzylorda, for__asia__riyadh, for__asia__sakhalin, for__asia__samarkand, for__asia__seoul, for__asia__shanghai, for__asia__singapore, for__asia__srednekolymsk, for__asia__taipei, for__asia__tashkent, for__asia__tbilisi, for__asia__tehran, for__asia__thimphu, for__asia__tokyo, for__asia__tomsk, for__asia__ulaanbaatar, for__asia__urumqi, for__asia__ust_nera, for__asia__vladivostok, for__asia__yakutsk, for__asia__yangon, for__asia__yekaterinburg, for__asia__yerevan, for__atlantic__azores, for__atlantic__bermuda, for__atlantic__canary, for__atlantic__cape_verde, for__atlantic__faroe, for__atlantic__madeira, for__atlantic__south_georgia, for__atlantic__stanley, for__australia__adelaide, for__australia__brisbane, for__australia__broken_hill, for__australia__darwin, for__australia__eucla, for__australia__hobart, for__australia__lindeman, for__australia__lord_howe, for__australia__melbourne, for__australia__perth, for__australia__sydney, for__europe__andorra, for__europe__astrakhan, for__europe__athens, for__europe__belgrade, for__europe__berlin, for__europe__brussels, for__europe__bucharest, for__europe__budapest, for__europe__chisinau, for__europe__dublin, for__europe__gibraltar, for__europe__helsinki, for__europe__istanbul, for__europe__kaliningrad, for__europe__kirov, for__europe__kyiv, for__europe__lisbon, for__europe__london, for__europe__madrid, for__europe__malta, for__europe__minsk, for__europe__moscow, for__europe__paris, for__europe__prague, for__europe__riga, for__europe__rome, for__europe__samara, for__europe__saratov, for__europe__simferopol, for__europe__sofia, for__europe__tallinn, for__europe__tirane, for__europe__ulyanovsk, for__europe__vienna, for__europe__vilnius, for__europe__volgograd, for__europe__warsaw, for__europe__zurich, for__indian__chagos, for__indian__maldives, for__indian__mauritius, for__pacific__apia, for__pacific__auckland, for__pacific__bougainville, for__pacific__chatham, for__pacific__easter, for__pacific__efate, for__pacific__fakaofo, for__pacific__fiji, for__pacific__galapagos, for__pacific__gambier, for__pacific__guadalcanal, for__pacific__guam, for__pacific__honolulu, for__pacific__kanton, for__pacific__kiritimati, for__pacific__kosrae, for__pacific__kwajalein, for__pacific__marquesas, for__pacific__nauru, for__pacific__niue, for__pacific__norfolk, for__pacific__noumea, for__pacific__pago_pago, for__pacific__palau, for__pacific__pitcairn, for__pacific__port_moresby, for__pacific__rarotonga, for__pacific__tahiti, for__pacific__tarawa, for__pacific__tongatapu
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

@docs for__africa__abidjan, for__africa__algiers, for__africa__bissau, for__africa__cairo, for__africa__casablanca, for__africa__ceuta, for__africa__el_aaiun, for__africa__johannesburg, for__africa__juba, for__africa__khartoum, for__africa__lagos, for__africa__maputo, for__africa__monrovia, for__africa__nairobi, for__africa__ndjamena, for__africa__sao_tome, for__africa__tripoli, for__africa__tunis, for__africa__windhoek, for__america__adak, for__america__anchorage, for__america__araguaina, for__america__argentina__buenos_aires, for__america__argentina__catamarca, for__america__argentina__cordoba, for__america__argentina__jujuy, for__america__argentina__la_rioja, for__america__argentina__mendoza, for__america__argentina__rio_gallegos, for__america__argentina__salta, for__america__argentina__san_juan, for__america__argentina__san_luis, for__america__argentina__tucuman, for__america__argentina__ushuaia, for__america__asuncion, for__america__bahia, for__america__bahia_banderas, for__america__barbados, for__america__belem, for__america__belize, for__america__boa_vista, for__america__bogota, for__america__boise, for__america__cambridge_bay, for__america__campo_grande, for__america__cancun, for__america__caracas, for__america__cayenne, for__america__chicago, for__america__chihuahua, for__america__ciudad_juarez, for__america__costa_rica, for__america__cuiaba, for__america__danmarkshavn, for__america__dawson, for__america__dawson_creek, for__america__denver, for__america__detroit, for__america__edmonton, for__america__eirunepe, for__america__el_salvador, for__america__fort_nelson, for__america__fortaleza, for__america__glace_bay, for__america__goose_bay, for__america__grand_turk, for__america__guatemala, for__america__guayaquil, for__america__guyana, for__america__halifax, for__america__havana, for__america__hermosillo, for__america__indiana__indianapolis, for__america__indiana__knox, for__america__indiana__marengo, for__america__indiana__petersburg, for__america__indiana__tell_city, for__america__indiana__vevay, for__america__indiana__vincennes, for__america__indiana__winamac, for__america__inuvik, for__america__iqaluit, for__america__jamaica, for__america__juneau, for__america__kentucky__louisville, for__america__kentucky__monticello, for__america__la_paz, for__america__lima, for__america__los_angeles, for__america__maceio, for__america__managua, for__america__manaus, for__america__martinique, for__america__matamoros, for__america__mazatlan, for__america__menominee, for__america__merida, for__america__metlakatla, for__america__mexico_city, for__america__miquelon, for__america__moncton, for__america__monterrey, for__america__montevideo, for__america__new_york, for__america__nome, for__america__noronha, for__america__north_dakota__beulah, for__america__north_dakota__center, for__america__north_dakota__new_salem, for__america__nuuk, for__america__ojinaga, for__america__panama, for__america__paramaribo, for__america__phoenix, for__america__port_au_prince, for__america__porto_velho, for__america__puerto_rico, for__america__punta_arenas, for__america__rankin_inlet, for__america__recife, for__america__regina, for__america__resolute, for__america__rio_branco, for__america__santarem, for__america__santiago, for__america__santo_domingo, for__america__sao_paulo, for__america__scoresbysund, for__america__sitka, for__america__st_johns, for__america__swift_current, for__america__tegucigalpa, for__america__thule, for__america__tijuana, for__america__toronto, for__america__vancouver, for__america__whitehorse, for__america__winnipeg, for__america__yakutat, for__antarctica__casey, for__antarctica__davis, for__antarctica__macquarie, for__antarctica__mawson, for__antarctica__palmer, for__antarctica__rothera, for__antarctica__troll, for__antarctica__vostok, for__asia__almaty, for__asia__amman, for__asia__anadyr, for__asia__aqtau, for__asia__aqtobe, for__asia__ashgabat, for__asia__atyrau, for__asia__baghdad, for__asia__baku, for__asia__bangkok, for__asia__barnaul, for__asia__beirut, for__asia__bishkek, for__asia__chita, for__asia__colombo, for__asia__damascus, for__asia__dhaka, for__asia__dili, for__asia__dubai, for__asia__dushanbe, for__asia__famagusta, for__asia__gaza, for__asia__hebron, for__asia__ho_chi_minh, for__asia__hong_kong, for__asia__hovd, for__asia__irkutsk, for__asia__jakarta, for__asia__jayapura, for__asia__jerusalem, for__asia__kabul, for__asia__kamchatka, for__asia__karachi, for__asia__kathmandu, for__asia__khandyga, for__asia__kolkata, for__asia__krasnoyarsk, for__asia__kuching, for__asia__macau, for__asia__magadan, for__asia__makassar, for__asia__manila, for__asia__nicosia, for__asia__novokuznetsk, for__asia__novosibirsk, for__asia__omsk, for__asia__oral, for__asia__pontianak, for__asia__pyongyang, for__asia__qatar, for__asia__qostanay, for__asia__qyzylorda, for__asia__riyadh, for__asia__sakhalin, for__asia__samarkand, for__asia__seoul, for__asia__shanghai, for__asia__singapore, for__asia__srednekolymsk, for__asia__taipei, for__asia__tashkent, for__asia__tbilisi, for__asia__tehran, for__asia__thimphu, for__asia__tokyo, for__asia__tomsk, for__asia__ulaanbaatar, for__asia__urumqi, for__asia__ust_nera, for__asia__vladivostok, for__asia__yakutsk, for__asia__yangon, for__asia__yekaterinburg, for__asia__yerevan, for__atlantic__azores, for__atlantic__bermuda, for__atlantic__canary, for__atlantic__cape_verde, for__atlantic__faroe, for__atlantic__madeira, for__atlantic__south_georgia, for__atlantic__stanley, for__australia__adelaide, for__australia__brisbane, for__australia__broken_hill, for__australia__darwin, for__australia__eucla, for__australia__hobart, for__australia__lindeman, for__australia__lord_howe, for__australia__melbourne, for__australia__perth, for__australia__sydney, for__europe__andorra, for__europe__astrakhan, for__europe__athens, for__europe__belgrade, for__europe__berlin, for__europe__brussels, for__europe__bucharest, for__europe__budapest, for__europe__chisinau, for__europe__dublin, for__europe__gibraltar, for__europe__helsinki, for__europe__istanbul, for__europe__kaliningrad, for__europe__kirov, for__europe__kyiv, for__europe__lisbon, for__europe__london, for__europe__madrid, for__europe__malta, for__europe__minsk, for__europe__moscow, for__europe__paris, for__europe__prague, for__europe__riga, for__europe__rome, for__europe__samara, for__europe__saratov, for__europe__simferopol, for__europe__sofia, for__europe__tallinn, for__europe__tirane, for__europe__ulyanovsk, for__europe__vienna, for__europe__vilnius, for__europe__volgograd, for__europe__warsaw, for__europe__zurich, for__indian__chagos, for__indian__maldives, for__indian__mauritius, for__pacific__apia, for__pacific__auckland, for__pacific__bougainville, for__pacific__chatham, for__pacific__easter, for__pacific__efate, for__pacific__fakaofo, for__pacific__fiji, for__pacific__galapagos, for__pacific__gambier, for__pacific__guadalcanal, for__pacific__guam, for__pacific__honolulu, for__pacific__kanton, for__pacific__kiritimati, for__pacific__kosrae, for__pacific__kwajalein, for__pacific__marquesas, for__pacific__nauru, for__pacific__niue, for__pacific__norfolk, for__pacific__noumea, for__pacific__pago_pago, for__pacific__palau, for__pacific__pitcairn, for__pacific__port_moresby, for__pacific__rarotonga, for__pacific__tahiti, for__pacific__tarawa, for__pacific__tongatapu

-}

import Internal
import Time exposing (Posix, Zone, ZoneName)


{-| What release of the IANA Time Zone Database is this data from?

    version
    --> "2024b"

-}
version : String
version =
    "2024b"


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


forZoneName : String -> Zone -> Posix -> Result Error Abbreviation
forZoneName name =
    let
        withError =
            ((<<) << (<<)) (Result.mapError (UnknownZoneOffset (Time.Name name)))
    in
    case name of
        "Africa/Abidjan" ->
            withError for__africa__abidjan

        "Africa/Algiers" ->
            withError for__africa__algiers

        "Africa/Bissau" ->
            withError for__africa__bissau

        "Africa/Cairo" ->
            withError for__africa__cairo

        "Africa/Casablanca" ->
            withError for__africa__casablanca

        "Africa/Ceuta" ->
            withError for__africa__ceuta

        "Africa/El_Aaiun" ->
            withError for__africa__el_aaiun

        "Africa/Johannesburg" ->
            withError for__africa__johannesburg

        "Africa/Juba" ->
            withError for__africa__juba

        "Africa/Khartoum" ->
            withError for__africa__khartoum

        "Africa/Lagos" ->
            withError for__africa__lagos

        "Africa/Maputo" ->
            withError for__africa__maputo

        "Africa/Monrovia" ->
            withError for__africa__monrovia

        "Africa/Nairobi" ->
            withError for__africa__nairobi

        "Africa/Ndjamena" ->
            withError for__africa__ndjamena

        "Africa/Sao_Tome" ->
            withError for__africa__sao_tome

        "Africa/Tripoli" ->
            withError for__africa__tripoli

        "Africa/Tunis" ->
            withError for__africa__tunis

        "Africa/Windhoek" ->
            withError for__africa__windhoek

        "America/Adak" ->
            withError for__america__adak

        "America/Anchorage" ->
            withError for__america__anchorage

        "America/Araguaina" ->
            withError for__america__araguaina

        "America/Argentina/Buenos_Aires" ->
            withError for__america__argentina__buenos_aires

        "America/Argentina/Catamarca" ->
            withError for__america__argentina__catamarca

        "America/Argentina/Cordoba" ->
            withError for__america__argentina__cordoba

        "America/Argentina/Jujuy" ->
            withError for__america__argentina__jujuy

        "America/Argentina/La_Rioja" ->
            withError for__america__argentina__la_rioja

        "America/Argentina/Mendoza" ->
            withError for__america__argentina__mendoza

        "America/Argentina/Rio_Gallegos" ->
            withError for__america__argentina__rio_gallegos

        "America/Argentina/Salta" ->
            withError for__america__argentina__salta

        "America/Argentina/San_Juan" ->
            withError for__america__argentina__san_juan

        "America/Argentina/San_Luis" ->
            withError for__america__argentina__san_luis

        "America/Argentina/Tucuman" ->
            withError for__america__argentina__tucuman

        "America/Argentina/Ushuaia" ->
            withError for__america__argentina__ushuaia

        "America/Asuncion" ->
            withError for__america__asuncion

        "America/Bahia" ->
            withError for__america__bahia

        "America/Bahia_Banderas" ->
            withError for__america__bahia_banderas

        "America/Barbados" ->
            withError for__america__barbados

        "America/Belem" ->
            withError for__america__belem

        "America/Belize" ->
            withError for__america__belize

        "America/Boa_Vista" ->
            withError for__america__boa_vista

        "America/Bogota" ->
            withError for__america__bogota

        "America/Boise" ->
            withError for__america__boise

        "America/Cambridge_Bay" ->
            withError for__america__cambridge_bay

        "America/Campo_Grande" ->
            withError for__america__campo_grande

        "America/Cancun" ->
            withError for__america__cancun

        "America/Caracas" ->
            withError for__america__caracas

        "America/Cayenne" ->
            withError for__america__cayenne

        "America/Chicago" ->
            withError for__america__chicago

        "America/Chihuahua" ->
            withError for__america__chihuahua

        "America/Ciudad_Juarez" ->
            withError for__america__ciudad_juarez

        "America/Costa_Rica" ->
            withError for__america__costa_rica

        "America/Cuiaba" ->
            withError for__america__cuiaba

        "America/Danmarkshavn" ->
            withError for__america__danmarkshavn

        "America/Dawson" ->
            withError for__america__dawson

        "America/Dawson_Creek" ->
            withError for__america__dawson_creek

        "America/Denver" ->
            withError for__america__denver

        "America/Detroit" ->
            withError for__america__detroit

        "America/Edmonton" ->
            withError for__america__edmonton

        "America/Eirunepe" ->
            withError for__america__eirunepe

        "America/El_Salvador" ->
            withError for__america__el_salvador

        "America/Fort_Nelson" ->
            withError for__america__fort_nelson

        "America/Fortaleza" ->
            withError for__america__fortaleza

        "America/Glace_Bay" ->
            withError for__america__glace_bay

        "America/Goose_Bay" ->
            withError for__america__goose_bay

        "America/Grand_Turk" ->
            withError for__america__grand_turk

        "America/Guatemala" ->
            withError for__america__guatemala

        "America/Guayaquil" ->
            withError for__america__guayaquil

        "America/Guyana" ->
            withError for__america__guyana

        "America/Halifax" ->
            withError for__america__halifax

        "America/Havana" ->
            withError for__america__havana

        "America/Hermosillo" ->
            withError for__america__hermosillo

        "America/Indiana/Indianapolis" ->
            withError for__america__indiana__indianapolis

        "America/Indiana/Knox" ->
            withError for__america__indiana__knox

        "America/Indiana/Marengo" ->
            withError for__america__indiana__marengo

        "America/Indiana/Petersburg" ->
            withError for__america__indiana__petersburg

        "America/Indiana/Tell_City" ->
            withError for__america__indiana__tell_city

        "America/Indiana/Vevay" ->
            withError for__america__indiana__vevay

        "America/Indiana/Vincennes" ->
            withError for__america__indiana__vincennes

        "America/Indiana/Winamac" ->
            withError for__america__indiana__winamac

        "America/Inuvik" ->
            withError for__america__inuvik

        "America/Iqaluit" ->
            withError for__america__iqaluit

        "America/Jamaica" ->
            withError for__america__jamaica

        "America/Juneau" ->
            withError for__america__juneau

        "America/Kentucky/Louisville" ->
            withError for__america__kentucky__louisville

        "America/Kentucky/Monticello" ->
            withError for__america__kentucky__monticello

        "America/La_Paz" ->
            withError for__america__la_paz

        "America/Lima" ->
            withError for__america__lima

        "America/Los_Angeles" ->
            withError for__america__los_angeles

        "America/Maceio" ->
            withError for__america__maceio

        "America/Managua" ->
            withError for__america__managua

        "America/Manaus" ->
            withError for__america__manaus

        "America/Martinique" ->
            withError for__america__martinique

        "America/Matamoros" ->
            withError for__america__matamoros

        "America/Mazatlan" ->
            withError for__america__mazatlan

        "America/Menominee" ->
            withError for__america__menominee

        "America/Merida" ->
            withError for__america__merida

        "America/Metlakatla" ->
            withError for__america__metlakatla

        "America/Mexico_City" ->
            withError for__america__mexico_city

        "America/Miquelon" ->
            withError for__america__miquelon

        "America/Moncton" ->
            withError for__america__moncton

        "America/Monterrey" ->
            withError for__america__monterrey

        "America/Montevideo" ->
            withError for__america__montevideo

        "America/New_York" ->
            withError for__america__new_york

        "America/Nome" ->
            withError for__america__nome

        "America/Noronha" ->
            withError for__america__noronha

        "America/North_Dakota/Beulah" ->
            withError for__america__north_dakota__beulah

        "America/North_Dakota/Center" ->
            withError for__america__north_dakota__center

        "America/North_Dakota/New_Salem" ->
            withError for__america__north_dakota__new_salem

        "America/Nuuk" ->
            withError for__america__nuuk

        "America/Ojinaga" ->
            withError for__america__ojinaga

        "America/Panama" ->
            withError for__america__panama

        "America/Paramaribo" ->
            withError for__america__paramaribo

        "America/Phoenix" ->
            withError for__america__phoenix

        "America/Port-au-Prince" ->
            withError for__america__port_au_prince

        "America/Porto_Velho" ->
            withError for__america__porto_velho

        "America/Puerto_Rico" ->
            withError for__america__puerto_rico

        "America/Punta_Arenas" ->
            withError for__america__punta_arenas

        "America/Rankin_Inlet" ->
            withError for__america__rankin_inlet

        "America/Recife" ->
            withError for__america__recife

        "America/Regina" ->
            withError for__america__regina

        "America/Resolute" ->
            withError for__america__resolute

        "America/Rio_Branco" ->
            withError for__america__rio_branco

        "America/Santarem" ->
            withError for__america__santarem

        "America/Santiago" ->
            withError for__america__santiago

        "America/Santo_Domingo" ->
            withError for__america__santo_domingo

        "America/Sao_Paulo" ->
            withError for__america__sao_paulo

        "America/Scoresbysund" ->
            withError for__america__scoresbysund

        "America/Sitka" ->
            withError for__america__sitka

        "America/St_Johns" ->
            withError for__america__st_johns

        "America/Swift_Current" ->
            withError for__america__swift_current

        "America/Tegucigalpa" ->
            withError for__america__tegucigalpa

        "America/Thule" ->
            withError for__america__thule

        "America/Tijuana" ->
            withError for__america__tijuana

        "America/Toronto" ->
            withError for__america__toronto

        "America/Vancouver" ->
            withError for__america__vancouver

        "America/Whitehorse" ->
            withError for__america__whitehorse

        "America/Winnipeg" ->
            withError for__america__winnipeg

        "America/Yakutat" ->
            withError for__america__yakutat

        "Antarctica/Casey" ->
            withError for__antarctica__casey

        "Antarctica/Davis" ->
            withError for__antarctica__davis

        "Antarctica/Macquarie" ->
            withError for__antarctica__macquarie

        "Antarctica/Mawson" ->
            withError for__antarctica__mawson

        "Antarctica/Palmer" ->
            withError for__antarctica__palmer

        "Antarctica/Rothera" ->
            withError for__antarctica__rothera

        "Antarctica/Troll" ->
            withError for__antarctica__troll

        "Antarctica/Vostok" ->
            withError for__antarctica__vostok

        "Asia/Almaty" ->
            withError for__asia__almaty

        "Asia/Amman" ->
            withError for__asia__amman

        "Asia/Anadyr" ->
            withError for__asia__anadyr

        "Asia/Aqtau" ->
            withError for__asia__aqtau

        "Asia/Aqtobe" ->
            withError for__asia__aqtobe

        "Asia/Ashgabat" ->
            withError for__asia__ashgabat

        "Asia/Atyrau" ->
            withError for__asia__atyrau

        "Asia/Baghdad" ->
            withError for__asia__baghdad

        "Asia/Baku" ->
            withError for__asia__baku

        "Asia/Bangkok" ->
            withError for__asia__bangkok

        "Asia/Barnaul" ->
            withError for__asia__barnaul

        "Asia/Beirut" ->
            withError for__asia__beirut

        "Asia/Bishkek" ->
            withError for__asia__bishkek

        "Asia/Chita" ->
            withError for__asia__chita

        "Asia/Colombo" ->
            withError for__asia__colombo

        "Asia/Damascus" ->
            withError for__asia__damascus

        "Asia/Dhaka" ->
            withError for__asia__dhaka

        "Asia/Dili" ->
            withError for__asia__dili

        "Asia/Dubai" ->
            withError for__asia__dubai

        "Asia/Dushanbe" ->
            withError for__asia__dushanbe

        "Asia/Famagusta" ->
            withError for__asia__famagusta

        "Asia/Gaza" ->
            withError for__asia__gaza

        "Asia/Hebron" ->
            withError for__asia__hebron

        "Asia/Ho_Chi_Minh" ->
            withError for__asia__ho_chi_minh

        "Asia/Hong_Kong" ->
            withError for__asia__hong_kong

        "Asia/Hovd" ->
            withError for__asia__hovd

        "Asia/Irkutsk" ->
            withError for__asia__irkutsk

        "Asia/Jakarta" ->
            withError for__asia__jakarta

        "Asia/Jayapura" ->
            withError for__asia__jayapura

        "Asia/Jerusalem" ->
            withError for__asia__jerusalem

        "Asia/Kabul" ->
            withError for__asia__kabul

        "Asia/Kamchatka" ->
            withError for__asia__kamchatka

        "Asia/Karachi" ->
            withError for__asia__karachi

        "Asia/Kathmandu" ->
            withError for__asia__kathmandu

        "Asia/Khandyga" ->
            withError for__asia__khandyga

        "Asia/Kolkata" ->
            withError for__asia__kolkata

        "Asia/Krasnoyarsk" ->
            withError for__asia__krasnoyarsk

        "Asia/Kuching" ->
            withError for__asia__kuching

        "Asia/Macau" ->
            withError for__asia__macau

        "Asia/Magadan" ->
            withError for__asia__magadan

        "Asia/Makassar" ->
            withError for__asia__makassar

        "Asia/Manila" ->
            withError for__asia__manila

        "Asia/Nicosia" ->
            withError for__asia__nicosia

        "Asia/Novokuznetsk" ->
            withError for__asia__novokuznetsk

        "Asia/Novosibirsk" ->
            withError for__asia__novosibirsk

        "Asia/Omsk" ->
            withError for__asia__omsk

        "Asia/Oral" ->
            withError for__asia__oral

        "Asia/Pontianak" ->
            withError for__asia__pontianak

        "Asia/Pyongyang" ->
            withError for__asia__pyongyang

        "Asia/Qatar" ->
            withError for__asia__qatar

        "Asia/Qostanay" ->
            withError for__asia__qostanay

        "Asia/Qyzylorda" ->
            withError for__asia__qyzylorda

        "Asia/Riyadh" ->
            withError for__asia__riyadh

        "Asia/Sakhalin" ->
            withError for__asia__sakhalin

        "Asia/Samarkand" ->
            withError for__asia__samarkand

        "Asia/Seoul" ->
            withError for__asia__seoul

        "Asia/Shanghai" ->
            withError for__asia__shanghai

        "Asia/Singapore" ->
            withError for__asia__singapore

        "Asia/Srednekolymsk" ->
            withError for__asia__srednekolymsk

        "Asia/Taipei" ->
            withError for__asia__taipei

        "Asia/Tashkent" ->
            withError for__asia__tashkent

        "Asia/Tbilisi" ->
            withError for__asia__tbilisi

        "Asia/Tehran" ->
            withError for__asia__tehran

        "Asia/Thimphu" ->
            withError for__asia__thimphu

        "Asia/Tokyo" ->
            withError for__asia__tokyo

        "Asia/Tomsk" ->
            withError for__asia__tomsk

        "Asia/Ulaanbaatar" ->
            withError for__asia__ulaanbaatar

        "Asia/Urumqi" ->
            withError for__asia__urumqi

        "Asia/Ust-Nera" ->
            withError for__asia__ust_nera

        "Asia/Vladivostok" ->
            withError for__asia__vladivostok

        "Asia/Yakutsk" ->
            withError for__asia__yakutsk

        "Asia/Yangon" ->
            withError for__asia__yangon

        "Asia/Yekaterinburg" ->
            withError for__asia__yekaterinburg

        "Asia/Yerevan" ->
            withError for__asia__yerevan

        "Atlantic/Azores" ->
            withError for__atlantic__azores

        "Atlantic/Bermuda" ->
            withError for__atlantic__bermuda

        "Atlantic/Canary" ->
            withError for__atlantic__canary

        "Atlantic/Cape_Verde" ->
            withError for__atlantic__cape_verde

        "Atlantic/Faroe" ->
            withError for__atlantic__faroe

        "Atlantic/Madeira" ->
            withError for__atlantic__madeira

        "Atlantic/South_Georgia" ->
            withError for__atlantic__south_georgia

        "Atlantic/Stanley" ->
            withError for__atlantic__stanley

        "Australia/Adelaide" ->
            withError for__australia__adelaide

        "Australia/Brisbane" ->
            withError for__australia__brisbane

        "Australia/Broken_Hill" ->
            withError for__australia__broken_hill

        "Australia/Darwin" ->
            withError for__australia__darwin

        "Australia/Eucla" ->
            withError for__australia__eucla

        "Australia/Hobart" ->
            withError for__australia__hobart

        "Australia/Lindeman" ->
            withError for__australia__lindeman

        "Australia/Lord_Howe" ->
            withError for__australia__lord_howe

        "Australia/Melbourne" ->
            withError for__australia__melbourne

        "Australia/Perth" ->
            withError for__australia__perth

        "Australia/Sydney" ->
            withError for__australia__sydney

        "Europe/Andorra" ->
            withError for__europe__andorra

        "Europe/Astrakhan" ->
            withError for__europe__astrakhan

        "Europe/Athens" ->
            withError for__europe__athens

        "Europe/Belgrade" ->
            withError for__europe__belgrade

        "Europe/Berlin" ->
            withError for__europe__berlin

        "Europe/Brussels" ->
            withError for__europe__brussels

        "Europe/Bucharest" ->
            withError for__europe__bucharest

        "Europe/Budapest" ->
            withError for__europe__budapest

        "Europe/Chisinau" ->
            withError for__europe__chisinau

        "Europe/Dublin" ->
            withError for__europe__dublin

        "Europe/Gibraltar" ->
            withError for__europe__gibraltar

        "Europe/Helsinki" ->
            withError for__europe__helsinki

        "Europe/Istanbul" ->
            withError for__europe__istanbul

        "Europe/Kaliningrad" ->
            withError for__europe__kaliningrad

        "Europe/Kirov" ->
            withError for__europe__kirov

        "Europe/Kyiv" ->
            withError for__europe__kyiv

        "Europe/Lisbon" ->
            withError for__europe__lisbon

        "Europe/London" ->
            withError for__europe__london

        "Europe/Madrid" ->
            withError for__europe__madrid

        "Europe/Malta" ->
            withError for__europe__malta

        "Europe/Minsk" ->
            withError for__europe__minsk

        "Europe/Moscow" ->
            withError for__europe__moscow

        "Europe/Paris" ->
            withError for__europe__paris

        "Europe/Prague" ->
            withError for__europe__prague

        "Europe/Riga" ->
            withError for__europe__riga

        "Europe/Rome" ->
            withError for__europe__rome

        "Europe/Samara" ->
            withError for__europe__samara

        "Europe/Saratov" ->
            withError for__europe__saratov

        "Europe/Simferopol" ->
            withError for__europe__simferopol

        "Europe/Sofia" ->
            withError for__europe__sofia

        "Europe/Tallinn" ->
            withError for__europe__tallinn

        "Europe/Tirane" ->
            withError for__europe__tirane

        "Europe/Ulyanovsk" ->
            withError for__europe__ulyanovsk

        "Europe/Vienna" ->
            withError for__europe__vienna

        "Europe/Vilnius" ->
            withError for__europe__vilnius

        "Europe/Volgograd" ->
            withError for__europe__volgograd

        "Europe/Warsaw" ->
            withError for__europe__warsaw

        "Europe/Zurich" ->
            withError for__europe__zurich

        "Indian/Chagos" ->
            withError for__indian__chagos

        "Indian/Maldives" ->
            withError for__indian__maldives

        "Indian/Mauritius" ->
            withError for__indian__mauritius

        "Pacific/Apia" ->
            withError for__pacific__apia

        "Pacific/Auckland" ->
            withError for__pacific__auckland

        "Pacific/Bougainville" ->
            withError for__pacific__bougainville

        "Pacific/Chatham" ->
            withError for__pacific__chatham

        "Pacific/Easter" ->
            withError for__pacific__easter

        "Pacific/Efate" ->
            withError for__pacific__efate

        "Pacific/Fakaofo" ->
            withError for__pacific__fakaofo

        "Pacific/Fiji" ->
            withError for__pacific__fiji

        "Pacific/Galapagos" ->
            withError for__pacific__galapagos

        "Pacific/Gambier" ->
            withError for__pacific__gambier

        "Pacific/Guadalcanal" ->
            withError for__pacific__guadalcanal

        "Pacific/Guam" ->
            withError for__pacific__guam

        "Pacific/Honolulu" ->
            withError for__pacific__honolulu

        "Pacific/Kanton" ->
            withError for__pacific__kanton

        "Pacific/Kiritimati" ->
            withError for__pacific__kiritimati

        "Pacific/Kosrae" ->
            withError for__pacific__kosrae

        "Pacific/Kwajalein" ->
            withError for__pacific__kwajalein

        "Pacific/Marquesas" ->
            withError for__pacific__marquesas

        "Pacific/Nauru" ->
            withError for__pacific__nauru

        "Pacific/Niue" ->
            withError for__pacific__niue

        "Pacific/Norfolk" ->
            withError for__pacific__norfolk

        "Pacific/Noumea" ->
            withError for__pacific__noumea

        "Pacific/Pago_Pago" ->
            withError for__pacific__pago_pago

        "Pacific/Palau" ->
            withError for__pacific__palau

        "Pacific/Pitcairn" ->
            withError for__pacific__pitcairn

        "Pacific/Port_Moresby" ->
            withError for__pacific__port_moresby

        "Pacific/Rarotonga" ->
            withError for__pacific__rarotonga

        "Pacific/Tahiti" ->
            withError for__pacific__tahiti

        "Pacific/Tarawa" ->
            withError for__pacific__tarawa

        "Pacific/Tongatapu" ->
            withError for__pacific__tongatapu

        "Africa/Accra" ->
            withError for__africa__abidjan

        "Africa/Addis_Ababa" ->
            withError for__africa__nairobi

        "Africa/Asmara" ->
            withError for__africa__nairobi

        "Africa/Asmera" ->
            withError for__africa__nairobi

        "Africa/Bamako" ->
            withError for__africa__abidjan

        "Africa/Bangui" ->
            withError for__africa__lagos

        "Africa/Banjul" ->
            withError for__africa__abidjan

        "Africa/Blantyre" ->
            withError for__africa__maputo

        "Africa/Brazzaville" ->
            withError for__africa__lagos

        "Africa/Bujumbura" ->
            withError for__africa__maputo

        "Africa/Conakry" ->
            withError for__africa__abidjan

        "Africa/Dakar" ->
            withError for__africa__abidjan

        "Africa/Dar_es_Salaam" ->
            withError for__africa__nairobi

        "Africa/Djibouti" ->
            withError for__africa__nairobi

        "Africa/Douala" ->
            withError for__africa__lagos

        "Africa/Freetown" ->
            withError for__africa__abidjan

        "Africa/Gaborone" ->
            withError for__africa__maputo

        "Africa/Harare" ->
            withError for__africa__maputo

        "Africa/Kampala" ->
            withError for__africa__nairobi

        "Africa/Kigali" ->
            withError for__africa__maputo

        "Africa/Kinshasa" ->
            withError for__africa__lagos

        "Africa/Libreville" ->
            withError for__africa__lagos

        "Africa/Lome" ->
            withError for__africa__abidjan

        "Africa/Luanda" ->
            withError for__africa__lagos

        "Africa/Lubumbashi" ->
            withError for__africa__maputo

        "Africa/Lusaka" ->
            withError for__africa__maputo

        "Africa/Malabo" ->
            withError for__africa__lagos

        "Africa/Maseru" ->
            withError for__africa__johannesburg

        "Africa/Mbabane" ->
            withError for__africa__johannesburg

        "Africa/Mogadishu" ->
            withError for__africa__nairobi

        "Africa/Niamey" ->
            withError for__africa__lagos

        "Africa/Nouakchott" ->
            withError for__africa__abidjan

        "Africa/Ouagadougou" ->
            withError for__africa__abidjan

        "Africa/Porto-Novo" ->
            withError for__africa__lagos

        "Africa/Timbuktu" ->
            withError for__africa__abidjan

        "America/Anguilla" ->
            withError for__america__puerto_rico

        "America/Antigua" ->
            withError for__america__puerto_rico

        "America/Argentina/ComodRivadavia" ->
            withError for__america__argentina__catamarca

        "America/Aruba" ->
            withError for__america__puerto_rico

        "America/Atikokan" ->
            withError for__america__panama

        "America/Atka" ->
            withError for__america__adak

        "America/Blanc-Sablon" ->
            withError for__america__puerto_rico

        "America/Buenos_Aires" ->
            withError for__america__argentina__buenos_aires

        "America/Catamarca" ->
            withError for__america__argentina__catamarca

        "America/Cayman" ->
            withError for__america__panama

        "America/Coral_Harbour" ->
            withError for__america__panama

        "America/Cordoba" ->
            withError for__america__argentina__cordoba

        "America/Creston" ->
            withError for__america__phoenix

        "America/Curacao" ->
            withError for__america__puerto_rico

        "America/Dominica" ->
            withError for__america__puerto_rico

        "America/Ensenada" ->
            withError for__america__tijuana

        "America/Fort_Wayne" ->
            withError for__america__indiana__indianapolis

        "America/Godthab" ->
            withError for__america__nuuk

        "America/Grenada" ->
            withError for__america__puerto_rico

        "America/Guadeloupe" ->
            withError for__america__puerto_rico

        "America/Indianapolis" ->
            withError for__america__indiana__indianapolis

        "America/Jujuy" ->
            withError for__america__argentina__jujuy

        "America/Knox_IN" ->
            withError for__america__indiana__knox

        "America/Kralendijk" ->
            withError for__america__puerto_rico

        "America/Louisville" ->
            withError for__america__kentucky__louisville

        "America/Lower_Princes" ->
            withError for__america__puerto_rico

        "America/Marigot" ->
            withError for__america__puerto_rico

        "America/Mendoza" ->
            withError for__america__argentina__mendoza

        "America/Montreal" ->
            withError for__america__toronto

        "America/Montserrat" ->
            withError for__america__puerto_rico

        "America/Nassau" ->
            withError for__america__toronto

        "America/Nipigon" ->
            withError for__america__toronto

        "America/Pangnirtung" ->
            withError for__america__iqaluit

        "America/Port_of_Spain" ->
            withError for__america__puerto_rico

        "America/Porto_Acre" ->
            withError for__america__rio_branco

        "America/Rainy_River" ->
            withError for__america__winnipeg

        "America/Rosario" ->
            withError for__america__argentina__cordoba

        "America/Santa_Isabel" ->
            withError for__america__tijuana

        "America/Shiprock" ->
            withError for__america__denver

        "America/St_Barthelemy" ->
            withError for__america__puerto_rico

        "America/St_Kitts" ->
            withError for__america__puerto_rico

        "America/St_Lucia" ->
            withError for__america__puerto_rico

        "America/St_Thomas" ->
            withError for__america__puerto_rico

        "America/St_Vincent" ->
            withError for__america__puerto_rico

        "America/Thunder_Bay" ->
            withError for__america__toronto

        "America/Tortola" ->
            withError for__america__puerto_rico

        "America/Virgin" ->
            withError for__america__puerto_rico

        "America/Yellowknife" ->
            withError for__america__edmonton

        "Antarctica/DumontDUrville" ->
            withError for__pacific__port_moresby

        "Antarctica/McMurdo" ->
            withError for__pacific__auckland

        "Antarctica/South_Pole" ->
            withError for__pacific__auckland

        "Antarctica/Syowa" ->
            withError for__asia__riyadh

        "Arctic/Longyearbyen" ->
            withError for__europe__berlin

        "Asia/Aden" ->
            withError for__asia__riyadh

        "Asia/Ashkhabad" ->
            withError for__asia__ashgabat

        "Asia/Bahrain" ->
            withError for__asia__qatar

        "Asia/Brunei" ->
            withError for__asia__kuching

        "Asia/Calcutta" ->
            withError for__asia__kolkata

        "Asia/Choibalsan" ->
            withError for__asia__ulaanbaatar

        "Asia/Chongqing" ->
            withError for__asia__shanghai

        "Asia/Chungking" ->
            withError for__asia__shanghai

        "Asia/Dacca" ->
            withError for__asia__dhaka

        "Asia/Harbin" ->
            withError for__asia__shanghai

        "Asia/Istanbul" ->
            withError for__europe__istanbul

        "Asia/Kashgar" ->
            withError for__asia__urumqi

        "Asia/Katmandu" ->
            withError for__asia__kathmandu

        "Asia/Kuala_Lumpur" ->
            withError for__asia__singapore

        "Asia/Kuwait" ->
            withError for__asia__riyadh

        "Asia/Macao" ->
            withError for__asia__macau

        "Asia/Muscat" ->
            withError for__asia__dubai

        "Asia/Phnom_Penh" ->
            withError for__asia__bangkok

        "Asia/Rangoon" ->
            withError for__asia__yangon

        "Asia/Saigon" ->
            withError for__asia__ho_chi_minh

        "Asia/Tel_Aviv" ->
            withError for__asia__jerusalem

        "Asia/Thimbu" ->
            withError for__asia__thimphu

        "Asia/Ujung_Pandang" ->
            withError for__asia__makassar

        "Asia/Ulan_Bator" ->
            withError for__asia__ulaanbaatar

        "Asia/Vientiane" ->
            withError for__asia__bangkok

        "Atlantic/Faeroe" ->
            withError for__atlantic__faroe

        "Atlantic/Jan_Mayen" ->
            withError for__europe__berlin

        "Atlantic/Reykjavik" ->
            withError for__africa__abidjan

        "Atlantic/St_Helena" ->
            withError for__africa__abidjan

        "Australia/ACT" ->
            withError for__australia__sydney

        "Australia/Canberra" ->
            withError for__australia__sydney

        "Australia/Currie" ->
            withError for__australia__hobart

        "Australia/LHI" ->
            withError for__australia__lord_howe

        "Australia/NSW" ->
            withError for__australia__sydney

        "Australia/North" ->
            withError for__australia__darwin

        "Australia/Queensland" ->
            withError for__australia__brisbane

        "Australia/South" ->
            withError for__australia__adelaide

        "Australia/Tasmania" ->
            withError for__australia__hobart

        "Australia/Victoria" ->
            withError for__australia__melbourne

        "Australia/West" ->
            withError for__australia__perth

        "Australia/Yancowinna" ->
            withError for__australia__broken_hill

        "Brazil/Acre" ->
            withError for__america__rio_branco

        "Brazil/DeNoronha" ->
            withError for__america__noronha

        "Brazil/East" ->
            withError for__america__sao_paulo

        "Brazil/West" ->
            withError for__america__manaus

        "CET" ->
            withError for__europe__brussels

        "CST6CDT" ->
            withError for__america__chicago

        "Canada/Atlantic" ->
            withError for__america__halifax

        "Canada/Central" ->
            withError for__america__winnipeg

        "Canada/Eastern" ->
            withError for__america__toronto

        "Canada/Mountain" ->
            withError for__america__edmonton

        "Canada/Newfoundland" ->
            withError for__america__st_johns

        "Canada/Pacific" ->
            withError for__america__vancouver

        "Canada/Saskatchewan" ->
            withError for__america__regina

        "Canada/Yukon" ->
            withError for__america__whitehorse

        "Chile/Continental" ->
            withError for__america__santiago

        "Chile/EasterIsland" ->
            withError for__pacific__easter

        "Cuba" ->
            withError for__america__havana

        "EET" ->
            withError for__europe__athens

        "EST" ->
            withError for__america__panama

        "EST5EDT" ->
            withError for__america__new_york

        "Egypt" ->
            withError for__africa__cairo

        "Eire" ->
            withError for__europe__dublin

        "Europe/Amsterdam" ->
            withError for__europe__brussels

        "Europe/Belfast" ->
            withError for__europe__london

        "Europe/Bratislava" ->
            withError for__europe__prague

        "Europe/Busingen" ->
            withError for__europe__zurich

        "Europe/Copenhagen" ->
            withError for__europe__berlin

        "Europe/Guernsey" ->
            withError for__europe__london

        "Europe/Isle_of_Man" ->
            withError for__europe__london

        "Europe/Jersey" ->
            withError for__europe__london

        "Europe/Kiev" ->
            withError for__europe__kyiv

        "Europe/Ljubljana" ->
            withError for__europe__belgrade

        "Europe/Luxembourg" ->
            withError for__europe__brussels

        "Europe/Mariehamn" ->
            withError for__europe__helsinki

        "Europe/Monaco" ->
            withError for__europe__paris

        "Europe/Nicosia" ->
            withError for__asia__nicosia

        "Europe/Oslo" ->
            withError for__europe__berlin

        "Europe/Podgorica" ->
            withError for__europe__belgrade

        "Europe/San_Marino" ->
            withError for__europe__rome

        "Europe/Sarajevo" ->
            withError for__europe__belgrade

        "Europe/Skopje" ->
            withError for__europe__belgrade

        "Europe/Stockholm" ->
            withError for__europe__berlin

        "Europe/Tiraspol" ->
            withError for__europe__chisinau

        "Europe/Uzhgorod" ->
            withError for__europe__kyiv

        "Europe/Vaduz" ->
            withError for__europe__zurich

        "Europe/Vatican" ->
            withError for__europe__rome

        "Europe/Zagreb" ->
            withError for__europe__belgrade

        "Europe/Zaporozhye" ->
            withError for__europe__kyiv

        "GB" ->
            withError for__europe__london

        "GB-Eire" ->
            withError for__europe__london

        "HST" ->
            withError for__pacific__honolulu

        "Hongkong" ->
            withError for__asia__hong_kong

        "Iceland" ->
            withError for__africa__abidjan

        "Indian/Antananarivo" ->
            withError for__africa__nairobi

        "Indian/Christmas" ->
            withError for__asia__bangkok

        "Indian/Cocos" ->
            withError for__asia__yangon

        "Indian/Comoro" ->
            withError for__africa__nairobi

        "Indian/Kerguelen" ->
            withError for__indian__maldives

        "Indian/Mahe" ->
            withError for__asia__dubai

        "Indian/Mayotte" ->
            withError for__africa__nairobi

        "Indian/Reunion" ->
            withError for__asia__dubai

        "Iran" ->
            withError for__asia__tehran

        "Israel" ->
            withError for__asia__jerusalem

        "Jamaica" ->
            withError for__america__jamaica

        "Japan" ->
            withError for__asia__tokyo

        "Kwajalein" ->
            withError for__pacific__kwajalein

        "Libya" ->
            withError for__africa__tripoli

        "MET" ->
            withError for__europe__brussels

        "MST" ->
            withError for__america__phoenix

        "MST7MDT" ->
            withError for__america__denver

        "Mexico/BajaNorte" ->
            withError for__america__tijuana

        "Mexico/BajaSur" ->
            withError for__america__mazatlan

        "Mexico/General" ->
            withError for__america__mexico_city

        "NZ" ->
            withError for__pacific__auckland

        "NZ-CHAT" ->
            withError for__pacific__chatham

        "Navajo" ->
            withError for__america__denver

        "PRC" ->
            withError for__asia__shanghai

        "PST8PDT" ->
            withError for__america__los_angeles

        "Pacific/Chuuk" ->
            withError for__pacific__port_moresby

        "Pacific/Enderbury" ->
            withError for__pacific__kanton

        "Pacific/Funafuti" ->
            withError for__pacific__tarawa

        "Pacific/Johnston" ->
            withError for__pacific__honolulu

        "Pacific/Majuro" ->
            withError for__pacific__tarawa

        "Pacific/Midway" ->
            withError for__pacific__pago_pago

        "Pacific/Pohnpei" ->
            withError for__pacific__guadalcanal

        "Pacific/Ponape" ->
            withError for__pacific__guadalcanal

        "Pacific/Saipan" ->
            withError for__pacific__guam

        "Pacific/Samoa" ->
            withError for__pacific__pago_pago

        "Pacific/Truk" ->
            withError for__pacific__port_moresby

        "Pacific/Wake" ->
            withError for__pacific__tarawa

        "Pacific/Wallis" ->
            withError for__pacific__tarawa

        "Pacific/Yap" ->
            withError for__pacific__port_moresby

        "Poland" ->
            withError for__europe__warsaw

        "Portugal" ->
            withError for__europe__lisbon

        "ROC" ->
            withError for__asia__taipei

        "ROK" ->
            withError for__asia__seoul

        "Singapore" ->
            withError for__asia__singapore

        "Turkey" ->
            withError for__europe__istanbul

        "US/Alaska" ->
            withError for__america__anchorage

        "US/Aleutian" ->
            withError for__america__adak

        "US/Arizona" ->
            withError for__america__phoenix

        "US/Central" ->
            withError for__america__chicago

        "US/East-Indiana" ->
            withError for__america__indiana__indianapolis

        "US/Eastern" ->
            withError for__america__new_york

        "US/Hawaii" ->
            withError for__pacific__honolulu

        "US/Indiana-Starke" ->
            withError for__america__indiana__knox

        "US/Michigan" ->
            withError for__america__detroit

        "US/Mountain" ->
            withError for__america__denver

        "US/Pacific" ->
            withError for__america__los_angeles

        "US/Samoa" ->
            withError for__pacific__pago_pago

        "W-SU" ->
            withError for__europe__moscow

        "WET" ->
            withError for__europe__lisbon

        other ->
            \_ _ -> Err (UnknownZoneName other)


{-| `Africa/Abidjan`
-}
for__africa__abidjan : Zone -> Posix -> Result Int Abbreviation
for__africa__abidjan =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -17 || offsetMinutes == -16 then
                Ok Lmt

            else if offsetMinutes == 0 then
                Ok (ShortName "GMT")

            else
                Err offsetMinutes
        )


{-| `Africa/Algiers`
-}
for__africa__algiers : Zone -> Posix -> Result Int Abbreviation
for__africa__algiers =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 12 || offsetMinutes == 13 then
                Ok Lmt

            else if offsetMinutes == 120 then
                Ok (ShortName "CEST")

            else if offsetMinutes == 9 || offsetMinutes == 10 then
                Ok (ShortName "PMT")

            else if offsetMinutes == 60 then
                Ok
                    (if posixSeconds >= 357523200 then
                        ShortName "CET"

                     else if posixSeconds >= 325468800 then
                        ShortName "WEST"

                     else if posixSeconds >= 246236400 then
                        ShortName "CET"

                     else if posixSeconds >= 41468400 then
                        ShortName "WEST"

                     else if posixSeconds >= -942012000 then
                        ShortName "CET"

                     else
                        ShortName "WEST"
                    )

            else if offsetMinutes == 0 then
                Ok (ShortName "WET")

            else
                Err offsetMinutes
        )


{-| `Africa/Bissau`
-}
for__africa__bissau : Zone -> Posix -> Result Int Abbreviation
for__africa__bissau =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -63 || offsetMinutes == -62 then
                Ok Lmt

            else if offsetMinutes == -60 then
                Ok (Offset offsetMinutes)

            else if offsetMinutes == 0 then
                Ok (ShortName "GMT")

            else
                Err offsetMinutes
        )


{-| `Africa/Cairo`
-}
for__africa__cairo : Zone -> Posix -> Result Int Abbreviation
for__africa__cairo =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 125 || offsetMinutes == 126 then
                Ok Lmt

            else if offsetMinutes == 180 then
                Ok (ShortName "EEST")

            else if offsetMinutes == 120 then
                Ok (ShortName "EET")

            else
                Err offsetMinutes
        )


{-| `Africa/Casablanca`
-}
for__africa__casablanca : Zone -> Posix -> Result Int Abbreviation
for__africa__casablanca =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -31 || offsetMinutes == -30 then
                Ok Lmt

            else if offsetMinutes == 0 || offsetMinutes == 60 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `Africa/Ceuta`
-}
for__africa__ceuta : Zone -> Posix -> Result Int Abbreviation
for__africa__ceuta =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -22 || offsetMinutes == -21 then
                Ok Lmt

            else if offsetMinutes == 120 then
                Ok (ShortName "CEST")

            else if offsetMinutes == 60 then
                Ok
                    (if posixSeconds >= 448243200 then
                        ShortName "CET"

                     else
                        ShortName "WEST"
                    )

            else if offsetMinutes == 0 then
                Ok (ShortName "WET")

            else
                Err offsetMinutes
        )


{-| `Africa/El_Aaiun`
-}
for__africa__el_aaiun : Zone -> Posix -> Result Int Abbreviation
for__africa__el_aaiun =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -53 || offsetMinutes == -52 then
                Ok Lmt

            else if offsetMinutes == -60 || offsetMinutes == 0 || offsetMinutes == 60 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `Africa/Johannesburg`
-}
for__africa__johannesburg : Zone -> Posix -> Result Int Abbreviation
for__africa__johannesburg =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 112 then
                Ok Lmt

            else if offsetMinutes == 90 || offsetMinutes == 120 || offsetMinutes == 180 then
                Ok (ShortName "SAST")

            else
                Err offsetMinutes
        )


{-| `Africa/Juba`
-}
for__africa__juba : Zone -> Posix -> Result Int Abbreviation
for__africa__juba =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 126 || offsetMinutes == 127 then
                Ok Lmt

            else if offsetMinutes == 180 then
                Ok
                    (if posixSeconds >= 947930400 then
                        ShortName "EAT"

                     else
                        ShortName "CAST"
                    )

            else if offsetMinutes == 120 then
                Ok (ShortName "CAT")

            else
                Err offsetMinutes
        )


{-| `Africa/Khartoum`
-}
for__africa__khartoum : Zone -> Posix -> Result Int Abbreviation
for__africa__khartoum =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 130 || offsetMinutes == 131 then
                Ok Lmt

            else if offsetMinutes == 180 then
                Ok
                    (if posixSeconds >= 947930400 then
                        ShortName "EAT"

                     else
                        ShortName "CAST"
                    )

            else if offsetMinutes == 120 then
                Ok (ShortName "CAT")

            else
                Err offsetMinutes
        )


{-| `Africa/Lagos`
-}
for__africa__lagos : Zone -> Posix -> Result Int Abbreviation
for__africa__lagos =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 13 || offsetMinutes == 14 then
                Ok Lmt

            else if offsetMinutes == 30 then
                Ok (Offset offsetMinutes)

            else if offsetMinutes == 0 then
                Ok (ShortName "GMT")

            else if offsetMinutes == 60 then
                Ok (ShortName "WAT")

            else
                Err offsetMinutes
        )


{-| `Africa/Maputo`
-}
for__africa__maputo : Zone -> Posix -> Result Int Abbreviation
for__africa__maputo =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 130 || offsetMinutes == 131 then
                Ok Lmt

            else if offsetMinutes == 120 then
                Ok (ShortName "CAT")

            else
                Err offsetMinutes
        )


{-| `Africa/Monrovia`
-}
for__africa__monrovia : Zone -> Posix -> Result Int Abbreviation
for__africa__monrovia =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -44 || offsetMinutes == -43 then
                Ok
                    (if posixSeconds >= -2776979812 then
                        ShortName "MMT"

                     else
                        Lmt
                    )

            else if offsetMinutes == 0 then
                Ok (ShortName "GMT")

            else if offsetMinutes == -45 then
                Ok (ShortName "MMT")

            else
                Err offsetMinutes
        )


{-| `Africa/Nairobi`
-}
for__africa__nairobi : Zone -> Posix -> Result Int Abbreviation
for__africa__nairobi =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 147 || offsetMinutes == 148 then
                Ok Lmt

            else if offsetMinutes == 150 || offsetMinutes == 165 then
                Ok (Offset offsetMinutes)

            else if offsetMinutes == 180 then
                Ok (ShortName "EAT")

            else
                Err offsetMinutes
        )


{-| `Africa/Ndjamena`
-}
for__africa__ndjamena : Zone -> Posix -> Result Int Abbreviation
for__africa__ndjamena =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 61 then
                Ok Lmt

            else if offsetMinutes == 60 then
                Ok
                    (if posixSeconds >= -1830387612 then
                        ShortName "WAT"

                     else
                        Lmt
                    )

            else if offsetMinutes == 120 then
                Ok (ShortName "WAST")

            else
                Err offsetMinutes
        )


{-| `Africa/Sao_Tome`
-}
for__africa__sao_tome : Zone -> Posix -> Result Int Abbreviation
for__africa__sao_tome =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -37 || offsetMinutes == -36 || offsetMinutes == 26 || offsetMinutes == 27 then
                Ok Lmt

            else if offsetMinutes == 0 then
                Ok (ShortName "GMT")

            else if offsetMinutes == 60 then
                Ok (ShortName "WAT")

            else
                Err offsetMinutes
        )


{-| `Africa/Tripoli`
-}
for__africa__tripoli : Zone -> Posix -> Result Int Abbreviation
for__africa__tripoli =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 52 || offsetMinutes == 53 then
                Ok Lmt

            else if offsetMinutes == 120 then
                Ok
                    (if posixSeconds >= 1382659200 then
                        ShortName "EET"

                     else if posixSeconds >= 1364515200 then
                        ShortName "CEST"

                     else if posixSeconds >= 875916000 then
                        ShortName "EET"

                     else if posixSeconds >= 860108400 then
                        ShortName "CEST"

                     else if posixSeconds >= 641775600 then
                        ShortName "EET"

                     else if posixSeconds >= 386463600 then
                        ShortName "CEST"

                     else if posixSeconds >= -347158800 then
                        ShortName "EET"

                     else
                        ShortName "CEST"
                    )

            else if offsetMinutes == 60 then
                Ok (ShortName "CET")

            else
                Err offsetMinutes
        )


{-| `Africa/Tunis`
-}
for__africa__tunis : Zone -> Posix -> Result Int Abbreviation
for__africa__tunis =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 40 || offsetMinutes == 41 then
                Ok Lmt

            else if offsetMinutes == 120 then
                Ok (ShortName "CEST")

            else if offsetMinutes == 60 then
                Ok (ShortName "CET")

            else if offsetMinutes == 9 || offsetMinutes == 10 then
                Ok (ShortName "PMT")

            else
                Err offsetMinutes
        )


{-| `Africa/Windhoek`
-}
for__africa__windhoek : Zone -> Posix -> Result Int Abbreviation
for__africa__windhoek =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 68 || offsetMinutes == 69 then
                Ok Lmt

            else if offsetMinutes == 90 then
                Ok (Offset offsetMinutes)

            else if offsetMinutes == 180 then
                Ok (ShortName "SAST")

            else if offsetMinutes == 120 then
                Ok
                    (if posixSeconds >= 637970400 then
                        ShortName "CAT"

                     else
                        ShortName "SAST"
                    )

            else if offsetMinutes == 60 then
                Ok (ShortName "WAT")

            else
                Err offsetMinutes
        )


{-| `America/Adak`
-}
for__america__adak : Zone -> Posix -> Result Int Abbreviation
for__america__adak =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -707 || offsetMinutes == -706 || offsetMinutes == 733 || offsetMinutes == 734 then
                Ok Lmt

            else if offsetMinutes == -540 then
                Ok (ShortName "HDT")

            else if offsetMinutes == -660 then
                Ok
                    (if posixSeconds >= -86878800 then
                        ShortName "BST"

                     else
                        ShortName "NST"
                    )

            else if offsetMinutes == -600 then
                Ok
                    (if posixSeconds >= 439034400 then
                        ShortName "HST"

                     else if posixSeconds >= 436363200 then
                        ShortName "AHST"

                     else if posixSeconds >= -21466800 then
                        ShortName "BDT"

                     else if posixSeconds >= -769395600 then
                        ShortName "NPT"

                     else
                        ShortName "NWT"
                    )

            else
                Err offsetMinutes
        )


{-| `America/Anchorage`
-}
for__america__anchorage : Zone -> Posix -> Result Int Abbreviation
for__america__anchorage =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -599 || offsetMinutes == 840 || offsetMinutes == 841 then
                Ok Lmt

            else if offsetMinutes == -600 then
                Ok
                    (if posixSeconds >= -86882400 then
                        ShortName "AHST"

                     else if posixSeconds >= -2188951224 then
                        ShortName "AST"

                     else
                        Lmt
                    )

            else if offsetMinutes == -480 then
                Ok (ShortName "AKDT")

            else if offsetMinutes == -540 then
                Ok
                    (if posixSeconds >= 439030800 then
                        ShortName "AKST"

                     else if posixSeconds >= 436359600 then
                        ShortName "YST"

                     else if posixSeconds >= -21470400 then
                        ShortName "AHDT"

                     else if posixSeconds >= -769395600 then
                        ShortName "APT"

                     else
                        ShortName "AWT"
                    )

            else
                Err offsetMinutes
        )


{-| `America/Araguaina`
-}
for__america__araguaina : Zone -> Posix -> Result Int Abbreviation
for__america__araguaina =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -193 || offsetMinutes == -192 then
                Ok Lmt

            else if offsetMinutes == -180 || offsetMinutes == -120 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `America/Argentina/Buenos_Aires`
-}
for__america__argentina__buenos_aires : Zone -> Posix -> Result Int Abbreviation
for__america__argentina__buenos_aires =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -234 || offsetMinutes == -233 then
                Ok Lmt

            else if offsetMinutes == -240 || offsetMinutes == -180 || offsetMinutes == -120 then
                Ok (Offset offsetMinutes)

            else if offsetMinutes == -257 || offsetMinutes == -256 then
                Ok (ShortName "CMT")

            else
                Err offsetMinutes
        )


{-| `America/Argentina/Catamarca`
-}
for__america__argentina__catamarca : Zone -> Posix -> Result Int Abbreviation
for__america__argentina__catamarca =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -264 || offsetMinutes == -263 then
                Ok Lmt

            else if offsetMinutes == -240 || offsetMinutes == -180 || offsetMinutes == -120 then
                Ok (Offset offsetMinutes)

            else if offsetMinutes == -257 || offsetMinutes == -256 then
                Ok (ShortName "CMT")

            else
                Err offsetMinutes
        )


{-| `America/Argentina/Cordoba`
-}
for__america__argentina__cordoba : Zone -> Posix -> Result Int Abbreviation
for__america__argentina__cordoba =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -257 || offsetMinutes == -256 then
                Ok
                    (if posixSeconds >= -2372096592 then
                        ShortName "CMT"

                     else
                        Lmt
                    )

            else if offsetMinutes == -240 || offsetMinutes == -180 || offsetMinutes == -120 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `America/Argentina/Jujuy`
-}
for__america__argentina__jujuy : Zone -> Posix -> Result Int Abbreviation
for__america__argentina__jujuy =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -262 || offsetMinutes == -261 then
                Ok Lmt

            else if offsetMinutes == -240 || offsetMinutes == -180 || offsetMinutes == -120 then
                Ok (Offset offsetMinutes)

            else if offsetMinutes == -257 || offsetMinutes == -256 then
                Ok (ShortName "CMT")

            else
                Err offsetMinutes
        )


{-| `America/Argentina/La_Rioja`
-}
for__america__argentina__la_rioja : Zone -> Posix -> Result Int Abbreviation
for__america__argentina__la_rioja =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -268 || offsetMinutes == -267 then
                Ok Lmt

            else if offsetMinutes == -240 || offsetMinutes == -180 || offsetMinutes == -120 then
                Ok (Offset offsetMinutes)

            else if offsetMinutes == -257 || offsetMinutes == -256 then
                Ok (ShortName "CMT")

            else
                Err offsetMinutes
        )


{-| `America/Argentina/Mendoza`
-}
for__america__argentina__mendoza : Zone -> Posix -> Result Int Abbreviation
for__america__argentina__mendoza =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -276 || offsetMinutes == -275 then
                Ok Lmt

            else if offsetMinutes == -240 || offsetMinutes == -180 || offsetMinutes == -120 then
                Ok (Offset offsetMinutes)

            else if offsetMinutes == -257 || offsetMinutes == -256 then
                Ok (ShortName "CMT")

            else
                Err offsetMinutes
        )


{-| `America/Argentina/Rio_Gallegos`
-}
for__america__argentina__rio_gallegos : Zone -> Posix -> Result Int Abbreviation
for__america__argentina__rio_gallegos =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -277 || offsetMinutes == -276 then
                Ok Lmt

            else if offsetMinutes == -240 || offsetMinutes == -180 || offsetMinutes == -120 then
                Ok (Offset offsetMinutes)

            else if offsetMinutes == -257 || offsetMinutes == -256 then
                Ok (ShortName "CMT")

            else
                Err offsetMinutes
        )


{-| `America/Argentina/Salta`
-}
for__america__argentina__salta : Zone -> Posix -> Result Int Abbreviation
for__america__argentina__salta =
    for__america__argentina__jujuy


{-| `America/Argentina/San_Juan`
-}
for__america__argentina__san_juan : Zone -> Posix -> Result Int Abbreviation
for__america__argentina__san_juan =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -275 || offsetMinutes == -274 then
                Ok Lmt

            else if offsetMinutes == -240 || offsetMinutes == -180 || offsetMinutes == -120 then
                Ok (Offset offsetMinutes)

            else if offsetMinutes == -257 || offsetMinutes == -256 then
                Ok (ShortName "CMT")

            else
                Err offsetMinutes
        )


{-| `America/Argentina/San_Luis`
-}
for__america__argentina__san_luis : Zone -> Posix -> Result Int Abbreviation
for__america__argentina__san_luis =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -266 || offsetMinutes == -265 then
                Ok Lmt

            else if offsetMinutes == -240 || offsetMinutes == -180 || offsetMinutes == -120 then
                Ok (Offset offsetMinutes)

            else if offsetMinutes == -257 || offsetMinutes == -256 then
                Ok (ShortName "CMT")

            else
                Err offsetMinutes
        )


{-| `America/Argentina/Tucuman`
-}
for__america__argentina__tucuman : Zone -> Posix -> Result Int Abbreviation
for__america__argentina__tucuman =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -261 || offsetMinutes == -260 then
                Ok Lmt

            else if offsetMinutes == -240 || offsetMinutes == -180 || offsetMinutes == -120 then
                Ok (Offset offsetMinutes)

            else if offsetMinutes == -257 || offsetMinutes == -256 then
                Ok (ShortName "CMT")

            else
                Err offsetMinutes
        )


{-| `America/Argentina/Ushuaia`
-}
for__america__argentina__ushuaia : Zone -> Posix -> Result Int Abbreviation
for__america__argentina__ushuaia =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -274 || offsetMinutes == -273 then
                Ok Lmt

            else if offsetMinutes == -240 || offsetMinutes == -180 || offsetMinutes == -120 then
                Ok (Offset offsetMinutes)

            else if offsetMinutes == -257 || offsetMinutes == -256 then
                Ok (ShortName "CMT")

            else
                Err offsetMinutes
        )


{-| `America/Asuncion`
-}
for__america__asuncion : Zone -> Posix -> Result Int Abbreviation
for__america__asuncion =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -231 || offsetMinutes == -230 then
                Ok
                    (if posixSeconds >= -2524507760 then
                        ShortName "AMT"

                     else
                        Lmt
                    )

            else if offsetMinutes == -240 || offsetMinutes == -180 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `America/Bahia`
-}
for__america__bahia : Zone -> Posix -> Result Int Abbreviation
for__america__bahia =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -155 || offsetMinutes == -154 then
                Ok Lmt

            else if offsetMinutes == -180 || offsetMinutes == -120 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `America/Bahia_Banderas`
-}
for__america__bahia_banderas : Zone -> Posix -> Result Int Abbreviation
for__america__bahia_banderas =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -421 then
                Ok Lmt

            else if offsetMinutes == -300 then
                Ok (ShortName "CDT")

            else if offsetMinutes == -360 then
                Ok
                    (if posixSeconds >= 1288508400 then
                        ShortName "CST"

                     else if posixSeconds >= 828867600 then
                        ShortName "MDT"

                     else if posixSeconds >= -1191344400 then
                        ShortName "CST"

                     else if posixSeconds >= -1220461200 then
                        ShortName "MDT"

                     else
                        ShortName "CST"
                    )

            else if offsetMinutes == -420 then
                Ok (ShortName "MST")

            else
                Err offsetMinutes
        )


{-| `America/Barbados`
-}
for__america__barbados : Zone -> Posix -> Result Int Abbreviation
for__america__barbados =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -239 || offsetMinutes == -238 then
                Ok Lmt

            else if offsetMinutes == -210 then
                Ok (Offset offsetMinutes)

            else if offsetMinutes == -180 then
                Ok (ShortName "ADT")

            else if offsetMinutes == -240 then
                Ok (ShortName "AST")

            else
                Err offsetMinutes
        )


{-| `America/Belem`
-}
for__america__belem : Zone -> Posix -> Result Int Abbreviation
for__america__belem =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -194 || offsetMinutes == -193 then
                Ok Lmt

            else if offsetMinutes == -180 || offsetMinutes == -120 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `America/Belize`
-}
for__america__belize : Zone -> Posix -> Result Int Abbreviation
for__america__belize =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -353 || offsetMinutes == -352 then
                Ok Lmt

            else if offsetMinutes == -330 then
                Ok (Offset offsetMinutes)

            else if offsetMinutes == -360 then
                Ok (ShortName "CST")

            else if offsetMinutes == -300 then
                Ok
                    (if posixSeconds >= 123919200 then
                        ShortName "CDT"

                     else if posixSeconds >= -769395600 then
                        ShortName "CPT"

                     else
                        ShortName "CWT"
                    )

            else
                Err offsetMinutes
        )


{-| `America/Boa_Vista`
-}
for__america__boa_vista : Zone -> Posix -> Result Int Abbreviation
for__america__boa_vista =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -243 || offsetMinutes == -242 then
                Ok Lmt

            else if offsetMinutes == -240 || offsetMinutes == -180 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `America/Bogota`
-}
for__america__bogota : Zone -> Posix -> Result Int Abbreviation
for__america__bogota =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -297 || offsetMinutes == -296 then
                Ok
                    (if posixSeconds >= -2707671824 then
                        ShortName "BMT"

                     else
                        Lmt
                    )

            else if offsetMinutes == -300 || offsetMinutes == -240 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `America/Boise`
-}
for__america__boise : Zone -> Posix -> Result Int Abbreviation
for__america__boise =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -465 || offsetMinutes == -464 then
                Ok Lmt

            else if offsetMinutes == -360 then
                Ok
                    (if posixSeconds >= -84380400 then
                        ShortName "MDT"

                     else if posixSeconds >= -769395600 then
                        ShortName "MPT"

                     else
                        ShortName "MWT"
                    )

            else if offsetMinutes == -420 then
                Ok
                    (if posixSeconds >= -1471788000 then
                        ShortName "MST"

                     else
                        ShortName "PDT"
                    )

            else if offsetMinutes == -480 then
                Ok (ShortName "PST")

            else
                Err offsetMinutes
        )


{-| `America/Cambridge_Bay`
-}
for__america__cambridge_bay : Zone -> Posix -> Result Int Abbreviation
for__america__cambridge_bay =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 0 then
                Ok Uninhabited

            else if offsetMinutes == -300 then
                Ok
                    (if posixSeconds >= 972802800 then
                        ShortName "EST"

                     else
                        ShortName "CDT"
                    )

            else if offsetMinutes == -420 then
                Ok (ShortName "MST")

            else if offsetMinutes == -360 then
                Ok
                    (if posixSeconds >= 986115600 then
                        ShortName "MDT"

                     else if posixSeconds >= 941356800 then
                        ShortName "CST"

                     else if posixSeconds >= 73472400 then
                        ShortName "MDT"

                     else if posixSeconds >= -769395600 then
                        ShortName "MPT"

                     else
                        ShortName "MWT"
                    )

            else
                Err offsetMinutes
        )


{-| `America/Campo_Grande`
-}
for__america__campo_grande : Zone -> Posix -> Result Int Abbreviation
for__america__campo_grande =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -219 || offsetMinutes == -218 then
                Ok Lmt

            else if offsetMinutes == -240 || offsetMinutes == -180 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `America/Cancun`
-}
for__america__cancun : Zone -> Posix -> Result Int Abbreviation
for__america__cancun =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -348 || offsetMinutes == -347 then
                Ok Lmt

            else if offsetMinutes == -360 then
                Ok (ShortName "CST")

            else if offsetMinutes == -240 then
                Ok (ShortName "EDT")

            else if offsetMinutes == -300 then
                Ok
                    (if posixSeconds >= 1422777600 then
                        ShortName "EST"

                     else if posixSeconds >= 902037600 then
                        ShortName "CDT"

                     else if posixSeconds >= 877849200 then
                        ShortName "EST"

                     else if posixSeconds >= 828864000 then
                        ShortName "CDT"

                     else
                        ShortName "EST"
                    )

            else
                Err offsetMinutes
        )


{-| `America/Caracas`
-}
for__america__caracas : Zone -> Posix -> Result Int Abbreviation
for__america__caracas =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -268 || offsetMinutes == -267 then
                Ok
                    (if posixSeconds >= -2524505536 then
                        ShortName "CMT"

                     else
                        Lmt
                    )

            else if offsetMinutes == -270 || offsetMinutes == -240 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `America/Cayenne`
-}
for__america__cayenne : Zone -> Posix -> Result Int Abbreviation
for__america__cayenne =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -210 || offsetMinutes == -209 then
                Ok Lmt

            else if offsetMinutes == -240 || offsetMinutes == -180 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `America/Chicago`
-}
for__america__chicago : Zone -> Posix -> Result Int Abbreviation
for__america__chicago =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -351 || offsetMinutes == -350 then
                Ok Lmt

            else if offsetMinutes == -300 then
                Ok
                    (if posixSeconds >= -747244800 then
                        ShortName "CDT"

                     else if posixSeconds >= -769395600 then
                        ShortName "CPT"

                     else if posixSeconds >= -880214400 then
                        ShortName "CWT"

                     else if posixSeconds >= -1031500800 then
                        ShortName "CDT"

                     else if posixSeconds >= -1067788800 then
                        ShortName "EST"

                     else
                        ShortName "CDT"
                    )

            else if offsetMinutes == -360 then
                Ok (ShortName "CST")

            else
                Err offsetMinutes
        )


{-| `America/Chihuahua`
-}
for__america__chihuahua : Zone -> Posix -> Result Int Abbreviation
for__america__chihuahua =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -425 || offsetMinutes == -424 then
                Ok Lmt

            else if offsetMinutes == -300 then
                Ok (ShortName "CDT")

            else if offsetMinutes == -360 then
                Ok
                    (if posixSeconds >= 1667116800 then
                        ShortName "CST"

                     else if posixSeconds >= 891766800 then
                        ShortName "MDT"

                     else if posixSeconds >= -1191344400 then
                        ShortName "CST"

                     else if posixSeconds >= -1220461200 then
                        ShortName "MDT"

                     else
                        ShortName "CST"
                    )

            else if offsetMinutes == -420 then
                Ok (ShortName "MST")

            else
                Err offsetMinutes
        )


{-| `America/Ciudad_Juarez`
-}
for__america__ciudad_juarez : Zone -> Posix -> Result Int Abbreviation
for__america__ciudad_juarez =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -426 || offsetMinutes == -425 then
                Ok Lmt

            else if offsetMinutes == -300 then
                Ok (ShortName "CDT")

            else if offsetMinutes == -360 then
                Ok
                    (if posixSeconds >= 1667116800 then
                        ShortName "CST"

                     else if posixSeconds >= 891766800 then
                        ShortName "MDT"

                     else if posixSeconds >= -1191344400 then
                        ShortName "CST"

                     else if posixSeconds >= -1220461200 then
                        ShortName "MDT"

                     else
                        ShortName "CST"
                    )

            else if offsetMinutes == -420 then
                Ok (ShortName "MST")

            else
                Err offsetMinutes
        )


{-| `America/Costa_Rica`
-}
for__america__costa_rica : Zone -> Posix -> Result Int Abbreviation
for__america__costa_rica =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -337 || offsetMinutes == -336 then
                Ok
                    (if posixSeconds >= -2524501427 then
                        ShortName "SJMT"

                     else
                        Lmt
                    )

            else if offsetMinutes == -300 then
                Ok (ShortName "CDT")

            else if offsetMinutes == -360 then
                Ok (ShortName "CST")

            else
                Err offsetMinutes
        )


{-| `America/Cuiaba`
-}
for__america__cuiaba : Zone -> Posix -> Result Int Abbreviation
for__america__cuiaba =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -225 || offsetMinutes == -224 then
                Ok Lmt

            else if offsetMinutes == -240 || offsetMinutes == -180 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `America/Danmarkshavn`
-}
for__america__danmarkshavn : Zone -> Posix -> Result Int Abbreviation
for__america__danmarkshavn =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -75 || offsetMinutes == -74 then
                Ok Lmt

            else if offsetMinutes == -180 || offsetMinutes == -120 then
                Ok (Offset offsetMinutes)

            else if offsetMinutes == 0 then
                Ok (ShortName "GMT")

            else
                Err offsetMinutes
        )


{-| `America/Dawson`
-}
for__america__dawson : Zone -> Posix -> Result Int Abbreviation
for__america__dawson =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -558 || offsetMinutes == -557 then
                Ok Lmt

            else if offsetMinutes == -420 then
                Ok
                    (if posixSeconds >= 1604214000 then
                        ShortName "MST"

                     else if posixSeconds >= 325677600 then
                        ShortName "PDT"

                     else
                        ShortName "YDDT"
                    )

            else if offsetMinutes == -480 then
                Ok
                    (if posixSeconds >= 120646800 then
                        ShortName "PST"

                     else if posixSeconds >= -769395600 then
                        ShortName "YPT"

                     else if posixSeconds >= -880203600 then
                        ShortName "YWT"

                     else
                        ShortName "YDT"
                    )

            else if offsetMinutes == -540 then
                Ok (ShortName "YST")

            else
                Err offsetMinutes
        )


{-| `America/Dawson_Creek`
-}
for__america__dawson_creek : Zone -> Posix -> Result Int Abbreviation
for__america__dawson_creek =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -481 then
                Ok Lmt

            else if offsetMinutes == -480 then
                Ok
                    (if posixSeconds >= -2713881544 then
                        ShortName "PST"

                     else
                        Lmt
                    )

            else if offsetMinutes == -420 then
                Ok
                    (if posixSeconds >= 84013200 then
                        ShortName "MST"

                     else if posixSeconds >= -715788000 then
                        ShortName "PDT"

                     else if posixSeconds >= -769395600 then
                        ShortName "PPT"

                     else if posixSeconds >= -880207200 then
                        ShortName "PWT"

                     else
                        ShortName "PDT"
                    )

            else
                Err offsetMinutes
        )


{-| `America/Denver`
-}
for__america__denver : Zone -> Posix -> Result Int Abbreviation
for__america__denver =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -419 then
                Ok Lmt

            else if offsetMinutes == -420 then
                Ok
                    (if posixSeconds >= -2717643600 then
                        ShortName "MST"

                     else
                        Lmt
                    )

            else if offsetMinutes == -360 then
                Ok
                    (if posixSeconds >= -147884400 then
                        ShortName "MDT"

                     else if posixSeconds >= -769395600 then
                        ShortName "MPT"

                     else if posixSeconds >= -880210800 then
                        ShortName "MWT"

                     else
                        ShortName "MDT"
                    )

            else
                Err offsetMinutes
        )


{-| `America/Detroit`
-}
for__america__detroit : Zone -> Posix -> Result Int Abbreviation
for__america__detroit =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -333 || offsetMinutes == -332 then
                Ok Lmt

            else if offsetMinutes == -360 then
                Ok (ShortName "CST")

            else if offsetMinutes == -300 then
                Ok (ShortName "EST")

            else if offsetMinutes == -240 then
                Ok
                    (if posixSeconds >= -684349200 then
                        ShortName "EDT"

                     else if posixSeconds >= -769395600 then
                        ShortName "EPT"

                     else
                        ShortName "EWT"
                    )

            else
                Err offsetMinutes
        )


{-| `America/Edmonton`
-}
for__america__edmonton : Zone -> Posix -> Result Int Abbreviation
for__america__edmonton =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -454 || offsetMinutes == -453 then
                Ok Lmt

            else if offsetMinutes == -360 then
                Ok
                    (if posixSeconds >= -715791600 then
                        ShortName "MDT"

                     else if posixSeconds >= -769395600 then
                        ShortName "MPT"

                     else if posixSeconds >= -880210800 then
                        ShortName "MWT"

                     else
                        ShortName "MDT"
                    )

            else if offsetMinutes == -420 then
                Ok (ShortName "MST")

            else
                Err offsetMinutes
        )


{-| `America/Eirunepe`
-}
for__america__eirunepe : Zone -> Posix -> Result Int Abbreviation
for__america__eirunepe =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -280 || offsetMinutes == -279 then
                Ok Lmt

            else if offsetMinutes == -300 || offsetMinutes == -240 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `America/El_Salvador`
-}
for__america__el_salvador : Zone -> Posix -> Result Int Abbreviation
for__america__el_salvador =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -357 || offsetMinutes == -356 then
                Ok Lmt

            else if offsetMinutes == -300 then
                Ok (ShortName "CDT")

            else if offsetMinutes == -360 then
                Ok (ShortName "CST")

            else
                Err offsetMinutes
        )


{-| `America/Fort_Nelson`
-}
for__america__fort_nelson : Zone -> Posix -> Result Int Abbreviation
for__america__fort_nelson =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -491 || offsetMinutes == -490 then
                Ok Lmt

            else if offsetMinutes == -420 then
                Ok
                    (if posixSeconds >= 1425808800 then
                        ShortName "MST"

                     else if posixSeconds >= -715788000 then
                        ShortName "PDT"

                     else if posixSeconds >= -769395600 then
                        ShortName "PPT"

                     else if posixSeconds >= -880207200 then
                        ShortName "PWT"

                     else
                        ShortName "PDT"
                    )

            else if offsetMinutes == -480 then
                Ok (ShortName "PST")

            else
                Err offsetMinutes
        )


{-| `America/Fortaleza`
-}
for__america__fortaleza : Zone -> Posix -> Result Int Abbreviation
for__america__fortaleza =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -154 then
                Ok Lmt

            else if offsetMinutes == -180 || offsetMinutes == -120 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `America/Glace_Bay`
-}
for__america__glace_bay : Zone -> Posix -> Result Int Abbreviation
for__america__glace_bay =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -239 then
                Ok Lmt

            else if offsetMinutes == -240 then
                Ok
                    (if posixSeconds >= -2131646412 then
                        ShortName "AST"

                     else
                        Lmt
                    )

            else if offsetMinutes == -180 then
                Ok
                    (if posixSeconds >= -526500000 then
                        ShortName "ADT"

                     else if posixSeconds >= -769395600 then
                        ShortName "APT"

                     else if posixSeconds >= -880221600 then
                        ShortName "AWT"

                     else
                        ShortName "ADT"
                    )

            else
                Err offsetMinutes
        )


{-| `America/Goose_Bay`
-}
for__america__goose_bay : Zone -> Posix -> Result Int Abbreviation
for__america__goose_bay =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -242 || offsetMinutes == -241 then
                Ok Lmt

            else if offsetMinutes == -120 then
                Ok (ShortName "ADDT")

            else if offsetMinutes == -180 then
                Ok (ShortName "ADT")

            else if offsetMinutes == -240 then
                Ok (ShortName "AST")

            else if offsetMinutes == -151 then
                Ok (ShortName "NDT")

            else if offsetMinutes == -150 then
                Ok
                    (if posixSeconds >= -746044200 then
                        ShortName "NDT"

                     else if posixSeconds >= -769395600 then
                        ShortName "NPT"

                     else if posixSeconds >= -872368200 then
                        ShortName "NWT"

                     else
                        ShortName "NDT"
                    )

            else if offsetMinutes == -211 || offsetMinutes == -210 then
                Ok (ShortName "NST")

            else
                Err offsetMinutes
        )


{-| `America/Grand_Turk`
-}
for__america__grand_turk : Zone -> Posix -> Result Int Abbreviation
for__america__grand_turk =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -285 || offsetMinutes == -284 then
                Ok Lmt

            else if offsetMinutes == -240 then
                Ok
                    (if posixSeconds >= 1520751600 then
                        ShortName "EDT"

                     else if posixSeconds >= 1425798000 then
                        ShortName "AST"

                     else
                        ShortName "EDT"
                    )

            else if offsetMinutes == -300 then
                Ok (ShortName "EST")

            else if offsetMinutes == -308 || offsetMinutes == -307 then
                Ok (ShortName "KMT")

            else
                Err offsetMinutes
        )


{-| `America/Guatemala`
-}
for__america__guatemala : Zone -> Posix -> Result Int Abbreviation
for__america__guatemala =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -363 || offsetMinutes == -362 then
                Ok Lmt

            else if offsetMinutes == -300 then
                Ok (ShortName "CDT")

            else if offsetMinutes == -360 then
                Ok (ShortName "CST")

            else
                Err offsetMinutes
        )


{-| `America/Guayaquil`
-}
for__america__guayaquil : Zone -> Posix -> Result Int Abbreviation
for__america__guayaquil =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -320 || offsetMinutes == -319 then
                Ok Lmt

            else if offsetMinutes == -300 || offsetMinutes == -240 then
                Ok (Offset offsetMinutes)

            else if offsetMinutes == -314 then
                Ok (ShortName "QMT")

            else
                Err offsetMinutes
        )


{-| `America/Guyana`
-}
for__america__guyana : Zone -> Posix -> Result Int Abbreviation
for__america__guyana =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -233 || offsetMinutes == -232 then
                Ok Lmt

            else if offsetMinutes == -240 || offsetMinutes == -225 || offsetMinutes == -180 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `America/Halifax`
-}
for__america__halifax : Zone -> Posix -> Result Int Abbreviation
for__america__halifax =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -255 || offsetMinutes == -254 then
                Ok Lmt

            else if offsetMinutes == -180 then
                Ok
                    (if posixSeconds >= -747252000 then
                        ShortName "ADT"

                     else if posixSeconds >= -769395600 then
                        ShortName "APT"

                     else if posixSeconds >= -880221600 then
                        ShortName "AWT"

                     else
                        ShortName "ADT"
                    )

            else if offsetMinutes == -240 then
                Ok (ShortName "AST")

            else
                Err offsetMinutes
        )


{-| `America/Havana`
-}
for__america__havana : Zone -> Posix -> Result Int Abbreviation
for__america__havana =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -330 || offsetMinutes == -329 then
                Ok
                    (if posixSeconds >= -2524501832 then
                        ShortName "HMT"

                     else
                        Lmt
                    )

            else if offsetMinutes == -240 then
                Ok (ShortName "CDT")

            else if offsetMinutes == -300 then
                Ok (ShortName "CST")

            else
                Err offsetMinutes
        )


{-| `America/Hermosillo`
-}
for__america__hermosillo : Zone -> Posix -> Result Int Abbreviation
for__america__hermosillo =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -444 || offsetMinutes == -443 then
                Ok Lmt

            else if offsetMinutes == -360 then
                Ok
                    (if posixSeconds >= 828867600 then
                        ShortName "MDT"

                     else if posixSeconds >= -1191344400 then
                        ShortName "CST"

                     else if posixSeconds >= -1220461200 then
                        ShortName "MDT"

                     else
                        ShortName "CST"
                    )

            else if offsetMinutes == -420 then
                Ok (ShortName "MST")

            else
                Err offsetMinutes
        )


{-| `America/Indiana/Indianapolis`
-}
for__america__indiana__indianapolis : Zone -> Posix -> Result Int Abbreviation
for__america__indiana__indianapolis =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -345 || offsetMinutes == -344 then
                Ok Lmt

            else if offsetMinutes == -300 then
                Ok
                    (if posixSeconds >= -463593600 then
                        ShortName "EST"

                     else if posixSeconds >= -747244800 then
                        ShortName "CDT"

                     else if posixSeconds >= -769395600 then
                        ShortName "CPT"

                     else if posixSeconds >= -880214400 then
                        ShortName "CWT"

                     else
                        ShortName "CDT"
                    )

            else if offsetMinutes == -360 then
                Ok (ShortName "CST")

            else if offsetMinutes == -240 then
                Ok (ShortName "EDT")

            else
                Err offsetMinutes
        )


{-| `America/Indiana/Knox`
-}
for__america__indiana__knox : Zone -> Posix -> Result Int Abbreviation
for__america__indiana__knox =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -347 || offsetMinutes == -346 then
                Ok Lmt

            else if offsetMinutes == -300 then
                Ok
                    (if posixSeconds >= 1143961200 then
                        ShortName "CDT"

                     else if posixSeconds >= 688546800 then
                        ShortName "EST"

                     else if posixSeconds >= -84384000 then
                        ShortName "CDT"

                     else if posixSeconds >= -242236800 then
                        ShortName "EST"

                     else if posixSeconds >= -715795200 then
                        ShortName "CDT"

                     else if posixSeconds >= -769395600 then
                        ShortName "CPT"

                     else if posixSeconds >= -880214400 then
                        ShortName "CWT"

                     else
                        ShortName "CDT"
                    )

            else if offsetMinutes == -360 then
                Ok (ShortName "CST")

            else
                Err offsetMinutes
        )


{-| `America/Indiana/Marengo`
-}
for__america__indiana__marengo : Zone -> Posix -> Result Int Abbreviation
for__america__indiana__marengo =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -346 || offsetMinutes == -345 then
                Ok Lmt

            else if offsetMinutes == -300 then
                Ok
                    (if posixSeconds >= 152089200 then
                        ShortName "EST"

                     else if posixSeconds >= 126687600 then
                        ShortName "CDT"

                     else if posixSeconds >= -273686400 then
                        ShortName "EST"

                     else if posixSeconds >= -589392000 then
                        ShortName "CDT"

                     else if posixSeconds >= -769395600 then
                        ShortName "CPT"

                     else if posixSeconds >= -880214400 then
                        ShortName "CWT"

                     else
                        ShortName "CDT"
                    )

            else if offsetMinutes == -360 then
                Ok (ShortName "CST")

            else if offsetMinutes == -240 then
                Ok (ShortName "EDT")

            else
                Err offsetMinutes
        )


{-| `America/Indiana/Petersburg`
-}
for__america__indiana__petersburg : Zone -> Posix -> Result Int Abbreviation
for__america__indiana__petersburg =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -350 || offsetMinutes == -349 then
                Ok Lmt

            else if offsetMinutes == -300 then
                Ok
                    (if posixSeconds >= 1194159600 then
                        ShortName "EST"

                     else if posixSeconds >= 1143961200 then
                        ShortName "CDT"

                     else if posixSeconds >= 247042800 then
                        ShortName "EST"

                     else if posixSeconds >= -84384000 then
                        ShortName "CDT"

                     else if posixSeconds >= -147888000 then
                        ShortName "EST"

                     else if posixSeconds >= -462996000 then
                        ShortName "CDT"

                     else if posixSeconds >= -769395600 then
                        ShortName "CPT"

                     else if posixSeconds >= -880214400 then
                        ShortName "CWT"

                     else
                        ShortName "CDT"
                    )

            else if offsetMinutes == -360 then
                Ok (ShortName "CST")

            else
                Err offsetMinutes
        )


{-| `America/Indiana/Tell_City`
-}
for__america__indiana__tell_city : Zone -> Posix -> Result Int Abbreviation
for__america__indiana__tell_city =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -348 || offsetMinutes == -347 then
                Ok Lmt

            else if offsetMinutes == -300 then
                Ok
                    (if posixSeconds >= 1143961200 then
                        ShortName "CDT"

                     else if posixSeconds >= -5767200 then
                        ShortName "EST"

                     else if posixSeconds >= -52934400 then
                        ShortName "CDT"

                     else if posixSeconds >= -179337600 then
                        ShortName "EST"

                     else if posixSeconds >= -462996000 then
                        ShortName "CDT"

                     else if posixSeconds >= -769395600 then
                        ShortName "CPT"

                     else if posixSeconds >= -880214400 then
                        ShortName "CWT"

                     else
                        ShortName "CDT"
                    )

            else if offsetMinutes == -360 then
                Ok (ShortName "CST")

            else if offsetMinutes == -240 then
                Ok (ShortName "EDT")

            else
                Err offsetMinutes
        )


{-| `America/Indiana/Vevay`
-}
for__america__indiana__vevay : Zone -> Posix -> Result Int Abbreviation
for__america__indiana__vevay =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -341 || offsetMinutes == -340 then
                Ok Lmt

            else if offsetMinutes == -300 then
                Ok
                    (if posixSeconds >= -495043200 then
                        ShortName "EST"

                     else if posixSeconds >= -769395600 then
                        ShortName "CPT"

                     else if posixSeconds >= -880214400 then
                        ShortName "CWT"

                     else
                        ShortName "CDT"
                    )

            else if offsetMinutes == -360 then
                Ok (ShortName "CST")

            else if offsetMinutes == -240 then
                Ok (ShortName "EDT")

            else
                Err offsetMinutes
        )


{-| `America/Indiana/Vincennes`
-}
for__america__indiana__vincennes : Zone -> Posix -> Result Int Abbreviation
for__america__indiana__vincennes =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -351 || offsetMinutes == -350 then
                Ok Lmt

            else if offsetMinutes == -300 then
                Ok
                    (if posixSeconds >= 1194159600 then
                        ShortName "EST"

                     else if posixSeconds >= 1143961200 then
                        ShortName "CDT"

                     else if posixSeconds >= -179337600 then
                        ShortName "EST"

                     else if posixSeconds >= -747244800 then
                        ShortName "CDT"

                     else if posixSeconds >= -769395600 then
                        ShortName "CPT"

                     else if posixSeconds >= -880214400 then
                        ShortName "CWT"

                     else
                        ShortName "CDT"
                    )

            else if offsetMinutes == -360 then
                Ok (ShortName "CST")

            else if offsetMinutes == -240 then
                Ok (ShortName "EDT")

            else
                Err offsetMinutes
        )


{-| `America/Indiana/Winamac`
-}
for__america__indiana__winamac : Zone -> Posix -> Result Int Abbreviation
for__america__indiana__winamac =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -347 || offsetMinutes == -346 then
                Ok Lmt

            else if offsetMinutes == -300 then
                Ok
                    (if posixSeconds >= 1143961200 then
                        ShortName "CDT"

                     else if posixSeconds >= -273686400 then
                        ShortName "EST"

                     else if posixSeconds >= -747244800 then
                        ShortName "CDT"

                     else if posixSeconds >= -769395600 then
                        ShortName "CPT"

                     else if posixSeconds >= -880214400 then
                        ShortName "CWT"

                     else
                        ShortName "CDT"
                    )

            else if offsetMinutes == -360 then
                Ok (ShortName "CST")

            else if offsetMinutes == -240 then
                Ok (ShortName "EDT")

            else
                Err offsetMinutes
        )


{-| `America/Inuvik`
-}
for__america__inuvik : Zone -> Posix -> Result Int Abbreviation
for__america__inuvik =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 0 then
                Ok Uninhabited

            else if offsetMinutes == -360 then
                Ok (ShortName "MDT")

            else if offsetMinutes == -420 then
                Ok
                    (if posixSeconds >= 309945600 then
                        ShortName "MST"

                     else
                        ShortName "PDT"
                    )

            else if offsetMinutes == -480 then
                Ok (ShortName "PST")

            else
                Err offsetMinutes
        )


{-| `America/Iqaluit`
-}
for__america__iqaluit : Zone -> Posix -> Result Int Abbreviation
for__america__iqaluit =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 0 then
                Ok Uninhabited

            else if offsetMinutes == -360 then
                Ok (ShortName "CST")

            else if offsetMinutes == -300 then
                Ok
                    (if posixSeconds >= 972802800 then
                        ShortName "EST"

                     else if posixSeconds >= 954662400 then
                        ShortName "CDT"

                     else
                        ShortName "EST"
                    )

            else if offsetMinutes == -240 then
                Ok
                    (if posixSeconds >= 73465200 then
                        ShortName "EDT"

                     else if posixSeconds >= -769395600 then
                        ShortName "EPT"

                     else
                        ShortName "EWT"
                    )

            else
                Err offsetMinutes
        )


{-| `America/Jamaica`
-}
for__america__jamaica : Zone -> Posix -> Result Int Abbreviation
for__america__jamaica =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -308 || offsetMinutes == -307 then
                Ok
                    (if posixSeconds >= -2524503170 then
                        ShortName "KMT"

                     else
                        Lmt
                    )

            else if offsetMinutes == -240 then
                Ok (ShortName "EDT")

            else if offsetMinutes == -300 then
                Ok (ShortName "EST")

            else
                Err offsetMinutes
        )


{-| `America/Juneau`
-}
for__america__juneau : Zone -> Posix -> Result Int Abbreviation
for__america__juneau =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -538 || offsetMinutes == -537 || offsetMinutes == 902 || offsetMinutes == 903 then
                Ok Lmt

            else if offsetMinutes == -480 then
                Ok
                    (if posixSeconds >= 452084400 then
                        ShortName "AKDT"

                     else if posixSeconds >= 341402400 then
                        ShortName "PST"

                     else if posixSeconds >= 325677600 then
                        ShortName "YDT"

                     else
                        ShortName "PST"
                    )

            else if offsetMinutes == -420 then
                Ok
                    (if posixSeconds >= -21477600 then
                        ShortName "PDT"

                     else if posixSeconds >= -769395600 then
                        ShortName "PPT"

                     else
                        ShortName "PWT"
                    )

            else if offsetMinutes == -540 then
                Ok
                    (if posixSeconds >= 439030800 then
                        ShortName "AKST"

                     else
                        ShortName "YST"
                    )

            else
                Err offsetMinutes
        )


{-| `America/Kentucky/Louisville`
-}
for__america__kentucky__louisville : Zone -> Posix -> Result Int Abbreviation
for__america__kentucky__louisville =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -344 || offsetMinutes == -343 then
                Ok Lmt

            else if offsetMinutes == -300 then
                Ok
                    (if posixSeconds >= 152089200 then
                        ShortName "EST"

                     else if posixSeconds >= 126687600 then
                        ShortName "CDT"

                     else if posixSeconds >= -266432400 then
                        ShortName "EST"

                     else if posixSeconds >= -747251940 then
                        ShortName "CDT"

                     else if posixSeconds >= -769395600 then
                        ShortName "CPT"

                     else if posixSeconds >= -880214400 then
                        ShortName "CWT"

                     else
                        ShortName "CDT"
                    )

            else if offsetMinutes == -360 then
                Ok (ShortName "CST")

            else if offsetMinutes == -240 then
                Ok (ShortName "EDT")

            else
                Err offsetMinutes
        )


{-| `America/Kentucky/Monticello`
-}
for__america__kentucky__monticello : Zone -> Posix -> Result Int Abbreviation
for__america__kentucky__monticello =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -340 || offsetMinutes == -339 then
                Ok Lmt

            else if offsetMinutes == -300 then
                Ok
                    (if posixSeconds >= 972802800 then
                        ShortName "EST"

                     else if posixSeconds >= -52934400 then
                        ShortName "CDT"

                     else if posixSeconds >= -769395600 then
                        ShortName "CPT"

                     else if posixSeconds >= -880214400 then
                        ShortName "CWT"

                     else
                        ShortName "CDT"
                    )

            else if offsetMinutes == -360 then
                Ok (ShortName "CST")

            else if offsetMinutes == -240 then
                Ok (ShortName "EDT")

            else
                Err offsetMinutes
        )


{-| `America/La_Paz`
-}
for__america__la_paz : Zone -> Posix -> Result Int Abbreviation
for__america__la_paz =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -273 || offsetMinutes == -272 then
                Ok
                    (if posixSeconds >= -2524505244 then
                        ShortName "CMT"

                     else
                        Lmt
                    )

            else if offsetMinutes == -240 then
                Ok (Offset offsetMinutes)

            else if offsetMinutes == -213 || offsetMinutes == -212 then
                Ok (ShortName "BST")

            else
                Err offsetMinutes
        )


{-| `America/Lima`
-}
for__america__lima : Zone -> Posix -> Result Int Abbreviation
for__america__lima =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -309 || offsetMinutes == -308 then
                Ok Lmt

            else if offsetMinutes == -300 || offsetMinutes == -240 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `America/Los_Angeles`
-}
for__america__los_angeles : Zone -> Posix -> Result Int Abbreviation
for__america__los_angeles =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -473 || offsetMinutes == -472 then
                Ok Lmt

            else if offsetMinutes == -420 then
                Ok
                    (if posixSeconds >= -687967140 then
                        ShortName "PDT"

                     else if posixSeconds >= -769395600 then
                        ShortName "PPT"

                     else if posixSeconds >= -880207200 then
                        ShortName "PWT"

                     else
                        ShortName "PDT"
                    )

            else if offsetMinutes == -480 then
                Ok (ShortName "PST")

            else
                Err offsetMinutes
        )


{-| `America/Maceio`
-}
for__america__maceio : Zone -> Posix -> Result Int Abbreviation
for__america__maceio =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -143 || offsetMinutes == -142 then
                Ok Lmt

            else if offsetMinutes == -180 || offsetMinutes == -120 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `America/Managua`
-}
for__america__managua : Zone -> Posix -> Result Int Abbreviation
for__america__managua =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -346 || offsetMinutes == -345 then
                Ok
                    (if posixSeconds >= -2524500892 then
                        ShortName "MMT"

                     else
                        Lmt
                    )

            else if offsetMinutes == -360 then
                Ok (ShortName "CST")

            else if offsetMinutes == -300 then
                Ok
                    (if posixSeconds >= 1113112800 then
                        ShortName "CDT"

                     else if posixSeconds >= 694260000 then
                        ShortName "EST"

                     else if posixSeconds >= 290584800 then
                        ShortName "CDT"

                     else
                        ShortName "EST"
                    )

            else
                Err offsetMinutes
        )


{-| `America/Manaus`
-}
for__america__manaus : Zone -> Posix -> Result Int Abbreviation
for__america__manaus =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -241 then
                Ok Lmt

            else if offsetMinutes == -240 then
                Ok
                    (if posixSeconds >= -1767211196 then
                        Offset offsetMinutes

                     else
                        Lmt
                    )

            else if offsetMinutes == -180 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `America/Martinique`
-}
for__america__martinique : Zone -> Posix -> Result Int Abbreviation
for__america__martinique =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -245 || offsetMinutes == -244 then
                Ok
                    (if posixSeconds >= -2524506940 then
                        ShortName "FFMT"

                     else
                        Lmt
                    )

            else if offsetMinutes == -180 then
                Ok (ShortName "ADT")

            else if offsetMinutes == -240 then
                Ok (ShortName "AST")

            else
                Err offsetMinutes
        )


{-| `America/Matamoros`
-}
for__america__matamoros : Zone -> Posix -> Result Int Abbreviation
for__america__matamoros =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -390 then
                Ok Lmt

            else if offsetMinutes == -300 then
                Ok (ShortName "CDT")

            else if offsetMinutes == -360 then
                Ok (ShortName "CST")

            else
                Err offsetMinutes
        )


{-| `America/Mazatlan`
-}
for__america__mazatlan : Zone -> Posix -> Result Int Abbreviation
for__america__mazatlan =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -426 || offsetMinutes == -425 then
                Ok Lmt

            else if offsetMinutes == -360 then
                Ok
                    (if posixSeconds >= 828867600 then
                        ShortName "MDT"

                     else if posixSeconds >= -1191344400 then
                        ShortName "CST"

                     else if posixSeconds >= -1220461200 then
                        ShortName "MDT"

                     else
                        ShortName "CST"
                    )

            else if offsetMinutes == -420 then
                Ok (ShortName "MST")

            else
                Err offsetMinutes
        )


{-| `America/Menominee`
-}
for__america__menominee : Zone -> Posix -> Result Int Abbreviation
for__america__menominee =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -351 || offsetMinutes == -350 then
                Ok Lmt

            else if offsetMinutes == -300 then
                Ok
                    (if posixSeconds >= 104914800 then
                        ShortName "CDT"

                     else if posixSeconds >= -21484800 then
                        ShortName "EST"

                     else if posixSeconds >= -747244800 then
                        ShortName "CDT"

                     else if posixSeconds >= -769395600 then
                        ShortName "CPT"

                     else if posixSeconds >= -880214400 then
                        ShortName "CWT"

                     else
                        ShortName "CDT"
                    )

            else if offsetMinutes == -360 then
                Ok (ShortName "CST")

            else
                Err offsetMinutes
        )


{-| `America/Merida`
-}
for__america__merida : Zone -> Posix -> Result Int Abbreviation
for__america__merida =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -359 || offsetMinutes == -358 then
                Ok Lmt

            else if offsetMinutes == -360 then
                Ok (ShortName "CST")

            else if offsetMinutes == -300 then
                Ok
                    (if posixSeconds >= 828864000 then
                        ShortName "CDT"

                     else
                        ShortName "EST"
                    )

            else
                Err offsetMinutes
        )


{-| `America/Metlakatla`
-}
for__america__metlakatla : Zone -> Posix -> Result Int Abbreviation
for__america__metlakatla =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -527 || offsetMinutes == -526 || offsetMinutes == 913 || offsetMinutes == 914 then
                Ok Lmt

            else if offsetMinutes == -540 then
                Ok (ShortName "AKST")

            else if offsetMinutes == -480 then
                Ok
                    (if posixSeconds >= 1541325600 then
                        ShortName "PST"

                     else if posixSeconds >= 1457866800 then
                        ShortName "AKDT"

                     else
                        ShortName "PST"
                    )

            else if offsetMinutes == -420 then
                Ok
                    (if posixSeconds >= -21477600 then
                        ShortName "PDT"

                     else if posixSeconds >= -769395600 then
                        ShortName "PPT"

                     else
                        ShortName "PWT"
                    )

            else
                Err offsetMinutes
        )


{-| `America/Mexico_City`
-}
for__america__mexico_city : Zone -> Posix -> Result Int Abbreviation
for__america__mexico_city =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -397 || offsetMinutes == -396 then
                Ok Lmt

            else if offsetMinutes == -300 then
                Ok
                    (if posixSeconds >= -627501600 then
                        ShortName "CDT"

                     else if posixSeconds >= -821901600 then
                        ShortName "CWT"

                     else
                        ShortName "CDT"
                    )

            else if offsetMinutes == -360 then
                Ok
                    (if posixSeconds >= -1191344400 then
                        ShortName "CST"

                     else if posixSeconds >= -1220461200 then
                        ShortName "MDT"

                     else
                        ShortName "CST"
                    )

            else if offsetMinutes == -420 then
                Ok (ShortName "MST")

            else
                Err offsetMinutes
        )


{-| `America/Miquelon`
-}
for__america__miquelon : Zone -> Posix -> Result Int Abbreviation
for__america__miquelon =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -225 || offsetMinutes == -224 then
                Ok Lmt

            else if offsetMinutes == -180 || offsetMinutes == -120 then
                Ok (Offset offsetMinutes)

            else if offsetMinutes == -240 then
                Ok (ShortName "AST")

            else
                Err offsetMinutes
        )


{-| `America/Moncton`
-}
for__america__moncton : Zone -> Posix -> Result Int Abbreviation
for__america__moncton =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -260 || offsetMinutes == -259 then
                Ok Lmt

            else if offsetMinutes == -180 then
                Ok
                    (if posixSeconds >= -747252000 then
                        ShortName "ADT"

                     else if posixSeconds >= -769395600 then
                        ShortName "APT"

                     else if posixSeconds >= -880221600 then
                        ShortName "AWT"

                     else
                        ShortName "ADT"
                    )

            else if offsetMinutes == -240 then
                Ok (ShortName "AST")

            else if offsetMinutes == -300 then
                Ok (ShortName "EST")

            else
                Err offsetMinutes
        )


{-| `America/Monterrey`
-}
for__america__monterrey : Zone -> Posix -> Result Int Abbreviation
for__america__monterrey =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -402 || offsetMinutes == -401 then
                Ok Lmt

            else if offsetMinutes == -300 then
                Ok (ShortName "CDT")

            else if offsetMinutes == -360 then
                Ok
                    (if posixSeconds >= -1191344400 then
                        ShortName "CST"

                     else if posixSeconds >= -1220461200 then
                        ShortName "MDT"

                     else
                        ShortName "CST"
                    )

            else if offsetMinutes == -420 then
                Ok (ShortName "MST")

            else
                Err offsetMinutes
        )


{-| `America/Montevideo`
-}
for__america__montevideo : Zone -> Posix -> Result Int Abbreviation
for__america__montevideo =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -225 || offsetMinutes == -224 then
                Ok
                    (if posixSeconds >= -1942690509 then
                        ShortName "MMT"

                     else
                        Lmt
                    )

            else if offsetMinutes == -240 || offsetMinutes == -210 || offsetMinutes == -180 || offsetMinutes == -150 || offsetMinutes == -120 || offsetMinutes == -90 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `America/New_York`
-}
for__america__new_york : Zone -> Posix -> Result Int Abbreviation
for__america__new_york =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -297 || offsetMinutes == -296 then
                Ok Lmt

            else if offsetMinutes == -240 then
                Ok
                    (if posixSeconds >= -747248400 then
                        ShortName "EDT"

                     else if posixSeconds >= -769395600 then
                        ShortName "EPT"

                     else if posixSeconds >= -880218000 then
                        ShortName "EWT"

                     else
                        ShortName "EDT"
                    )

            else if offsetMinutes == -300 then
                Ok (ShortName "EST")

            else
                Err offsetMinutes
        )


{-| `America/Nome`
-}
for__america__nome : Zone -> Posix -> Result Int Abbreviation
for__america__nome =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -662 || offsetMinutes == -661 || offsetMinutes == 778 || offsetMinutes == 779 then
                Ok Lmt

            else if offsetMinutes == -480 then
                Ok (ShortName "AKDT")

            else if offsetMinutes == -660 then
                Ok
                    (if posixSeconds >= -86878800 then
                        ShortName "BST"

                     else
                        ShortName "NST"
                    )

            else if offsetMinutes == -600 then
                Ok
                    (if posixSeconds >= -21466800 then
                        ShortName "BDT"

                     else if posixSeconds >= -769395600 then
                        ShortName "NPT"

                     else
                        ShortName "NWT"
                    )

            else if offsetMinutes == -540 then
                Ok
                    (if posixSeconds >= 439030800 then
                        ShortName "AKST"

                     else
                        ShortName "YST"
                    )

            else
                Err offsetMinutes
        )


{-| `America/Noronha`
-}
for__america__noronha : Zone -> Posix -> Result Int Abbreviation
for__america__noronha =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -130 || offsetMinutes == -129 then
                Ok Lmt

            else if offsetMinutes == -120 || offsetMinutes == -60 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `America/North_Dakota/Beulah`
-}
for__america__north_dakota__beulah : Zone -> Posix -> Result Int Abbreviation
for__america__north_dakota__beulah =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -408 || offsetMinutes == -407 then
                Ok Lmt

            else if offsetMinutes == -360 then
                Ok
                    (if posixSeconds >= 1289116800 then
                        ShortName "CST"

                     else if posixSeconds >= -84380400 then
                        ShortName "MDT"

                     else if posixSeconds >= -769395600 then
                        ShortName "MPT"

                     else if posixSeconds >= -880210800 then
                        ShortName "MWT"

                     else
                        ShortName "MDT"
                    )

            else if offsetMinutes == -420 then
                Ok (ShortName "MST")

            else
                Err offsetMinutes
        )


{-| `America/North_Dakota/Center`
-}
for__america__north_dakota__center : Zone -> Posix -> Result Int Abbreviation
for__america__north_dakota__center =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -406 || offsetMinutes == -405 then
                Ok Lmt

            else if offsetMinutes == -300 then
                Ok (ShortName "CDT")

            else if offsetMinutes == -360 then
                Ok
                    (if posixSeconds >= 720000000 then
                        ShortName "CST"

                     else if posixSeconds >= -84380400 then
                        ShortName "MDT"

                     else if posixSeconds >= -769395600 then
                        ShortName "MPT"

                     else if posixSeconds >= -880210800 then
                        ShortName "MWT"

                     else
                        ShortName "MDT"
                    )

            else if offsetMinutes == -420 then
                Ok (ShortName "MST")

            else
                Err offsetMinutes
        )


{-| `America/North_Dakota/New_Salem`
-}
for__america__north_dakota__new_salem : Zone -> Posix -> Result Int Abbreviation
for__america__north_dakota__new_salem =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -406 || offsetMinutes == -405 then
                Ok Lmt

            else if offsetMinutes == -300 then
                Ok (ShortName "CDT")

            else if offsetMinutes == -360 then
                Ok
                    (if posixSeconds >= 1067155200 then
                        ShortName "CST"

                     else if posixSeconds >= -84380400 then
                        ShortName "MDT"

                     else if posixSeconds >= -769395600 then
                        ShortName "MPT"

                     else if posixSeconds >= -880210800 then
                        ShortName "MWT"

                     else
                        ShortName "MDT"
                    )

            else if offsetMinutes == -420 then
                Ok (ShortName "MST")

            else
                Err offsetMinutes
        )


{-| `America/Nuuk`
-}
for__america__nuuk : Zone -> Posix -> Result Int Abbreviation
for__america__nuuk =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -207 || offsetMinutes == -206 then
                Ok Lmt

            else if offsetMinutes == -180 || offsetMinutes == -120 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `America/Ojinaga`
-}
for__america__ojinaga : Zone -> Posix -> Result Int Abbreviation
for__america__ojinaga =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -418 || offsetMinutes == -417 then
                Ok Lmt

            else if offsetMinutes == -300 then
                Ok (ShortName "CDT")

            else if offsetMinutes == -360 then
                Ok
                    (if posixSeconds >= 1667116800 then
                        ShortName "CST"

                     else if posixSeconds >= 891766800 then
                        ShortName "MDT"

                     else if posixSeconds >= -1191344400 then
                        ShortName "CST"

                     else if posixSeconds >= -1220461200 then
                        ShortName "MDT"

                     else
                        ShortName "CST"
                    )

            else if offsetMinutes == -420 then
                Ok (ShortName "MST")

            else
                Err offsetMinutes
        )


{-| `America/Panama`
-}
for__america__panama : Zone -> Posix -> Result Int Abbreviation
for__america__panama =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -318 then
                Ok Lmt

            else if offsetMinutes == -319 then
                Ok
                    (if posixSeconds >= -2524502512 then
                        ShortName "CMT"

                     else
                        Lmt
                    )

            else if offsetMinutes == -320 then
                Ok (ShortName "CMT")

            else if offsetMinutes == -300 then
                Ok (ShortName "EST")

            else
                Err offsetMinutes
        )


{-| `America/Paramaribo`
-}
for__america__paramaribo : Zone -> Posix -> Result Int Abbreviation
for__america__paramaribo =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -221 || offsetMinutes == -220 then
                Ok
                    (if posixSeconds >= -1861906760 then
                        ShortName "PMT"

                     else
                        Lmt
                    )

            else if offsetMinutes == -210 || offsetMinutes == -180 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `America/Phoenix`
-}
for__america__phoenix : Zone -> Posix -> Result Int Abbreviation
for__america__phoenix =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -449 || offsetMinutes == -448 then
                Ok Lmt

            else if offsetMinutes == -360 then
                Ok
                    (if posixSeconds >= -84380400 then
                        ShortName "MDT"

                     else if posixSeconds >= -880210800 then
                        ShortName "MWT"

                     else
                        ShortName "MDT"
                    )

            else if offsetMinutes == -420 then
                Ok (ShortName "MST")

            else
                Err offsetMinutes
        )


{-| `America/Port-au-Prince`
-}
for__america__port_au_prince : Zone -> Posix -> Result Int Abbreviation
for__america__port_au_prince =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -290 then
                Ok Lmt

            else if offsetMinutes == -289 then
                Ok
                    (if posixSeconds >= -2524504240 then
                        ShortName "PPMT"

                     else
                        Lmt
                    )

            else if offsetMinutes == -240 then
                Ok (ShortName "EDT")

            else if offsetMinutes == -300 then
                Ok (ShortName "EST")

            else
                Err offsetMinutes
        )


{-| `America/Porto_Velho`
-}
for__america__porto_velho : Zone -> Posix -> Result Int Abbreviation
for__america__porto_velho =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -256 || offsetMinutes == -255 then
                Ok Lmt

            else if offsetMinutes == -240 || offsetMinutes == -180 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `America/Puerto_Rico`
-}
for__america__puerto_rico : Zone -> Posix -> Result Int Abbreviation
for__america__puerto_rico =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -265 || offsetMinutes == -264 then
                Ok Lmt

            else if offsetMinutes == -240 then
                Ok (ShortName "AST")

            else if offsetMinutes == -180 then
                Ok
                    (if posixSeconds >= -769395600 then
                        ShortName "APT"

                     else
                        ShortName "AWT"
                    )

            else
                Err offsetMinutes
        )


{-| `America/Punta_Arenas`
-}
for__america__punta_arenas : Zone -> Posix -> Result Int Abbreviation
for__america__punta_arenas =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -284 then
                Ok Lmt

            else if offsetMinutes == -283 then
                Ok
                    (if posixSeconds >= -2524504580 then
                        ShortName "SMT"

                     else
                        Lmt
                    )

            else if offsetMinutes == -300 || offsetMinutes == -240 || offsetMinutes == -180 then
                Ok (Offset offsetMinutes)

            else if offsetMinutes == -282 then
                Ok (ShortName "SMT")

            else
                Err offsetMinutes
        )


{-| `America/Rankin_Inlet`
-}
for__america__rankin_inlet : Zone -> Posix -> Result Int Abbreviation
for__america__rankin_inlet =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 0 then
                Ok Uninhabited

            else if offsetMinutes == -300 then
                Ok
                    (if posixSeconds >= 986112000 then
                        ShortName "CDT"

                     else if posixSeconds >= 972802800 then
                        ShortName "EST"

                     else
                        ShortName "CDT"
                    )

            else if offsetMinutes == -360 then
                Ok (ShortName "CST")

            else
                Err offsetMinutes
        )


{-| `America/Recife`
-}
for__america__recife : Zone -> Posix -> Result Int Abbreviation
for__america__recife =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -140 || offsetMinutes == -139 then
                Ok Lmt

            else if offsetMinutes == -180 || offsetMinutes == -120 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `America/Regina`
-}
for__america__regina : Zone -> Posix -> Result Int Abbreviation
for__america__regina =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -419 || offsetMinutes == -418 then
                Ok Lmt

            else if offsetMinutes == -360 then
                Ok
                    (if posixSeconds >= -305737200 then
                        ShortName "CST"

                     else if posixSeconds >= -748450800 then
                        ShortName "MDT"

                     else if posixSeconds >= -769395600 then
                        ShortName "MPT"

                     else if posixSeconds >= -880210800 then
                        ShortName "MWT"

                     else
                        ShortName "MDT"
                    )

            else if offsetMinutes == -420 then
                Ok (ShortName "MST")

            else
                Err offsetMinutes
        )


{-| `America/Resolute`
-}
for__america__resolute : Zone -> Posix -> Result Int Abbreviation
for__america__resolute =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 0 then
                Ok Uninhabited

            else if offsetMinutes == -300 then
                Ok
                    (if posixSeconds >= 1173600000 then
                        ShortName "CDT"

                     else if posixSeconds >= 1162105200 then
                        ShortName "EST"

                     else if posixSeconds >= 986112000 then
                        ShortName "CDT"

                     else if posixSeconds >= 972802800 then
                        ShortName "EST"

                     else
                        ShortName "CDT"
                    )

            else if offsetMinutes == -360 then
                Ok (ShortName "CST")

            else
                Err offsetMinutes
        )


{-| `America/Rio_Branco`
-}
for__america__rio_branco : Zone -> Posix -> Result Int Abbreviation
for__america__rio_branco =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -272 || offsetMinutes == -271 then
                Ok Lmt

            else if offsetMinutes == -300 || offsetMinutes == -240 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `America/Santarem`
-}
for__america__santarem : Zone -> Posix -> Result Int Abbreviation
for__america__santarem =
    for__america__campo_grande


{-| `America/Santiago`
-}
for__america__santiago : Zone -> Posix -> Result Int Abbreviation
for__america__santiago =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -283 || offsetMinutes == -282 then
                Ok
                    (if posixSeconds >= -2524504635 then
                        ShortName "SMT"

                     else
                        Lmt
                    )

            else if offsetMinutes == -300 || offsetMinutes == -240 || offsetMinutes == -180 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `America/Santo_Domingo`
-}
for__america__santo_domingo : Zone -> Posix -> Result Int Abbreviation
for__america__santo_domingo =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -279 then
                Ok Lmt

            else if offsetMinutes == -280 then
                Ok
                    (if posixSeconds >= -2524504824 then
                        ShortName "SDMT"

                     else
                        Lmt
                    )

            else if offsetMinutes == -270 then
                Ok (Offset offsetMinutes)

            else if offsetMinutes == -240 then
                Ok
                    (if posixSeconds >= 152082000 then
                        ShortName "AST"

                     else
                        ShortName "EDT"
                    )

            else if offsetMinutes == -300 then
                Ok (ShortName "EST")

            else
                Err offsetMinutes
        )


{-| `America/Sao_Paulo`
-}
for__america__sao_paulo : Zone -> Posix -> Result Int Abbreviation
for__america__sao_paulo =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -187 || offsetMinutes == -186 then
                Ok Lmt

            else if offsetMinutes == -180 || offsetMinutes == -120 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `America/Scoresbysund`
-}
for__america__scoresbysund : Zone -> Posix -> Result Int Abbreviation
for__america__scoresbysund =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -88 || offsetMinutes == -87 then
                Ok Lmt

            else if offsetMinutes == -120 || offsetMinutes == -60 || offsetMinutes == 0 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `America/Sitka`
-}
for__america__sitka : Zone -> Posix -> Result Int Abbreviation
for__america__sitka =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -542 || offsetMinutes == -541 || offsetMinutes == 898 || offsetMinutes == 899 then
                Ok Lmt

            else if offsetMinutes == -480 then
                Ok
                    (if posixSeconds >= 452084400 then
                        ShortName "AKDT"

                     else
                        ShortName "PST"
                    )

            else if offsetMinutes == -420 then
                Ok
                    (if posixSeconds >= -21477600 then
                        ShortName "PDT"

                     else if posixSeconds >= -769395600 then
                        ShortName "PPT"

                     else
                        ShortName "PWT"
                    )

            else if offsetMinutes == -540 then
                Ok
                    (if posixSeconds >= 439030800 then
                        ShortName "AKST"

                     else
                        ShortName "YST"
                    )

            else
                Err offsetMinutes
        )


{-| `America/St_Johns`
-}
for__america__st_johns : Zone -> Posix -> Result Int Abbreviation
for__america__st_johns =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -211 || offsetMinutes == -210 then
                Ok
                    (if posixSeconds >= -2713897748 then
                        ShortName "NST"

                     else
                        Lmt
                    )

            else if offsetMinutes == -90 then
                Ok (ShortName "NDDT")

            else if offsetMinutes == -151 then
                Ok (ShortName "NDT")

            else if offsetMinutes == -150 then
                Ok
                    (if posixSeconds >= -746044200 then
                        ShortName "NDT"

                     else if posixSeconds >= -769395600 then
                        ShortName "NPT"

                     else if posixSeconds >= -872368200 then
                        ShortName "NWT"

                     else
                        ShortName "NDT"
                    )

            else
                Err offsetMinutes
        )


{-| `America/Swift_Current`
-}
for__america__swift_current : Zone -> Posix -> Result Int Abbreviation
for__america__swift_current =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -432 || offsetMinutes == -431 then
                Ok Lmt

            else if offsetMinutes == -360 then
                Ok
                    (if posixSeconds >= 73472400 then
                        ShortName "CST"

                     else if posixSeconds >= -747241200 then
                        ShortName "MDT"

                     else if posixSeconds >= -769395600 then
                        ShortName "MPT"

                     else if posixSeconds >= -880210800 then
                        ShortName "MWT"

                     else
                        ShortName "MDT"
                    )

            else if offsetMinutes == -420 then
                Ok (ShortName "MST")

            else
                Err offsetMinutes
        )


{-| `America/Tegucigalpa`
-}
for__america__tegucigalpa : Zone -> Posix -> Result Int Abbreviation
for__america__tegucigalpa =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -349 || offsetMinutes == -348 then
                Ok Lmt

            else if offsetMinutes == -300 then
                Ok (ShortName "CDT")

            else if offsetMinutes == -360 then
                Ok (ShortName "CST")

            else
                Err offsetMinutes
        )


{-| `America/Thule`
-}
for__america__thule : Zone -> Posix -> Result Int Abbreviation
for__america__thule =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -276 || offsetMinutes == -275 then
                Ok Lmt

            else if offsetMinutes == -180 then
                Ok (ShortName "ADT")

            else if offsetMinutes == -240 then
                Ok (ShortName "AST")

            else
                Err offsetMinutes
        )


{-| `America/Tijuana`
-}
for__america__tijuana : Zone -> Posix -> Result Int Abbreviation
for__america__tijuana =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -469 || offsetMinutes == -468 then
                Ok Lmt

            else if offsetMinutes == -420 then
                Ok
                    (if posixSeconds >= -686073600 then
                        ShortName "PDT"

                     else if posixSeconds >= -769395600 then
                        ShortName "PPT"

                     else if posixSeconds >= -873820800 then
                        ShortName "PWT"

                     else if posixSeconds >= -1222963200 then
                        ShortName "PDT"

                     else
                        ShortName "MST"
                    )

            else if offsetMinutes == -480 then
                Ok (ShortName "PST")

            else
                Err offsetMinutes
        )


{-| `America/Toronto`
-}
for__america__toronto : Zone -> Posix -> Result Int Abbreviation
for__america__toronto =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -318 || offsetMinutes == -317 then
                Ok Lmt

            else if offsetMinutes == -240 then
                Ok
                    (if posixSeconds >= -747248400 then
                        ShortName "EDT"

                     else if posixSeconds >= -769395600 then
                        ShortName "EPT"

                     else if posixSeconds >= -880218000 then
                        ShortName "EWT"

                     else
                        ShortName "EDT"
                    )

            else if offsetMinutes == -300 then
                Ok (ShortName "EST")

            else
                Err offsetMinutes
        )


{-| `America/Vancouver`
-}
for__america__vancouver : Zone -> Posix -> Result Int Abbreviation
for__america__vancouver =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -493 || offsetMinutes == -492 then
                Ok Lmt

            else if offsetMinutes == -420 then
                Ok
                    (if posixSeconds >= -747237600 then
                        ShortName "PDT"

                     else if posixSeconds >= -769395600 then
                        ShortName "PPT"

                     else if posixSeconds >= -880207200 then
                        ShortName "PWT"

                     else
                        ShortName "PDT"
                    )

            else if offsetMinutes == -480 then
                Ok (ShortName "PST")

            else
                Err offsetMinutes
        )


{-| `America/Whitehorse`
-}
for__america__whitehorse : Zone -> Posix -> Result Int Abbreviation
for__america__whitehorse =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -541 then
                Ok Lmt

            else if offsetMinutes == -540 then
                Ok
                    (if posixSeconds >= -2188997988 then
                        ShortName "YST"

                     else
                        Lmt
                    )

            else if offsetMinutes == -420 then
                Ok
                    (if posixSeconds >= 1604214000 then
                        ShortName "MST"

                     else if posixSeconds >= 325677600 then
                        ShortName "PDT"

                     else
                        ShortName "YDDT"
                    )

            else if offsetMinutes == -480 then
                Ok
                    (if posixSeconds >= -121273200 then
                        ShortName "PST"

                     else if posixSeconds >= -769395600 then
                        ShortName "YPT"

                     else if posixSeconds >= -880203600 then
                        ShortName "YWT"

                     else
                        ShortName "YDT"
                    )

            else
                Err offsetMinutes
        )


{-| `America/Winnipeg`
-}
for__america__winnipeg : Zone -> Posix -> Result Int Abbreviation
for__america__winnipeg =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -389 || offsetMinutes == -388 then
                Ok Lmt

            else if offsetMinutes == -300 then
                Ok
                    (if posixSeconds >= -746035200 then
                        ShortName "CDT"

                     else if posixSeconds >= -769395600 then
                        ShortName "CPT"

                     else if posixSeconds >= -880214400 then
                        ShortName "CWT"

                     else
                        ShortName "CDT"
                    )

            else if offsetMinutes == -360 then
                Ok (ShortName "CST")

            else
                Err offsetMinutes
        )


{-| `America/Yakutat`
-}
for__america__yakutat : Zone -> Posix -> Result Int Abbreviation
for__america__yakutat =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -559 || offsetMinutes == -558 || offsetMinutes == 881 || offsetMinutes == 882 then
                Ok Lmt

            else if offsetMinutes == -540 then
                Ok
                    (if posixSeconds >= 439030800 then
                        ShortName "AKST"

                     else
                        ShortName "YST"
                    )

            else if offsetMinutes == -480 then
                Ok
                    (if posixSeconds >= 452084400 then
                        ShortName "AKDT"

                     else if posixSeconds >= -21474000 then
                        ShortName "YDT"

                     else if posixSeconds >= -769395600 then
                        ShortName "YPT"

                     else
                        ShortName "YWT"
                    )

            else
                Err offsetMinutes
        )


{-| `Antarctica/Casey`
-}
for__antarctica__casey : Zone -> Posix -> Result Int Abbreviation
for__antarctica__casey =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 480 || offsetMinutes == 660 then
                Ok (Offset offsetMinutes)

            else if offsetMinutes == 0 then
                Ok Uninhabited

            else
                Err offsetMinutes
        )


{-| `Antarctica/Davis`
-}
for__antarctica__davis : Zone -> Posix -> Result Int Abbreviation
for__antarctica__davis =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 300 || offsetMinutes == 420 then
                Ok (Offset offsetMinutes)

            else if offsetMinutes == 0 then
                Ok Uninhabited

            else
                Err offsetMinutes
        )


{-| `Antarctica/Macquarie`
-}
for__antarctica__macquarie : Zone -> Posix -> Result Int Abbreviation
for__antarctica__macquarie =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 0 then
                Ok Uninhabited

            else if offsetMinutes == 660 then
                Ok (ShortName "AEDT")

            else if offsetMinutes == 600 then
                Ok (ShortName "AEST")

            else
                Err offsetMinutes
        )


{-| `Antarctica/Mawson`
-}
for__antarctica__mawson : Zone -> Posix -> Result Int Abbreviation
for__antarctica__mawson =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 300 || offsetMinutes == 360 then
                Ok (Offset offsetMinutes)

            else if offsetMinutes == 0 then
                Ok Uninhabited

            else
                Err offsetMinutes
        )


{-| `Antarctica/Palmer`
-}
for__antarctica__palmer : Zone -> Posix -> Result Int Abbreviation
for__antarctica__palmer =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -240 || offsetMinutes == -180 || offsetMinutes == -120 then
                Ok (Offset offsetMinutes)

            else if offsetMinutes == 0 then
                Ok Uninhabited

            else
                Err offsetMinutes
        )


{-| `Antarctica/Rothera`
-}
for__antarctica__rothera : Zone -> Posix -> Result Int Abbreviation
for__antarctica__rothera =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -180 then
                Ok (Offset offsetMinutes)

            else if offsetMinutes == 0 then
                Ok Uninhabited

            else
                Err offsetMinutes
        )


{-| `Antarctica/Troll`
-}
for__antarctica__troll : Zone -> Posix -> Result Int Abbreviation
for__antarctica__troll =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 0 then
                Ok
                    (if posixSeconds >= 1108166400 then
                        Offset offsetMinutes

                     else
                        Uninhabited
                    )

            else
                Err offsetMinutes
        )


{-| `Antarctica/Vostok`
-}
for__antarctica__vostok : Zone -> Posix -> Result Int Abbreviation
for__antarctica__vostok =
    for__antarctica__davis


{-| `Asia/Almaty`
-}
for__asia__almaty : Zone -> Posix -> Result Int Abbreviation
for__asia__almaty =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 307 || offsetMinutes == 308 then
                Ok Lmt

            else if offsetMinutes == 300 || offsetMinutes == 360 || offsetMinutes == 420 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `Asia/Amman`
-}
for__asia__amman : Zone -> Posix -> Result Int Abbreviation
for__asia__amman =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 143 || offsetMinutes == 144 then
                Ok Lmt

            else if offsetMinutes == 180 then
                Ok
                    (if posixSeconds >= 1666908000 then
                        Offset offsetMinutes

                     else
                        ShortName "EEST"
                    )

            else if offsetMinutes == 120 then
                Ok (ShortName "EET")

            else
                Err offsetMinutes
        )


{-| `Asia/Anadyr`
-}
for__asia__anadyr : Zone -> Posix -> Result Int Abbreviation
for__asia__anadyr =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 709 || offsetMinutes == 710 then
                Ok Lmt

            else if offsetMinutes == 660 || offsetMinutes == 720 || offsetMinutes == 780 || offsetMinutes == 840 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `Asia/Aqtau`
-}
for__asia__aqtau : Zone -> Posix -> Result Int Abbreviation
for__asia__aqtau =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 201 || offsetMinutes == 202 then
                Ok Lmt

            else if offsetMinutes == 240 || offsetMinutes == 300 || offsetMinutes == 360 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `Asia/Aqtobe`
-}
for__asia__aqtobe : Zone -> Posix -> Result Int Abbreviation
for__asia__aqtobe =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 228 || offsetMinutes == 229 then
                Ok Lmt

            else if offsetMinutes == 240 || offsetMinutes == 300 || offsetMinutes == 360 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `Asia/Ashgabat`
-}
for__asia__ashgabat : Zone -> Posix -> Result Int Abbreviation
for__asia__ashgabat =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 233 || offsetMinutes == 234 then
                Ok Lmt

            else if offsetMinutes == 240 || offsetMinutes == 300 || offsetMinutes == 360 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `Asia/Atyrau`
-}
for__asia__atyrau : Zone -> Posix -> Result Int Abbreviation
for__asia__atyrau =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 207 || offsetMinutes == 208 then
                Ok Lmt

            else if offsetMinutes == 180 || offsetMinutes == 240 || offsetMinutes == 300 || offsetMinutes == 360 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `Asia/Baghdad`
-}
for__asia__baghdad : Zone -> Posix -> Result Int Abbreviation
for__asia__baghdad =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 177 || offsetMinutes == 178 then
                Ok
                    (if posixSeconds >= -2524532260 then
                        ShortName "BMT"

                     else
                        Lmt
                    )

            else if offsetMinutes == 180 || offsetMinutes == 240 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `Asia/Baku`
-}
for__asia__baku : Zone -> Posix -> Result Int Abbreviation
for__asia__baku =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 199 || offsetMinutes == 200 then
                Ok Lmt

            else if offsetMinutes == 180 || offsetMinutes == 240 || offsetMinutes == 300 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `Asia/Bangkok`
-}
for__asia__bangkok : Zone -> Posix -> Result Int Abbreviation
for__asia__bangkok =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 402 || offsetMinutes == 403 then
                Ok
                    (if posixSeconds >= -2840164924 then
                        ShortName "BMT"

                     else
                        Lmt
                    )

            else if offsetMinutes == 420 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `Asia/Barnaul`
-}
for__asia__barnaul : Zone -> Posix -> Result Int Abbreviation
for__asia__barnaul =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 335 then
                Ok Lmt

            else if offsetMinutes == 360 || offsetMinutes == 420 || offsetMinutes == 480 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `Asia/Beirut`
-}
for__asia__beirut : Zone -> Posix -> Result Int Abbreviation
for__asia__beirut =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 142 then
                Ok Lmt

            else if offsetMinutes == 180 then
                Ok (ShortName "EEST")

            else if offsetMinutes == 120 then
                Ok (ShortName "EET")

            else
                Err offsetMinutes
        )


{-| `Asia/Bishkek`
-}
for__asia__bishkek : Zone -> Posix -> Result Int Abbreviation
for__asia__bishkek =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 298 || offsetMinutes == 299 then
                Ok Lmt

            else if offsetMinutes == 300 || offsetMinutes == 360 || offsetMinutes == 420 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `Asia/Chita`
-}
for__asia__chita : Zone -> Posix -> Result Int Abbreviation
for__asia__chita =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 453 || offsetMinutes == 454 then
                Ok Lmt

            else if offsetMinutes == 480 || offsetMinutes == 540 || offsetMinutes == 600 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `Asia/Colombo`
-}
for__asia__colombo : Zone -> Posix -> Result Int Abbreviation
for__asia__colombo =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 319 || offsetMinutes == 320 then
                Ok
                    (if posixSeconds >= -2840159964 then
                        ShortName "MMT"

                     else
                        Lmt
                    )

            else if offsetMinutes == 330 || offsetMinutes == 360 || offsetMinutes == 390 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `Asia/Damascus`
-}
for__asia__damascus : Zone -> Posix -> Result Int Abbreviation
for__asia__damascus =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 145 || offsetMinutes == 146 then
                Ok Lmt

            else if offsetMinutes == 180 then
                Ok
                    (if posixSeconds >= 1666904400 then
                        Offset offsetMinutes

                     else
                        ShortName "EEST"
                    )

            else if offsetMinutes == 120 then
                Ok (ShortName "EET")

            else
                Err offsetMinutes
        )


{-| `Asia/Dhaka`
-}
for__asia__dhaka : Zone -> Posix -> Result Int Abbreviation
for__asia__dhaka =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 361 || offsetMinutes == 362 then
                Ok Lmt

            else if offsetMinutes == 330 || offsetMinutes == 360 || offsetMinutes == 390 || offsetMinutes == 420 then
                Ok (Offset offsetMinutes)

            else if offsetMinutes == 353 || offsetMinutes == 354 then
                Ok (ShortName "HMT")

            else
                Err offsetMinutes
        )


{-| `Asia/Dili`
-}
for__asia__dili : Zone -> Posix -> Result Int Abbreviation
for__asia__dili =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 502 || offsetMinutes == 503 then
                Ok Lmt

            else if offsetMinutes == 480 || offsetMinutes == 540 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `Asia/Dubai`
-}
for__asia__dubai : Zone -> Posix -> Result Int Abbreviation
for__asia__dubai =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 221 || offsetMinutes == 222 then
                Ok Lmt

            else if offsetMinutes == 240 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `Asia/Dushanbe`
-}
for__asia__dushanbe : Zone -> Posix -> Result Int Abbreviation
for__asia__dushanbe =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 275 || offsetMinutes == 276 then
                Ok Lmt

            else if offsetMinutes == 300 || offsetMinutes == 360 || offsetMinutes == 420 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `Asia/Famagusta`
-}
for__asia__famagusta : Zone -> Posix -> Result Int Abbreviation
for__asia__famagusta =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 135 || offsetMinutes == 136 then
                Ok Lmt

            else if offsetMinutes == 180 then
                Ok
                    (if posixSeconds >= 1473282000 then
                        Offset offsetMinutes

                     else
                        ShortName "EEST"
                    )

            else if offsetMinutes == 120 then
                Ok (ShortName "EET")

            else
                Err offsetMinutes
        )


{-| `Asia/Gaza`
-}
for__asia__gaza : Zone -> Posix -> Result Int Abbreviation
for__asia__gaza =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 137 || offsetMinutes == 138 then
                Ok Lmt

            else if offsetMinutes == 180 then
                Ok
                    (if posixSeconds >= 828655200 then
                        ShortName "EEST"

                     else if posixSeconds >= 142380000 then
                        ShortName "IDT"

                     else
                        ShortName "EEST"
                    )

            else if offsetMinutes == 120 then
                Ok
                    (if posixSeconds >= 820447200 then
                        ShortName "EET"

                     else if posixSeconds >= -81313200 then
                        ShortName "IST"

                     else
                        ShortName "EET"
                    )

            else
                Err offsetMinutes
        )


{-| `Asia/Hebron`
-}
for__asia__hebron : Zone -> Posix -> Result Int Abbreviation
for__asia__hebron =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 140 || offsetMinutes == 141 then
                Ok Lmt

            else if offsetMinutes == 180 then
                Ok
                    (if posixSeconds >= 828655200 then
                        ShortName "EEST"

                     else if posixSeconds >= 142380000 then
                        ShortName "IDT"

                     else
                        ShortName "EEST"
                    )

            else if offsetMinutes == 120 then
                Ok
                    (if posixSeconds >= 820447200 then
                        ShortName "EET"

                     else if posixSeconds >= -81313200 then
                        ShortName "IST"

                     else
                        ShortName "EET"
                    )

            else
                Err offsetMinutes
        )


{-| `Asia/Ho_Chi_Minh`
-}
for__asia__ho_chi_minh : Zone -> Posix -> Result Int Abbreviation
for__asia__ho_chi_minh =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 426 || offsetMinutes == 427 then
                Ok
                    (if posixSeconds >= -2004073590 then
                        ShortName "PLMT"

                     else
                        Lmt
                    )

            else if offsetMinutes == 420 || offsetMinutes == 480 || offsetMinutes == 540 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `Asia/Hong_Kong`
-}
for__asia__hong_kong : Zone -> Posix -> Result Int Abbreviation
for__asia__hong_kong =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 456 || offsetMinutes == 457 then
                Ok Lmt

            else if offsetMinutes == 540 then
                Ok
                    (if posixSeconds >= -747907200 then
                        ShortName "HKST"

                     else if posixSeconds >= -884248200 then
                        ShortName "JST"

                     else
                        ShortName "HKST"
                    )

            else if offsetMinutes == 480 then
                Ok (ShortName "HKT")

            else if offsetMinutes == 510 then
                Ok (ShortName "HKWT")

            else
                Err offsetMinutes
        )


{-| `Asia/Hovd`
-}
for__asia__hovd : Zone -> Posix -> Result Int Abbreviation
for__asia__hovd =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 366 || offsetMinutes == 367 then
                Ok Lmt

            else if offsetMinutes == 360 || offsetMinutes == 420 || offsetMinutes == 480 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `Asia/Irkutsk`
-}
for__asia__irkutsk : Zone -> Posix -> Result Int Abbreviation
for__asia__irkutsk =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 417 || offsetMinutes == 418 then
                Ok
                    (if posixSeconds >= -2840165825 then
                        ShortName "IMT"

                     else
                        Lmt
                    )

            else if offsetMinutes == 420 || offsetMinutes == 480 || offsetMinutes == 540 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `Asia/Jakarta`
-}
for__asia__jakarta : Zone -> Posix -> Result Int Abbreviation
for__asia__jakarta =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 427 || offsetMinutes == 428 then
                Ok
                    (if posixSeconds >= -3231299232 then
                        ShortName "BMT"

                     else
                        Lmt
                    )

            else if offsetMinutes == 440 || offsetMinutes == 450 || offsetMinutes == 480 || offsetMinutes == 540 then
                Ok (Offset offsetMinutes)

            else if offsetMinutes == 420 then
                Ok (ShortName "WIB")

            else
                Err offsetMinutes
        )


{-| `Asia/Jayapura`
-}
for__asia__jayapura : Zone -> Posix -> Result Int Abbreviation
for__asia__jayapura =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 562 || offsetMinutes == 563 then
                Ok Lmt

            else if offsetMinutes == 570 then
                Ok (Offset offsetMinutes)

            else if offsetMinutes == 540 then
                Ok
                    (if posixSeconds >= -189423000 then
                        ShortName "WIT"

                     else
                        Offset offsetMinutes
                    )

            else
                Err offsetMinutes
        )


{-| `Asia/Jerusalem`
-}
for__asia__jerusalem : Zone -> Posix -> Result Int Abbreviation
for__asia__jerusalem =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 140 || offsetMinutes == 141 then
                Ok
                    (if posixSeconds >= -2840149254 then
                        ShortName "JMT"

                     else
                        Lmt
                    )

            else if offsetMinutes == 240 then
                Ok (ShortName "IDDT")

            else if offsetMinutes == 180 then
                Ok (ShortName "IDT")

            else if offsetMinutes == 120 then
                Ok (ShortName "IST")

            else
                Err offsetMinutes
        )


{-| `Asia/Kabul`
-}
for__asia__kabul : Zone -> Posix -> Result Int Abbreviation
for__asia__kabul =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 276 || offsetMinutes == 277 then
                Ok Lmt

            else if offsetMinutes == 240 || offsetMinutes == 270 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `Asia/Kamchatka`
-}
for__asia__kamchatka : Zone -> Posix -> Result Int Abbreviation
for__asia__kamchatka =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 634 || offsetMinutes == 635 then
                Ok Lmt

            else if offsetMinutes == 660 || offsetMinutes == 720 || offsetMinutes == 780 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `Asia/Karachi`
-}
for__asia__karachi : Zone -> Posix -> Result Int Abbreviation
for__asia__karachi =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 268 || offsetMinutes == 269 then
                Ok Lmt

            else if offsetMinutes == 330 || offsetMinutes == 390 then
                Ok (Offset offsetMinutes)

            else if offsetMinutes == 300 then
                Ok
                    (if posixSeconds >= 38775600 then
                        ShortName "PKT"

                     else
                        Offset offsetMinutes
                    )

            else if offsetMinutes == 360 then
                Ok (ShortName "PKST")

            else
                Err offsetMinutes
        )


{-| `Asia/Kathmandu`
-}
for__asia__kathmandu : Zone -> Posix -> Result Int Abbreviation
for__asia__kathmandu =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 341 || offsetMinutes == 342 then
                Ok Lmt

            else if offsetMinutes == 330 || offsetMinutes == 345 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `Asia/Khandyga`
-}
for__asia__khandyga : Zone -> Posix -> Result Int Abbreviation
for__asia__khandyga =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 542 || offsetMinutes == 543 then
                Ok Lmt

            else if offsetMinutes == 480 || offsetMinutes == 540 || offsetMinutes == 600 || offsetMinutes == 660 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `Asia/Kolkata`
-}
for__asia__kolkata : Zone -> Posix -> Result Int Abbreviation
for__asia__kolkata =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 353 || offsetMinutes == 354 then
                Ok
                    (if posixSeconds >= -3645237208 then
                        ShortName "HMT"

                     else
                        Lmt
                    )

            else if offsetMinutes == 390 then
                Ok (Offset offsetMinutes)

            else if offsetMinutes == 330 then
                Ok (ShortName "IST")

            else if offsetMinutes == 321 || offsetMinutes == 322 then
                Ok (ShortName "MMT")

            else
                Err offsetMinutes
        )


{-| `Asia/Krasnoyarsk`
-}
for__asia__krasnoyarsk : Zone -> Posix -> Result Int Abbreviation
for__asia__krasnoyarsk =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 371 || offsetMinutes == 372 then
                Ok Lmt

            else if offsetMinutes == 360 || offsetMinutes == 420 || offsetMinutes == 480 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `Asia/Kuching`
-}
for__asia__kuching : Zone -> Posix -> Result Int Abbreviation
for__asia__kuching =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 441 || offsetMinutes == 442 then
                Ok Lmt

            else if offsetMinutes == 450 || offsetMinutes == 480 || offsetMinutes == 500 || offsetMinutes == 540 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `Asia/Macau`
-}
for__asia__macau : Zone -> Posix -> Result Int Abbreviation
for__asia__macau =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 454 || offsetMinutes == 455 then
                Ok Lmt

            else if offsetMinutes == 600 then
                Ok (Offset offsetMinutes)

            else if offsetMinutes == 540 then
                Ok
                    (if posixSeconds >= -747046800 then
                        ShortName "CDT"

                     else
                        Offset offsetMinutes
                    )

            else if offsetMinutes == 480 then
                Ok (ShortName "CST")

            else
                Err offsetMinutes
        )


{-| `Asia/Magadan`
-}
for__asia__magadan : Zone -> Posix -> Result Int Abbreviation
for__asia__magadan =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 603 || offsetMinutes == 604 then
                Ok Lmt

            else if offsetMinutes == 600 || offsetMinutes == 660 || offsetMinutes == 720 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `Asia/Makassar`
-}
for__asia__makassar : Zone -> Posix -> Result Int Abbreviation
for__asia__makassar =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 477 || offsetMinutes == 478 then
                Ok
                    (if posixSeconds >= -1577951856 then
                        ShortName "MMT"

                     else
                        Lmt
                    )

            else if offsetMinutes == 540 then
                Ok (Offset offsetMinutes)

            else if offsetMinutes == 480 then
                Ok
                    (if posixSeconds >= -766054800 then
                        ShortName "WITA"

                     else
                        Offset offsetMinutes
                    )

            else
                Err offsetMinutes
        )


{-| `Asia/Manila`
-}
for__asia__manila : Zone -> Posix -> Result Int Abbreviation
for__asia__manila =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -956 || offsetMinutes == 484 then
                Ok Lmt

            else if offsetMinutes == 540 then
                Ok
                    (if posixSeconds >= -496224000 then
                        ShortName "PDT"

                     else if posixSeconds >= -873273600 then
                        ShortName "JST"

                     else
                        ShortName "PDT"
                    )

            else if offsetMinutes == 480 then
                Ok (ShortName "PST")

            else
                Err offsetMinutes
        )


{-| `Asia/Nicosia`
-}
for__asia__nicosia : Zone -> Posix -> Result Int Abbreviation
for__asia__nicosia =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 133 || offsetMinutes == 134 then
                Ok Lmt

            else if offsetMinutes == 180 then
                Ok (ShortName "EEST")

            else if offsetMinutes == 120 then
                Ok (ShortName "EET")

            else
                Err offsetMinutes
        )


{-| `Asia/Novokuznetsk`
-}
for__asia__novokuznetsk : Zone -> Posix -> Result Int Abbreviation
for__asia__novokuznetsk =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 348 || offsetMinutes == 349 then
                Ok Lmt

            else if offsetMinutes == 360 || offsetMinutes == 420 || offsetMinutes == 480 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `Asia/Novosibirsk`
-}
for__asia__novosibirsk : Zone -> Posix -> Result Int Abbreviation
for__asia__novosibirsk =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 331 || offsetMinutes == 332 then
                Ok Lmt

            else if offsetMinutes == 360 || offsetMinutes == 420 || offsetMinutes == 480 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `Asia/Omsk`
-}
for__asia__omsk : Zone -> Posix -> Result Int Abbreviation
for__asia__omsk =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 293 || offsetMinutes == 294 then
                Ok Lmt

            else if offsetMinutes == 300 || offsetMinutes == 360 || offsetMinutes == 420 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `Asia/Oral`
-}
for__asia__oral : Zone -> Posix -> Result Int Abbreviation
for__asia__oral =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 205 || offsetMinutes == 206 then
                Ok Lmt

            else if offsetMinutes == 180 || offsetMinutes == 240 || offsetMinutes == 300 || offsetMinutes == 360 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `Asia/Pontianak`
-}
for__asia__pontianak : Zone -> Posix -> Result Int Abbreviation
for__asia__pontianak =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 437 || offsetMinutes == 438 then
                Ok
                    (if posixSeconds >= -1946186240 then
                        ShortName "PMT"

                     else
                        Lmt
                    )

            else if offsetMinutes == 450 || offsetMinutes == 540 then
                Ok (Offset offsetMinutes)

            else if offsetMinutes == 480 then
                Ok
                    (if posixSeconds >= -189415800 then
                        ShortName "WITA"

                     else
                        Offset offsetMinutes
                    )

            else if offsetMinutes == 420 then
                Ok (ShortName "WIB")

            else
                Err offsetMinutes
        )


{-| `Asia/Pyongyang`
-}
for__asia__pyongyang : Zone -> Posix -> Result Int Abbreviation
for__asia__pyongyang =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 503 then
                Ok Lmt

            else if offsetMinutes == 540 then
                Ok
                    (if posixSeconds >= -768646800 then
                        ShortName "KST"

                     else
                        ShortName "JST"
                    )

            else if offsetMinutes == 510 then
                Ok (ShortName "KST")

            else
                Err offsetMinutes
        )


{-| `Asia/Qatar`
-}
for__asia__qatar : Zone -> Posix -> Result Int Abbreviation
for__asia__qatar =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 206 || offsetMinutes == 207 then
                Ok Lmt

            else if offsetMinutes == 180 || offsetMinutes == 240 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `Asia/Qostanay`
-}
for__asia__qostanay : Zone -> Posix -> Result Int Abbreviation
for__asia__qostanay =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 254 || offsetMinutes == 255 then
                Ok Lmt

            else if offsetMinutes == 240 || offsetMinutes == 300 || offsetMinutes == 360 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `Asia/Qyzylorda`
-}
for__asia__qyzylorda : Zone -> Posix -> Result Int Abbreviation
for__asia__qyzylorda =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 261 || offsetMinutes == 262 then
                Ok Lmt

            else if offsetMinutes == 240 || offsetMinutes == 300 || offsetMinutes == 360 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `Asia/Riyadh`
-}
for__asia__riyadh : Zone -> Posix -> Result Int Abbreviation
for__asia__riyadh =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 186 || offsetMinutes == 187 then
                Ok Lmt

            else if offsetMinutes == 180 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `Asia/Sakhalin`
-}
for__asia__sakhalin : Zone -> Posix -> Result Int Abbreviation
for__asia__sakhalin =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 570 || offsetMinutes == 571 then
                Ok Lmt

            else if offsetMinutes == 540 || offsetMinutes == 600 || offsetMinutes == 660 || offsetMinutes == 720 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `Asia/Samarkand`
-}
for__asia__samarkand : Zone -> Posix -> Result Int Abbreviation
for__asia__samarkand =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 267 || offsetMinutes == 268 then
                Ok Lmt

            else if offsetMinutes == 240 || offsetMinutes == 300 || offsetMinutes == 360 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `Asia/Seoul`
-}
for__asia__seoul : Zone -> Posix -> Result Int Abbreviation
for__asia__seoul =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 507 || offsetMinutes == 508 then
                Ok Lmt

            else if offsetMinutes == 540 then
                Ok
                    (if posixSeconds >= -767350800 then
                        ShortName "KST"

                     else
                        ShortName "JST"
                    )

            else if offsetMinutes == 570 || offsetMinutes == 600 then
                Ok (ShortName "KDT")

            else if offsetMinutes == 510 then
                Ok (ShortName "KST")

            else
                Err offsetMinutes
        )


{-| `Asia/Shanghai`
-}
for__asia__shanghai : Zone -> Posix -> Result Int Abbreviation
for__asia__shanghai =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 485 || offsetMinutes == 486 then
                Ok Lmt

            else if offsetMinutes == 540 then
                Ok (ShortName "CDT")

            else if offsetMinutes == 480 then
                Ok (ShortName "CST")

            else
                Err offsetMinutes
        )


{-| `Asia/Singapore`
-}
for__asia__singapore : Zone -> Posix -> Result Int Abbreviation
for__asia__singapore =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 415 || offsetMinutes == 416 then
                Ok
                    (if posixSeconds >= -2177477725 then
                        ShortName "SMT"

                     else
                        Lmt
                    )

            else if offsetMinutes == 420 || offsetMinutes == 440 || offsetMinutes == 450 || offsetMinutes == 480 || offsetMinutes == 540 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `Asia/Srednekolymsk`
-}
for__asia__srednekolymsk : Zone -> Posix -> Result Int Abbreviation
for__asia__srednekolymsk =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 614 || offsetMinutes == 615 then
                Ok Lmt

            else if offsetMinutes == 600 || offsetMinutes == 660 || offsetMinutes == 720 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `Asia/Taipei`
-}
for__asia__taipei : Zone -> Posix -> Result Int Abbreviation
for__asia__taipei =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 486 then
                Ok Lmt

            else if offsetMinutes == 480 then
                Ok (ShortName "CST")

            else if offsetMinutes == 540 then
                Ok
                    (if posixSeconds >= -745833600 then
                        ShortName "CDT"

                     else
                        ShortName "JST"
                    )

            else
                Err offsetMinutes
        )


{-| `Asia/Tashkent`
-}
for__asia__tashkent : Zone -> Posix -> Result Int Abbreviation
for__asia__tashkent =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 277 || offsetMinutes == 278 then
                Ok Lmt

            else if offsetMinutes == 300 || offsetMinutes == 360 || offsetMinutes == 420 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `Asia/Tbilisi`
-}
for__asia__tbilisi : Zone -> Posix -> Result Int Abbreviation
for__asia__tbilisi =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 179 then
                Ok
                    (if posixSeconds >= -2840151551 then
                        ShortName "TBMT"

                     else
                        Lmt
                    )

            else if offsetMinutes == 180 then
                Ok
                    (if posixSeconds >= -1441162751 then
                        Offset offsetMinutes

                     else if posixSeconds >= -2840151551 then
                        ShortName "TBMT"

                     else
                        Lmt
                    )

            else if offsetMinutes == 240 || offsetMinutes == 300 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `Asia/Tehran`
-}
for__asia__tehran : Zone -> Posix -> Result Int Abbreviation
for__asia__tehran =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 205 || offsetMinutes == 206 then
                Ok
                    (if posixSeconds >= -1704165944 then
                        ShortName "TMT"

                     else
                        Lmt
                    )

            else if offsetMinutes == 210 || offsetMinutes == 240 || offsetMinutes == 270 || offsetMinutes == 300 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `Asia/Thimphu`
-}
for__asia__thimphu : Zone -> Posix -> Result Int Abbreviation
for__asia__thimphu =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 358 || offsetMinutes == 359 then
                Ok Lmt

            else if offsetMinutes == 330 || offsetMinutes == 360 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `Asia/Tokyo`
-}
for__asia__tokyo : Zone -> Posix -> Result Int Abbreviation
for__asia__tokyo =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 558 || offsetMinutes == 559 then
                Ok Lmt

            else if offsetMinutes == 600 then
                Ok (ShortName "JDT")

            else if offsetMinutes == 540 then
                Ok (ShortName "JST")

            else
                Err offsetMinutes
        )


{-| `Asia/Tomsk`
-}
for__asia__tomsk : Zone -> Posix -> Result Int Abbreviation
for__asia__tomsk =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 339 || offsetMinutes == 340 then
                Ok Lmt

            else if offsetMinutes == 360 || offsetMinutes == 420 || offsetMinutes == 480 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `Asia/Ulaanbaatar`
-}
for__asia__ulaanbaatar : Zone -> Posix -> Result Int Abbreviation
for__asia__ulaanbaatar =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 427 || offsetMinutes == 428 then
                Ok Lmt

            else if offsetMinutes == 420 || offsetMinutes == 480 || offsetMinutes == 540 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `Asia/Urumqi`
-}
for__asia__urumqi : Zone -> Posix -> Result Int Abbreviation
for__asia__urumqi =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 350 || offsetMinutes == 351 then
                Ok Lmt

            else if offsetMinutes == 360 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `Asia/Ust-Nera`
-}
for__asia__ust_nera : Zone -> Posix -> Result Int Abbreviation
for__asia__ust_nera =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 572 || offsetMinutes == 573 then
                Ok Lmt

            else if offsetMinutes == 480 || offsetMinutes == 540 || offsetMinutes == 600 || offsetMinutes == 660 || offsetMinutes == 720 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `Asia/Vladivostok`
-}
for__asia__vladivostok : Zone -> Posix -> Result Int Abbreviation
for__asia__vladivostok =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 527 || offsetMinutes == 528 then
                Ok Lmt

            else if offsetMinutes == 540 || offsetMinutes == 600 || offsetMinutes == 660 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `Asia/Yakutsk`
-}
for__asia__yakutsk : Zone -> Posix -> Result Int Abbreviation
for__asia__yakutsk =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 518 || offsetMinutes == 519 then
                Ok Lmt

            else if offsetMinutes == 480 || offsetMinutes == 540 || offsetMinutes == 600 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `Asia/Yangon`
-}
for__asia__yangon : Zone -> Posix -> Result Int Abbreviation
for__asia__yangon =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 384 || offsetMinutes == 385 then
                Ok
                    (if posixSeconds >= -2840163887 then
                        ShortName "RMT"

                     else
                        Lmt
                    )

            else if offsetMinutes == 390 || offsetMinutes == 540 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `Asia/Yekaterinburg`
-}
for__asia__yekaterinburg : Zone -> Posix -> Result Int Abbreviation
for__asia__yekaterinburg =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 242 || offsetMinutes == 243 then
                Ok Lmt

            else if offsetMinutes == 240 || offsetMinutes == 300 || offsetMinutes == 360 then
                Ok (Offset offsetMinutes)

            else if offsetMinutes == 225 || offsetMinutes == 226 then
                Ok (ShortName "PMT")

            else
                Err offsetMinutes
        )


{-| `Asia/Yerevan`
-}
for__asia__yerevan : Zone -> Posix -> Result Int Abbreviation
for__asia__yerevan =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 178 then
                Ok Lmt

            else if offsetMinutes == 180 || offsetMinutes == 240 || offsetMinutes == 300 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `Atlantic/Azores`
-}
for__atlantic__azores : Zone -> Posix -> Result Int Abbreviation
for__atlantic__azores =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -103 || offsetMinutes == -102 then
                Ok Lmt

            else if offsetMinutes == -120 || offsetMinutes == -60 then
                Ok (Offset offsetMinutes)

            else if offsetMinutes == 0 then
                Ok
                    (if posixSeconds >= 740278800 then
                        Offset offsetMinutes

                     else if posixSeconds >= 725421600 then
                        ShortName "WET"

                     else
                        Offset offsetMinutes
                    )

            else if offsetMinutes == -115 || offsetMinutes == -114 then
                Ok (ShortName "HMT")

            else if offsetMinutes == 60 then
                Ok (ShortName "WEST")

            else
                Err offsetMinutes
        )


{-| `Atlantic/Bermuda`
-}
for__atlantic__bermuda : Zone -> Posix -> Result Int Abbreviation
for__atlantic__bermuda =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -260 || offsetMinutes == -259 then
                Ok
                    (if posixSeconds >= -2524506042 then
                        ShortName "BMT"

                     else
                        Lmt
                    )

            else if offsetMinutes == -180 then
                Ok (ShortName "ADT")

            else if offsetMinutes == -240 then
                Ok (ShortName "AST")

            else if offsetMinutes == -200 || offsetMinutes == -199 then
                Ok (ShortName "BST")

            else
                Err offsetMinutes
        )


{-| `Atlantic/Canary`
-}
for__atlantic__canary : Zone -> Posix -> Result Int Abbreviation
for__atlantic__canary =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -62 || offsetMinutes == -61 then
                Ok Lmt

            else if offsetMinutes == -60 then
                Ok (Offset offsetMinutes)

            else if offsetMinutes == 60 then
                Ok (ShortName "WEST")

            else if offsetMinutes == 0 then
                Ok (ShortName "WET")

            else
                Err offsetMinutes
        )


{-| `Atlantic/Cape_Verde`
-}
for__atlantic__cape_verde : Zone -> Posix -> Result Int Abbreviation
for__atlantic__cape_verde =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -95 || offsetMinutes == -94 then
                Ok Lmt

            else if offsetMinutes == -120 || offsetMinutes == -60 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `Atlantic/Faroe`
-}
for__atlantic__faroe : Zone -> Posix -> Result Int Abbreviation
for__atlantic__faroe =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -28 || offsetMinutes == -27 then
                Ok Lmt

            else if offsetMinutes == 60 then
                Ok (ShortName "WEST")

            else if offsetMinutes == 0 then
                Ok (ShortName "WET")

            else
                Err offsetMinutes
        )


{-| `Atlantic/Madeira`
-}
for__atlantic__madeira : Zone -> Posix -> Result Int Abbreviation
for__atlantic__madeira =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -68 || offsetMinutes == -67 then
                Ok
                    (if posixSeconds >= -2713906344 then
                        ShortName "FMT"

                     else
                        Lmt
                    )

            else if offsetMinutes == -60 then
                Ok (Offset offsetMinutes)

            else if offsetMinutes == 0 then
                Ok
                    (if posixSeconds >= -102546000 then
                        ShortName "WET"

                     else
                        Offset offsetMinutes
                    )

            else if offsetMinutes == 60 then
                Ok
                    (if posixSeconds >= 386726400 then
                        ShortName "WEST"

                     else
                        Offset offsetMinutes
                    )

            else
                Err offsetMinutes
        )


{-| `Atlantic/South_Georgia`
-}
for__atlantic__south_georgia : Zone -> Posix -> Result Int Abbreviation
for__atlantic__south_georgia =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -147 || offsetMinutes == -146 then
                Ok Lmt

            else if offsetMinutes == -120 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `Atlantic/Stanley`
-}
for__atlantic__stanley : Zone -> Posix -> Result Int Abbreviation
for__atlantic__stanley =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -232 || offsetMinutes == -231 then
                Ok
                    (if posixSeconds >= -2524507716 then
                        ShortName "SMT"

                     else
                        Lmt
                    )

            else if offsetMinutes == -240 || offsetMinutes == -180 || offsetMinutes == -120 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `Australia/Adelaide`
-}
for__australia__adelaide : Zone -> Posix -> Result Int Abbreviation
for__australia__adelaide =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 554 || offsetMinutes == 555 then
                Ok Lmt

            else if offsetMinutes == 630 then
                Ok (ShortName "ACDT")

            else if offsetMinutes == 540 || offsetMinutes == 570 then
                Ok (ShortName "ACST")

            else
                Err offsetMinutes
        )


{-| `Australia/Brisbane`
-}
for__australia__brisbane : Zone -> Posix -> Result Int Abbreviation
for__australia__brisbane =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 612 || offsetMinutes == 613 then
                Ok Lmt

            else if offsetMinutes == 660 then
                Ok (ShortName "AEDT")

            else if offsetMinutes == 600 then
                Ok (ShortName "AEST")

            else
                Err offsetMinutes
        )


{-| `Australia/Broken_Hill`
-}
for__australia__broken_hill : Zone -> Posix -> Result Int Abbreviation
for__australia__broken_hill =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 565 || offsetMinutes == 566 then
                Ok Lmt

            else if offsetMinutes == 630 then
                Ok (ShortName "ACDT")

            else if offsetMinutes == 540 || offsetMinutes == 570 then
                Ok (ShortName "ACST")

            else if offsetMinutes == 600 then
                Ok (ShortName "AEST")

            else
                Err offsetMinutes
        )


{-| `Australia/Darwin`
-}
for__australia__darwin : Zone -> Posix -> Result Int Abbreviation
for__australia__darwin =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 523 || offsetMinutes == 524 then
                Ok Lmt

            else if offsetMinutes == 630 then
                Ok (ShortName "ACDT")

            else if offsetMinutes == 540 || offsetMinutes == 570 then
                Ok (ShortName "ACST")

            else
                Err offsetMinutes
        )


{-| `Australia/Eucla`
-}
for__australia__eucla : Zone -> Posix -> Result Int Abbreviation
for__australia__eucla =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 515 || offsetMinutes == 516 then
                Ok Lmt

            else if offsetMinutes == 525 || offsetMinutes == 585 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `Australia/Hobart`
-}
for__australia__hobart : Zone -> Posix -> Result Int Abbreviation
for__australia__hobart =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 589 || offsetMinutes == 590 then
                Ok Lmt

            else if offsetMinutes == 660 then
                Ok (ShortName "AEDT")

            else if offsetMinutes == 600 then
                Ok (ShortName "AEST")

            else
                Err offsetMinutes
        )


{-| `Australia/Lindeman`
-}
for__australia__lindeman : Zone -> Posix -> Result Int Abbreviation
for__australia__lindeman =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 595 || offsetMinutes == 596 then
                Ok Lmt

            else if offsetMinutes == 660 then
                Ok (ShortName "AEDT")

            else if offsetMinutes == 600 then
                Ok (ShortName "AEST")

            else
                Err offsetMinutes
        )


{-| `Australia/Lord_Howe`
-}
for__australia__lord_howe : Zone -> Posix -> Result Int Abbreviation
for__australia__lord_howe =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 636 || offsetMinutes == 637 then
                Ok Lmt

            else if offsetMinutes == 630 || offsetMinutes == 660 || offsetMinutes == 690 then
                Ok (Offset offsetMinutes)

            else if offsetMinutes == 600 then
                Ok (ShortName "AEST")

            else
                Err offsetMinutes
        )


{-| `Australia/Melbourne`
-}
for__australia__melbourne : Zone -> Posix -> Result Int Abbreviation
for__australia__melbourne =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 579 || offsetMinutes == 580 then
                Ok Lmt

            else if offsetMinutes == 660 then
                Ok (ShortName "AEDT")

            else if offsetMinutes == 600 then
                Ok (ShortName "AEST")

            else
                Err offsetMinutes
        )


{-| `Australia/Perth`
-}
for__australia__perth : Zone -> Posix -> Result Int Abbreviation
for__australia__perth =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 463 || offsetMinutes == 464 then
                Ok Lmt

            else if offsetMinutes == 540 then
                Ok (ShortName "AWDT")

            else if offsetMinutes == 480 then
                Ok (ShortName "AWST")

            else
                Err offsetMinutes
        )


{-| `Australia/Sydney`
-}
for__australia__sydney : Zone -> Posix -> Result Int Abbreviation
for__australia__sydney =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 604 || offsetMinutes == 605 then
                Ok Lmt

            else if offsetMinutes == 660 then
                Ok (ShortName "AEDT")

            else if offsetMinutes == 600 then
                Ok (ShortName "AEST")

            else
                Err offsetMinutes
        )


{-| `Europe/Andorra`
-}
for__europe__andorra : Zone -> Posix -> Result Int Abbreviation
for__europe__andorra =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 6 || offsetMinutes == 7 then
                Ok Lmt

            else if offsetMinutes == 120 then
                Ok (ShortName "CEST")

            else if offsetMinutes == 60 then
                Ok (ShortName "CET")

            else if offsetMinutes == 0 then
                Ok (ShortName "WET")

            else
                Err offsetMinutes
        )


{-| `Europe/Astrakhan`
-}
for__europe__astrakhan : Zone -> Posix -> Result Int Abbreviation
for__europe__astrakhan =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 192 || offsetMinutes == 193 then
                Ok Lmt

            else if offsetMinutes == 180 || offsetMinutes == 240 || offsetMinutes == 300 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `Europe/Athens`
-}
for__europe__athens : Zone -> Posix -> Result Int Abbreviation
for__europe__athens =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 94 || offsetMinutes == 95 then
                Ok
                    (if posixSeconds >= -2344642492 then
                        ShortName "AMT"

                     else
                        Lmt
                    )

            else if offsetMinutes == 60 then
                Ok (ShortName "CET")

            else if offsetMinutes == 180 then
                Ok (ShortName "EEST")

            else if offsetMinutes == 120 then
                Ok
                    (if posixSeconds >= -812422800 then
                        ShortName "EET"

                     else if posixSeconds >= -904878000 then
                        ShortName "CEST"

                     else
                        ShortName "EET"
                    )

            else
                Err offsetMinutes
        )


{-| `Europe/Belgrade`
-}
for__europe__belgrade : Zone -> Posix -> Result Int Abbreviation
for__europe__belgrade =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 82 then
                Ok Lmt

            else if offsetMinutes == 120 then
                Ok (ShortName "CEST")

            else if offsetMinutes == 60 then
                Ok (ShortName "CET")

            else
                Err offsetMinutes
        )


{-| `Europe/Berlin`
-}
for__europe__berlin : Zone -> Posix -> Result Int Abbreviation
for__europe__berlin =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 53 || offsetMinutes == 54 then
                Ok Lmt

            else if offsetMinutes == 180 then
                Ok (ShortName "CEMT")

            else if offsetMinutes == 120 then
                Ok (ShortName "CEST")

            else if offsetMinutes == 60 then
                Ok (ShortName "CET")

            else
                Err offsetMinutes
        )


{-| `Europe/Brussels`
-}
for__europe__brussels : Zone -> Posix -> Result Int Abbreviation
for__europe__brussels =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 17 || offsetMinutes == 18 then
                Ok
                    (if posixSeconds >= -2840141850 then
                        ShortName "BMT"

                     else
                        Lmt
                    )

            else if offsetMinutes == 120 then
                Ok (ShortName "CEST")

            else if offsetMinutes == 60 then
                Ok
                    (if posixSeconds >= -857257200 then
                        ShortName "CET"

                     else if posixSeconds >= -1604278800 then
                        ShortName "WEST"

                     else
                        ShortName "CET"
                    )

            else if offsetMinutes == 0 then
                Ok (ShortName "WET")

            else
                Err offsetMinutes
        )


{-| `Europe/Bucharest`
-}
for__europe__bucharest : Zone -> Posix -> Result Int Abbreviation
for__europe__bucharest =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 104 || offsetMinutes == 105 then
                Ok
                    (if posixSeconds >= -2469404664 then
                        ShortName "BMT"

                     else
                        Lmt
                    )

            else if offsetMinutes == 180 then
                Ok (ShortName "EEST")

            else if offsetMinutes == 120 then
                Ok (ShortName "EET")

            else
                Err offsetMinutes
        )


{-| `Europe/Budapest`
-}
for__europe__budapest : Zone -> Posix -> Result Int Abbreviation
for__europe__budapest =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 76 || offsetMinutes == 77 then
                Ok Lmt

            else if offsetMinutes == 120 then
                Ok (ShortName "CEST")

            else if offsetMinutes == 60 then
                Ok (ShortName "CET")

            else
                Err offsetMinutes
        )


{-| `Europe/Chisinau`
-}
for__europe__chisinau : Zone -> Posix -> Result Int Abbreviation
for__europe__chisinau =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 116 then
                Ok Lmt

            else if offsetMinutes == 115 then
                Ok
                    (if posixSeconds >= -2840147720 then
                        ShortName "CMT"

                     else
                        Lmt
                    )

            else if offsetMinutes == 104 || offsetMinutes == 105 then
                Ok (ShortName "BMT")

            else if offsetMinutes == 60 then
                Ok (ShortName "CET")

            else if offsetMinutes == 180 then
                Ok
                    (if posixSeconds >= 641944800 then
                        ShortName "EEST"

                     else if posixSeconds >= -800157600 then
                        ShortName "MSK"

                     else
                        ShortName "EEST"
                    )

            else if offsetMinutes == 120 then
                Ok
                    (if posixSeconds >= 654652800 then
                        ShortName "EET"

                     else if posixSeconds >= -898138800 then
                        ShortName "CEST"

                     else
                        ShortName "EET"
                    )

            else if offsetMinutes == 240 then
                Ok (ShortName "MSD")

            else
                Err offsetMinutes
        )


{-| `Europe/Dublin`
-}
for__europe__dublin : Zone -> Posix -> Result Int Abbreviation
for__europe__dublin =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -26 || offsetMinutes == -25 then
                Ok
                    (if posixSeconds >= -2821649679 then
                        ShortName "DMT"

                     else
                        Lmt
                    )

            else if offsetMinutes == 60 then
                Ok
                    (if posixSeconds >= -1507500000 then
                        ShortName "IST"

                     else
                        ShortName "BST"
                    )

            else if offsetMinutes == 0 then
                Ok (ShortName "GMT")

            else if offsetMinutes == 34 || offsetMinutes == 35 then
                Ok (ShortName "IST")

            else
                Err offsetMinutes
        )


{-| `Europe/Gibraltar`
-}
for__europe__gibraltar : Zone -> Posix -> Result Int Abbreviation
for__europe__gibraltar =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -22 || offsetMinutes == -21 then
                Ok Lmt

            else if offsetMinutes == 120 then
                Ok
                    (if posixSeconds >= 386125200 then
                        ShortName "CEST"

                     else
                        ShortName "BDST"
                    )

            else if offsetMinutes == 60 then
                Ok
                    (if posixSeconds >= -401320800 then
                        ShortName "CET"

                     else
                        ShortName "BST"
                    )

            else if offsetMinutes == 0 then
                Ok (ShortName "GMT")

            else
                Err offsetMinutes
        )


{-| `Europe/Helsinki`
-}
for__europe__helsinki : Zone -> Posix -> Result Int Abbreviation
for__europe__helsinki =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 99 || offsetMinutes == 100 then
                Ok
                    (if posixSeconds >= -2890258789 then
                        ShortName "HMT"

                     else
                        Lmt
                    )

            else if offsetMinutes == 180 then
                Ok (ShortName "EEST")

            else if offsetMinutes == 120 then
                Ok (ShortName "EET")

            else
                Err offsetMinutes
        )


{-| `Europe/Istanbul`
-}
for__europe__istanbul : Zone -> Posix -> Result Int Abbreviation
for__europe__istanbul =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 115 then
                Ok Lmt

            else if offsetMinutes == 116 then
                Ok
                    (if posixSeconds >= -2840147752 then
                        ShortName "IMT"

                     else
                        Lmt
                    )

            else if offsetMinutes == 240 then
                Ok (Offset offsetMinutes)

            else if offsetMinutes == 180 then
                Ok
                    (if posixSeconds >= 1473195600 then
                        Offset offsetMinutes

                     else if posixSeconds >= 482799600 then
                        ShortName "EEST"

                     else if posixSeconds >= 267915600 then
                        Offset offsetMinutes

                     else
                        ShortName "EEST"
                    )

            else if offsetMinutes == 120 then
                Ok (ShortName "EET")

            else if offsetMinutes == 117 then
                Ok (ShortName "IMT")

            else
                Err offsetMinutes
        )


{-| `Europe/Kaliningrad`
-}
for__europe__kaliningrad : Zone -> Posix -> Result Int Abbreviation
for__europe__kaliningrad =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 82 then
                Ok Lmt

            else if offsetMinutes == 120 then
                Ok
                    (if posixSeconds >= -780372000 then
                        ShortName "EET"

                     else
                        ShortName "CEST"
                    )

            else if offsetMinutes == 60 then
                Ok (ShortName "CET")

            else if offsetMinutes == 180 then
                Ok
                    (if posixSeconds >= 1301184000 then
                        Offset offsetMinutes

                     else if posixSeconds >= 606870000 then
                        ShortName "EEST"

                     else if posixSeconds >= -749095200 then
                        ShortName "MSK"

                     else
                        ShortName "EEST"
                    )

            else if offsetMinutes == 240 then
                Ok (ShortName "MSD")

            else
                Err offsetMinutes
        )


{-| `Europe/Kirov`
-}
for__europe__kirov : Zone -> Posix -> Result Int Abbreviation
for__europe__kirov =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 198 || offsetMinutes == 199 then
                Ok Lmt

            else if offsetMinutes == 300 then
                Ok (Offset offsetMinutes)

            else if offsetMinutes == 240 then
                Ok
                    (if posixSeconds >= 1301180400 then
                        ShortName "MSK"

                     else if posixSeconds >= 701820000 then
                        ShortName "MSD"

                     else if posixSeconds >= 670374000 then
                        Offset offsetMinutes

                     else if posixSeconds >= 606866400 then
                        ShortName "MSD"

                     else
                        Offset offsetMinutes
                    )

            else if offsetMinutes == 180 then
                Ok
                    (if posixSeconds >= 622594800 then
                        ShortName "MSK"

                     else
                        Offset offsetMinutes
                    )

            else
                Err offsetMinutes
        )


{-| `Europe/Kyiv`
-}
for__europe__kyiv : Zone -> Posix -> Result Int Abbreviation
for__europe__kyiv =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 122 || offsetMinutes == 123 then
                Ok
                    (if posixSeconds >= -2840148124 then
                        ShortName "KMT"

                     else
                        Lmt
                    )

            else if offsetMinutes == 60 then
                Ok (ShortName "CET")

            else if offsetMinutes == 120 then
                Ok
                    (if posixSeconds >= 686102400 then
                        ShortName "EET"

                     else if posixSeconds >= -892522800 then
                        ShortName "CEST"

                     else
                        ShortName "EET"
                    )

            else if offsetMinutes == 240 then
                Ok (ShortName "MSD")

            else if offsetMinutes == 180 then
                Ok
                    (if posixSeconds >= 646783200 then
                        ShortName "EEST"

                     else
                        ShortName "MSK"
                    )

            else
                Err offsetMinutes
        )


{-| `Europe/Lisbon`
-}
for__europe__lisbon : Zone -> Posix -> Result Int Abbreviation
for__europe__lisbon =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -37 || offsetMinutes == -36 then
                Ok Lmt

            else if offsetMinutes == 120 then
                Ok
                    (if posixSeconds >= 733280400 then
                        ShortName "CEST"

                     else
                        ShortName "WEMT"
                    )

            else if offsetMinutes == 60 then
                Ok
                    (if posixSeconds >= 828234000 then
                        ShortName "WEST"

                     else if posixSeconds >= 717555600 then
                        ShortName "CET"

                     else if posixSeconds >= 228268800 then
                        ShortName "WEST"

                     else if posixSeconds >= -102549600 then
                        ShortName "CET"

                     else
                        ShortName "WEST"
                    )

            else if offsetMinutes == 0 then
                Ok (ShortName "WET")

            else
                Err offsetMinutes
        )


{-| `Europe/London`
-}
for__europe__london : Zone -> Posix -> Result Int Abbreviation
for__europe__london =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -2 || offsetMinutes == -1 then
                Ok Lmt

            else if offsetMinutes == 120 then
                Ok (ShortName "BDST")

            else if offsetMinutes == 60 then
                Ok (ShortName "BST")

            else if offsetMinutes == 0 then
                Ok (ShortName "GMT")

            else
                Err offsetMinutes
        )


{-| `Europe/Madrid`
-}
for__europe__madrid : Zone -> Posix -> Result Int Abbreviation
for__europe__madrid =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -15 || offsetMinutes == -14 then
                Ok Lmt

            else if offsetMinutes == 120 then
                Ok
                    (if posixSeconds >= -873079200 then
                        ShortName "CEST"

                     else
                        ShortName "WEMT"
                    )

            else if offsetMinutes == 60 then
                Ok
                    (if posixSeconds >= -940208400 then
                        ShortName "CET"

                     else
                        ShortName "WEST"
                    )

            else if offsetMinutes == 0 then
                Ok (ShortName "WET")

            else
                Err offsetMinutes
        )


{-| `Europe/Malta`
-}
for__europe__malta : Zone -> Posix -> Result Int Abbreviation
for__europe__malta =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 58 || offsetMinutes == 59 then
                Ok Lmt

            else if offsetMinutes == 120 then
                Ok (ShortName "CEST")

            else if offsetMinutes == 60 then
                Ok (ShortName "CET")

            else
                Err offsetMinutes
        )


{-| `Europe/Minsk`
-}
for__europe__minsk : Zone -> Posix -> Result Int Abbreviation
for__europe__minsk =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 111 then
                Ok Lmt

            else if offsetMinutes == 110 then
                Ok
                    (if posixSeconds >= -2840147416 then
                        ShortName "MMT"

                     else
                        Lmt
                    )

            else if offsetMinutes == 60 then
                Ok (ShortName "CET")

            else if offsetMinutes == 120 then
                Ok
                    (if posixSeconds >= 686102400 then
                        ShortName "EET"

                     else if posixSeconds >= -899780400 then
                        ShortName "CEST"

                     else
                        ShortName "EET"
                    )

            else if offsetMinutes == 240 then
                Ok (ShortName "MSD")

            else if offsetMinutes == 180 then
                Ok
                    (if posixSeconds >= 1301184000 then
                        Offset offsetMinutes

                     else if posixSeconds >= 670374000 then
                        ShortName "EEST"

                     else
                        ShortName "MSK"
                    )

            else
                Err offsetMinutes
        )


{-| `Europe/Moscow`
-}
for__europe__moscow : Zone -> Posix -> Result Int Abbreviation
for__europe__moscow =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 150 || offsetMinutes == 151 then
                Ok
                    (if posixSeconds >= -2840149817 then
                        ShortName "MMT"

                     else
                        Lmt
                    )

            else if offsetMinutes == 300 then
                Ok (Offset offsetMinutes)

            else if offsetMinutes == 120 then
                Ok (ShortName "EET")

            else if offsetMinutes == 271 || offsetMinutes == 272 then
                Ok (ShortName "MDST")

            else if offsetMinutes == 152 then
                Ok (ShortName "MMT")

            else if offsetMinutes == 240 then
                Ok
                    (if posixSeconds >= 1301180400 then
                        ShortName "MSK"

                     else
                        ShortName "MSD"
                    )

            else if offsetMinutes == 180 then
                Ok
                    (if posixSeconds >= 695779200 then
                        ShortName "MSK"

                     else if posixSeconds >= 670374000 then
                        ShortName "EEST"

                     else
                        ShortName "MSK"
                    )

            else if offsetMinutes == 211 || offsetMinutes == 212 then
                Ok (ShortName "MST")

            else
                Err offsetMinutes
        )


{-| `Europe/Paris`
-}
for__europe__paris : Zone -> Posix -> Result Int Abbreviation
for__europe__paris =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 9 || offsetMinutes == 10 then
                Ok
                    (if posixSeconds >= -2486592561 then
                        ShortName "PMT"

                     else
                        Lmt
                    )

            else if offsetMinutes == 120 then
                Ok
                    (if posixSeconds >= 196819200 then
                        ShortName "CEST"

                     else if posixSeconds >= -800071200 then
                        ShortName "WEMT"

                     else
                        ShortName "CEST"
                    )

            else if offsetMinutes == 60 then
                Ok
                    (if posixSeconds >= -766623600 then
                        ShortName "CET"

                     else if posixSeconds >= -796266000 then
                        ShortName "WEST"

                     else if posixSeconds >= -857257200 then
                        ShortName "CET"

                     else
                        ShortName "WEST"
                    )

            else if offsetMinutes == 0 then
                Ok (ShortName "WET")

            else
                Err offsetMinutes
        )


{-| `Europe/Prague`
-}
for__europe__prague : Zone -> Posix -> Result Int Abbreviation
for__europe__prague =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 57 || offsetMinutes == 58 then
                Ok
                    (if posixSeconds >= -3786829064 then
                        ShortName "PMT"

                     else
                        Lmt
                    )

            else if offsetMinutes == 120 then
                Ok (ShortName "CEST")

            else if offsetMinutes == 60 then
                Ok (ShortName "CET")

            else if offsetMinutes == 0 then
                Ok (ShortName "GMT")

            else
                Err offsetMinutes
        )


{-| `Europe/Riga`
-}
for__europe__riga : Zone -> Posix -> Result Int Abbreviation
for__europe__riga =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 96 || offsetMinutes == 97 then
                Ok
                    (if posixSeconds >= -2840146594 then
                        ShortName "RMT"

                     else
                        Lmt
                    )

            else if offsetMinutes == 60 then
                Ok (ShortName "CET")

            else if offsetMinutes == 120 then
                Ok
                    (if posixSeconds >= 622598400 then
                        ShortName "EET"

                     else if posixSeconds >= -899521200 then
                        ShortName "CEST"

                     else
                        ShortName "EET"
                    )

            else if offsetMinutes == 156 || offsetMinutes == 157 then
                Ok (ShortName "LST")

            else if offsetMinutes == 240 then
                Ok (ShortName "MSD")

            else if offsetMinutes == 180 then
                Ok
                    (if posixSeconds >= 606870000 then
                        ShortName "EEST"

                     else
                        ShortName "MSK"
                    )

            else
                Err offsetMinutes
        )


{-| `Europe/Rome`
-}
for__europe__rome : Zone -> Posix -> Result Int Abbreviation
for__europe__rome =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 49 || offsetMinutes == 50 then
                Ok
                    (if posixSeconds >= -3252098996 then
                        ShortName "RMT"

                     else
                        Lmt
                    )

            else if offsetMinutes == 120 then
                Ok (ShortName "CEST")

            else if offsetMinutes == 60 then
                Ok (ShortName "CET")

            else
                Err offsetMinutes
        )


{-| `Europe/Samara`
-}
for__europe__samara : Zone -> Posix -> Result Int Abbreviation
for__europe__samara =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 200 || offsetMinutes == 201 then
                Ok Lmt

            else if offsetMinutes == 180 || offsetMinutes == 240 || offsetMinutes == 300 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `Europe/Saratov`
-}
for__europe__saratov : Zone -> Posix -> Result Int Abbreviation
for__europe__saratov =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 184 || offsetMinutes == 185 then
                Ok Lmt

            else if offsetMinutes == 180 || offsetMinutes == 240 || offsetMinutes == 300 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `Europe/Simferopol`
-}
for__europe__simferopol : Zone -> Posix -> Result Int Abbreviation
for__europe__simferopol =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 137 then
                Ok Lmt

            else if offsetMinutes == 136 then
                Ok
                    (if posixSeconds >= -2840148984 then
                        ShortName "SMT"

                     else
                        Lmt
                    )

            else if offsetMinutes == 60 then
                Ok (ShortName "CET")

            else if offsetMinutes == 120 then
                Ok
                    (if posixSeconds >= 646786800 then
                        ShortName "EET"

                     else if posixSeconds >= -888894000 then
                        ShortName "CEST"

                     else
                        ShortName "EET"
                    )

            else if offsetMinutes == 240 then
                Ok
                    (if posixSeconds >= 1396137600 then
                        ShortName "MSK"

                     else
                        ShortName "MSD"
                    )

            else if offsetMinutes == 180 then
                Ok
                    (if posixSeconds >= 1414274400 then
                        ShortName "MSK"

                     else if posixSeconds >= 859683600 then
                        ShortName "EEST"

                     else if posixSeconds >= 780447600 then
                        ShortName "MSK"

                     else if posixSeconds >= 701827200 then
                        ShortName "EEST"

                     else
                        ShortName "MSK"
                    )

            else
                Err offsetMinutes
        )


{-| `Europe/Sofia`
-}
for__europe__sofia : Zone -> Posix -> Result Int Abbreviation
for__europe__sofia =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 93 || offsetMinutes == 94 then
                Ok Lmt

            else if offsetMinutes == 60 then
                Ok (ShortName "CET")

            else if offsetMinutes == 180 then
                Ok (ShortName "EEST")

            else if offsetMinutes == 120 then
                Ok
                    (if posixSeconds >= -781048800 then
                        ShortName "EET"

                     else if posixSeconds >= -844556400 then
                        ShortName "CEST"

                     else
                        ShortName "EET"
                    )

            else if offsetMinutes == 116 || offsetMinutes == 117 then
                Ok (ShortName "IMT")

            else
                Err offsetMinutes
        )


{-| `Europe/Tallinn`
-}
for__europe__tallinn : Zone -> Posix -> Result Int Abbreviation
for__europe__tallinn =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 99 then
                Ok
                    (if posixSeconds >= -2840146740 then
                        ShortName "TMT"

                     else
                        Lmt
                    )

            else if offsetMinutes == 120 then
                Ok
                    (if posixSeconds >= 622598400 then
                        ShortName "EET"

                     else if posixSeconds >= -892954800 then
                        ShortName "CEST"

                     else if posixSeconds >= -1535938740 then
                        ShortName "EET"

                     else
                        ShortName "CEST"
                    )

            else if offsetMinutes == 60 then
                Ok (ShortName "CET")

            else if offsetMinutes == 240 then
                Ok (ShortName "MSD")

            else if offsetMinutes == 180 then
                Ok
                    (if posixSeconds >= 606870000 then
                        ShortName "EEST"

                     else
                        ShortName "MSK"
                    )

            else
                Err offsetMinutes
        )


{-| `Europe/Tirane`
-}
for__europe__tirane : Zone -> Posix -> Result Int Abbreviation
for__europe__tirane =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 79 || offsetMinutes == 80 then
                Ok Lmt

            else if offsetMinutes == 120 then
                Ok (ShortName "CEST")

            else if offsetMinutes == 60 then
                Ok (ShortName "CET")

            else
                Err offsetMinutes
        )


{-| `Europe/Ulyanovsk`
-}
for__europe__ulyanovsk : Zone -> Posix -> Result Int Abbreviation
for__europe__ulyanovsk =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 193 || offsetMinutes == 194 then
                Ok Lmt

            else if offsetMinutes == 120 || offsetMinutes == 180 || offsetMinutes == 240 || offsetMinutes == 300 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `Europe/Vienna`
-}
for__europe__vienna : Zone -> Posix -> Result Int Abbreviation
for__europe__vienna =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 65 || offsetMinutes == 66 then
                Ok Lmt

            else if offsetMinutes == 120 then
                Ok (ShortName "CEST")

            else if offsetMinutes == 60 then
                Ok (ShortName "CET")

            else
                Err offsetMinutes
        )


{-| `Europe/Vilnius`
-}
for__europe__vilnius : Zone -> Posix -> Result Int Abbreviation
for__europe__vilnius =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 101 || offsetMinutes == 102 then
                Ok Lmt

            else if offsetMinutes == 60 then
                Ok (ShortName "CET")

            else if offsetMinutes == 120 then
                Ok
                    (if posixSeconds >= 941331600 then
                        ShortName "EET"

                     else if posixSeconds >= 891133200 then
                        ShortName "CEST"

                     else if posixSeconds >= 622598400 then
                        ShortName "EET"

                     else if posixSeconds >= -900126000 then
                        ShortName "CEST"

                     else
                        ShortName "EET"
                    )

            else if offsetMinutes == 95 || offsetMinutes == 96 then
                Ok (ShortName "KMT")

            else if offsetMinutes == 240 then
                Ok (ShortName "MSD")

            else if offsetMinutes == 180 then
                Ok
                    (if posixSeconds >= 606870000 then
                        ShortName "EEST"

                     else
                        ShortName "MSK"
                    )

            else if offsetMinutes == 84 then
                Ok (ShortName "WMT")

            else
                Err offsetMinutes
        )


{-| `Europe/Volgograd`
-}
for__europe__volgograd : Zone -> Posix -> Result Int Abbreviation
for__europe__volgograd =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 177 || offsetMinutes == 178 then
                Ok Lmt

            else if offsetMinutes == 300 then
                Ok (Offset offsetMinutes)

            else if offsetMinutes == 240 then
                Ok
                    (if posixSeconds >= 1540681200 then
                        Offset offsetMinutes

                     else if posixSeconds >= 1301180400 then
                        ShortName "MSK"

                     else if posixSeconds >= 701820000 then
                        ShortName "MSD"

                     else if posixSeconds >= 670374000 then
                        Offset offsetMinutes

                     else if posixSeconds >= 575416800 then
                        ShortName "MSD"

                     else
                        Offset offsetMinutes
                    )

            else if offsetMinutes == 180 then
                Ok
                    (if posixSeconds >= 591145200 then
                        ShortName "MSK"

                     else
                        Offset offsetMinutes
                    )

            else
                Err offsetMinutes
        )


{-| `Europe/Warsaw`
-}
for__europe__warsaw : Zone -> Posix -> Result Int Abbreviation
for__europe__warsaw =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 84 then
                Ok
                    (if posixSeconds >= -2840145840 then
                        ShortName "WMT"

                     else
                        Lmt
                    )

            else if offsetMinutes == 120 then
                Ok
                    (if posixSeconds >= -931734000 then
                        ShortName "CEST"

                     else if posixSeconds >= -1618700400 then
                        ShortName "EET"

                     else
                        ShortName "CEST"
                    )

            else if offsetMinutes == 60 then
                Ok (ShortName "CET")

            else if offsetMinutes == 180 then
                Ok (ShortName "EEST")

            else
                Err offsetMinutes
        )


{-| `Europe/Zurich`
-}
for__europe__zurich : Zone -> Posix -> Result Int Abbreviation
for__europe__zurich =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 34 || offsetMinutes == 35 then
                Ok Lmt

            else if offsetMinutes == 29 || offsetMinutes == 30 then
                Ok (ShortName "BMT")

            else if offsetMinutes == 120 then
                Ok (ShortName "CEST")

            else if offsetMinutes == 60 then
                Ok (ShortName "CET")

            else
                Err offsetMinutes
        )


{-| `Indian/Chagos`
-}
for__indian__chagos : Zone -> Posix -> Result Int Abbreviation
for__indian__chagos =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 289 || offsetMinutes == 290 then
                Ok Lmt

            else if offsetMinutes == 300 || offsetMinutes == 360 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `Indian/Maldives`
-}
for__indian__maldives : Zone -> Posix -> Result Int Abbreviation
for__indian__maldives =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 294 then
                Ok
                    (if posixSeconds >= -2840158440 then
                        ShortName "MMT"

                     else
                        Lmt
                    )

            else if offsetMinutes == 300 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `Indian/Mauritius`
-}
for__indian__mauritius : Zone -> Posix -> Result Int Abbreviation
for__indian__mauritius =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 230 then
                Ok Lmt

            else if offsetMinutes == 240 || offsetMinutes == 300 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `Pacific/Apia`
-}
for__pacific__apia : Zone -> Posix -> Result Int Abbreviation
for__pacific__apia =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -687 || offsetMinutes == -686 || offsetMinutes == 753 || offsetMinutes == 754 then
                Ok Lmt

            else if offsetMinutes == -690 || offsetMinutes == -660 || offsetMinutes == -600 || offsetMinutes == 780 || offsetMinutes == 840 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `Pacific/Auckland`
-}
for__pacific__auckland : Zone -> Posix -> Result Int Abbreviation
for__pacific__auckland =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 699 || offsetMinutes == 700 then
                Ok Lmt

            else if offsetMinutes == 780 then
                Ok (ShortName "NZDT")

            else if offsetMinutes == 690 then
                Ok (ShortName "NZMT")

            else if offsetMinutes == 720 || offsetMinutes == 750 then
                Ok (ShortName "NZST")

            else
                Err offsetMinutes
        )


{-| `Pacific/Bougainville`
-}
for__pacific__bougainville : Zone -> Posix -> Result Int Abbreviation
for__pacific__bougainville =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 622 || offsetMinutes == 623 then
                Ok Lmt

            else if offsetMinutes == 540 || offsetMinutes == 600 || offsetMinutes == 660 then
                Ok (Offset offsetMinutes)

            else if offsetMinutes == 588 || offsetMinutes == 589 then
                Ok (ShortName "PMMT")

            else
                Err offsetMinutes
        )


{-| `Pacific/Chatham`
-}
for__pacific__chatham : Zone -> Posix -> Result Int Abbreviation
for__pacific__chatham =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 733 || offsetMinutes == 734 then
                Ok Lmt

            else if offsetMinutes == 735 || offsetMinutes == 765 || offsetMinutes == 825 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `Pacific/Easter`
-}
for__pacific__easter : Zone -> Posix -> Result Int Abbreviation
for__pacific__easter =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -438 || offsetMinutes == -437 then
                Ok
                    (if posixSeconds >= -2524495352 then
                        ShortName "EMT"

                     else
                        Lmt
                    )

            else if offsetMinutes == -420 || offsetMinutes == -360 || offsetMinutes == -300 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `Pacific/Efate`
-}
for__pacific__efate : Zone -> Posix -> Result Int Abbreviation
for__pacific__efate =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 673 || offsetMinutes == 674 then
                Ok Lmt

            else if offsetMinutes == 660 || offsetMinutes == 720 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `Pacific/Fakaofo`
-}
for__pacific__fakaofo : Zone -> Posix -> Result Int Abbreviation
for__pacific__fakaofo =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -685 || offsetMinutes == -684 then
                Ok Lmt

            else if offsetMinutes == -660 || offsetMinutes == 780 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `Pacific/Fiji`
-}
for__pacific__fiji : Zone -> Posix -> Result Int Abbreviation
for__pacific__fiji =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 715 || offsetMinutes == 716 then
                Ok Lmt

            else if offsetMinutes == 720 || offsetMinutes == 780 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `Pacific/Galapagos`
-}
for__pacific__galapagos : Zone -> Posix -> Result Int Abbreviation
for__pacific__galapagos =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -359 || offsetMinutes == -358 then
                Ok Lmt

            else if offsetMinutes == -360 || offsetMinutes == -300 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `Pacific/Gambier`
-}
for__pacific__gambier : Zone -> Posix -> Result Int Abbreviation
for__pacific__gambier =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -539 then
                Ok Lmt

            else if offsetMinutes == -540 then
                Ok
                    (if posixSeconds >= -1806678012 then
                        Offset offsetMinutes

                     else
                        Lmt
                    )

            else
                Err offsetMinutes
        )


{-| `Pacific/Guadalcanal`
-}
for__pacific__guadalcanal : Zone -> Posix -> Result Int Abbreviation
for__pacific__guadalcanal =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 639 || offsetMinutes == 640 then
                Ok Lmt

            else if offsetMinutes == 660 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `Pacific/Guam`
-}
for__pacific__guam : Zone -> Posix -> Result Int Abbreviation
for__pacific__guam =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -861 || offsetMinutes == 579 then
                Ok Lmt

            else if offsetMinutes == 540 then
                Ok (Offset offsetMinutes)

            else if offsetMinutes == 660 then
                Ok (ShortName "GDT")

            else if offsetMinutes == 600 then
                Ok
                    (if posixSeconds >= 977493600 then
                        ShortName "ChST"

                     else
                        ShortName "GST"
                    )

            else
                Err offsetMinutes
        )


{-| `Pacific/Honolulu`
-}
for__pacific__honolulu : Zone -> Posix -> Result Int Abbreviation
for__pacific__honolulu =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -632 || offsetMinutes == -631 then
                Ok Lmt

            else if offsetMinutes == -570 then
                Ok
                    (if posixSeconds >= -769395600 then
                        ShortName "HPT"

                     else if posixSeconds >= -880198200 then
                        ShortName "HWT"

                     else
                        ShortName "HDT"
                    )

            else if offsetMinutes == -630 || offsetMinutes == -600 then
                Ok (ShortName "HST")

            else
                Err offsetMinutes
        )


{-| `Pacific/Kanton`
-}
for__pacific__kanton : Zone -> Posix -> Result Int Abbreviation
for__pacific__kanton =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -720 || offsetMinutes == -660 || offsetMinutes == 780 then
                Ok (Offset offsetMinutes)

            else if offsetMinutes == 0 then
                Ok Uninhabited

            else
                Err offsetMinutes
        )


{-| `Pacific/Kiritimati`
-}
for__pacific__kiritimati : Zone -> Posix -> Result Int Abbreviation
for__pacific__kiritimati =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -630 || offsetMinutes == -629 then
                Ok Lmt

            else if offsetMinutes == -640 || offsetMinutes == -600 || offsetMinutes == 840 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `Pacific/Kosrae`
-}
for__pacific__kosrae : Zone -> Posix -> Result Int Abbreviation
for__pacific__kosrae =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -789 || offsetMinutes == -788 || offsetMinutes == 651 || offsetMinutes == 652 then
                Ok Lmt

            else if offsetMinutes == 540 || offsetMinutes == 600 || offsetMinutes == 660 || offsetMinutes == 720 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `Pacific/Kwajalein`
-}
for__pacific__kwajalein : Zone -> Posix -> Result Int Abbreviation
for__pacific__kwajalein =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 669 || offsetMinutes == 670 then
                Ok Lmt

            else if offsetMinutes == -720 || offsetMinutes == 540 || offsetMinutes == 600 || offsetMinutes == 660 || offsetMinutes == 720 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `Pacific/Marquesas`
-}
for__pacific__marquesas : Zone -> Posix -> Result Int Abbreviation
for__pacific__marquesas =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -558 then
                Ok Lmt

            else if offsetMinutes == -570 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `Pacific/Nauru`
-}
for__pacific__nauru : Zone -> Posix -> Result Int Abbreviation
for__pacific__nauru =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 667 || offsetMinutes == 668 then
                Ok Lmt

            else if offsetMinutes == 540 || offsetMinutes == 690 || offsetMinutes == 720 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `Pacific/Niue`
-}
for__pacific__niue : Zone -> Posix -> Result Int Abbreviation
for__pacific__niue =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -679 then
                Ok Lmt

            else if offsetMinutes == -680 then
                Ok
                    (if posixSeconds >= -543069620 then
                        Offset offsetMinutes

                     else
                        Lmt
                    )

            else if offsetMinutes == -660 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `Pacific/Norfolk`
-}
for__pacific__norfolk : Zone -> Posix -> Result Int Abbreviation
for__pacific__norfolk =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 671 then
                Ok Lmt

            else if offsetMinutes == 672 then
                Ok
                    (if posixSeconds >= -2177493112 then
                        Offset offsetMinutes

                     else
                        Lmt
                    )

            else if offsetMinutes == 660 || offsetMinutes == 690 || offsetMinutes == 750 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `Pacific/Noumea`
-}
for__pacific__noumea : Zone -> Posix -> Result Int Abbreviation
for__pacific__noumea =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 665 || offsetMinutes == 666 then
                Ok Lmt

            else if offsetMinutes == 660 || offsetMinutes == 720 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `Pacific/Pago_Pago`
-}
for__pacific__pago_pago : Zone -> Posix -> Result Int Abbreviation
for__pacific__pago_pago =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -683 || offsetMinutes == -682 || offsetMinutes == 757 || offsetMinutes == 758 then
                Ok Lmt

            else if offsetMinutes == -660 then
                Ok (ShortName "SST")

            else
                Err offsetMinutes
        )


{-| `Pacific/Palau`
-}
for__pacific__palau : Zone -> Posix -> Result Int Abbreviation
for__pacific__palau =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -903 || offsetMinutes == -902 || offsetMinutes == 537 || offsetMinutes == 538 then
                Ok Lmt

            else if offsetMinutes == 540 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `Pacific/Pitcairn`
-}
for__pacific__pitcairn : Zone -> Posix -> Result Int Abbreviation
for__pacific__pitcairn =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -521 || offsetMinutes == -520 then
                Ok Lmt

            else if offsetMinutes == -510 || offsetMinutes == -480 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `Pacific/Port_Moresby`
-}
for__pacific__port_moresby : Zone -> Posix -> Result Int Abbreviation
for__pacific__port_moresby =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 588 || offsetMinutes == 589 then
                Ok
                    (if posixSeconds >= -2840176120 then
                        ShortName "PMMT"

                     else
                        Lmt
                    )

            else if offsetMinutes == 600 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `Pacific/Rarotonga`
-}
for__pacific__rarotonga : Zone -> Posix -> Result Int Abbreviation
for__pacific__rarotonga =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -640 || offsetMinutes == -639 || offsetMinutes == 800 || offsetMinutes == 801 then
                Ok Lmt

            else if offsetMinutes == -630 || offsetMinutes == -600 || offsetMinutes == -570 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `Pacific/Tahiti`
-}
for__pacific__tahiti : Zone -> Posix -> Result Int Abbreviation
for__pacific__tahiti =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == -599 || offsetMinutes == -598 then
                Ok Lmt

            else if offsetMinutes == -600 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `Pacific/Tarawa`
-}
for__pacific__tarawa : Zone -> Posix -> Result Int Abbreviation
for__pacific__tarawa =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 692 || offsetMinutes == 693 then
                Ok Lmt

            else if offsetMinutes == 720 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )


{-| `Pacific/Tongatapu`
-}
for__pacific__tongatapu : Zone -> Posix -> Result Int Abbreviation
for__pacific__tongatapu =
    Internal.toTime
        (\offsetMinutes posixSeconds ->
            if offsetMinutes == 739 then
                Ok Lmt

            else if offsetMinutes == 740 then
                Ok
                    (if posixSeconds >= -767189952 then
                        Offset offsetMinutes

                     else
                        Lmt
                    )

            else if offsetMinutes == 780 || offsetMinutes == 840 then
                Ok (Offset offsetMinutes)

            else
                Err offsetMinutes
        )
