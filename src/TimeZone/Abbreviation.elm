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
                    (\{ offsetMinutes } ->
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
                (.offsetMinutes >> Offset)
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
        (\time ->
            if time.offsetMinutes == -17 || time.offsetMinutes == -16 then
                Ok Lmt

            else if time.offsetMinutes == 0 then
                Ok (ShortName "GMT")

            else
                Err time.offsetMinutes
        )


{-| `Africa/Algiers`
-}
for__africa__algiers : Zone -> Posix -> Result Int Abbreviation
for__africa__algiers =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 12 || time.offsetMinutes == 13 then
                Ok Lmt

            else if time.offsetMinutes == 120 then
                Ok (ShortName "CEST")

            else if time.offsetMinutes == 9 || time.offsetMinutes == 10 then
                Ok (ShortName "PMT")

            else if time.offsetMinutes == 60 then
                Ok
                    (if time.posixSeconds >= 357523200 then
                        ShortName "CET"

                     else if time.posixSeconds >= 325468800 then
                        ShortName "WEST"

                     else if time.posixSeconds >= 246236400 then
                        ShortName "CET"

                     else if time.posixSeconds >= 41468400 then
                        ShortName "WEST"

                     else if time.posixSeconds >= -942012000 then
                        ShortName "CET"

                     else
                        ShortName "WEST"
                    )

            else if time.offsetMinutes == 0 then
                Ok (ShortName "WET")

            else
                Err time.offsetMinutes
        )


{-| `Africa/Bissau`
-}
for__africa__bissau : Zone -> Posix -> Result Int Abbreviation
for__africa__bissau =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -63 || time.offsetMinutes == -62 then
                Ok Lmt

            else if time.offsetMinutes == -60 then
                Ok (Offset time.offsetMinutes)

            else if time.offsetMinutes == 0 then
                Ok (ShortName "GMT")

            else
                Err time.offsetMinutes
        )


{-| `Africa/Cairo`
-}
for__africa__cairo : Zone -> Posix -> Result Int Abbreviation
for__africa__cairo =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 125 || time.offsetMinutes == 126 then
                Ok Lmt

            else if time.offsetMinutes == 180 then
                Ok (ShortName "EEST")

            else if time.offsetMinutes == 120 then
                Ok (ShortName "EET")

            else
                Err time.offsetMinutes
        )


{-| `Africa/Casablanca`
-}
for__africa__casablanca : Zone -> Posix -> Result Int Abbreviation
for__africa__casablanca =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -31 || time.offsetMinutes == -30 then
                Ok Lmt

            else if time.offsetMinutes == 0 || time.offsetMinutes == 60 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `Africa/Ceuta`
-}
for__africa__ceuta : Zone -> Posix -> Result Int Abbreviation
for__africa__ceuta =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -22 || time.offsetMinutes == -21 then
                Ok Lmt

            else if time.offsetMinutes == 120 then
                Ok (ShortName "CEST")

            else if time.offsetMinutes == 60 then
                Ok
                    (if time.posixSeconds >= 448243200 then
                        ShortName "CET"

                     else
                        ShortName "WEST"
                    )

            else if time.offsetMinutes == 0 then
                Ok (ShortName "WET")

            else
                Err time.offsetMinutes
        )


{-| `Africa/El_Aaiun`
-}
for__africa__el_aaiun : Zone -> Posix -> Result Int Abbreviation
for__africa__el_aaiun =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -53 || time.offsetMinutes == -52 then
                Ok Lmt

            else if time.offsetMinutes == -60 || time.offsetMinutes == 0 || time.offsetMinutes == 60 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `Africa/Johannesburg`
-}
for__africa__johannesburg : Zone -> Posix -> Result Int Abbreviation
for__africa__johannesburg =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 112 then
                Ok Lmt

            else if time.offsetMinutes == 90 || time.offsetMinutes == 120 || time.offsetMinutes == 180 then
                Ok (ShortName "SAST")

            else
                Err time.offsetMinutes
        )


{-| `Africa/Juba`
-}
for__africa__juba : Zone -> Posix -> Result Int Abbreviation
for__africa__juba =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 126 || time.offsetMinutes == 127 then
                Ok Lmt

            else if time.offsetMinutes == 180 then
                Ok
                    (if time.posixSeconds >= 947930400 then
                        ShortName "EAT"

                     else
                        ShortName "CAST"
                    )

            else if time.offsetMinutes == 120 then
                Ok (ShortName "CAT")

            else
                Err time.offsetMinutes
        )


{-| `Africa/Khartoum`
-}
for__africa__khartoum : Zone -> Posix -> Result Int Abbreviation
for__africa__khartoum =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 130 || time.offsetMinutes == 131 then
                Ok Lmt

            else if time.offsetMinutes == 180 then
                Ok
                    (if time.posixSeconds >= 947930400 then
                        ShortName "EAT"

                     else
                        ShortName "CAST"
                    )

            else if time.offsetMinutes == 120 then
                Ok (ShortName "CAT")

            else
                Err time.offsetMinutes
        )


{-| `Africa/Lagos`
-}
for__africa__lagos : Zone -> Posix -> Result Int Abbreviation
for__africa__lagos =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 13 || time.offsetMinutes == 14 then
                Ok Lmt

            else if time.offsetMinutes == 30 then
                Ok (Offset time.offsetMinutes)

            else if time.offsetMinutes == 0 then
                Ok (ShortName "GMT")

            else if time.offsetMinutes == 60 then
                Ok (ShortName "WAT")

            else
                Err time.offsetMinutes
        )


{-| `Africa/Maputo`
-}
for__africa__maputo : Zone -> Posix -> Result Int Abbreviation
for__africa__maputo =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 130 || time.offsetMinutes == 131 then
                Ok Lmt

            else if time.offsetMinutes == 120 then
                Ok (ShortName "CAT")

            else
                Err time.offsetMinutes
        )


{-| `Africa/Monrovia`
-}
for__africa__monrovia : Zone -> Posix -> Result Int Abbreviation
for__africa__monrovia =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -44 || time.offsetMinutes == -43 then
                Ok
                    (if time.posixSeconds >= -2776979812 then
                        ShortName "MMT"

                     else
                        Lmt
                    )

            else if time.offsetMinutes == 0 then
                Ok (ShortName "GMT")

            else if time.offsetMinutes == -45 then
                Ok (ShortName "MMT")

            else
                Err time.offsetMinutes
        )


{-| `Africa/Nairobi`
-}
for__africa__nairobi : Zone -> Posix -> Result Int Abbreviation
for__africa__nairobi =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 147 || time.offsetMinutes == 148 then
                Ok Lmt

            else if time.offsetMinutes == 150 || time.offsetMinutes == 165 then
                Ok (Offset time.offsetMinutes)

            else if time.offsetMinutes == 180 then
                Ok (ShortName "EAT")

            else
                Err time.offsetMinutes
        )


{-| `Africa/Ndjamena`
-}
for__africa__ndjamena : Zone -> Posix -> Result Int Abbreviation
for__africa__ndjamena =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 61 then
                Ok Lmt

            else if time.offsetMinutes == 60 then
                Ok
                    (if time.posixSeconds >= -1830387612 then
                        ShortName "WAT"

                     else
                        Lmt
                    )

            else if time.offsetMinutes == 120 then
                Ok (ShortName "WAST")

            else
                Err time.offsetMinutes
        )


{-| `Africa/Sao_Tome`
-}
for__africa__sao_tome : Zone -> Posix -> Result Int Abbreviation
for__africa__sao_tome =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -37 || time.offsetMinutes == -36 || time.offsetMinutes == 26 || time.offsetMinutes == 27 then
                Ok Lmt

            else if time.offsetMinutes == 0 then
                Ok (ShortName "GMT")

            else if time.offsetMinutes == 60 then
                Ok (ShortName "WAT")

            else
                Err time.offsetMinutes
        )


{-| `Africa/Tripoli`
-}
for__africa__tripoli : Zone -> Posix -> Result Int Abbreviation
for__africa__tripoli =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 52 || time.offsetMinutes == 53 then
                Ok Lmt

            else if time.offsetMinutes == 120 then
                Ok
                    (if time.posixSeconds >= 1382659200 then
                        ShortName "EET"

                     else if time.posixSeconds >= 1364515200 then
                        ShortName "CEST"

                     else if time.posixSeconds >= 875916000 then
                        ShortName "EET"

                     else if time.posixSeconds >= 860108400 then
                        ShortName "CEST"

                     else if time.posixSeconds >= 641775600 then
                        ShortName "EET"

                     else if time.posixSeconds >= 386463600 then
                        ShortName "CEST"

                     else if time.posixSeconds >= -347158800 then
                        ShortName "EET"

                     else
                        ShortName "CEST"
                    )

            else if time.offsetMinutes == 60 then
                Ok (ShortName "CET")

            else
                Err time.offsetMinutes
        )


{-| `Africa/Tunis`
-}
for__africa__tunis : Zone -> Posix -> Result Int Abbreviation
for__africa__tunis =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 40 || time.offsetMinutes == 41 then
                Ok Lmt

            else if time.offsetMinutes == 120 then
                Ok (ShortName "CEST")

            else if time.offsetMinutes == 60 then
                Ok (ShortName "CET")

            else if time.offsetMinutes == 9 || time.offsetMinutes == 10 then
                Ok (ShortName "PMT")

            else
                Err time.offsetMinutes
        )


{-| `Africa/Windhoek`
-}
for__africa__windhoek : Zone -> Posix -> Result Int Abbreviation
for__africa__windhoek =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 68 || time.offsetMinutes == 69 then
                Ok Lmt

            else if time.offsetMinutes == 90 then
                Ok (Offset time.offsetMinutes)

            else if time.offsetMinutes == 180 then
                Ok (ShortName "SAST")

            else if time.offsetMinutes == 120 then
                Ok
                    (if time.posixSeconds >= 637970400 then
                        ShortName "CAT"

                     else
                        ShortName "SAST"
                    )

            else if time.offsetMinutes == 60 then
                Ok (ShortName "WAT")

            else
                Err time.offsetMinutes
        )


{-| `America/Adak`
-}
for__america__adak : Zone -> Posix -> Result Int Abbreviation
for__america__adak =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -707 || time.offsetMinutes == -706 || time.offsetMinutes == 733 || time.offsetMinutes == 734 then
                Ok Lmt

            else if time.offsetMinutes == -540 then
                Ok (ShortName "HDT")

            else if time.offsetMinutes == -660 then
                Ok
                    (if time.posixSeconds >= -86878800 then
                        ShortName "BST"

                     else
                        ShortName "NST"
                    )

            else if time.offsetMinutes == -600 then
                Ok
                    (if time.posixSeconds >= 439034400 then
                        ShortName "HST"

                     else if time.posixSeconds >= 436363200 then
                        ShortName "AHST"

                     else if time.posixSeconds >= -21466800 then
                        ShortName "BDT"

                     else if time.posixSeconds >= -769395600 then
                        ShortName "NPT"

                     else
                        ShortName "NWT"
                    )

            else
                Err time.offsetMinutes
        )


{-| `America/Anchorage`
-}
for__america__anchorage : Zone -> Posix -> Result Int Abbreviation
for__america__anchorage =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -599 || time.offsetMinutes == 840 || time.offsetMinutes == 841 then
                Ok Lmt

            else if time.offsetMinutes == -600 then
                Ok
                    (if time.posixSeconds >= -86882400 then
                        ShortName "AHST"

                     else if time.posixSeconds >= -2188951224 then
                        ShortName "AST"

                     else
                        Lmt
                    )

            else if time.offsetMinutes == -480 then
                Ok (ShortName "AKDT")

            else if time.offsetMinutes == -540 then
                Ok
                    (if time.posixSeconds >= 439030800 then
                        ShortName "AKST"

                     else if time.posixSeconds >= 436359600 then
                        ShortName "YST"

                     else if time.posixSeconds >= -21470400 then
                        ShortName "AHDT"

                     else if time.posixSeconds >= -769395600 then
                        ShortName "APT"

                     else
                        ShortName "AWT"
                    )

            else
                Err time.offsetMinutes
        )


{-| `America/Araguaina`
-}
for__america__araguaina : Zone -> Posix -> Result Int Abbreviation
for__america__araguaina =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -193 || time.offsetMinutes == -192 then
                Ok Lmt

            else if time.offsetMinutes == -180 || time.offsetMinutes == -120 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `America/Argentina/Buenos_Aires`
-}
for__america__argentina__buenos_aires : Zone -> Posix -> Result Int Abbreviation
for__america__argentina__buenos_aires =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -234 || time.offsetMinutes == -233 then
                Ok Lmt

            else if time.offsetMinutes == -240 || time.offsetMinutes == -180 || time.offsetMinutes == -120 then
                Ok (Offset time.offsetMinutes)

            else if time.offsetMinutes == -257 || time.offsetMinutes == -256 then
                Ok (ShortName "CMT")

            else
                Err time.offsetMinutes
        )


{-| `America/Argentina/Catamarca`
-}
for__america__argentina__catamarca : Zone -> Posix -> Result Int Abbreviation
for__america__argentina__catamarca =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -264 || time.offsetMinutes == -263 then
                Ok Lmt

            else if time.offsetMinutes == -240 || time.offsetMinutes == -180 || time.offsetMinutes == -120 then
                Ok (Offset time.offsetMinutes)

            else if time.offsetMinutes == -257 || time.offsetMinutes == -256 then
                Ok (ShortName "CMT")

            else
                Err time.offsetMinutes
        )


{-| `America/Argentina/Cordoba`
-}
for__america__argentina__cordoba : Zone -> Posix -> Result Int Abbreviation
for__america__argentina__cordoba =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -257 || time.offsetMinutes == -256 then
                Ok
                    (if time.posixSeconds >= -2372096592 then
                        ShortName "CMT"

                     else
                        Lmt
                    )

            else if time.offsetMinutes == -240 || time.offsetMinutes == -180 || time.offsetMinutes == -120 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `America/Argentina/Jujuy`
-}
for__america__argentina__jujuy : Zone -> Posix -> Result Int Abbreviation
for__america__argentina__jujuy =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -262 || time.offsetMinutes == -261 then
                Ok Lmt

            else if time.offsetMinutes == -240 || time.offsetMinutes == -180 || time.offsetMinutes == -120 then
                Ok (Offset time.offsetMinutes)

            else if time.offsetMinutes == -257 || time.offsetMinutes == -256 then
                Ok (ShortName "CMT")

            else
                Err time.offsetMinutes
        )


{-| `America/Argentina/La_Rioja`
-}
for__america__argentina__la_rioja : Zone -> Posix -> Result Int Abbreviation
for__america__argentina__la_rioja =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -268 || time.offsetMinutes == -267 then
                Ok Lmt

            else if time.offsetMinutes == -240 || time.offsetMinutes == -180 || time.offsetMinutes == -120 then
                Ok (Offset time.offsetMinutes)

            else if time.offsetMinutes == -257 || time.offsetMinutes == -256 then
                Ok (ShortName "CMT")

            else
                Err time.offsetMinutes
        )


{-| `America/Argentina/Mendoza`
-}
for__america__argentina__mendoza : Zone -> Posix -> Result Int Abbreviation
for__america__argentina__mendoza =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -276 || time.offsetMinutes == -275 then
                Ok Lmt

            else if time.offsetMinutes == -240 || time.offsetMinutes == -180 || time.offsetMinutes == -120 then
                Ok (Offset time.offsetMinutes)

            else if time.offsetMinutes == -257 || time.offsetMinutes == -256 then
                Ok (ShortName "CMT")

            else
                Err time.offsetMinutes
        )


{-| `America/Argentina/Rio_Gallegos`
-}
for__america__argentina__rio_gallegos : Zone -> Posix -> Result Int Abbreviation
for__america__argentina__rio_gallegos =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -277 || time.offsetMinutes == -276 then
                Ok Lmt

            else if time.offsetMinutes == -240 || time.offsetMinutes == -180 || time.offsetMinutes == -120 then
                Ok (Offset time.offsetMinutes)

            else if time.offsetMinutes == -257 || time.offsetMinutes == -256 then
                Ok (ShortName "CMT")

            else
                Err time.offsetMinutes
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
        (\time ->
            if time.offsetMinutes == -275 || time.offsetMinutes == -274 then
                Ok Lmt

            else if time.offsetMinutes == -240 || time.offsetMinutes == -180 || time.offsetMinutes == -120 then
                Ok (Offset time.offsetMinutes)

            else if time.offsetMinutes == -257 || time.offsetMinutes == -256 then
                Ok (ShortName "CMT")

            else
                Err time.offsetMinutes
        )


{-| `America/Argentina/San_Luis`
-}
for__america__argentina__san_luis : Zone -> Posix -> Result Int Abbreviation
for__america__argentina__san_luis =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -266 || time.offsetMinutes == -265 then
                Ok Lmt

            else if time.offsetMinutes == -240 || time.offsetMinutes == -180 || time.offsetMinutes == -120 then
                Ok (Offset time.offsetMinutes)

            else if time.offsetMinutes == -257 || time.offsetMinutes == -256 then
                Ok (ShortName "CMT")

            else
                Err time.offsetMinutes
        )


{-| `America/Argentina/Tucuman`
-}
for__america__argentina__tucuman : Zone -> Posix -> Result Int Abbreviation
for__america__argentina__tucuman =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -261 || time.offsetMinutes == -260 then
                Ok Lmt

            else if time.offsetMinutes == -240 || time.offsetMinutes == -180 || time.offsetMinutes == -120 then
                Ok (Offset time.offsetMinutes)

            else if time.offsetMinutes == -257 || time.offsetMinutes == -256 then
                Ok (ShortName "CMT")

            else
                Err time.offsetMinutes
        )


{-| `America/Argentina/Ushuaia`
-}
for__america__argentina__ushuaia : Zone -> Posix -> Result Int Abbreviation
for__america__argentina__ushuaia =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -274 || time.offsetMinutes == -273 then
                Ok Lmt

            else if time.offsetMinutes == -240 || time.offsetMinutes == -180 || time.offsetMinutes == -120 then
                Ok (Offset time.offsetMinutes)

            else if time.offsetMinutes == -257 || time.offsetMinutes == -256 then
                Ok (ShortName "CMT")

            else
                Err time.offsetMinutes
        )


{-| `America/Asuncion`
-}
for__america__asuncion : Zone -> Posix -> Result Int Abbreviation
for__america__asuncion =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -231 || time.offsetMinutes == -230 then
                Ok
                    (if time.posixSeconds >= -2524507760 then
                        ShortName "AMT"

                     else
                        Lmt
                    )

            else if time.offsetMinutes == -240 || time.offsetMinutes == -180 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `America/Bahia`
-}
for__america__bahia : Zone -> Posix -> Result Int Abbreviation
for__america__bahia =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -155 || time.offsetMinutes == -154 then
                Ok Lmt

            else if time.offsetMinutes == -180 || time.offsetMinutes == -120 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `America/Bahia_Banderas`
-}
for__america__bahia_banderas : Zone -> Posix -> Result Int Abbreviation
for__america__bahia_banderas =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -421 then
                Ok Lmt

            else if time.offsetMinutes == -300 then
                Ok (ShortName "CDT")

            else if time.offsetMinutes == -360 then
                Ok
                    (if time.posixSeconds >= 1288508400 then
                        ShortName "CST"

                     else if time.posixSeconds >= 828867600 then
                        ShortName "MDT"

                     else if time.posixSeconds >= -1191344400 then
                        ShortName "CST"

                     else if time.posixSeconds >= -1220461200 then
                        ShortName "MDT"

                     else
                        ShortName "CST"
                    )

            else if time.offsetMinutes == -420 then
                Ok (ShortName "MST")

            else
                Err time.offsetMinutes
        )


{-| `America/Barbados`
-}
for__america__barbados : Zone -> Posix -> Result Int Abbreviation
for__america__barbados =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -239 || time.offsetMinutes == -238 then
                Ok Lmt

            else if time.offsetMinutes == -210 then
                Ok (Offset time.offsetMinutes)

            else if time.offsetMinutes == -180 then
                Ok (ShortName "ADT")

            else if time.offsetMinutes == -240 then
                Ok (ShortName "AST")

            else
                Err time.offsetMinutes
        )


{-| `America/Belem`
-}
for__america__belem : Zone -> Posix -> Result Int Abbreviation
for__america__belem =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -194 || time.offsetMinutes == -193 then
                Ok Lmt

            else if time.offsetMinutes == -180 || time.offsetMinutes == -120 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `America/Belize`
-}
for__america__belize : Zone -> Posix -> Result Int Abbreviation
for__america__belize =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -353 || time.offsetMinutes == -352 then
                Ok Lmt

            else if time.offsetMinutes == -330 then
                Ok (Offset time.offsetMinutes)

            else if time.offsetMinutes == -360 then
                Ok (ShortName "CST")

            else if time.offsetMinutes == -300 then
                Ok
                    (if time.posixSeconds >= 123919200 then
                        ShortName "CDT"

                     else if time.posixSeconds >= -769395600 then
                        ShortName "CPT"

                     else
                        ShortName "CWT"
                    )

            else
                Err time.offsetMinutes
        )


{-| `America/Boa_Vista`
-}
for__america__boa_vista : Zone -> Posix -> Result Int Abbreviation
for__america__boa_vista =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -243 || time.offsetMinutes == -242 then
                Ok Lmt

            else if time.offsetMinutes == -240 || time.offsetMinutes == -180 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `America/Bogota`
-}
for__america__bogota : Zone -> Posix -> Result Int Abbreviation
for__america__bogota =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -297 || time.offsetMinutes == -296 then
                Ok
                    (if time.posixSeconds >= -2707671824 then
                        ShortName "BMT"

                     else
                        Lmt
                    )

            else if time.offsetMinutes == -300 || time.offsetMinutes == -240 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `America/Boise`
-}
for__america__boise : Zone -> Posix -> Result Int Abbreviation
for__america__boise =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -465 || time.offsetMinutes == -464 then
                Ok Lmt

            else if time.offsetMinutes == -360 then
                Ok
                    (if time.posixSeconds >= -84380400 then
                        ShortName "MDT"

                     else if time.posixSeconds >= -769395600 then
                        ShortName "MPT"

                     else
                        ShortName "MWT"
                    )

            else if time.offsetMinutes == -420 then
                Ok
                    (if time.posixSeconds >= -1471788000 then
                        ShortName "MST"

                     else
                        ShortName "PDT"
                    )

            else if time.offsetMinutes == -480 then
                Ok (ShortName "PST")

            else
                Err time.offsetMinutes
        )


{-| `America/Cambridge_Bay`
-}
for__america__cambridge_bay : Zone -> Posix -> Result Int Abbreviation
for__america__cambridge_bay =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 0 then
                Ok Uninhabited

            else if time.offsetMinutes == -300 then
                Ok
                    (if time.posixSeconds >= 972802800 then
                        ShortName "EST"

                     else
                        ShortName "CDT"
                    )

            else if time.offsetMinutes == -420 then
                Ok (ShortName "MST")

            else if time.offsetMinutes == -360 then
                Ok
                    (if time.posixSeconds >= 986115600 then
                        ShortName "MDT"

                     else if time.posixSeconds >= 941356800 then
                        ShortName "CST"

                     else if time.posixSeconds >= 73472400 then
                        ShortName "MDT"

                     else if time.posixSeconds >= -769395600 then
                        ShortName "MPT"

                     else
                        ShortName "MWT"
                    )

            else
                Err time.offsetMinutes
        )


{-| `America/Campo_Grande`
-}
for__america__campo_grande : Zone -> Posix -> Result Int Abbreviation
for__america__campo_grande =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -219 || time.offsetMinutes == -218 then
                Ok Lmt

            else if time.offsetMinutes == -240 || time.offsetMinutes == -180 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `America/Cancun`
-}
for__america__cancun : Zone -> Posix -> Result Int Abbreviation
for__america__cancun =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -348 || time.offsetMinutes == -347 then
                Ok Lmt

            else if time.offsetMinutes == -360 then
                Ok (ShortName "CST")

            else if time.offsetMinutes == -240 then
                Ok (ShortName "EDT")

            else if time.offsetMinutes == -300 then
                Ok
                    (if time.posixSeconds >= 1422777600 then
                        ShortName "EST"

                     else if time.posixSeconds >= 902037600 then
                        ShortName "CDT"

                     else if time.posixSeconds >= 877849200 then
                        ShortName "EST"

                     else if time.posixSeconds >= 828864000 then
                        ShortName "CDT"

                     else
                        ShortName "EST"
                    )

            else
                Err time.offsetMinutes
        )


{-| `America/Caracas`
-}
for__america__caracas : Zone -> Posix -> Result Int Abbreviation
for__america__caracas =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -268 || time.offsetMinutes == -267 then
                Ok
                    (if time.posixSeconds >= -2524505536 then
                        ShortName "CMT"

                     else
                        Lmt
                    )

            else if time.offsetMinutes == -270 || time.offsetMinutes == -240 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `America/Cayenne`
-}
for__america__cayenne : Zone -> Posix -> Result Int Abbreviation
for__america__cayenne =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -210 || time.offsetMinutes == -209 then
                Ok Lmt

            else if time.offsetMinutes == -240 || time.offsetMinutes == -180 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `America/Chicago`
-}
for__america__chicago : Zone -> Posix -> Result Int Abbreviation
for__america__chicago =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -351 || time.offsetMinutes == -350 then
                Ok Lmt

            else if time.offsetMinutes == -300 then
                Ok
                    (if time.posixSeconds >= -747244800 then
                        ShortName "CDT"

                     else if time.posixSeconds >= -769395600 then
                        ShortName "CPT"

                     else if time.posixSeconds >= -880214400 then
                        ShortName "CWT"

                     else if time.posixSeconds >= -1031500800 then
                        ShortName "CDT"

                     else if time.posixSeconds >= -1067788800 then
                        ShortName "EST"

                     else
                        ShortName "CDT"
                    )

            else if time.offsetMinutes == -360 then
                Ok (ShortName "CST")

            else
                Err time.offsetMinutes
        )


{-| `America/Chihuahua`
-}
for__america__chihuahua : Zone -> Posix -> Result Int Abbreviation
for__america__chihuahua =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -425 || time.offsetMinutes == -424 then
                Ok Lmt

            else if time.offsetMinutes == -300 then
                Ok (ShortName "CDT")

            else if time.offsetMinutes == -360 then
                Ok
                    (if time.posixSeconds >= 1667116800 then
                        ShortName "CST"

                     else if time.posixSeconds >= 891766800 then
                        ShortName "MDT"

                     else if time.posixSeconds >= -1191344400 then
                        ShortName "CST"

                     else if time.posixSeconds >= -1220461200 then
                        ShortName "MDT"

                     else
                        ShortName "CST"
                    )

            else if time.offsetMinutes == -420 then
                Ok (ShortName "MST")

            else
                Err time.offsetMinutes
        )


{-| `America/Ciudad_Juarez`
-}
for__america__ciudad_juarez : Zone -> Posix -> Result Int Abbreviation
for__america__ciudad_juarez =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -426 || time.offsetMinutes == -425 then
                Ok Lmt

            else if time.offsetMinutes == -300 then
                Ok (ShortName "CDT")

            else if time.offsetMinutes == -360 then
                Ok
                    (if time.posixSeconds >= 1667116800 then
                        ShortName "CST"

                     else if time.posixSeconds >= 891766800 then
                        ShortName "MDT"

                     else if time.posixSeconds >= -1191344400 then
                        ShortName "CST"

                     else if time.posixSeconds >= -1220461200 then
                        ShortName "MDT"

                     else
                        ShortName "CST"
                    )

            else if time.offsetMinutes == -420 then
                Ok (ShortName "MST")

            else
                Err time.offsetMinutes
        )


{-| `America/Costa_Rica`
-}
for__america__costa_rica : Zone -> Posix -> Result Int Abbreviation
for__america__costa_rica =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -337 || time.offsetMinutes == -336 then
                Ok
                    (if time.posixSeconds >= -2524501427 then
                        ShortName "SJMT"

                     else
                        Lmt
                    )

            else if time.offsetMinutes == -300 then
                Ok (ShortName "CDT")

            else if time.offsetMinutes == -360 then
                Ok (ShortName "CST")

            else
                Err time.offsetMinutes
        )


{-| `America/Cuiaba`
-}
for__america__cuiaba : Zone -> Posix -> Result Int Abbreviation
for__america__cuiaba =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -225 || time.offsetMinutes == -224 then
                Ok Lmt

            else if time.offsetMinutes == -240 || time.offsetMinutes == -180 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `America/Danmarkshavn`
-}
for__america__danmarkshavn : Zone -> Posix -> Result Int Abbreviation
for__america__danmarkshavn =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -75 || time.offsetMinutes == -74 then
                Ok Lmt

            else if time.offsetMinutes == -180 || time.offsetMinutes == -120 then
                Ok (Offset time.offsetMinutes)

            else if time.offsetMinutes == 0 then
                Ok (ShortName "GMT")

            else
                Err time.offsetMinutes
        )


{-| `America/Dawson`
-}
for__america__dawson : Zone -> Posix -> Result Int Abbreviation
for__america__dawson =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -558 || time.offsetMinutes == -557 then
                Ok Lmt

            else if time.offsetMinutes == -420 then
                Ok
                    (if time.posixSeconds >= 1604214000 then
                        ShortName "MST"

                     else if time.posixSeconds >= 325677600 then
                        ShortName "PDT"

                     else
                        ShortName "YDDT"
                    )

            else if time.offsetMinutes == -480 then
                Ok
                    (if time.posixSeconds >= 120646800 then
                        ShortName "PST"

                     else if time.posixSeconds >= -769395600 then
                        ShortName "YPT"

                     else if time.posixSeconds >= -880203600 then
                        ShortName "YWT"

                     else
                        ShortName "YDT"
                    )

            else if time.offsetMinutes == -540 then
                Ok (ShortName "YST")

            else
                Err time.offsetMinutes
        )


{-| `America/Dawson_Creek`
-}
for__america__dawson_creek : Zone -> Posix -> Result Int Abbreviation
for__america__dawson_creek =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -481 then
                Ok Lmt

            else if time.offsetMinutes == -480 then
                Ok
                    (if time.posixSeconds >= -2713881544 then
                        ShortName "PST"

                     else
                        Lmt
                    )

            else if time.offsetMinutes == -420 then
                Ok
                    (if time.posixSeconds >= 84013200 then
                        ShortName "MST"

                     else if time.posixSeconds >= -715788000 then
                        ShortName "PDT"

                     else if time.posixSeconds >= -769395600 then
                        ShortName "PPT"

                     else if time.posixSeconds >= -880207200 then
                        ShortName "PWT"

                     else
                        ShortName "PDT"
                    )

            else
                Err time.offsetMinutes
        )


{-| `America/Denver`
-}
for__america__denver : Zone -> Posix -> Result Int Abbreviation
for__america__denver =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -419 then
                Ok Lmt

            else if time.offsetMinutes == -420 then
                Ok
                    (if time.posixSeconds >= -2717643600 then
                        ShortName "MST"

                     else
                        Lmt
                    )

            else if time.offsetMinutes == -360 then
                Ok
                    (if time.posixSeconds >= -147884400 then
                        ShortName "MDT"

                     else if time.posixSeconds >= -769395600 then
                        ShortName "MPT"

                     else if time.posixSeconds >= -880210800 then
                        ShortName "MWT"

                     else
                        ShortName "MDT"
                    )

            else
                Err time.offsetMinutes
        )


{-| `America/Detroit`
-}
for__america__detroit : Zone -> Posix -> Result Int Abbreviation
for__america__detroit =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -333 || time.offsetMinutes == -332 then
                Ok Lmt

            else if time.offsetMinutes == -360 then
                Ok (ShortName "CST")

            else if time.offsetMinutes == -300 then
                Ok (ShortName "EST")

            else if time.offsetMinutes == -240 then
                Ok
                    (if time.posixSeconds >= -684349200 then
                        ShortName "EDT"

                     else if time.posixSeconds >= -769395600 then
                        ShortName "EPT"

                     else
                        ShortName "EWT"
                    )

            else
                Err time.offsetMinutes
        )


{-| `America/Edmonton`
-}
for__america__edmonton : Zone -> Posix -> Result Int Abbreviation
for__america__edmonton =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -454 || time.offsetMinutes == -453 then
                Ok Lmt

            else if time.offsetMinutes == -360 then
                Ok
                    (if time.posixSeconds >= -715791600 then
                        ShortName "MDT"

                     else if time.posixSeconds >= -769395600 then
                        ShortName "MPT"

                     else if time.posixSeconds >= -880210800 then
                        ShortName "MWT"

                     else
                        ShortName "MDT"
                    )

            else if time.offsetMinutes == -420 then
                Ok (ShortName "MST")

            else
                Err time.offsetMinutes
        )


{-| `America/Eirunepe`
-}
for__america__eirunepe : Zone -> Posix -> Result Int Abbreviation
for__america__eirunepe =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -280 || time.offsetMinutes == -279 then
                Ok Lmt

            else if time.offsetMinutes == -300 || time.offsetMinutes == -240 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `America/El_Salvador`
-}
for__america__el_salvador : Zone -> Posix -> Result Int Abbreviation
for__america__el_salvador =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -357 || time.offsetMinutes == -356 then
                Ok Lmt

            else if time.offsetMinutes == -300 then
                Ok (ShortName "CDT")

            else if time.offsetMinutes == -360 then
                Ok (ShortName "CST")

            else
                Err time.offsetMinutes
        )


{-| `America/Fort_Nelson`
-}
for__america__fort_nelson : Zone -> Posix -> Result Int Abbreviation
for__america__fort_nelson =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -491 || time.offsetMinutes == -490 then
                Ok Lmt

            else if time.offsetMinutes == -420 then
                Ok
                    (if time.posixSeconds >= 1425808800 then
                        ShortName "MST"

                     else if time.posixSeconds >= -715788000 then
                        ShortName "PDT"

                     else if time.posixSeconds >= -769395600 then
                        ShortName "PPT"

                     else if time.posixSeconds >= -880207200 then
                        ShortName "PWT"

                     else
                        ShortName "PDT"
                    )

            else if time.offsetMinutes == -480 then
                Ok (ShortName "PST")

            else
                Err time.offsetMinutes
        )


{-| `America/Fortaleza`
-}
for__america__fortaleza : Zone -> Posix -> Result Int Abbreviation
for__america__fortaleza =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -154 then
                Ok Lmt

            else if time.offsetMinutes == -180 || time.offsetMinutes == -120 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `America/Glace_Bay`
-}
for__america__glace_bay : Zone -> Posix -> Result Int Abbreviation
for__america__glace_bay =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -239 then
                Ok Lmt

            else if time.offsetMinutes == -240 then
                Ok
                    (if time.posixSeconds >= -2131646412 then
                        ShortName "AST"

                     else
                        Lmt
                    )

            else if time.offsetMinutes == -180 then
                Ok
                    (if time.posixSeconds >= -526500000 then
                        ShortName "ADT"

                     else if time.posixSeconds >= -769395600 then
                        ShortName "APT"

                     else if time.posixSeconds >= -880221600 then
                        ShortName "AWT"

                     else
                        ShortName "ADT"
                    )

            else
                Err time.offsetMinutes
        )


{-| `America/Goose_Bay`
-}
for__america__goose_bay : Zone -> Posix -> Result Int Abbreviation
for__america__goose_bay =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -242 || time.offsetMinutes == -241 then
                Ok Lmt

            else if time.offsetMinutes == -120 then
                Ok (ShortName "ADDT")

            else if time.offsetMinutes == -180 then
                Ok (ShortName "ADT")

            else if time.offsetMinutes == -240 then
                Ok (ShortName "AST")

            else if time.offsetMinutes == -151 then
                Ok (ShortName "NDT")

            else if time.offsetMinutes == -150 then
                Ok
                    (if time.posixSeconds >= -746044200 then
                        ShortName "NDT"

                     else if time.posixSeconds >= -769395600 then
                        ShortName "NPT"

                     else if time.posixSeconds >= -872368200 then
                        ShortName "NWT"

                     else
                        ShortName "NDT"
                    )

            else if time.offsetMinutes == -211 || time.offsetMinutes == -210 then
                Ok (ShortName "NST")

            else
                Err time.offsetMinutes
        )


{-| `America/Grand_Turk`
-}
for__america__grand_turk : Zone -> Posix -> Result Int Abbreviation
for__america__grand_turk =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -285 || time.offsetMinutes == -284 then
                Ok Lmt

            else if time.offsetMinutes == -240 then
                Ok
                    (if time.posixSeconds >= 1520751600 then
                        ShortName "EDT"

                     else if time.posixSeconds >= 1425798000 then
                        ShortName "AST"

                     else
                        ShortName "EDT"
                    )

            else if time.offsetMinutes == -300 then
                Ok (ShortName "EST")

            else if time.offsetMinutes == -308 || time.offsetMinutes == -307 then
                Ok (ShortName "KMT")

            else
                Err time.offsetMinutes
        )


{-| `America/Guatemala`
-}
for__america__guatemala : Zone -> Posix -> Result Int Abbreviation
for__america__guatemala =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -363 || time.offsetMinutes == -362 then
                Ok Lmt

            else if time.offsetMinutes == -300 then
                Ok (ShortName "CDT")

            else if time.offsetMinutes == -360 then
                Ok (ShortName "CST")

            else
                Err time.offsetMinutes
        )


{-| `America/Guayaquil`
-}
for__america__guayaquil : Zone -> Posix -> Result Int Abbreviation
for__america__guayaquil =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -320 || time.offsetMinutes == -319 then
                Ok Lmt

            else if time.offsetMinutes == -300 || time.offsetMinutes == -240 then
                Ok (Offset time.offsetMinutes)

            else if time.offsetMinutes == -314 then
                Ok (ShortName "QMT")

            else
                Err time.offsetMinutes
        )


{-| `America/Guyana`
-}
for__america__guyana : Zone -> Posix -> Result Int Abbreviation
for__america__guyana =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -233 || time.offsetMinutes == -232 then
                Ok Lmt

            else if time.offsetMinutes == -240 || time.offsetMinutes == -225 || time.offsetMinutes == -180 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `America/Halifax`
-}
for__america__halifax : Zone -> Posix -> Result Int Abbreviation
for__america__halifax =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -255 || time.offsetMinutes == -254 then
                Ok Lmt

            else if time.offsetMinutes == -180 then
                Ok
                    (if time.posixSeconds >= -747252000 then
                        ShortName "ADT"

                     else if time.posixSeconds >= -769395600 then
                        ShortName "APT"

                     else if time.posixSeconds >= -880221600 then
                        ShortName "AWT"

                     else
                        ShortName "ADT"
                    )

            else if time.offsetMinutes == -240 then
                Ok (ShortName "AST")

            else
                Err time.offsetMinutes
        )


{-| `America/Havana`
-}
for__america__havana : Zone -> Posix -> Result Int Abbreviation
for__america__havana =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -330 || time.offsetMinutes == -329 then
                Ok
                    (if time.posixSeconds >= -2524501832 then
                        ShortName "HMT"

                     else
                        Lmt
                    )

            else if time.offsetMinutes == -240 then
                Ok (ShortName "CDT")

            else if time.offsetMinutes == -300 then
                Ok (ShortName "CST")

            else
                Err time.offsetMinutes
        )


{-| `America/Hermosillo`
-}
for__america__hermosillo : Zone -> Posix -> Result Int Abbreviation
for__america__hermosillo =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -444 || time.offsetMinutes == -443 then
                Ok Lmt

            else if time.offsetMinutes == -360 then
                Ok
                    (if time.posixSeconds >= 828867600 then
                        ShortName "MDT"

                     else if time.posixSeconds >= -1191344400 then
                        ShortName "CST"

                     else if time.posixSeconds >= -1220461200 then
                        ShortName "MDT"

                     else
                        ShortName "CST"
                    )

            else if time.offsetMinutes == -420 then
                Ok (ShortName "MST")

            else
                Err time.offsetMinutes
        )


{-| `America/Indiana/Indianapolis`
-}
for__america__indiana__indianapolis : Zone -> Posix -> Result Int Abbreviation
for__america__indiana__indianapolis =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -345 || time.offsetMinutes == -344 then
                Ok Lmt

            else if time.offsetMinutes == -300 then
                Ok
                    (if time.posixSeconds >= -463593600 then
                        ShortName "EST"

                     else if time.posixSeconds >= -747244800 then
                        ShortName "CDT"

                     else if time.posixSeconds >= -769395600 then
                        ShortName "CPT"

                     else if time.posixSeconds >= -880214400 then
                        ShortName "CWT"

                     else
                        ShortName "CDT"
                    )

            else if time.offsetMinutes == -360 then
                Ok (ShortName "CST")

            else if time.offsetMinutes == -240 then
                Ok (ShortName "EDT")

            else
                Err time.offsetMinutes
        )


{-| `America/Indiana/Knox`
-}
for__america__indiana__knox : Zone -> Posix -> Result Int Abbreviation
for__america__indiana__knox =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -347 || time.offsetMinutes == -346 then
                Ok Lmt

            else if time.offsetMinutes == -300 then
                Ok
                    (if time.posixSeconds >= 1143961200 then
                        ShortName "CDT"

                     else if time.posixSeconds >= 688546800 then
                        ShortName "EST"

                     else if time.posixSeconds >= -84384000 then
                        ShortName "CDT"

                     else if time.posixSeconds >= -242236800 then
                        ShortName "EST"

                     else if time.posixSeconds >= -715795200 then
                        ShortName "CDT"

                     else if time.posixSeconds >= -769395600 then
                        ShortName "CPT"

                     else if time.posixSeconds >= -880214400 then
                        ShortName "CWT"

                     else
                        ShortName "CDT"
                    )

            else if time.offsetMinutes == -360 then
                Ok (ShortName "CST")

            else
                Err time.offsetMinutes
        )


{-| `America/Indiana/Marengo`
-}
for__america__indiana__marengo : Zone -> Posix -> Result Int Abbreviation
for__america__indiana__marengo =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -346 || time.offsetMinutes == -345 then
                Ok Lmt

            else if time.offsetMinutes == -300 then
                Ok
                    (if time.posixSeconds >= 152089200 then
                        ShortName "EST"

                     else if time.posixSeconds >= 126687600 then
                        ShortName "CDT"

                     else if time.posixSeconds >= -273686400 then
                        ShortName "EST"

                     else if time.posixSeconds >= -589392000 then
                        ShortName "CDT"

                     else if time.posixSeconds >= -769395600 then
                        ShortName "CPT"

                     else if time.posixSeconds >= -880214400 then
                        ShortName "CWT"

                     else
                        ShortName "CDT"
                    )

            else if time.offsetMinutes == -360 then
                Ok (ShortName "CST")

            else if time.offsetMinutes == -240 then
                Ok (ShortName "EDT")

            else
                Err time.offsetMinutes
        )


{-| `America/Indiana/Petersburg`
-}
for__america__indiana__petersburg : Zone -> Posix -> Result Int Abbreviation
for__america__indiana__petersburg =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -350 || time.offsetMinutes == -349 then
                Ok Lmt

            else if time.offsetMinutes == -300 then
                Ok
                    (if time.posixSeconds >= 1194159600 then
                        ShortName "EST"

                     else if time.posixSeconds >= 1143961200 then
                        ShortName "CDT"

                     else if time.posixSeconds >= 247042800 then
                        ShortName "EST"

                     else if time.posixSeconds >= -84384000 then
                        ShortName "CDT"

                     else if time.posixSeconds >= -147888000 then
                        ShortName "EST"

                     else if time.posixSeconds >= -462996000 then
                        ShortName "CDT"

                     else if time.posixSeconds >= -769395600 then
                        ShortName "CPT"

                     else if time.posixSeconds >= -880214400 then
                        ShortName "CWT"

                     else
                        ShortName "CDT"
                    )

            else if time.offsetMinutes == -360 then
                Ok (ShortName "CST")

            else
                Err time.offsetMinutes
        )


{-| `America/Indiana/Tell_City`
-}
for__america__indiana__tell_city : Zone -> Posix -> Result Int Abbreviation
for__america__indiana__tell_city =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -348 || time.offsetMinutes == -347 then
                Ok Lmt

            else if time.offsetMinutes == -300 then
                Ok
                    (if time.posixSeconds >= 1143961200 then
                        ShortName "CDT"

                     else if time.posixSeconds >= -5767200 then
                        ShortName "EST"

                     else if time.posixSeconds >= -52934400 then
                        ShortName "CDT"

                     else if time.posixSeconds >= -179337600 then
                        ShortName "EST"

                     else if time.posixSeconds >= -462996000 then
                        ShortName "CDT"

                     else if time.posixSeconds >= -769395600 then
                        ShortName "CPT"

                     else if time.posixSeconds >= -880214400 then
                        ShortName "CWT"

                     else
                        ShortName "CDT"
                    )

            else if time.offsetMinutes == -360 then
                Ok (ShortName "CST")

            else if time.offsetMinutes == -240 then
                Ok (ShortName "EDT")

            else
                Err time.offsetMinutes
        )


{-| `America/Indiana/Vevay`
-}
for__america__indiana__vevay : Zone -> Posix -> Result Int Abbreviation
for__america__indiana__vevay =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -341 || time.offsetMinutes == -340 then
                Ok Lmt

            else if time.offsetMinutes == -300 then
                Ok
                    (if time.posixSeconds >= -495043200 then
                        ShortName "EST"

                     else if time.posixSeconds >= -769395600 then
                        ShortName "CPT"

                     else if time.posixSeconds >= -880214400 then
                        ShortName "CWT"

                     else
                        ShortName "CDT"
                    )

            else if time.offsetMinutes == -360 then
                Ok (ShortName "CST")

            else if time.offsetMinutes == -240 then
                Ok (ShortName "EDT")

            else
                Err time.offsetMinutes
        )


{-| `America/Indiana/Vincennes`
-}
for__america__indiana__vincennes : Zone -> Posix -> Result Int Abbreviation
for__america__indiana__vincennes =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -351 || time.offsetMinutes == -350 then
                Ok Lmt

            else if time.offsetMinutes == -300 then
                Ok
                    (if time.posixSeconds >= 1194159600 then
                        ShortName "EST"

                     else if time.posixSeconds >= 1143961200 then
                        ShortName "CDT"

                     else if time.posixSeconds >= -179337600 then
                        ShortName "EST"

                     else if time.posixSeconds >= -747244800 then
                        ShortName "CDT"

                     else if time.posixSeconds >= -769395600 then
                        ShortName "CPT"

                     else if time.posixSeconds >= -880214400 then
                        ShortName "CWT"

                     else
                        ShortName "CDT"
                    )

            else if time.offsetMinutes == -360 then
                Ok (ShortName "CST")

            else if time.offsetMinutes == -240 then
                Ok (ShortName "EDT")

            else
                Err time.offsetMinutes
        )


{-| `America/Indiana/Winamac`
-}
for__america__indiana__winamac : Zone -> Posix -> Result Int Abbreviation
for__america__indiana__winamac =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -347 || time.offsetMinutes == -346 then
                Ok Lmt

            else if time.offsetMinutes == -300 then
                Ok
                    (if time.posixSeconds >= 1143961200 then
                        ShortName "CDT"

                     else if time.posixSeconds >= -273686400 then
                        ShortName "EST"

                     else if time.posixSeconds >= -747244800 then
                        ShortName "CDT"

                     else if time.posixSeconds >= -769395600 then
                        ShortName "CPT"

                     else if time.posixSeconds >= -880214400 then
                        ShortName "CWT"

                     else
                        ShortName "CDT"
                    )

            else if time.offsetMinutes == -360 then
                Ok (ShortName "CST")

            else if time.offsetMinutes == -240 then
                Ok (ShortName "EDT")

            else
                Err time.offsetMinutes
        )


{-| `America/Inuvik`
-}
for__america__inuvik : Zone -> Posix -> Result Int Abbreviation
for__america__inuvik =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 0 then
                Ok Uninhabited

            else if time.offsetMinutes == -360 then
                Ok (ShortName "MDT")

            else if time.offsetMinutes == -420 then
                Ok
                    (if time.posixSeconds >= 309945600 then
                        ShortName "MST"

                     else
                        ShortName "PDT"
                    )

            else if time.offsetMinutes == -480 then
                Ok (ShortName "PST")

            else
                Err time.offsetMinutes
        )


{-| `America/Iqaluit`
-}
for__america__iqaluit : Zone -> Posix -> Result Int Abbreviation
for__america__iqaluit =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 0 then
                Ok Uninhabited

            else if time.offsetMinutes == -360 then
                Ok (ShortName "CST")

            else if time.offsetMinutes == -300 then
                Ok
                    (if time.posixSeconds >= 972802800 then
                        ShortName "EST"

                     else if time.posixSeconds >= 954662400 then
                        ShortName "CDT"

                     else
                        ShortName "EST"
                    )

            else if time.offsetMinutes == -240 then
                Ok
                    (if time.posixSeconds >= 73465200 then
                        ShortName "EDT"

                     else if time.posixSeconds >= -769395600 then
                        ShortName "EPT"

                     else
                        ShortName "EWT"
                    )

            else
                Err time.offsetMinutes
        )


{-| `America/Jamaica`
-}
for__america__jamaica : Zone -> Posix -> Result Int Abbreviation
for__america__jamaica =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -308 || time.offsetMinutes == -307 then
                Ok
                    (if time.posixSeconds >= -2524503170 then
                        ShortName "KMT"

                     else
                        Lmt
                    )

            else if time.offsetMinutes == -240 then
                Ok (ShortName "EDT")

            else if time.offsetMinutes == -300 then
                Ok (ShortName "EST")

            else
                Err time.offsetMinutes
        )


{-| `America/Juneau`
-}
for__america__juneau : Zone -> Posix -> Result Int Abbreviation
for__america__juneau =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -538 || time.offsetMinutes == -537 || time.offsetMinutes == 902 || time.offsetMinutes == 903 then
                Ok Lmt

            else if time.offsetMinutes == -480 then
                Ok
                    (if time.posixSeconds >= 452084400 then
                        ShortName "AKDT"

                     else if time.posixSeconds >= 341402400 then
                        ShortName "PST"

                     else if time.posixSeconds >= 325677600 then
                        ShortName "YDT"

                     else
                        ShortName "PST"
                    )

            else if time.offsetMinutes == -420 then
                Ok
                    (if time.posixSeconds >= -21477600 then
                        ShortName "PDT"

                     else if time.posixSeconds >= -769395600 then
                        ShortName "PPT"

                     else
                        ShortName "PWT"
                    )

            else if time.offsetMinutes == -540 then
                Ok
                    (if time.posixSeconds >= 439030800 then
                        ShortName "AKST"

                     else
                        ShortName "YST"
                    )

            else
                Err time.offsetMinutes
        )


{-| `America/Kentucky/Louisville`
-}
for__america__kentucky__louisville : Zone -> Posix -> Result Int Abbreviation
for__america__kentucky__louisville =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -344 || time.offsetMinutes == -343 then
                Ok Lmt

            else if time.offsetMinutes == -300 then
                Ok
                    (if time.posixSeconds >= 152089200 then
                        ShortName "EST"

                     else if time.posixSeconds >= 126687600 then
                        ShortName "CDT"

                     else if time.posixSeconds >= -266432400 then
                        ShortName "EST"

                     else if time.posixSeconds >= -747251940 then
                        ShortName "CDT"

                     else if time.posixSeconds >= -769395600 then
                        ShortName "CPT"

                     else if time.posixSeconds >= -880214400 then
                        ShortName "CWT"

                     else
                        ShortName "CDT"
                    )

            else if time.offsetMinutes == -360 then
                Ok (ShortName "CST")

            else if time.offsetMinutes == -240 then
                Ok (ShortName "EDT")

            else
                Err time.offsetMinutes
        )


{-| `America/Kentucky/Monticello`
-}
for__america__kentucky__monticello : Zone -> Posix -> Result Int Abbreviation
for__america__kentucky__monticello =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -340 || time.offsetMinutes == -339 then
                Ok Lmt

            else if time.offsetMinutes == -300 then
                Ok
                    (if time.posixSeconds >= 972802800 then
                        ShortName "EST"

                     else if time.posixSeconds >= -52934400 then
                        ShortName "CDT"

                     else if time.posixSeconds >= -769395600 then
                        ShortName "CPT"

                     else if time.posixSeconds >= -880214400 then
                        ShortName "CWT"

                     else
                        ShortName "CDT"
                    )

            else if time.offsetMinutes == -360 then
                Ok (ShortName "CST")

            else if time.offsetMinutes == -240 then
                Ok (ShortName "EDT")

            else
                Err time.offsetMinutes
        )


{-| `America/La_Paz`
-}
for__america__la_paz : Zone -> Posix -> Result Int Abbreviation
for__america__la_paz =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -273 || time.offsetMinutes == -272 then
                Ok
                    (if time.posixSeconds >= -2524505244 then
                        ShortName "CMT"

                     else
                        Lmt
                    )

            else if time.offsetMinutes == -240 then
                Ok (Offset time.offsetMinutes)

            else if time.offsetMinutes == -213 || time.offsetMinutes == -212 then
                Ok (ShortName "BST")

            else
                Err time.offsetMinutes
        )


{-| `America/Lima`
-}
for__america__lima : Zone -> Posix -> Result Int Abbreviation
for__america__lima =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -309 || time.offsetMinutes == -308 then
                Ok Lmt

            else if time.offsetMinutes == -300 || time.offsetMinutes == -240 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `America/Los_Angeles`
-}
for__america__los_angeles : Zone -> Posix -> Result Int Abbreviation
for__america__los_angeles =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -473 || time.offsetMinutes == -472 then
                Ok Lmt

            else if time.offsetMinutes == -420 then
                Ok
                    (if time.posixSeconds >= -687967140 then
                        ShortName "PDT"

                     else if time.posixSeconds >= -769395600 then
                        ShortName "PPT"

                     else if time.posixSeconds >= -880207200 then
                        ShortName "PWT"

                     else
                        ShortName "PDT"
                    )

            else if time.offsetMinutes == -480 then
                Ok (ShortName "PST")

            else
                Err time.offsetMinutes
        )


{-| `America/Maceio`
-}
for__america__maceio : Zone -> Posix -> Result Int Abbreviation
for__america__maceio =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -143 || time.offsetMinutes == -142 then
                Ok Lmt

            else if time.offsetMinutes == -180 || time.offsetMinutes == -120 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `America/Managua`
-}
for__america__managua : Zone -> Posix -> Result Int Abbreviation
for__america__managua =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -346 || time.offsetMinutes == -345 then
                Ok
                    (if time.posixSeconds >= -2524500892 then
                        ShortName "MMT"

                     else
                        Lmt
                    )

            else if time.offsetMinutes == -360 then
                Ok (ShortName "CST")

            else if time.offsetMinutes == -300 then
                Ok
                    (if time.posixSeconds >= 1113112800 then
                        ShortName "CDT"

                     else if time.posixSeconds >= 694260000 then
                        ShortName "EST"

                     else if time.posixSeconds >= 290584800 then
                        ShortName "CDT"

                     else
                        ShortName "EST"
                    )

            else
                Err time.offsetMinutes
        )


{-| `America/Manaus`
-}
for__america__manaus : Zone -> Posix -> Result Int Abbreviation
for__america__manaus =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -241 then
                Ok Lmt

            else if time.offsetMinutes == -240 then
                Ok
                    (if time.posixSeconds >= -1767211196 then
                        Offset time.offsetMinutes

                     else
                        Lmt
                    )

            else if time.offsetMinutes == -180 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `America/Martinique`
-}
for__america__martinique : Zone -> Posix -> Result Int Abbreviation
for__america__martinique =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -245 || time.offsetMinutes == -244 then
                Ok
                    (if time.posixSeconds >= -2524506940 then
                        ShortName "FFMT"

                     else
                        Lmt
                    )

            else if time.offsetMinutes == -180 then
                Ok (ShortName "ADT")

            else if time.offsetMinutes == -240 then
                Ok (ShortName "AST")

            else
                Err time.offsetMinutes
        )


{-| `America/Matamoros`
-}
for__america__matamoros : Zone -> Posix -> Result Int Abbreviation
for__america__matamoros =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -390 then
                Ok Lmt

            else if time.offsetMinutes == -300 then
                Ok (ShortName "CDT")

            else if time.offsetMinutes == -360 then
                Ok (ShortName "CST")

            else
                Err time.offsetMinutes
        )


{-| `America/Mazatlan`
-}
for__america__mazatlan : Zone -> Posix -> Result Int Abbreviation
for__america__mazatlan =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -426 || time.offsetMinutes == -425 then
                Ok Lmt

            else if time.offsetMinutes == -360 then
                Ok
                    (if time.posixSeconds >= 828867600 then
                        ShortName "MDT"

                     else if time.posixSeconds >= -1191344400 then
                        ShortName "CST"

                     else if time.posixSeconds >= -1220461200 then
                        ShortName "MDT"

                     else
                        ShortName "CST"
                    )

            else if time.offsetMinutes == -420 then
                Ok (ShortName "MST")

            else
                Err time.offsetMinutes
        )


{-| `America/Menominee`
-}
for__america__menominee : Zone -> Posix -> Result Int Abbreviation
for__america__menominee =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -351 || time.offsetMinutes == -350 then
                Ok Lmt

            else if time.offsetMinutes == -300 then
                Ok
                    (if time.posixSeconds >= 104914800 then
                        ShortName "CDT"

                     else if time.posixSeconds >= -21484800 then
                        ShortName "EST"

                     else if time.posixSeconds >= -747244800 then
                        ShortName "CDT"

                     else if time.posixSeconds >= -769395600 then
                        ShortName "CPT"

                     else if time.posixSeconds >= -880214400 then
                        ShortName "CWT"

                     else
                        ShortName "CDT"
                    )

            else if time.offsetMinutes == -360 then
                Ok (ShortName "CST")

            else
                Err time.offsetMinutes
        )


{-| `America/Merida`
-}
for__america__merida : Zone -> Posix -> Result Int Abbreviation
for__america__merida =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -359 || time.offsetMinutes == -358 then
                Ok Lmt

            else if time.offsetMinutes == -360 then
                Ok (ShortName "CST")

            else if time.offsetMinutes == -300 then
                Ok
                    (if time.posixSeconds >= 828864000 then
                        ShortName "CDT"

                     else
                        ShortName "EST"
                    )

            else
                Err time.offsetMinutes
        )


{-| `America/Metlakatla`
-}
for__america__metlakatla : Zone -> Posix -> Result Int Abbreviation
for__america__metlakatla =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -527 || time.offsetMinutes == -526 || time.offsetMinutes == 913 || time.offsetMinutes == 914 then
                Ok Lmt

            else if time.offsetMinutes == -540 then
                Ok (ShortName "AKST")

            else if time.offsetMinutes == -480 then
                Ok
                    (if time.posixSeconds >= 1541325600 then
                        ShortName "PST"

                     else if time.posixSeconds >= 1457866800 then
                        ShortName "AKDT"

                     else
                        ShortName "PST"
                    )

            else if time.offsetMinutes == -420 then
                Ok
                    (if time.posixSeconds >= -21477600 then
                        ShortName "PDT"

                     else if time.posixSeconds >= -769395600 then
                        ShortName "PPT"

                     else
                        ShortName "PWT"
                    )

            else
                Err time.offsetMinutes
        )


{-| `America/Mexico_City`
-}
for__america__mexico_city : Zone -> Posix -> Result Int Abbreviation
for__america__mexico_city =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -397 || time.offsetMinutes == -396 then
                Ok Lmt

            else if time.offsetMinutes == -300 then
                Ok
                    (if time.posixSeconds >= -627501600 then
                        ShortName "CDT"

                     else if time.posixSeconds >= -821901600 then
                        ShortName "CWT"

                     else
                        ShortName "CDT"
                    )

            else if time.offsetMinutes == -360 then
                Ok
                    (if time.posixSeconds >= -1191344400 then
                        ShortName "CST"

                     else if time.posixSeconds >= -1220461200 then
                        ShortName "MDT"

                     else
                        ShortName "CST"
                    )

            else if time.offsetMinutes == -420 then
                Ok (ShortName "MST")

            else
                Err time.offsetMinutes
        )


{-| `America/Miquelon`
-}
for__america__miquelon : Zone -> Posix -> Result Int Abbreviation
for__america__miquelon =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -225 || time.offsetMinutes == -224 then
                Ok Lmt

            else if time.offsetMinutes == -180 || time.offsetMinutes == -120 then
                Ok (Offset time.offsetMinutes)

            else if time.offsetMinutes == -240 then
                Ok (ShortName "AST")

            else
                Err time.offsetMinutes
        )


{-| `America/Moncton`
-}
for__america__moncton : Zone -> Posix -> Result Int Abbreviation
for__america__moncton =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -260 || time.offsetMinutes == -259 then
                Ok Lmt

            else if time.offsetMinutes == -180 then
                Ok
                    (if time.posixSeconds >= -747252000 then
                        ShortName "ADT"

                     else if time.posixSeconds >= -769395600 then
                        ShortName "APT"

                     else if time.posixSeconds >= -880221600 then
                        ShortName "AWT"

                     else
                        ShortName "ADT"
                    )

            else if time.offsetMinutes == -240 then
                Ok (ShortName "AST")

            else if time.offsetMinutes == -300 then
                Ok (ShortName "EST")

            else
                Err time.offsetMinutes
        )


{-| `America/Monterrey`
-}
for__america__monterrey : Zone -> Posix -> Result Int Abbreviation
for__america__monterrey =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -402 || time.offsetMinutes == -401 then
                Ok Lmt

            else if time.offsetMinutes == -300 then
                Ok (ShortName "CDT")

            else if time.offsetMinutes == -360 then
                Ok
                    (if time.posixSeconds >= -1191344400 then
                        ShortName "CST"

                     else if time.posixSeconds >= -1220461200 then
                        ShortName "MDT"

                     else
                        ShortName "CST"
                    )

            else if time.offsetMinutes == -420 then
                Ok (ShortName "MST")

            else
                Err time.offsetMinutes
        )


{-| `America/Montevideo`
-}
for__america__montevideo : Zone -> Posix -> Result Int Abbreviation
for__america__montevideo =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -225 || time.offsetMinutes == -224 then
                Ok
                    (if time.posixSeconds >= -1942690509 then
                        ShortName "MMT"

                     else
                        Lmt
                    )

            else if time.offsetMinutes == -240 || time.offsetMinutes == -210 || time.offsetMinutes == -180 || time.offsetMinutes == -150 || time.offsetMinutes == -120 || time.offsetMinutes == -90 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `America/New_York`
-}
for__america__new_york : Zone -> Posix -> Result Int Abbreviation
for__america__new_york =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -297 || time.offsetMinutes == -296 then
                Ok Lmt

            else if time.offsetMinutes == -240 then
                Ok
                    (if time.posixSeconds >= -747248400 then
                        ShortName "EDT"

                     else if time.posixSeconds >= -769395600 then
                        ShortName "EPT"

                     else if time.posixSeconds >= -880218000 then
                        ShortName "EWT"

                     else
                        ShortName "EDT"
                    )

            else if time.offsetMinutes == -300 then
                Ok (ShortName "EST")

            else
                Err time.offsetMinutes
        )


{-| `America/Nome`
-}
for__america__nome : Zone -> Posix -> Result Int Abbreviation
for__america__nome =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -662 || time.offsetMinutes == -661 || time.offsetMinutes == 778 || time.offsetMinutes == 779 then
                Ok Lmt

            else if time.offsetMinutes == -480 then
                Ok (ShortName "AKDT")

            else if time.offsetMinutes == -660 then
                Ok
                    (if time.posixSeconds >= -86878800 then
                        ShortName "BST"

                     else
                        ShortName "NST"
                    )

            else if time.offsetMinutes == -600 then
                Ok
                    (if time.posixSeconds >= -21466800 then
                        ShortName "BDT"

                     else if time.posixSeconds >= -769395600 then
                        ShortName "NPT"

                     else
                        ShortName "NWT"
                    )

            else if time.offsetMinutes == -540 then
                Ok
                    (if time.posixSeconds >= 439030800 then
                        ShortName "AKST"

                     else
                        ShortName "YST"
                    )

            else
                Err time.offsetMinutes
        )


{-| `America/Noronha`
-}
for__america__noronha : Zone -> Posix -> Result Int Abbreviation
for__america__noronha =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -130 || time.offsetMinutes == -129 then
                Ok Lmt

            else if time.offsetMinutes == -120 || time.offsetMinutes == -60 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `America/North_Dakota/Beulah`
-}
for__america__north_dakota__beulah : Zone -> Posix -> Result Int Abbreviation
for__america__north_dakota__beulah =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -408 || time.offsetMinutes == -407 then
                Ok Lmt

            else if time.offsetMinutes == -360 then
                Ok
                    (if time.posixSeconds >= 1289116800 then
                        ShortName "CST"

                     else if time.posixSeconds >= -84380400 then
                        ShortName "MDT"

                     else if time.posixSeconds >= -769395600 then
                        ShortName "MPT"

                     else if time.posixSeconds >= -880210800 then
                        ShortName "MWT"

                     else
                        ShortName "MDT"
                    )

            else if time.offsetMinutes == -420 then
                Ok (ShortName "MST")

            else
                Err time.offsetMinutes
        )


{-| `America/North_Dakota/Center`
-}
for__america__north_dakota__center : Zone -> Posix -> Result Int Abbreviation
for__america__north_dakota__center =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -406 || time.offsetMinutes == -405 then
                Ok Lmt

            else if time.offsetMinutes == -300 then
                Ok (ShortName "CDT")

            else if time.offsetMinutes == -360 then
                Ok
                    (if time.posixSeconds >= 720000000 then
                        ShortName "CST"

                     else if time.posixSeconds >= -84380400 then
                        ShortName "MDT"

                     else if time.posixSeconds >= -769395600 then
                        ShortName "MPT"

                     else if time.posixSeconds >= -880210800 then
                        ShortName "MWT"

                     else
                        ShortName "MDT"
                    )

            else if time.offsetMinutes == -420 then
                Ok (ShortName "MST")

            else
                Err time.offsetMinutes
        )


{-| `America/North_Dakota/New_Salem`
-}
for__america__north_dakota__new_salem : Zone -> Posix -> Result Int Abbreviation
for__america__north_dakota__new_salem =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -406 || time.offsetMinutes == -405 then
                Ok Lmt

            else if time.offsetMinutes == -300 then
                Ok (ShortName "CDT")

            else if time.offsetMinutes == -360 then
                Ok
                    (if time.posixSeconds >= 1067155200 then
                        ShortName "CST"

                     else if time.posixSeconds >= -84380400 then
                        ShortName "MDT"

                     else if time.posixSeconds >= -769395600 then
                        ShortName "MPT"

                     else if time.posixSeconds >= -880210800 then
                        ShortName "MWT"

                     else
                        ShortName "MDT"
                    )

            else if time.offsetMinutes == -420 then
                Ok (ShortName "MST")

            else
                Err time.offsetMinutes
        )


{-| `America/Nuuk`
-}
for__america__nuuk : Zone -> Posix -> Result Int Abbreviation
for__america__nuuk =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -207 || time.offsetMinutes == -206 then
                Ok Lmt

            else if time.offsetMinutes == -180 || time.offsetMinutes == -120 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `America/Ojinaga`
-}
for__america__ojinaga : Zone -> Posix -> Result Int Abbreviation
for__america__ojinaga =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -418 || time.offsetMinutes == -417 then
                Ok Lmt

            else if time.offsetMinutes == -300 then
                Ok (ShortName "CDT")

            else if time.offsetMinutes == -360 then
                Ok
                    (if time.posixSeconds >= 1667116800 then
                        ShortName "CST"

                     else if time.posixSeconds >= 891766800 then
                        ShortName "MDT"

                     else if time.posixSeconds >= -1191344400 then
                        ShortName "CST"

                     else if time.posixSeconds >= -1220461200 then
                        ShortName "MDT"

                     else
                        ShortName "CST"
                    )

            else if time.offsetMinutes == -420 then
                Ok (ShortName "MST")

            else
                Err time.offsetMinutes
        )


{-| `America/Panama`
-}
for__america__panama : Zone -> Posix -> Result Int Abbreviation
for__america__panama =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -318 then
                Ok Lmt

            else if time.offsetMinutes == -319 then
                Ok
                    (if time.posixSeconds >= -2524502512 then
                        ShortName "CMT"

                     else
                        Lmt
                    )

            else if time.offsetMinutes == -320 then
                Ok (ShortName "CMT")

            else if time.offsetMinutes == -300 then
                Ok (ShortName "EST")

            else
                Err time.offsetMinutes
        )


{-| `America/Paramaribo`
-}
for__america__paramaribo : Zone -> Posix -> Result Int Abbreviation
for__america__paramaribo =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -221 || time.offsetMinutes == -220 then
                Ok
                    (if time.posixSeconds >= -1861906760 then
                        ShortName "PMT"

                     else
                        Lmt
                    )

            else if time.offsetMinutes == -210 || time.offsetMinutes == -180 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `America/Phoenix`
-}
for__america__phoenix : Zone -> Posix -> Result Int Abbreviation
for__america__phoenix =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -449 || time.offsetMinutes == -448 then
                Ok Lmt

            else if time.offsetMinutes == -360 then
                Ok
                    (if time.posixSeconds >= -84380400 then
                        ShortName "MDT"

                     else if time.posixSeconds >= -880210800 then
                        ShortName "MWT"

                     else
                        ShortName "MDT"
                    )

            else if time.offsetMinutes == -420 then
                Ok (ShortName "MST")

            else
                Err time.offsetMinutes
        )


{-| `America/Port-au-Prince`
-}
for__america__port_au_prince : Zone -> Posix -> Result Int Abbreviation
for__america__port_au_prince =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -290 then
                Ok Lmt

            else if time.offsetMinutes == -289 then
                Ok
                    (if time.posixSeconds >= -2524504240 then
                        ShortName "PPMT"

                     else
                        Lmt
                    )

            else if time.offsetMinutes == -240 then
                Ok (ShortName "EDT")

            else if time.offsetMinutes == -300 then
                Ok (ShortName "EST")

            else
                Err time.offsetMinutes
        )


{-| `America/Porto_Velho`
-}
for__america__porto_velho : Zone -> Posix -> Result Int Abbreviation
for__america__porto_velho =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -256 || time.offsetMinutes == -255 then
                Ok Lmt

            else if time.offsetMinutes == -240 || time.offsetMinutes == -180 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `America/Puerto_Rico`
-}
for__america__puerto_rico : Zone -> Posix -> Result Int Abbreviation
for__america__puerto_rico =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -265 || time.offsetMinutes == -264 then
                Ok Lmt

            else if time.offsetMinutes == -240 then
                Ok (ShortName "AST")

            else if time.offsetMinutes == -180 then
                Ok
                    (if time.posixSeconds >= -769395600 then
                        ShortName "APT"

                     else
                        ShortName "AWT"
                    )

            else
                Err time.offsetMinutes
        )


{-| `America/Punta_Arenas`
-}
for__america__punta_arenas : Zone -> Posix -> Result Int Abbreviation
for__america__punta_arenas =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -284 then
                Ok Lmt

            else if time.offsetMinutes == -283 then
                Ok
                    (if time.posixSeconds >= -2524504580 then
                        ShortName "SMT"

                     else
                        Lmt
                    )

            else if time.offsetMinutes == -300 || time.offsetMinutes == -240 || time.offsetMinutes == -180 then
                Ok (Offset time.offsetMinutes)

            else if time.offsetMinutes == -282 then
                Ok (ShortName "SMT")

            else
                Err time.offsetMinutes
        )


{-| `America/Rankin_Inlet`
-}
for__america__rankin_inlet : Zone -> Posix -> Result Int Abbreviation
for__america__rankin_inlet =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 0 then
                Ok Uninhabited

            else if time.offsetMinutes == -300 then
                Ok
                    (if time.posixSeconds >= 986112000 then
                        ShortName "CDT"

                     else if time.posixSeconds >= 972802800 then
                        ShortName "EST"

                     else
                        ShortName "CDT"
                    )

            else if time.offsetMinutes == -360 then
                Ok (ShortName "CST")

            else
                Err time.offsetMinutes
        )


{-| `America/Recife`
-}
for__america__recife : Zone -> Posix -> Result Int Abbreviation
for__america__recife =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -140 || time.offsetMinutes == -139 then
                Ok Lmt

            else if time.offsetMinutes == -180 || time.offsetMinutes == -120 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `America/Regina`
-}
for__america__regina : Zone -> Posix -> Result Int Abbreviation
for__america__regina =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -419 || time.offsetMinutes == -418 then
                Ok Lmt

            else if time.offsetMinutes == -360 then
                Ok
                    (if time.posixSeconds >= -305737200 then
                        ShortName "CST"

                     else if time.posixSeconds >= -748450800 then
                        ShortName "MDT"

                     else if time.posixSeconds >= -769395600 then
                        ShortName "MPT"

                     else if time.posixSeconds >= -880210800 then
                        ShortName "MWT"

                     else
                        ShortName "MDT"
                    )

            else if time.offsetMinutes == -420 then
                Ok (ShortName "MST")

            else
                Err time.offsetMinutes
        )


{-| `America/Resolute`
-}
for__america__resolute : Zone -> Posix -> Result Int Abbreviation
for__america__resolute =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 0 then
                Ok Uninhabited

            else if time.offsetMinutes == -300 then
                Ok
                    (if time.posixSeconds >= 1173600000 then
                        ShortName "CDT"

                     else if time.posixSeconds >= 1162105200 then
                        ShortName "EST"

                     else if time.posixSeconds >= 986112000 then
                        ShortName "CDT"

                     else if time.posixSeconds >= 972802800 then
                        ShortName "EST"

                     else
                        ShortName "CDT"
                    )

            else if time.offsetMinutes == -360 then
                Ok (ShortName "CST")

            else
                Err time.offsetMinutes
        )


{-| `America/Rio_Branco`
-}
for__america__rio_branco : Zone -> Posix -> Result Int Abbreviation
for__america__rio_branco =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -272 || time.offsetMinutes == -271 then
                Ok Lmt

            else if time.offsetMinutes == -300 || time.offsetMinutes == -240 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
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
        (\time ->
            if time.offsetMinutes == -283 || time.offsetMinutes == -282 then
                Ok
                    (if time.posixSeconds >= -2524504635 then
                        ShortName "SMT"

                     else
                        Lmt
                    )

            else if time.offsetMinutes == -300 || time.offsetMinutes == -240 || time.offsetMinutes == -180 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `America/Santo_Domingo`
-}
for__america__santo_domingo : Zone -> Posix -> Result Int Abbreviation
for__america__santo_domingo =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -279 then
                Ok Lmt

            else if time.offsetMinutes == -280 then
                Ok
                    (if time.posixSeconds >= -2524504824 then
                        ShortName "SDMT"

                     else
                        Lmt
                    )

            else if time.offsetMinutes == -270 then
                Ok (Offset time.offsetMinutes)

            else if time.offsetMinutes == -240 then
                Ok
                    (if time.posixSeconds >= 152082000 then
                        ShortName "AST"

                     else
                        ShortName "EDT"
                    )

            else if time.offsetMinutes == -300 then
                Ok (ShortName "EST")

            else
                Err time.offsetMinutes
        )


{-| `America/Sao_Paulo`
-}
for__america__sao_paulo : Zone -> Posix -> Result Int Abbreviation
for__america__sao_paulo =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -187 || time.offsetMinutes == -186 then
                Ok Lmt

            else if time.offsetMinutes == -180 || time.offsetMinutes == -120 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `America/Scoresbysund`
-}
for__america__scoresbysund : Zone -> Posix -> Result Int Abbreviation
for__america__scoresbysund =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -88 || time.offsetMinutes == -87 then
                Ok Lmt

            else if time.offsetMinutes == -120 || time.offsetMinutes == -60 || time.offsetMinutes == 0 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `America/Sitka`
-}
for__america__sitka : Zone -> Posix -> Result Int Abbreviation
for__america__sitka =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -542 || time.offsetMinutes == -541 || time.offsetMinutes == 898 || time.offsetMinutes == 899 then
                Ok Lmt

            else if time.offsetMinutes == -480 then
                Ok
                    (if time.posixSeconds >= 452084400 then
                        ShortName "AKDT"

                     else
                        ShortName "PST"
                    )

            else if time.offsetMinutes == -420 then
                Ok
                    (if time.posixSeconds >= -21477600 then
                        ShortName "PDT"

                     else if time.posixSeconds >= -769395600 then
                        ShortName "PPT"

                     else
                        ShortName "PWT"
                    )

            else if time.offsetMinutes == -540 then
                Ok
                    (if time.posixSeconds >= 439030800 then
                        ShortName "AKST"

                     else
                        ShortName "YST"
                    )

            else
                Err time.offsetMinutes
        )


{-| `America/St_Johns`
-}
for__america__st_johns : Zone -> Posix -> Result Int Abbreviation
for__america__st_johns =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -211 || time.offsetMinutes == -210 then
                Ok
                    (if time.posixSeconds >= -2713897748 then
                        ShortName "NST"

                     else
                        Lmt
                    )

            else if time.offsetMinutes == -90 then
                Ok (ShortName "NDDT")

            else if time.offsetMinutes == -151 then
                Ok (ShortName "NDT")

            else if time.offsetMinutes == -150 then
                Ok
                    (if time.posixSeconds >= -746044200 then
                        ShortName "NDT"

                     else if time.posixSeconds >= -769395600 then
                        ShortName "NPT"

                     else if time.posixSeconds >= -872368200 then
                        ShortName "NWT"

                     else
                        ShortName "NDT"
                    )

            else
                Err time.offsetMinutes
        )


{-| `America/Swift_Current`
-}
for__america__swift_current : Zone -> Posix -> Result Int Abbreviation
for__america__swift_current =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -432 || time.offsetMinutes == -431 then
                Ok Lmt

            else if time.offsetMinutes == -360 then
                Ok
                    (if time.posixSeconds >= 73472400 then
                        ShortName "CST"

                     else if time.posixSeconds >= -747241200 then
                        ShortName "MDT"

                     else if time.posixSeconds >= -769395600 then
                        ShortName "MPT"

                     else if time.posixSeconds >= -880210800 then
                        ShortName "MWT"

                     else
                        ShortName "MDT"
                    )

            else if time.offsetMinutes == -420 then
                Ok (ShortName "MST")

            else
                Err time.offsetMinutes
        )


{-| `America/Tegucigalpa`
-}
for__america__tegucigalpa : Zone -> Posix -> Result Int Abbreviation
for__america__tegucigalpa =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -349 || time.offsetMinutes == -348 then
                Ok Lmt

            else if time.offsetMinutes == -300 then
                Ok (ShortName "CDT")

            else if time.offsetMinutes == -360 then
                Ok (ShortName "CST")

            else
                Err time.offsetMinutes
        )


{-| `America/Thule`
-}
for__america__thule : Zone -> Posix -> Result Int Abbreviation
for__america__thule =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -276 || time.offsetMinutes == -275 then
                Ok Lmt

            else if time.offsetMinutes == -180 then
                Ok (ShortName "ADT")

            else if time.offsetMinutes == -240 then
                Ok (ShortName "AST")

            else
                Err time.offsetMinutes
        )


{-| `America/Tijuana`
-}
for__america__tijuana : Zone -> Posix -> Result Int Abbreviation
for__america__tijuana =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -469 || time.offsetMinutes == -468 then
                Ok Lmt

            else if time.offsetMinutes == -420 then
                Ok
                    (if time.posixSeconds >= -686073600 then
                        ShortName "PDT"

                     else if time.posixSeconds >= -769395600 then
                        ShortName "PPT"

                     else if time.posixSeconds >= -873820800 then
                        ShortName "PWT"

                     else if time.posixSeconds >= -1222963200 then
                        ShortName "PDT"

                     else
                        ShortName "MST"
                    )

            else if time.offsetMinutes == -480 then
                Ok (ShortName "PST")

            else
                Err time.offsetMinutes
        )


{-| `America/Toronto`
-}
for__america__toronto : Zone -> Posix -> Result Int Abbreviation
for__america__toronto =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -318 || time.offsetMinutes == -317 then
                Ok Lmt

            else if time.offsetMinutes == -240 then
                Ok
                    (if time.posixSeconds >= -747248400 then
                        ShortName "EDT"

                     else if time.posixSeconds >= -769395600 then
                        ShortName "EPT"

                     else if time.posixSeconds >= -880218000 then
                        ShortName "EWT"

                     else
                        ShortName "EDT"
                    )

            else if time.offsetMinutes == -300 then
                Ok (ShortName "EST")

            else
                Err time.offsetMinutes
        )


{-| `America/Vancouver`
-}
for__america__vancouver : Zone -> Posix -> Result Int Abbreviation
for__america__vancouver =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -493 || time.offsetMinutes == -492 then
                Ok Lmt

            else if time.offsetMinutes == -420 then
                Ok
                    (if time.posixSeconds >= -747237600 then
                        ShortName "PDT"

                     else if time.posixSeconds >= -769395600 then
                        ShortName "PPT"

                     else if time.posixSeconds >= -880207200 then
                        ShortName "PWT"

                     else
                        ShortName "PDT"
                    )

            else if time.offsetMinutes == -480 then
                Ok (ShortName "PST")

            else
                Err time.offsetMinutes
        )


{-| `America/Whitehorse`
-}
for__america__whitehorse : Zone -> Posix -> Result Int Abbreviation
for__america__whitehorse =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -541 then
                Ok Lmt

            else if time.offsetMinutes == -540 then
                Ok
                    (if time.posixSeconds >= -2188997988 then
                        ShortName "YST"

                     else
                        Lmt
                    )

            else if time.offsetMinutes == -420 then
                Ok
                    (if time.posixSeconds >= 1604214000 then
                        ShortName "MST"

                     else if time.posixSeconds >= 325677600 then
                        ShortName "PDT"

                     else
                        ShortName "YDDT"
                    )

            else if time.offsetMinutes == -480 then
                Ok
                    (if time.posixSeconds >= -121273200 then
                        ShortName "PST"

                     else if time.posixSeconds >= -769395600 then
                        ShortName "YPT"

                     else if time.posixSeconds >= -880203600 then
                        ShortName "YWT"

                     else
                        ShortName "YDT"
                    )

            else
                Err time.offsetMinutes
        )


{-| `America/Winnipeg`
-}
for__america__winnipeg : Zone -> Posix -> Result Int Abbreviation
for__america__winnipeg =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -389 || time.offsetMinutes == -388 then
                Ok Lmt

            else if time.offsetMinutes == -300 then
                Ok
                    (if time.posixSeconds >= -746035200 then
                        ShortName "CDT"

                     else if time.posixSeconds >= -769395600 then
                        ShortName "CPT"

                     else if time.posixSeconds >= -880214400 then
                        ShortName "CWT"

                     else
                        ShortName "CDT"
                    )

            else if time.offsetMinutes == -360 then
                Ok (ShortName "CST")

            else
                Err time.offsetMinutes
        )


{-| `America/Yakutat`
-}
for__america__yakutat : Zone -> Posix -> Result Int Abbreviation
for__america__yakutat =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -559 || time.offsetMinutes == -558 || time.offsetMinutes == 881 || time.offsetMinutes == 882 then
                Ok Lmt

            else if time.offsetMinutes == -540 then
                Ok
                    (if time.posixSeconds >= 439030800 then
                        ShortName "AKST"

                     else
                        ShortName "YST"
                    )

            else if time.offsetMinutes == -480 then
                Ok
                    (if time.posixSeconds >= 452084400 then
                        ShortName "AKDT"

                     else if time.posixSeconds >= -21474000 then
                        ShortName "YDT"

                     else if time.posixSeconds >= -769395600 then
                        ShortName "YPT"

                     else
                        ShortName "YWT"
                    )

            else
                Err time.offsetMinutes
        )


{-| `Antarctica/Casey`
-}
for__antarctica__casey : Zone -> Posix -> Result Int Abbreviation
for__antarctica__casey =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 480 || time.offsetMinutes == 660 then
                Ok (Offset time.offsetMinutes)

            else if time.offsetMinutes == 0 then
                Ok Uninhabited

            else
                Err time.offsetMinutes
        )


{-| `Antarctica/Davis`
-}
for__antarctica__davis : Zone -> Posix -> Result Int Abbreviation
for__antarctica__davis =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 300 || time.offsetMinutes == 420 then
                Ok (Offset time.offsetMinutes)

            else if time.offsetMinutes == 0 then
                Ok Uninhabited

            else
                Err time.offsetMinutes
        )


{-| `Antarctica/Macquarie`
-}
for__antarctica__macquarie : Zone -> Posix -> Result Int Abbreviation
for__antarctica__macquarie =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 0 then
                Ok Uninhabited

            else if time.offsetMinutes == 660 then
                Ok (ShortName "AEDT")

            else if time.offsetMinutes == 600 then
                Ok (ShortName "AEST")

            else
                Err time.offsetMinutes
        )


{-| `Antarctica/Mawson`
-}
for__antarctica__mawson : Zone -> Posix -> Result Int Abbreviation
for__antarctica__mawson =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 300 || time.offsetMinutes == 360 then
                Ok (Offset time.offsetMinutes)

            else if time.offsetMinutes == 0 then
                Ok Uninhabited

            else
                Err time.offsetMinutes
        )


{-| `Antarctica/Palmer`
-}
for__antarctica__palmer : Zone -> Posix -> Result Int Abbreviation
for__antarctica__palmer =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -240 || time.offsetMinutes == -180 || time.offsetMinutes == -120 then
                Ok (Offset time.offsetMinutes)

            else if time.offsetMinutes == 0 then
                Ok Uninhabited

            else
                Err time.offsetMinutes
        )


{-| `Antarctica/Rothera`
-}
for__antarctica__rothera : Zone -> Posix -> Result Int Abbreviation
for__antarctica__rothera =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -180 then
                Ok (Offset time.offsetMinutes)

            else if time.offsetMinutes == 0 then
                Ok Uninhabited

            else
                Err time.offsetMinutes
        )


{-| `Antarctica/Troll`
-}
for__antarctica__troll : Zone -> Posix -> Result Int Abbreviation
for__antarctica__troll =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 0 then
                Ok
                    (if time.posixSeconds >= 1108166400 then
                        Offset time.offsetMinutes

                     else
                        Uninhabited
                    )

            else
                Err time.offsetMinutes
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
        (\time ->
            if time.offsetMinutes == 307 || time.offsetMinutes == 308 then
                Ok Lmt

            else if time.offsetMinutes == 300 || time.offsetMinutes == 360 || time.offsetMinutes == 420 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `Asia/Amman`
-}
for__asia__amman : Zone -> Posix -> Result Int Abbreviation
for__asia__amman =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 143 || time.offsetMinutes == 144 then
                Ok Lmt

            else if time.offsetMinutes == 180 then
                Ok
                    (if time.posixSeconds >= 1666908000 then
                        Offset time.offsetMinutes

                     else
                        ShortName "EEST"
                    )

            else if time.offsetMinutes == 120 then
                Ok (ShortName "EET")

            else
                Err time.offsetMinutes
        )


{-| `Asia/Anadyr`
-}
for__asia__anadyr : Zone -> Posix -> Result Int Abbreviation
for__asia__anadyr =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 709 || time.offsetMinutes == 710 then
                Ok Lmt

            else if time.offsetMinutes == 660 || time.offsetMinutes == 720 || time.offsetMinutes == 780 || time.offsetMinutes == 840 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `Asia/Aqtau`
-}
for__asia__aqtau : Zone -> Posix -> Result Int Abbreviation
for__asia__aqtau =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 201 || time.offsetMinutes == 202 then
                Ok Lmt

            else if time.offsetMinutes == 240 || time.offsetMinutes == 300 || time.offsetMinutes == 360 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `Asia/Aqtobe`
-}
for__asia__aqtobe : Zone -> Posix -> Result Int Abbreviation
for__asia__aqtobe =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 228 || time.offsetMinutes == 229 then
                Ok Lmt

            else if time.offsetMinutes == 240 || time.offsetMinutes == 300 || time.offsetMinutes == 360 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `Asia/Ashgabat`
-}
for__asia__ashgabat : Zone -> Posix -> Result Int Abbreviation
for__asia__ashgabat =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 233 || time.offsetMinutes == 234 then
                Ok Lmt

            else if time.offsetMinutes == 240 || time.offsetMinutes == 300 || time.offsetMinutes == 360 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `Asia/Atyrau`
-}
for__asia__atyrau : Zone -> Posix -> Result Int Abbreviation
for__asia__atyrau =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 207 || time.offsetMinutes == 208 then
                Ok Lmt

            else if time.offsetMinutes == 180 || time.offsetMinutes == 240 || time.offsetMinutes == 300 || time.offsetMinutes == 360 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `Asia/Baghdad`
-}
for__asia__baghdad : Zone -> Posix -> Result Int Abbreviation
for__asia__baghdad =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 177 || time.offsetMinutes == 178 then
                Ok
                    (if time.posixSeconds >= -2524532260 then
                        ShortName "BMT"

                     else
                        Lmt
                    )

            else if time.offsetMinutes == 180 || time.offsetMinutes == 240 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `Asia/Baku`
-}
for__asia__baku : Zone -> Posix -> Result Int Abbreviation
for__asia__baku =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 199 || time.offsetMinutes == 200 then
                Ok Lmt

            else if time.offsetMinutes == 180 || time.offsetMinutes == 240 || time.offsetMinutes == 300 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `Asia/Bangkok`
-}
for__asia__bangkok : Zone -> Posix -> Result Int Abbreviation
for__asia__bangkok =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 402 || time.offsetMinutes == 403 then
                Ok
                    (if time.posixSeconds >= -2840164924 then
                        ShortName "BMT"

                     else
                        Lmt
                    )

            else if time.offsetMinutes == 420 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `Asia/Barnaul`
-}
for__asia__barnaul : Zone -> Posix -> Result Int Abbreviation
for__asia__barnaul =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 335 then
                Ok Lmt

            else if time.offsetMinutes == 360 || time.offsetMinutes == 420 || time.offsetMinutes == 480 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `Asia/Beirut`
-}
for__asia__beirut : Zone -> Posix -> Result Int Abbreviation
for__asia__beirut =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 142 then
                Ok Lmt

            else if time.offsetMinutes == 180 then
                Ok (ShortName "EEST")

            else if time.offsetMinutes == 120 then
                Ok (ShortName "EET")

            else
                Err time.offsetMinutes
        )


{-| `Asia/Bishkek`
-}
for__asia__bishkek : Zone -> Posix -> Result Int Abbreviation
for__asia__bishkek =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 298 || time.offsetMinutes == 299 then
                Ok Lmt

            else if time.offsetMinutes == 300 || time.offsetMinutes == 360 || time.offsetMinutes == 420 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `Asia/Chita`
-}
for__asia__chita : Zone -> Posix -> Result Int Abbreviation
for__asia__chita =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 453 || time.offsetMinutes == 454 then
                Ok Lmt

            else if time.offsetMinutes == 480 || time.offsetMinutes == 540 || time.offsetMinutes == 600 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `Asia/Colombo`
-}
for__asia__colombo : Zone -> Posix -> Result Int Abbreviation
for__asia__colombo =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 319 || time.offsetMinutes == 320 then
                Ok
                    (if time.posixSeconds >= -2840159964 then
                        ShortName "MMT"

                     else
                        Lmt
                    )

            else if time.offsetMinutes == 330 || time.offsetMinutes == 360 || time.offsetMinutes == 390 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `Asia/Damascus`
-}
for__asia__damascus : Zone -> Posix -> Result Int Abbreviation
for__asia__damascus =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 145 || time.offsetMinutes == 146 then
                Ok Lmt

            else if time.offsetMinutes == 180 then
                Ok
                    (if time.posixSeconds >= 1666904400 then
                        Offset time.offsetMinutes

                     else
                        ShortName "EEST"
                    )

            else if time.offsetMinutes == 120 then
                Ok (ShortName "EET")

            else
                Err time.offsetMinutes
        )


{-| `Asia/Dhaka`
-}
for__asia__dhaka : Zone -> Posix -> Result Int Abbreviation
for__asia__dhaka =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 361 || time.offsetMinutes == 362 then
                Ok Lmt

            else if time.offsetMinutes == 330 || time.offsetMinutes == 360 || time.offsetMinutes == 390 || time.offsetMinutes == 420 then
                Ok (Offset time.offsetMinutes)

            else if time.offsetMinutes == 353 || time.offsetMinutes == 354 then
                Ok (ShortName "HMT")

            else
                Err time.offsetMinutes
        )


{-| `Asia/Dili`
-}
for__asia__dili : Zone -> Posix -> Result Int Abbreviation
for__asia__dili =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 502 || time.offsetMinutes == 503 then
                Ok Lmt

            else if time.offsetMinutes == 480 || time.offsetMinutes == 540 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `Asia/Dubai`
-}
for__asia__dubai : Zone -> Posix -> Result Int Abbreviation
for__asia__dubai =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 221 || time.offsetMinutes == 222 then
                Ok Lmt

            else if time.offsetMinutes == 240 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `Asia/Dushanbe`
-}
for__asia__dushanbe : Zone -> Posix -> Result Int Abbreviation
for__asia__dushanbe =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 275 || time.offsetMinutes == 276 then
                Ok Lmt

            else if time.offsetMinutes == 300 || time.offsetMinutes == 360 || time.offsetMinutes == 420 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `Asia/Famagusta`
-}
for__asia__famagusta : Zone -> Posix -> Result Int Abbreviation
for__asia__famagusta =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 135 || time.offsetMinutes == 136 then
                Ok Lmt

            else if time.offsetMinutes == 180 then
                Ok
                    (if time.posixSeconds >= 1473282000 then
                        Offset time.offsetMinutes

                     else
                        ShortName "EEST"
                    )

            else if time.offsetMinutes == 120 then
                Ok (ShortName "EET")

            else
                Err time.offsetMinutes
        )


{-| `Asia/Gaza`
-}
for__asia__gaza : Zone -> Posix -> Result Int Abbreviation
for__asia__gaza =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 137 || time.offsetMinutes == 138 then
                Ok Lmt

            else if time.offsetMinutes == 180 then
                Ok
                    (if time.posixSeconds >= 828655200 then
                        ShortName "EEST"

                     else if time.posixSeconds >= 142380000 then
                        ShortName "IDT"

                     else
                        ShortName "EEST"
                    )

            else if time.offsetMinutes == 120 then
                Ok
                    (if time.posixSeconds >= 820447200 then
                        ShortName "EET"

                     else if time.posixSeconds >= -81313200 then
                        ShortName "IST"

                     else
                        ShortName "EET"
                    )

            else
                Err time.offsetMinutes
        )


{-| `Asia/Hebron`
-}
for__asia__hebron : Zone -> Posix -> Result Int Abbreviation
for__asia__hebron =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 140 || time.offsetMinutes == 141 then
                Ok Lmt

            else if time.offsetMinutes == 180 then
                Ok
                    (if time.posixSeconds >= 828655200 then
                        ShortName "EEST"

                     else if time.posixSeconds >= 142380000 then
                        ShortName "IDT"

                     else
                        ShortName "EEST"
                    )

            else if time.offsetMinutes == 120 then
                Ok
                    (if time.posixSeconds >= 820447200 then
                        ShortName "EET"

                     else if time.posixSeconds >= -81313200 then
                        ShortName "IST"

                     else
                        ShortName "EET"
                    )

            else
                Err time.offsetMinutes
        )


{-| `Asia/Ho_Chi_Minh`
-}
for__asia__ho_chi_minh : Zone -> Posix -> Result Int Abbreviation
for__asia__ho_chi_minh =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 426 || time.offsetMinutes == 427 then
                Ok
                    (if time.posixSeconds >= -2004073590 then
                        ShortName "PLMT"

                     else
                        Lmt
                    )

            else if time.offsetMinutes == 420 || time.offsetMinutes == 480 || time.offsetMinutes == 540 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `Asia/Hong_Kong`
-}
for__asia__hong_kong : Zone -> Posix -> Result Int Abbreviation
for__asia__hong_kong =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 456 || time.offsetMinutes == 457 then
                Ok Lmt

            else if time.offsetMinutes == 540 then
                Ok
                    (if time.posixSeconds >= -747907200 then
                        ShortName "HKST"

                     else if time.posixSeconds >= -884248200 then
                        ShortName "JST"

                     else
                        ShortName "HKST"
                    )

            else if time.offsetMinutes == 480 then
                Ok (ShortName "HKT")

            else if time.offsetMinutes == 510 then
                Ok (ShortName "HKWT")

            else
                Err time.offsetMinutes
        )


{-| `Asia/Hovd`
-}
for__asia__hovd : Zone -> Posix -> Result Int Abbreviation
for__asia__hovd =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 366 || time.offsetMinutes == 367 then
                Ok Lmt

            else if time.offsetMinutes == 360 || time.offsetMinutes == 420 || time.offsetMinutes == 480 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `Asia/Irkutsk`
-}
for__asia__irkutsk : Zone -> Posix -> Result Int Abbreviation
for__asia__irkutsk =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 417 || time.offsetMinutes == 418 then
                Ok
                    (if time.posixSeconds >= -2840165825 then
                        ShortName "IMT"

                     else
                        Lmt
                    )

            else if time.offsetMinutes == 420 || time.offsetMinutes == 480 || time.offsetMinutes == 540 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `Asia/Jakarta`
-}
for__asia__jakarta : Zone -> Posix -> Result Int Abbreviation
for__asia__jakarta =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 427 || time.offsetMinutes == 428 then
                Ok
                    (if time.posixSeconds >= -3231299232 then
                        ShortName "BMT"

                     else
                        Lmt
                    )

            else if time.offsetMinutes == 440 || time.offsetMinutes == 450 || time.offsetMinutes == 480 || time.offsetMinutes == 540 then
                Ok (Offset time.offsetMinutes)

            else if time.offsetMinutes == 420 then
                Ok (ShortName "WIB")

            else
                Err time.offsetMinutes
        )


{-| `Asia/Jayapura`
-}
for__asia__jayapura : Zone -> Posix -> Result Int Abbreviation
for__asia__jayapura =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 562 || time.offsetMinutes == 563 then
                Ok Lmt

            else if time.offsetMinutes == 570 then
                Ok (Offset time.offsetMinutes)

            else if time.offsetMinutes == 540 then
                Ok
                    (if time.posixSeconds >= -189423000 then
                        ShortName "WIT"

                     else
                        Offset time.offsetMinutes
                    )

            else
                Err time.offsetMinutes
        )


{-| `Asia/Jerusalem`
-}
for__asia__jerusalem : Zone -> Posix -> Result Int Abbreviation
for__asia__jerusalem =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 140 || time.offsetMinutes == 141 then
                Ok
                    (if time.posixSeconds >= -2840149254 then
                        ShortName "JMT"

                     else
                        Lmt
                    )

            else if time.offsetMinutes == 240 then
                Ok (ShortName "IDDT")

            else if time.offsetMinutes == 180 then
                Ok (ShortName "IDT")

            else if time.offsetMinutes == 120 then
                Ok (ShortName "IST")

            else
                Err time.offsetMinutes
        )


{-| `Asia/Kabul`
-}
for__asia__kabul : Zone -> Posix -> Result Int Abbreviation
for__asia__kabul =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 276 || time.offsetMinutes == 277 then
                Ok Lmt

            else if time.offsetMinutes == 240 || time.offsetMinutes == 270 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `Asia/Kamchatka`
-}
for__asia__kamchatka : Zone -> Posix -> Result Int Abbreviation
for__asia__kamchatka =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 634 || time.offsetMinutes == 635 then
                Ok Lmt

            else if time.offsetMinutes == 660 || time.offsetMinutes == 720 || time.offsetMinutes == 780 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `Asia/Karachi`
-}
for__asia__karachi : Zone -> Posix -> Result Int Abbreviation
for__asia__karachi =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 268 || time.offsetMinutes == 269 then
                Ok Lmt

            else if time.offsetMinutes == 330 || time.offsetMinutes == 390 then
                Ok (Offset time.offsetMinutes)

            else if time.offsetMinutes == 300 then
                Ok
                    (if time.posixSeconds >= 38775600 then
                        ShortName "PKT"

                     else
                        Offset time.offsetMinutes
                    )

            else if time.offsetMinutes == 360 then
                Ok (ShortName "PKST")

            else
                Err time.offsetMinutes
        )


{-| `Asia/Kathmandu`
-}
for__asia__kathmandu : Zone -> Posix -> Result Int Abbreviation
for__asia__kathmandu =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 341 || time.offsetMinutes == 342 then
                Ok Lmt

            else if time.offsetMinutes == 330 || time.offsetMinutes == 345 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `Asia/Khandyga`
-}
for__asia__khandyga : Zone -> Posix -> Result Int Abbreviation
for__asia__khandyga =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 542 || time.offsetMinutes == 543 then
                Ok Lmt

            else if time.offsetMinutes == 480 || time.offsetMinutes == 540 || time.offsetMinutes == 600 || time.offsetMinutes == 660 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `Asia/Kolkata`
-}
for__asia__kolkata : Zone -> Posix -> Result Int Abbreviation
for__asia__kolkata =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 353 || time.offsetMinutes == 354 then
                Ok
                    (if time.posixSeconds >= -3645237208 then
                        ShortName "HMT"

                     else
                        Lmt
                    )

            else if time.offsetMinutes == 390 then
                Ok (Offset time.offsetMinutes)

            else if time.offsetMinutes == 330 then
                Ok (ShortName "IST")

            else if time.offsetMinutes == 321 || time.offsetMinutes == 322 then
                Ok (ShortName "MMT")

            else
                Err time.offsetMinutes
        )


{-| `Asia/Krasnoyarsk`
-}
for__asia__krasnoyarsk : Zone -> Posix -> Result Int Abbreviation
for__asia__krasnoyarsk =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 371 || time.offsetMinutes == 372 then
                Ok Lmt

            else if time.offsetMinutes == 360 || time.offsetMinutes == 420 || time.offsetMinutes == 480 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `Asia/Kuching`
-}
for__asia__kuching : Zone -> Posix -> Result Int Abbreviation
for__asia__kuching =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 441 || time.offsetMinutes == 442 then
                Ok Lmt

            else if time.offsetMinutes == 450 || time.offsetMinutes == 480 || time.offsetMinutes == 500 || time.offsetMinutes == 540 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `Asia/Macau`
-}
for__asia__macau : Zone -> Posix -> Result Int Abbreviation
for__asia__macau =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 454 || time.offsetMinutes == 455 then
                Ok Lmt

            else if time.offsetMinutes == 600 then
                Ok (Offset time.offsetMinutes)

            else if time.offsetMinutes == 540 then
                Ok
                    (if time.posixSeconds >= -747046800 then
                        ShortName "CDT"

                     else
                        Offset time.offsetMinutes
                    )

            else if time.offsetMinutes == 480 then
                Ok (ShortName "CST")

            else
                Err time.offsetMinutes
        )


{-| `Asia/Magadan`
-}
for__asia__magadan : Zone -> Posix -> Result Int Abbreviation
for__asia__magadan =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 603 || time.offsetMinutes == 604 then
                Ok Lmt

            else if time.offsetMinutes == 600 || time.offsetMinutes == 660 || time.offsetMinutes == 720 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `Asia/Makassar`
-}
for__asia__makassar : Zone -> Posix -> Result Int Abbreviation
for__asia__makassar =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 477 || time.offsetMinutes == 478 then
                Ok
                    (if time.posixSeconds >= -1577951856 then
                        ShortName "MMT"

                     else
                        Lmt
                    )

            else if time.offsetMinutes == 540 then
                Ok (Offset time.offsetMinutes)

            else if time.offsetMinutes == 480 then
                Ok
                    (if time.posixSeconds >= -766054800 then
                        ShortName "WITA"

                     else
                        Offset time.offsetMinutes
                    )

            else
                Err time.offsetMinutes
        )


{-| `Asia/Manila`
-}
for__asia__manila : Zone -> Posix -> Result Int Abbreviation
for__asia__manila =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -956 || time.offsetMinutes == 484 then
                Ok Lmt

            else if time.offsetMinutes == 540 then
                Ok
                    (if time.posixSeconds >= -496224000 then
                        ShortName "PDT"

                     else if time.posixSeconds >= -873273600 then
                        ShortName "JST"

                     else
                        ShortName "PDT"
                    )

            else if time.offsetMinutes == 480 then
                Ok (ShortName "PST")

            else
                Err time.offsetMinutes
        )


{-| `Asia/Nicosia`
-}
for__asia__nicosia : Zone -> Posix -> Result Int Abbreviation
for__asia__nicosia =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 133 || time.offsetMinutes == 134 then
                Ok Lmt

            else if time.offsetMinutes == 180 then
                Ok (ShortName "EEST")

            else if time.offsetMinutes == 120 then
                Ok (ShortName "EET")

            else
                Err time.offsetMinutes
        )


{-| `Asia/Novokuznetsk`
-}
for__asia__novokuznetsk : Zone -> Posix -> Result Int Abbreviation
for__asia__novokuznetsk =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 348 || time.offsetMinutes == 349 then
                Ok Lmt

            else if time.offsetMinutes == 360 || time.offsetMinutes == 420 || time.offsetMinutes == 480 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `Asia/Novosibirsk`
-}
for__asia__novosibirsk : Zone -> Posix -> Result Int Abbreviation
for__asia__novosibirsk =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 331 || time.offsetMinutes == 332 then
                Ok Lmt

            else if time.offsetMinutes == 360 || time.offsetMinutes == 420 || time.offsetMinutes == 480 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `Asia/Omsk`
-}
for__asia__omsk : Zone -> Posix -> Result Int Abbreviation
for__asia__omsk =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 293 || time.offsetMinutes == 294 then
                Ok Lmt

            else if time.offsetMinutes == 300 || time.offsetMinutes == 360 || time.offsetMinutes == 420 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `Asia/Oral`
-}
for__asia__oral : Zone -> Posix -> Result Int Abbreviation
for__asia__oral =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 205 || time.offsetMinutes == 206 then
                Ok Lmt

            else if time.offsetMinutes == 180 || time.offsetMinutes == 240 || time.offsetMinutes == 300 || time.offsetMinutes == 360 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `Asia/Pontianak`
-}
for__asia__pontianak : Zone -> Posix -> Result Int Abbreviation
for__asia__pontianak =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 437 || time.offsetMinutes == 438 then
                Ok
                    (if time.posixSeconds >= -1946186240 then
                        ShortName "PMT"

                     else
                        Lmt
                    )

            else if time.offsetMinutes == 450 || time.offsetMinutes == 540 then
                Ok (Offset time.offsetMinutes)

            else if time.offsetMinutes == 480 then
                Ok
                    (if time.posixSeconds >= -189415800 then
                        ShortName "WITA"

                     else
                        Offset time.offsetMinutes
                    )

            else if time.offsetMinutes == 420 then
                Ok (ShortName "WIB")

            else
                Err time.offsetMinutes
        )


{-| `Asia/Pyongyang`
-}
for__asia__pyongyang : Zone -> Posix -> Result Int Abbreviation
for__asia__pyongyang =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 503 then
                Ok Lmt

            else if time.offsetMinutes == 540 then
                Ok
                    (if time.posixSeconds >= -768646800 then
                        ShortName "KST"

                     else
                        ShortName "JST"
                    )

            else if time.offsetMinutes == 510 then
                Ok (ShortName "KST")

            else
                Err time.offsetMinutes
        )


{-| `Asia/Qatar`
-}
for__asia__qatar : Zone -> Posix -> Result Int Abbreviation
for__asia__qatar =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 206 || time.offsetMinutes == 207 then
                Ok Lmt

            else if time.offsetMinutes == 180 || time.offsetMinutes == 240 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `Asia/Qostanay`
-}
for__asia__qostanay : Zone -> Posix -> Result Int Abbreviation
for__asia__qostanay =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 254 || time.offsetMinutes == 255 then
                Ok Lmt

            else if time.offsetMinutes == 240 || time.offsetMinutes == 300 || time.offsetMinutes == 360 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `Asia/Qyzylorda`
-}
for__asia__qyzylorda : Zone -> Posix -> Result Int Abbreviation
for__asia__qyzylorda =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 261 || time.offsetMinutes == 262 then
                Ok Lmt

            else if time.offsetMinutes == 240 || time.offsetMinutes == 300 || time.offsetMinutes == 360 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `Asia/Riyadh`
-}
for__asia__riyadh : Zone -> Posix -> Result Int Abbreviation
for__asia__riyadh =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 186 || time.offsetMinutes == 187 then
                Ok Lmt

            else if time.offsetMinutes == 180 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `Asia/Sakhalin`
-}
for__asia__sakhalin : Zone -> Posix -> Result Int Abbreviation
for__asia__sakhalin =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 570 || time.offsetMinutes == 571 then
                Ok Lmt

            else if time.offsetMinutes == 540 || time.offsetMinutes == 600 || time.offsetMinutes == 660 || time.offsetMinutes == 720 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `Asia/Samarkand`
-}
for__asia__samarkand : Zone -> Posix -> Result Int Abbreviation
for__asia__samarkand =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 267 || time.offsetMinutes == 268 then
                Ok Lmt

            else if time.offsetMinutes == 240 || time.offsetMinutes == 300 || time.offsetMinutes == 360 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `Asia/Seoul`
-}
for__asia__seoul : Zone -> Posix -> Result Int Abbreviation
for__asia__seoul =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 507 || time.offsetMinutes == 508 then
                Ok Lmt

            else if time.offsetMinutes == 540 then
                Ok
                    (if time.posixSeconds >= -767350800 then
                        ShortName "KST"

                     else
                        ShortName "JST"
                    )

            else if time.offsetMinutes == 570 || time.offsetMinutes == 600 then
                Ok (ShortName "KDT")

            else if time.offsetMinutes == 510 then
                Ok (ShortName "KST")

            else
                Err time.offsetMinutes
        )


{-| `Asia/Shanghai`
-}
for__asia__shanghai : Zone -> Posix -> Result Int Abbreviation
for__asia__shanghai =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 485 || time.offsetMinutes == 486 then
                Ok Lmt

            else if time.offsetMinutes == 540 then
                Ok (ShortName "CDT")

            else if time.offsetMinutes == 480 then
                Ok (ShortName "CST")

            else
                Err time.offsetMinutes
        )


{-| `Asia/Singapore`
-}
for__asia__singapore : Zone -> Posix -> Result Int Abbreviation
for__asia__singapore =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 415 || time.offsetMinutes == 416 then
                Ok
                    (if time.posixSeconds >= -2177477725 then
                        ShortName "SMT"

                     else
                        Lmt
                    )

            else if time.offsetMinutes == 420 || time.offsetMinutes == 440 || time.offsetMinutes == 450 || time.offsetMinutes == 480 || time.offsetMinutes == 540 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `Asia/Srednekolymsk`
-}
for__asia__srednekolymsk : Zone -> Posix -> Result Int Abbreviation
for__asia__srednekolymsk =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 614 || time.offsetMinutes == 615 then
                Ok Lmt

            else if time.offsetMinutes == 600 || time.offsetMinutes == 660 || time.offsetMinutes == 720 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `Asia/Taipei`
-}
for__asia__taipei : Zone -> Posix -> Result Int Abbreviation
for__asia__taipei =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 486 then
                Ok Lmt

            else if time.offsetMinutes == 480 then
                Ok (ShortName "CST")

            else if time.offsetMinutes == 540 then
                Ok
                    (if time.posixSeconds >= -745833600 then
                        ShortName "CDT"

                     else
                        ShortName "JST"
                    )

            else
                Err time.offsetMinutes
        )


{-| `Asia/Tashkent`
-}
for__asia__tashkent : Zone -> Posix -> Result Int Abbreviation
for__asia__tashkent =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 277 || time.offsetMinutes == 278 then
                Ok Lmt

            else if time.offsetMinutes == 300 || time.offsetMinutes == 360 || time.offsetMinutes == 420 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `Asia/Tbilisi`
-}
for__asia__tbilisi : Zone -> Posix -> Result Int Abbreviation
for__asia__tbilisi =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 179 then
                Ok
                    (if time.posixSeconds >= -2840151551 then
                        ShortName "TBMT"

                     else
                        Lmt
                    )

            else if time.offsetMinutes == 180 then
                Ok
                    (if time.posixSeconds >= -1441162751 then
                        Offset time.offsetMinutes

                     else if time.posixSeconds >= -2840151551 then
                        ShortName "TBMT"

                     else
                        Lmt
                    )

            else if time.offsetMinutes == 240 || time.offsetMinutes == 300 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `Asia/Tehran`
-}
for__asia__tehran : Zone -> Posix -> Result Int Abbreviation
for__asia__tehran =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 205 || time.offsetMinutes == 206 then
                Ok
                    (if time.posixSeconds >= -1704165944 then
                        ShortName "TMT"

                     else
                        Lmt
                    )

            else if time.offsetMinutes == 210 || time.offsetMinutes == 240 || time.offsetMinutes == 270 || time.offsetMinutes == 300 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `Asia/Thimphu`
-}
for__asia__thimphu : Zone -> Posix -> Result Int Abbreviation
for__asia__thimphu =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 358 || time.offsetMinutes == 359 then
                Ok Lmt

            else if time.offsetMinutes == 330 || time.offsetMinutes == 360 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `Asia/Tokyo`
-}
for__asia__tokyo : Zone -> Posix -> Result Int Abbreviation
for__asia__tokyo =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 558 || time.offsetMinutes == 559 then
                Ok Lmt

            else if time.offsetMinutes == 600 then
                Ok (ShortName "JDT")

            else if time.offsetMinutes == 540 then
                Ok (ShortName "JST")

            else
                Err time.offsetMinutes
        )


{-| `Asia/Tomsk`
-}
for__asia__tomsk : Zone -> Posix -> Result Int Abbreviation
for__asia__tomsk =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 339 || time.offsetMinutes == 340 then
                Ok Lmt

            else if time.offsetMinutes == 360 || time.offsetMinutes == 420 || time.offsetMinutes == 480 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `Asia/Ulaanbaatar`
-}
for__asia__ulaanbaatar : Zone -> Posix -> Result Int Abbreviation
for__asia__ulaanbaatar =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 427 || time.offsetMinutes == 428 then
                Ok Lmt

            else if time.offsetMinutes == 420 || time.offsetMinutes == 480 || time.offsetMinutes == 540 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `Asia/Urumqi`
-}
for__asia__urumqi : Zone -> Posix -> Result Int Abbreviation
for__asia__urumqi =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 350 || time.offsetMinutes == 351 then
                Ok Lmt

            else if time.offsetMinutes == 360 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `Asia/Ust-Nera`
-}
for__asia__ust_nera : Zone -> Posix -> Result Int Abbreviation
for__asia__ust_nera =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 572 || time.offsetMinutes == 573 then
                Ok Lmt

            else if time.offsetMinutes == 480 || time.offsetMinutes == 540 || time.offsetMinutes == 600 || time.offsetMinutes == 660 || time.offsetMinutes == 720 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `Asia/Vladivostok`
-}
for__asia__vladivostok : Zone -> Posix -> Result Int Abbreviation
for__asia__vladivostok =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 527 || time.offsetMinutes == 528 then
                Ok Lmt

            else if time.offsetMinutes == 540 || time.offsetMinutes == 600 || time.offsetMinutes == 660 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `Asia/Yakutsk`
-}
for__asia__yakutsk : Zone -> Posix -> Result Int Abbreviation
for__asia__yakutsk =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 518 || time.offsetMinutes == 519 then
                Ok Lmt

            else if time.offsetMinutes == 480 || time.offsetMinutes == 540 || time.offsetMinutes == 600 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `Asia/Yangon`
-}
for__asia__yangon : Zone -> Posix -> Result Int Abbreviation
for__asia__yangon =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 384 || time.offsetMinutes == 385 then
                Ok
                    (if time.posixSeconds >= -2840163887 then
                        ShortName "RMT"

                     else
                        Lmt
                    )

            else if time.offsetMinutes == 390 || time.offsetMinutes == 540 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `Asia/Yekaterinburg`
-}
for__asia__yekaterinburg : Zone -> Posix -> Result Int Abbreviation
for__asia__yekaterinburg =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 242 || time.offsetMinutes == 243 then
                Ok Lmt

            else if time.offsetMinutes == 240 || time.offsetMinutes == 300 || time.offsetMinutes == 360 then
                Ok (Offset time.offsetMinutes)

            else if time.offsetMinutes == 225 || time.offsetMinutes == 226 then
                Ok (ShortName "PMT")

            else
                Err time.offsetMinutes
        )


{-| `Asia/Yerevan`
-}
for__asia__yerevan : Zone -> Posix -> Result Int Abbreviation
for__asia__yerevan =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 178 then
                Ok Lmt

            else if time.offsetMinutes == 180 || time.offsetMinutes == 240 || time.offsetMinutes == 300 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `Atlantic/Azores`
-}
for__atlantic__azores : Zone -> Posix -> Result Int Abbreviation
for__atlantic__azores =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -103 || time.offsetMinutes == -102 then
                Ok Lmt

            else if time.offsetMinutes == -120 || time.offsetMinutes == -60 then
                Ok (Offset time.offsetMinutes)

            else if time.offsetMinutes == 0 then
                Ok
                    (if time.posixSeconds >= 740278800 then
                        Offset time.offsetMinutes

                     else if time.posixSeconds >= 725421600 then
                        ShortName "WET"

                     else
                        Offset time.offsetMinutes
                    )

            else if time.offsetMinutes == -115 || time.offsetMinutes == -114 then
                Ok (ShortName "HMT")

            else if time.offsetMinutes == 60 then
                Ok (ShortName "WEST")

            else
                Err time.offsetMinutes
        )


{-| `Atlantic/Bermuda`
-}
for__atlantic__bermuda : Zone -> Posix -> Result Int Abbreviation
for__atlantic__bermuda =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -260 || time.offsetMinutes == -259 then
                Ok
                    (if time.posixSeconds >= -2524506042 then
                        ShortName "BMT"

                     else
                        Lmt
                    )

            else if time.offsetMinutes == -180 then
                Ok (ShortName "ADT")

            else if time.offsetMinutes == -240 then
                Ok (ShortName "AST")

            else if time.offsetMinutes == -200 || time.offsetMinutes == -199 then
                Ok (ShortName "BST")

            else
                Err time.offsetMinutes
        )


{-| `Atlantic/Canary`
-}
for__atlantic__canary : Zone -> Posix -> Result Int Abbreviation
for__atlantic__canary =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -62 || time.offsetMinutes == -61 then
                Ok Lmt

            else if time.offsetMinutes == -60 then
                Ok (Offset time.offsetMinutes)

            else if time.offsetMinutes == 60 then
                Ok (ShortName "WEST")

            else if time.offsetMinutes == 0 then
                Ok (ShortName "WET")

            else
                Err time.offsetMinutes
        )


{-| `Atlantic/Cape_Verde`
-}
for__atlantic__cape_verde : Zone -> Posix -> Result Int Abbreviation
for__atlantic__cape_verde =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -95 || time.offsetMinutes == -94 then
                Ok Lmt

            else if time.offsetMinutes == -120 || time.offsetMinutes == -60 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `Atlantic/Faroe`
-}
for__atlantic__faroe : Zone -> Posix -> Result Int Abbreviation
for__atlantic__faroe =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -28 || time.offsetMinutes == -27 then
                Ok Lmt

            else if time.offsetMinutes == 60 then
                Ok (ShortName "WEST")

            else if time.offsetMinutes == 0 then
                Ok (ShortName "WET")

            else
                Err time.offsetMinutes
        )


{-| `Atlantic/Madeira`
-}
for__atlantic__madeira : Zone -> Posix -> Result Int Abbreviation
for__atlantic__madeira =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -68 || time.offsetMinutes == -67 then
                Ok
                    (if time.posixSeconds >= -2713906344 then
                        ShortName "FMT"

                     else
                        Lmt
                    )

            else if time.offsetMinutes == -60 then
                Ok (Offset time.offsetMinutes)

            else if time.offsetMinutes == 0 then
                Ok
                    (if time.posixSeconds >= -102546000 then
                        ShortName "WET"

                     else
                        Offset time.offsetMinutes
                    )

            else if time.offsetMinutes == 60 then
                Ok
                    (if time.posixSeconds >= 386726400 then
                        ShortName "WEST"

                     else
                        Offset time.offsetMinutes
                    )

            else
                Err time.offsetMinutes
        )


{-| `Atlantic/South_Georgia`
-}
for__atlantic__south_georgia : Zone -> Posix -> Result Int Abbreviation
for__atlantic__south_georgia =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -147 || time.offsetMinutes == -146 then
                Ok Lmt

            else if time.offsetMinutes == -120 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `Atlantic/Stanley`
-}
for__atlantic__stanley : Zone -> Posix -> Result Int Abbreviation
for__atlantic__stanley =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -232 || time.offsetMinutes == -231 then
                Ok
                    (if time.posixSeconds >= -2524507716 then
                        ShortName "SMT"

                     else
                        Lmt
                    )

            else if time.offsetMinutes == -240 || time.offsetMinutes == -180 || time.offsetMinutes == -120 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `Australia/Adelaide`
-}
for__australia__adelaide : Zone -> Posix -> Result Int Abbreviation
for__australia__adelaide =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 554 || time.offsetMinutes == 555 then
                Ok Lmt

            else if time.offsetMinutes == 630 then
                Ok (ShortName "ACDT")

            else if time.offsetMinutes == 540 || time.offsetMinutes == 570 then
                Ok (ShortName "ACST")

            else
                Err time.offsetMinutes
        )


{-| `Australia/Brisbane`
-}
for__australia__brisbane : Zone -> Posix -> Result Int Abbreviation
for__australia__brisbane =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 612 || time.offsetMinutes == 613 then
                Ok Lmt

            else if time.offsetMinutes == 660 then
                Ok (ShortName "AEDT")

            else if time.offsetMinutes == 600 then
                Ok (ShortName "AEST")

            else
                Err time.offsetMinutes
        )


{-| `Australia/Broken_Hill`
-}
for__australia__broken_hill : Zone -> Posix -> Result Int Abbreviation
for__australia__broken_hill =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 565 || time.offsetMinutes == 566 then
                Ok Lmt

            else if time.offsetMinutes == 630 then
                Ok (ShortName "ACDT")

            else if time.offsetMinutes == 540 || time.offsetMinutes == 570 then
                Ok (ShortName "ACST")

            else if time.offsetMinutes == 600 then
                Ok (ShortName "AEST")

            else
                Err time.offsetMinutes
        )


{-| `Australia/Darwin`
-}
for__australia__darwin : Zone -> Posix -> Result Int Abbreviation
for__australia__darwin =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 523 || time.offsetMinutes == 524 then
                Ok Lmt

            else if time.offsetMinutes == 630 then
                Ok (ShortName "ACDT")

            else if time.offsetMinutes == 540 || time.offsetMinutes == 570 then
                Ok (ShortName "ACST")

            else
                Err time.offsetMinutes
        )


{-| `Australia/Eucla`
-}
for__australia__eucla : Zone -> Posix -> Result Int Abbreviation
for__australia__eucla =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 515 || time.offsetMinutes == 516 then
                Ok Lmt

            else if time.offsetMinutes == 525 || time.offsetMinutes == 585 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `Australia/Hobart`
-}
for__australia__hobart : Zone -> Posix -> Result Int Abbreviation
for__australia__hobart =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 589 || time.offsetMinutes == 590 then
                Ok Lmt

            else if time.offsetMinutes == 660 then
                Ok (ShortName "AEDT")

            else if time.offsetMinutes == 600 then
                Ok (ShortName "AEST")

            else
                Err time.offsetMinutes
        )


{-| `Australia/Lindeman`
-}
for__australia__lindeman : Zone -> Posix -> Result Int Abbreviation
for__australia__lindeman =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 595 || time.offsetMinutes == 596 then
                Ok Lmt

            else if time.offsetMinutes == 660 then
                Ok (ShortName "AEDT")

            else if time.offsetMinutes == 600 then
                Ok (ShortName "AEST")

            else
                Err time.offsetMinutes
        )


{-| `Australia/Lord_Howe`
-}
for__australia__lord_howe : Zone -> Posix -> Result Int Abbreviation
for__australia__lord_howe =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 636 || time.offsetMinutes == 637 then
                Ok Lmt

            else if time.offsetMinutes == 630 || time.offsetMinutes == 660 || time.offsetMinutes == 690 then
                Ok (Offset time.offsetMinutes)

            else if time.offsetMinutes == 600 then
                Ok (ShortName "AEST")

            else
                Err time.offsetMinutes
        )


{-| `Australia/Melbourne`
-}
for__australia__melbourne : Zone -> Posix -> Result Int Abbreviation
for__australia__melbourne =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 579 || time.offsetMinutes == 580 then
                Ok Lmt

            else if time.offsetMinutes == 660 then
                Ok (ShortName "AEDT")

            else if time.offsetMinutes == 600 then
                Ok (ShortName "AEST")

            else
                Err time.offsetMinutes
        )


{-| `Australia/Perth`
-}
for__australia__perth : Zone -> Posix -> Result Int Abbreviation
for__australia__perth =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 463 || time.offsetMinutes == 464 then
                Ok Lmt

            else if time.offsetMinutes == 540 then
                Ok (ShortName "AWDT")

            else if time.offsetMinutes == 480 then
                Ok (ShortName "AWST")

            else
                Err time.offsetMinutes
        )


{-| `Australia/Sydney`
-}
for__australia__sydney : Zone -> Posix -> Result Int Abbreviation
for__australia__sydney =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 604 || time.offsetMinutes == 605 then
                Ok Lmt

            else if time.offsetMinutes == 660 then
                Ok (ShortName "AEDT")

            else if time.offsetMinutes == 600 then
                Ok (ShortName "AEST")

            else
                Err time.offsetMinutes
        )


{-| `Europe/Andorra`
-}
for__europe__andorra : Zone -> Posix -> Result Int Abbreviation
for__europe__andorra =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 6 || time.offsetMinutes == 7 then
                Ok Lmt

            else if time.offsetMinutes == 120 then
                Ok (ShortName "CEST")

            else if time.offsetMinutes == 60 then
                Ok (ShortName "CET")

            else if time.offsetMinutes == 0 then
                Ok (ShortName "WET")

            else
                Err time.offsetMinutes
        )


{-| `Europe/Astrakhan`
-}
for__europe__astrakhan : Zone -> Posix -> Result Int Abbreviation
for__europe__astrakhan =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 192 || time.offsetMinutes == 193 then
                Ok Lmt

            else if time.offsetMinutes == 180 || time.offsetMinutes == 240 || time.offsetMinutes == 300 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `Europe/Athens`
-}
for__europe__athens : Zone -> Posix -> Result Int Abbreviation
for__europe__athens =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 94 || time.offsetMinutes == 95 then
                Ok
                    (if time.posixSeconds >= -2344642492 then
                        ShortName "AMT"

                     else
                        Lmt
                    )

            else if time.offsetMinutes == 60 then
                Ok (ShortName "CET")

            else if time.offsetMinutes == 180 then
                Ok (ShortName "EEST")

            else if time.offsetMinutes == 120 then
                Ok
                    (if time.posixSeconds >= -812422800 then
                        ShortName "EET"

                     else if time.posixSeconds >= -904878000 then
                        ShortName "CEST"

                     else
                        ShortName "EET"
                    )

            else
                Err time.offsetMinutes
        )


{-| `Europe/Belgrade`
-}
for__europe__belgrade : Zone -> Posix -> Result Int Abbreviation
for__europe__belgrade =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 82 then
                Ok Lmt

            else if time.offsetMinutes == 120 then
                Ok (ShortName "CEST")

            else if time.offsetMinutes == 60 then
                Ok (ShortName "CET")

            else
                Err time.offsetMinutes
        )


{-| `Europe/Berlin`
-}
for__europe__berlin : Zone -> Posix -> Result Int Abbreviation
for__europe__berlin =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 53 || time.offsetMinutes == 54 then
                Ok Lmt

            else if time.offsetMinutes == 180 then
                Ok (ShortName "CEMT")

            else if time.offsetMinutes == 120 then
                Ok (ShortName "CEST")

            else if time.offsetMinutes == 60 then
                Ok (ShortName "CET")

            else
                Err time.offsetMinutes
        )


{-| `Europe/Brussels`
-}
for__europe__brussels : Zone -> Posix -> Result Int Abbreviation
for__europe__brussels =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 17 || time.offsetMinutes == 18 then
                Ok
                    (if time.posixSeconds >= -2840141850 then
                        ShortName "BMT"

                     else
                        Lmt
                    )

            else if time.offsetMinutes == 120 then
                Ok (ShortName "CEST")

            else if time.offsetMinutes == 60 then
                Ok
                    (if time.posixSeconds >= -857257200 then
                        ShortName "CET"

                     else if time.posixSeconds >= -1604278800 then
                        ShortName "WEST"

                     else
                        ShortName "CET"
                    )

            else if time.offsetMinutes == 0 then
                Ok (ShortName "WET")

            else
                Err time.offsetMinutes
        )


{-| `Europe/Bucharest`
-}
for__europe__bucharest : Zone -> Posix -> Result Int Abbreviation
for__europe__bucharest =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 104 || time.offsetMinutes == 105 then
                Ok
                    (if time.posixSeconds >= -2469404664 then
                        ShortName "BMT"

                     else
                        Lmt
                    )

            else if time.offsetMinutes == 180 then
                Ok (ShortName "EEST")

            else if time.offsetMinutes == 120 then
                Ok (ShortName "EET")

            else
                Err time.offsetMinutes
        )


{-| `Europe/Budapest`
-}
for__europe__budapest : Zone -> Posix -> Result Int Abbreviation
for__europe__budapest =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 76 || time.offsetMinutes == 77 then
                Ok Lmt

            else if time.offsetMinutes == 120 then
                Ok (ShortName "CEST")

            else if time.offsetMinutes == 60 then
                Ok (ShortName "CET")

            else
                Err time.offsetMinutes
        )


{-| `Europe/Chisinau`
-}
for__europe__chisinau : Zone -> Posix -> Result Int Abbreviation
for__europe__chisinau =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 116 then
                Ok Lmt

            else if time.offsetMinutes == 115 then
                Ok
                    (if time.posixSeconds >= -2840147720 then
                        ShortName "CMT"

                     else
                        Lmt
                    )

            else if time.offsetMinutes == 104 || time.offsetMinutes == 105 then
                Ok (ShortName "BMT")

            else if time.offsetMinutes == 60 then
                Ok (ShortName "CET")

            else if time.offsetMinutes == 180 then
                Ok
                    (if time.posixSeconds >= 641944800 then
                        ShortName "EEST"

                     else if time.posixSeconds >= -800157600 then
                        ShortName "MSK"

                     else
                        ShortName "EEST"
                    )

            else if time.offsetMinutes == 120 then
                Ok
                    (if time.posixSeconds >= 654652800 then
                        ShortName "EET"

                     else if time.posixSeconds >= -898138800 then
                        ShortName "CEST"

                     else
                        ShortName "EET"
                    )

            else if time.offsetMinutes == 240 then
                Ok (ShortName "MSD")

            else
                Err time.offsetMinutes
        )


{-| `Europe/Dublin`
-}
for__europe__dublin : Zone -> Posix -> Result Int Abbreviation
for__europe__dublin =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -26 || time.offsetMinutes == -25 then
                Ok
                    (if time.posixSeconds >= -2821649679 then
                        ShortName "DMT"

                     else
                        Lmt
                    )

            else if time.offsetMinutes == 60 then
                Ok
                    (if time.posixSeconds >= -1507500000 then
                        ShortName "IST"

                     else
                        ShortName "BST"
                    )

            else if time.offsetMinutes == 0 then
                Ok (ShortName "GMT")

            else if time.offsetMinutes == 34 || time.offsetMinutes == 35 then
                Ok (ShortName "IST")

            else
                Err time.offsetMinutes
        )


{-| `Europe/Gibraltar`
-}
for__europe__gibraltar : Zone -> Posix -> Result Int Abbreviation
for__europe__gibraltar =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -22 || time.offsetMinutes == -21 then
                Ok Lmt

            else if time.offsetMinutes == 120 then
                Ok
                    (if time.posixSeconds >= 386125200 then
                        ShortName "CEST"

                     else
                        ShortName "BDST"
                    )

            else if time.offsetMinutes == 60 then
                Ok
                    (if time.posixSeconds >= -401320800 then
                        ShortName "CET"

                     else
                        ShortName "BST"
                    )

            else if time.offsetMinutes == 0 then
                Ok (ShortName "GMT")

            else
                Err time.offsetMinutes
        )


{-| `Europe/Helsinki`
-}
for__europe__helsinki : Zone -> Posix -> Result Int Abbreviation
for__europe__helsinki =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 99 || time.offsetMinutes == 100 then
                Ok
                    (if time.posixSeconds >= -2890258789 then
                        ShortName "HMT"

                     else
                        Lmt
                    )

            else if time.offsetMinutes == 180 then
                Ok (ShortName "EEST")

            else if time.offsetMinutes == 120 then
                Ok (ShortName "EET")

            else
                Err time.offsetMinutes
        )


{-| `Europe/Istanbul`
-}
for__europe__istanbul : Zone -> Posix -> Result Int Abbreviation
for__europe__istanbul =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 115 then
                Ok Lmt

            else if time.offsetMinutes == 116 then
                Ok
                    (if time.posixSeconds >= -2840147752 then
                        ShortName "IMT"

                     else
                        Lmt
                    )

            else if time.offsetMinutes == 240 then
                Ok (Offset time.offsetMinutes)

            else if time.offsetMinutes == 180 then
                Ok
                    (if time.posixSeconds >= 1473195600 then
                        Offset time.offsetMinutes

                     else if time.posixSeconds >= 482799600 then
                        ShortName "EEST"

                     else if time.posixSeconds >= 267915600 then
                        Offset time.offsetMinutes

                     else
                        ShortName "EEST"
                    )

            else if time.offsetMinutes == 120 then
                Ok (ShortName "EET")

            else if time.offsetMinutes == 117 then
                Ok (ShortName "IMT")

            else
                Err time.offsetMinutes
        )


{-| `Europe/Kaliningrad`
-}
for__europe__kaliningrad : Zone -> Posix -> Result Int Abbreviation
for__europe__kaliningrad =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 82 then
                Ok Lmt

            else if time.offsetMinutes == 120 then
                Ok
                    (if time.posixSeconds >= -780372000 then
                        ShortName "EET"

                     else
                        ShortName "CEST"
                    )

            else if time.offsetMinutes == 60 then
                Ok (ShortName "CET")

            else if time.offsetMinutes == 180 then
                Ok
                    (if time.posixSeconds >= 1301184000 then
                        Offset time.offsetMinutes

                     else if time.posixSeconds >= 606870000 then
                        ShortName "EEST"

                     else if time.posixSeconds >= -749095200 then
                        ShortName "MSK"

                     else
                        ShortName "EEST"
                    )

            else if time.offsetMinutes == 240 then
                Ok (ShortName "MSD")

            else
                Err time.offsetMinutes
        )


{-| `Europe/Kirov`
-}
for__europe__kirov : Zone -> Posix -> Result Int Abbreviation
for__europe__kirov =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 198 || time.offsetMinutes == 199 then
                Ok Lmt

            else if time.offsetMinutes == 300 then
                Ok (Offset time.offsetMinutes)

            else if time.offsetMinutes == 240 then
                Ok
                    (if time.posixSeconds >= 1301180400 then
                        ShortName "MSK"

                     else if time.posixSeconds >= 701820000 then
                        ShortName "MSD"

                     else if time.posixSeconds >= 670374000 then
                        Offset time.offsetMinutes

                     else if time.posixSeconds >= 606866400 then
                        ShortName "MSD"

                     else
                        Offset time.offsetMinutes
                    )

            else if time.offsetMinutes == 180 then
                Ok
                    (if time.posixSeconds >= 622594800 then
                        ShortName "MSK"

                     else
                        Offset time.offsetMinutes
                    )

            else
                Err time.offsetMinutes
        )


{-| `Europe/Kyiv`
-}
for__europe__kyiv : Zone -> Posix -> Result Int Abbreviation
for__europe__kyiv =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 122 || time.offsetMinutes == 123 then
                Ok
                    (if time.posixSeconds >= -2840148124 then
                        ShortName "KMT"

                     else
                        Lmt
                    )

            else if time.offsetMinutes == 60 then
                Ok (ShortName "CET")

            else if time.offsetMinutes == 120 then
                Ok
                    (if time.posixSeconds >= 686102400 then
                        ShortName "EET"

                     else if time.posixSeconds >= -892522800 then
                        ShortName "CEST"

                     else
                        ShortName "EET"
                    )

            else if time.offsetMinutes == 240 then
                Ok (ShortName "MSD")

            else if time.offsetMinutes == 180 then
                Ok
                    (if time.posixSeconds >= 646783200 then
                        ShortName "EEST"

                     else
                        ShortName "MSK"
                    )

            else
                Err time.offsetMinutes
        )


{-| `Europe/Lisbon`
-}
for__europe__lisbon : Zone -> Posix -> Result Int Abbreviation
for__europe__lisbon =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -37 || time.offsetMinutes == -36 then
                Ok Lmt

            else if time.offsetMinutes == 120 then
                Ok
                    (if time.posixSeconds >= 733280400 then
                        ShortName "CEST"

                     else
                        ShortName "WEMT"
                    )

            else if time.offsetMinutes == 60 then
                Ok
                    (if time.posixSeconds >= 828234000 then
                        ShortName "WEST"

                     else if time.posixSeconds >= 717555600 then
                        ShortName "CET"

                     else if time.posixSeconds >= 228268800 then
                        ShortName "WEST"

                     else if time.posixSeconds >= -102549600 then
                        ShortName "CET"

                     else
                        ShortName "WEST"
                    )

            else if time.offsetMinutes == 0 then
                Ok (ShortName "WET")

            else
                Err time.offsetMinutes
        )


{-| `Europe/London`
-}
for__europe__london : Zone -> Posix -> Result Int Abbreviation
for__europe__london =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -2 || time.offsetMinutes == -1 then
                Ok Lmt

            else if time.offsetMinutes == 120 then
                Ok (ShortName "BDST")

            else if time.offsetMinutes == 60 then
                Ok (ShortName "BST")

            else if time.offsetMinutes == 0 then
                Ok (ShortName "GMT")

            else
                Err time.offsetMinutes
        )


{-| `Europe/Madrid`
-}
for__europe__madrid : Zone -> Posix -> Result Int Abbreviation
for__europe__madrid =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -15 || time.offsetMinutes == -14 then
                Ok Lmt

            else if time.offsetMinutes == 120 then
                Ok
                    (if time.posixSeconds >= -873079200 then
                        ShortName "CEST"

                     else
                        ShortName "WEMT"
                    )

            else if time.offsetMinutes == 60 then
                Ok
                    (if time.posixSeconds >= -940208400 then
                        ShortName "CET"

                     else
                        ShortName "WEST"
                    )

            else if time.offsetMinutes == 0 then
                Ok (ShortName "WET")

            else
                Err time.offsetMinutes
        )


{-| `Europe/Malta`
-}
for__europe__malta : Zone -> Posix -> Result Int Abbreviation
for__europe__malta =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 58 || time.offsetMinutes == 59 then
                Ok Lmt

            else if time.offsetMinutes == 120 then
                Ok (ShortName "CEST")

            else if time.offsetMinutes == 60 then
                Ok (ShortName "CET")

            else
                Err time.offsetMinutes
        )


{-| `Europe/Minsk`
-}
for__europe__minsk : Zone -> Posix -> Result Int Abbreviation
for__europe__minsk =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 111 then
                Ok Lmt

            else if time.offsetMinutes == 110 then
                Ok
                    (if time.posixSeconds >= -2840147416 then
                        ShortName "MMT"

                     else
                        Lmt
                    )

            else if time.offsetMinutes == 60 then
                Ok (ShortName "CET")

            else if time.offsetMinutes == 120 then
                Ok
                    (if time.posixSeconds >= 686102400 then
                        ShortName "EET"

                     else if time.posixSeconds >= -899780400 then
                        ShortName "CEST"

                     else
                        ShortName "EET"
                    )

            else if time.offsetMinutes == 240 then
                Ok (ShortName "MSD")

            else if time.offsetMinutes == 180 then
                Ok
                    (if time.posixSeconds >= 1301184000 then
                        Offset time.offsetMinutes

                     else if time.posixSeconds >= 670374000 then
                        ShortName "EEST"

                     else
                        ShortName "MSK"
                    )

            else
                Err time.offsetMinutes
        )


{-| `Europe/Moscow`
-}
for__europe__moscow : Zone -> Posix -> Result Int Abbreviation
for__europe__moscow =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 150 || time.offsetMinutes == 151 then
                Ok
                    (if time.posixSeconds >= -2840149817 then
                        ShortName "MMT"

                     else
                        Lmt
                    )

            else if time.offsetMinutes == 300 then
                Ok (Offset time.offsetMinutes)

            else if time.offsetMinutes == 120 then
                Ok (ShortName "EET")

            else if time.offsetMinutes == 271 || time.offsetMinutes == 272 then
                Ok (ShortName "MDST")

            else if time.offsetMinutes == 152 then
                Ok (ShortName "MMT")

            else if time.offsetMinutes == 240 then
                Ok
                    (if time.posixSeconds >= 1301180400 then
                        ShortName "MSK"

                     else
                        ShortName "MSD"
                    )

            else if time.offsetMinutes == 180 then
                Ok
                    (if time.posixSeconds >= 695779200 then
                        ShortName "MSK"

                     else if time.posixSeconds >= 670374000 then
                        ShortName "EEST"

                     else
                        ShortName "MSK"
                    )

            else if time.offsetMinutes == 211 || time.offsetMinutes == 212 then
                Ok (ShortName "MST")

            else
                Err time.offsetMinutes
        )


{-| `Europe/Paris`
-}
for__europe__paris : Zone -> Posix -> Result Int Abbreviation
for__europe__paris =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 9 || time.offsetMinutes == 10 then
                Ok
                    (if time.posixSeconds >= -2486592561 then
                        ShortName "PMT"

                     else
                        Lmt
                    )

            else if time.offsetMinutes == 120 then
                Ok
                    (if time.posixSeconds >= 196819200 then
                        ShortName "CEST"

                     else if time.posixSeconds >= -800071200 then
                        ShortName "WEMT"

                     else
                        ShortName "CEST"
                    )

            else if time.offsetMinutes == 60 then
                Ok
                    (if time.posixSeconds >= -766623600 then
                        ShortName "CET"

                     else if time.posixSeconds >= -796266000 then
                        ShortName "WEST"

                     else if time.posixSeconds >= -857257200 then
                        ShortName "CET"

                     else
                        ShortName "WEST"
                    )

            else if time.offsetMinutes == 0 then
                Ok (ShortName "WET")

            else
                Err time.offsetMinutes
        )


{-| `Europe/Prague`
-}
for__europe__prague : Zone -> Posix -> Result Int Abbreviation
for__europe__prague =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 57 || time.offsetMinutes == 58 then
                Ok
                    (if time.posixSeconds >= -3786829064 then
                        ShortName "PMT"

                     else
                        Lmt
                    )

            else if time.offsetMinutes == 120 then
                Ok (ShortName "CEST")

            else if time.offsetMinutes == 60 then
                Ok (ShortName "CET")

            else if time.offsetMinutes == 0 then
                Ok (ShortName "GMT")

            else
                Err time.offsetMinutes
        )


{-| `Europe/Riga`
-}
for__europe__riga : Zone -> Posix -> Result Int Abbreviation
for__europe__riga =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 96 || time.offsetMinutes == 97 then
                Ok
                    (if time.posixSeconds >= -2840146594 then
                        ShortName "RMT"

                     else
                        Lmt
                    )

            else if time.offsetMinutes == 60 then
                Ok (ShortName "CET")

            else if time.offsetMinutes == 120 then
                Ok
                    (if time.posixSeconds >= 622598400 then
                        ShortName "EET"

                     else if time.posixSeconds >= -899521200 then
                        ShortName "CEST"

                     else
                        ShortName "EET"
                    )

            else if time.offsetMinutes == 156 || time.offsetMinutes == 157 then
                Ok (ShortName "LST")

            else if time.offsetMinutes == 240 then
                Ok (ShortName "MSD")

            else if time.offsetMinutes == 180 then
                Ok
                    (if time.posixSeconds >= 606870000 then
                        ShortName "EEST"

                     else
                        ShortName "MSK"
                    )

            else
                Err time.offsetMinutes
        )


{-| `Europe/Rome`
-}
for__europe__rome : Zone -> Posix -> Result Int Abbreviation
for__europe__rome =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 49 || time.offsetMinutes == 50 then
                Ok
                    (if time.posixSeconds >= -3252098996 then
                        ShortName "RMT"

                     else
                        Lmt
                    )

            else if time.offsetMinutes == 120 then
                Ok (ShortName "CEST")

            else if time.offsetMinutes == 60 then
                Ok (ShortName "CET")

            else
                Err time.offsetMinutes
        )


{-| `Europe/Samara`
-}
for__europe__samara : Zone -> Posix -> Result Int Abbreviation
for__europe__samara =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 200 || time.offsetMinutes == 201 then
                Ok Lmt

            else if time.offsetMinutes == 180 || time.offsetMinutes == 240 || time.offsetMinutes == 300 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `Europe/Saratov`
-}
for__europe__saratov : Zone -> Posix -> Result Int Abbreviation
for__europe__saratov =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 184 || time.offsetMinutes == 185 then
                Ok Lmt

            else if time.offsetMinutes == 180 || time.offsetMinutes == 240 || time.offsetMinutes == 300 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `Europe/Simferopol`
-}
for__europe__simferopol : Zone -> Posix -> Result Int Abbreviation
for__europe__simferopol =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 137 then
                Ok Lmt

            else if time.offsetMinutes == 136 then
                Ok
                    (if time.posixSeconds >= -2840148984 then
                        ShortName "SMT"

                     else
                        Lmt
                    )

            else if time.offsetMinutes == 60 then
                Ok (ShortName "CET")

            else if time.offsetMinutes == 120 then
                Ok
                    (if time.posixSeconds >= 646786800 then
                        ShortName "EET"

                     else if time.posixSeconds >= -888894000 then
                        ShortName "CEST"

                     else
                        ShortName "EET"
                    )

            else if time.offsetMinutes == 240 then
                Ok
                    (if time.posixSeconds >= 1396137600 then
                        ShortName "MSK"

                     else
                        ShortName "MSD"
                    )

            else if time.offsetMinutes == 180 then
                Ok
                    (if time.posixSeconds >= 1414274400 then
                        ShortName "MSK"

                     else if time.posixSeconds >= 859683600 then
                        ShortName "EEST"

                     else if time.posixSeconds >= 780447600 then
                        ShortName "MSK"

                     else if time.posixSeconds >= 701827200 then
                        ShortName "EEST"

                     else
                        ShortName "MSK"
                    )

            else
                Err time.offsetMinutes
        )


{-| `Europe/Sofia`
-}
for__europe__sofia : Zone -> Posix -> Result Int Abbreviation
for__europe__sofia =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 93 || time.offsetMinutes == 94 then
                Ok Lmt

            else if time.offsetMinutes == 60 then
                Ok (ShortName "CET")

            else if time.offsetMinutes == 180 then
                Ok (ShortName "EEST")

            else if time.offsetMinutes == 120 then
                Ok
                    (if time.posixSeconds >= -781048800 then
                        ShortName "EET"

                     else if time.posixSeconds >= -844556400 then
                        ShortName "CEST"

                     else
                        ShortName "EET"
                    )

            else if time.offsetMinutes == 116 || time.offsetMinutes == 117 then
                Ok (ShortName "IMT")

            else
                Err time.offsetMinutes
        )


{-| `Europe/Tallinn`
-}
for__europe__tallinn : Zone -> Posix -> Result Int Abbreviation
for__europe__tallinn =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 99 then
                Ok
                    (if time.posixSeconds >= -2840146740 then
                        ShortName "TMT"

                     else
                        Lmt
                    )

            else if time.offsetMinutes == 120 then
                Ok
                    (if time.posixSeconds >= 622598400 then
                        ShortName "EET"

                     else if time.posixSeconds >= -892954800 then
                        ShortName "CEST"

                     else if time.posixSeconds >= -1535938740 then
                        ShortName "EET"

                     else
                        ShortName "CEST"
                    )

            else if time.offsetMinutes == 60 then
                Ok (ShortName "CET")

            else if time.offsetMinutes == 240 then
                Ok (ShortName "MSD")

            else if time.offsetMinutes == 180 then
                Ok
                    (if time.posixSeconds >= 606870000 then
                        ShortName "EEST"

                     else
                        ShortName "MSK"
                    )

            else
                Err time.offsetMinutes
        )


{-| `Europe/Tirane`
-}
for__europe__tirane : Zone -> Posix -> Result Int Abbreviation
for__europe__tirane =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 79 || time.offsetMinutes == 80 then
                Ok Lmt

            else if time.offsetMinutes == 120 then
                Ok (ShortName "CEST")

            else if time.offsetMinutes == 60 then
                Ok (ShortName "CET")

            else
                Err time.offsetMinutes
        )


{-| `Europe/Ulyanovsk`
-}
for__europe__ulyanovsk : Zone -> Posix -> Result Int Abbreviation
for__europe__ulyanovsk =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 193 || time.offsetMinutes == 194 then
                Ok Lmt

            else if time.offsetMinutes == 120 || time.offsetMinutes == 180 || time.offsetMinutes == 240 || time.offsetMinutes == 300 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `Europe/Vienna`
-}
for__europe__vienna : Zone -> Posix -> Result Int Abbreviation
for__europe__vienna =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 65 || time.offsetMinutes == 66 then
                Ok Lmt

            else if time.offsetMinutes == 120 then
                Ok (ShortName "CEST")

            else if time.offsetMinutes == 60 then
                Ok (ShortName "CET")

            else
                Err time.offsetMinutes
        )


{-| `Europe/Vilnius`
-}
for__europe__vilnius : Zone -> Posix -> Result Int Abbreviation
for__europe__vilnius =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 101 || time.offsetMinutes == 102 then
                Ok Lmt

            else if time.offsetMinutes == 60 then
                Ok (ShortName "CET")

            else if time.offsetMinutes == 120 then
                Ok
                    (if time.posixSeconds >= 941331600 then
                        ShortName "EET"

                     else if time.posixSeconds >= 891133200 then
                        ShortName "CEST"

                     else if time.posixSeconds >= 622598400 then
                        ShortName "EET"

                     else if time.posixSeconds >= -900126000 then
                        ShortName "CEST"

                     else
                        ShortName "EET"
                    )

            else if time.offsetMinutes == 95 || time.offsetMinutes == 96 then
                Ok (ShortName "KMT")

            else if time.offsetMinutes == 240 then
                Ok (ShortName "MSD")

            else if time.offsetMinutes == 180 then
                Ok
                    (if time.posixSeconds >= 606870000 then
                        ShortName "EEST"

                     else
                        ShortName "MSK"
                    )

            else if time.offsetMinutes == 84 then
                Ok (ShortName "WMT")

            else
                Err time.offsetMinutes
        )


{-| `Europe/Volgograd`
-}
for__europe__volgograd : Zone -> Posix -> Result Int Abbreviation
for__europe__volgograd =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 177 || time.offsetMinutes == 178 then
                Ok Lmt

            else if time.offsetMinutes == 300 then
                Ok (Offset time.offsetMinutes)

            else if time.offsetMinutes == 240 then
                Ok
                    (if time.posixSeconds >= 1540681200 then
                        Offset time.offsetMinutes

                     else if time.posixSeconds >= 1301180400 then
                        ShortName "MSK"

                     else if time.posixSeconds >= 701820000 then
                        ShortName "MSD"

                     else if time.posixSeconds >= 670374000 then
                        Offset time.offsetMinutes

                     else if time.posixSeconds >= 575416800 then
                        ShortName "MSD"

                     else
                        Offset time.offsetMinutes
                    )

            else if time.offsetMinutes == 180 then
                Ok
                    (if time.posixSeconds >= 591145200 then
                        ShortName "MSK"

                     else
                        Offset time.offsetMinutes
                    )

            else
                Err time.offsetMinutes
        )


{-| `Europe/Warsaw`
-}
for__europe__warsaw : Zone -> Posix -> Result Int Abbreviation
for__europe__warsaw =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 84 then
                Ok
                    (if time.posixSeconds >= -2840145840 then
                        ShortName "WMT"

                     else
                        Lmt
                    )

            else if time.offsetMinutes == 120 then
                Ok
                    (if time.posixSeconds >= -931734000 then
                        ShortName "CEST"

                     else if time.posixSeconds >= -1618700400 then
                        ShortName "EET"

                     else
                        ShortName "CEST"
                    )

            else if time.offsetMinutes == 60 then
                Ok (ShortName "CET")

            else if time.offsetMinutes == 180 then
                Ok (ShortName "EEST")

            else
                Err time.offsetMinutes
        )


{-| `Europe/Zurich`
-}
for__europe__zurich : Zone -> Posix -> Result Int Abbreviation
for__europe__zurich =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 34 || time.offsetMinutes == 35 then
                Ok Lmt

            else if time.offsetMinutes == 29 || time.offsetMinutes == 30 then
                Ok (ShortName "BMT")

            else if time.offsetMinutes == 120 then
                Ok (ShortName "CEST")

            else if time.offsetMinutes == 60 then
                Ok (ShortName "CET")

            else
                Err time.offsetMinutes
        )


{-| `Indian/Chagos`
-}
for__indian__chagos : Zone -> Posix -> Result Int Abbreviation
for__indian__chagos =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 289 || time.offsetMinutes == 290 then
                Ok Lmt

            else if time.offsetMinutes == 300 || time.offsetMinutes == 360 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `Indian/Maldives`
-}
for__indian__maldives : Zone -> Posix -> Result Int Abbreviation
for__indian__maldives =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 294 then
                Ok
                    (if time.posixSeconds >= -2840158440 then
                        ShortName "MMT"

                     else
                        Lmt
                    )

            else if time.offsetMinutes == 300 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `Indian/Mauritius`
-}
for__indian__mauritius : Zone -> Posix -> Result Int Abbreviation
for__indian__mauritius =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 230 then
                Ok Lmt

            else if time.offsetMinutes == 240 || time.offsetMinutes == 300 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `Pacific/Apia`
-}
for__pacific__apia : Zone -> Posix -> Result Int Abbreviation
for__pacific__apia =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -687 || time.offsetMinutes == -686 || time.offsetMinutes == 753 || time.offsetMinutes == 754 then
                Ok Lmt

            else if time.offsetMinutes == -690 || time.offsetMinutes == -660 || time.offsetMinutes == -600 || time.offsetMinutes == 780 || time.offsetMinutes == 840 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `Pacific/Auckland`
-}
for__pacific__auckland : Zone -> Posix -> Result Int Abbreviation
for__pacific__auckland =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 699 || time.offsetMinutes == 700 then
                Ok Lmt

            else if time.offsetMinutes == 780 then
                Ok (ShortName "NZDT")

            else if time.offsetMinutes == 690 then
                Ok (ShortName "NZMT")

            else if time.offsetMinutes == 720 || time.offsetMinutes == 750 then
                Ok (ShortName "NZST")

            else
                Err time.offsetMinutes
        )


{-| `Pacific/Bougainville`
-}
for__pacific__bougainville : Zone -> Posix -> Result Int Abbreviation
for__pacific__bougainville =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 622 || time.offsetMinutes == 623 then
                Ok Lmt

            else if time.offsetMinutes == 540 || time.offsetMinutes == 600 || time.offsetMinutes == 660 then
                Ok (Offset time.offsetMinutes)

            else if time.offsetMinutes == 588 || time.offsetMinutes == 589 then
                Ok (ShortName "PMMT")

            else
                Err time.offsetMinutes
        )


{-| `Pacific/Chatham`
-}
for__pacific__chatham : Zone -> Posix -> Result Int Abbreviation
for__pacific__chatham =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 733 || time.offsetMinutes == 734 then
                Ok Lmt

            else if time.offsetMinutes == 735 || time.offsetMinutes == 765 || time.offsetMinutes == 825 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `Pacific/Easter`
-}
for__pacific__easter : Zone -> Posix -> Result Int Abbreviation
for__pacific__easter =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -438 || time.offsetMinutes == -437 then
                Ok
                    (if time.posixSeconds >= -2524495352 then
                        ShortName "EMT"

                     else
                        Lmt
                    )

            else if time.offsetMinutes == -420 || time.offsetMinutes == -360 || time.offsetMinutes == -300 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `Pacific/Efate`
-}
for__pacific__efate : Zone -> Posix -> Result Int Abbreviation
for__pacific__efate =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 673 || time.offsetMinutes == 674 then
                Ok Lmt

            else if time.offsetMinutes == 660 || time.offsetMinutes == 720 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `Pacific/Fakaofo`
-}
for__pacific__fakaofo : Zone -> Posix -> Result Int Abbreviation
for__pacific__fakaofo =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -685 || time.offsetMinutes == -684 then
                Ok Lmt

            else if time.offsetMinutes == -660 || time.offsetMinutes == 780 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `Pacific/Fiji`
-}
for__pacific__fiji : Zone -> Posix -> Result Int Abbreviation
for__pacific__fiji =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 715 || time.offsetMinutes == 716 then
                Ok Lmt

            else if time.offsetMinutes == 720 || time.offsetMinutes == 780 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `Pacific/Galapagos`
-}
for__pacific__galapagos : Zone -> Posix -> Result Int Abbreviation
for__pacific__galapagos =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -359 || time.offsetMinutes == -358 then
                Ok Lmt

            else if time.offsetMinutes == -360 || time.offsetMinutes == -300 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `Pacific/Gambier`
-}
for__pacific__gambier : Zone -> Posix -> Result Int Abbreviation
for__pacific__gambier =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -539 then
                Ok Lmt

            else if time.offsetMinutes == -540 then
                Ok
                    (if time.posixSeconds >= -1806678012 then
                        Offset time.offsetMinutes

                     else
                        Lmt
                    )

            else
                Err time.offsetMinutes
        )


{-| `Pacific/Guadalcanal`
-}
for__pacific__guadalcanal : Zone -> Posix -> Result Int Abbreviation
for__pacific__guadalcanal =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 639 || time.offsetMinutes == 640 then
                Ok Lmt

            else if time.offsetMinutes == 660 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `Pacific/Guam`
-}
for__pacific__guam : Zone -> Posix -> Result Int Abbreviation
for__pacific__guam =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -861 || time.offsetMinutes == 579 then
                Ok Lmt

            else if time.offsetMinutes == 540 then
                Ok (Offset time.offsetMinutes)

            else if time.offsetMinutes == 660 then
                Ok (ShortName "GDT")

            else if time.offsetMinutes == 600 then
                Ok
                    (if time.posixSeconds >= 977493600 then
                        ShortName "ChST"

                     else
                        ShortName "GST"
                    )

            else
                Err time.offsetMinutes
        )


{-| `Pacific/Honolulu`
-}
for__pacific__honolulu : Zone -> Posix -> Result Int Abbreviation
for__pacific__honolulu =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -632 || time.offsetMinutes == -631 then
                Ok Lmt

            else if time.offsetMinutes == -570 then
                Ok
                    (if time.posixSeconds >= -769395600 then
                        ShortName "HPT"

                     else if time.posixSeconds >= -880198200 then
                        ShortName "HWT"

                     else
                        ShortName "HDT"
                    )

            else if time.offsetMinutes == -630 || time.offsetMinutes == -600 then
                Ok (ShortName "HST")

            else
                Err time.offsetMinutes
        )


{-| `Pacific/Kanton`
-}
for__pacific__kanton : Zone -> Posix -> Result Int Abbreviation
for__pacific__kanton =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -720 || time.offsetMinutes == -660 || time.offsetMinutes == 780 then
                Ok (Offset time.offsetMinutes)

            else if time.offsetMinutes == 0 then
                Ok Uninhabited

            else
                Err time.offsetMinutes
        )


{-| `Pacific/Kiritimati`
-}
for__pacific__kiritimati : Zone -> Posix -> Result Int Abbreviation
for__pacific__kiritimati =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -630 || time.offsetMinutes == -629 then
                Ok Lmt

            else if time.offsetMinutes == -640 || time.offsetMinutes == -600 || time.offsetMinutes == 840 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `Pacific/Kosrae`
-}
for__pacific__kosrae : Zone -> Posix -> Result Int Abbreviation
for__pacific__kosrae =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -789 || time.offsetMinutes == -788 || time.offsetMinutes == 651 || time.offsetMinutes == 652 then
                Ok Lmt

            else if time.offsetMinutes == 540 || time.offsetMinutes == 600 || time.offsetMinutes == 660 || time.offsetMinutes == 720 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `Pacific/Kwajalein`
-}
for__pacific__kwajalein : Zone -> Posix -> Result Int Abbreviation
for__pacific__kwajalein =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 669 || time.offsetMinutes == 670 then
                Ok Lmt

            else if time.offsetMinutes == -720 || time.offsetMinutes == 540 || time.offsetMinutes == 600 || time.offsetMinutes == 660 || time.offsetMinutes == 720 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `Pacific/Marquesas`
-}
for__pacific__marquesas : Zone -> Posix -> Result Int Abbreviation
for__pacific__marquesas =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -558 then
                Ok Lmt

            else if time.offsetMinutes == -570 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `Pacific/Nauru`
-}
for__pacific__nauru : Zone -> Posix -> Result Int Abbreviation
for__pacific__nauru =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 667 || time.offsetMinutes == 668 then
                Ok Lmt

            else if time.offsetMinutes == 540 || time.offsetMinutes == 690 || time.offsetMinutes == 720 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `Pacific/Niue`
-}
for__pacific__niue : Zone -> Posix -> Result Int Abbreviation
for__pacific__niue =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -679 then
                Ok Lmt

            else if time.offsetMinutes == -680 then
                Ok
                    (if time.posixSeconds >= -543069620 then
                        Offset time.offsetMinutes

                     else
                        Lmt
                    )

            else if time.offsetMinutes == -660 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `Pacific/Norfolk`
-}
for__pacific__norfolk : Zone -> Posix -> Result Int Abbreviation
for__pacific__norfolk =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 671 then
                Ok Lmt

            else if time.offsetMinutes == 672 then
                Ok
                    (if time.posixSeconds >= -2177493112 then
                        Offset time.offsetMinutes

                     else
                        Lmt
                    )

            else if time.offsetMinutes == 660 || time.offsetMinutes == 690 || time.offsetMinutes == 750 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `Pacific/Noumea`
-}
for__pacific__noumea : Zone -> Posix -> Result Int Abbreviation
for__pacific__noumea =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 665 || time.offsetMinutes == 666 then
                Ok Lmt

            else if time.offsetMinutes == 660 || time.offsetMinutes == 720 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `Pacific/Pago_Pago`
-}
for__pacific__pago_pago : Zone -> Posix -> Result Int Abbreviation
for__pacific__pago_pago =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -683 || time.offsetMinutes == -682 || time.offsetMinutes == 757 || time.offsetMinutes == 758 then
                Ok Lmt

            else if time.offsetMinutes == -660 then
                Ok (ShortName "SST")

            else
                Err time.offsetMinutes
        )


{-| `Pacific/Palau`
-}
for__pacific__palau : Zone -> Posix -> Result Int Abbreviation
for__pacific__palau =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -903 || time.offsetMinutes == -902 || time.offsetMinutes == 537 || time.offsetMinutes == 538 then
                Ok Lmt

            else if time.offsetMinutes == 540 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `Pacific/Pitcairn`
-}
for__pacific__pitcairn : Zone -> Posix -> Result Int Abbreviation
for__pacific__pitcairn =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -521 || time.offsetMinutes == -520 then
                Ok Lmt

            else if time.offsetMinutes == -510 || time.offsetMinutes == -480 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `Pacific/Port_Moresby`
-}
for__pacific__port_moresby : Zone -> Posix -> Result Int Abbreviation
for__pacific__port_moresby =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 588 || time.offsetMinutes == 589 then
                Ok
                    (if time.posixSeconds >= -2840176120 then
                        ShortName "PMMT"

                     else
                        Lmt
                    )

            else if time.offsetMinutes == 600 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `Pacific/Rarotonga`
-}
for__pacific__rarotonga : Zone -> Posix -> Result Int Abbreviation
for__pacific__rarotonga =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -640 || time.offsetMinutes == -639 || time.offsetMinutes == 800 || time.offsetMinutes == 801 then
                Ok Lmt

            else if time.offsetMinutes == -630 || time.offsetMinutes == -600 || time.offsetMinutes == -570 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `Pacific/Tahiti`
-}
for__pacific__tahiti : Zone -> Posix -> Result Int Abbreviation
for__pacific__tahiti =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == -599 || time.offsetMinutes == -598 then
                Ok Lmt

            else if time.offsetMinutes == -600 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `Pacific/Tarawa`
-}
for__pacific__tarawa : Zone -> Posix -> Result Int Abbreviation
for__pacific__tarawa =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 692 || time.offsetMinutes == 693 then
                Ok Lmt

            else if time.offsetMinutes == 720 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )


{-| `Pacific/Tongatapu`
-}
for__pacific__tongatapu : Zone -> Posix -> Result Int Abbreviation
for__pacific__tongatapu =
    Internal.toTime
        (\time ->
            if time.offsetMinutes == 739 then
                Ok Lmt

            else if time.offsetMinutes == 740 then
                Ok
                    (if time.posixSeconds >= -767189952 then
                        Offset time.offsetMinutes

                     else
                        Lmt
                    )

            else if time.offsetMinutes == 780 || time.offsetMinutes == 840 then
                Ok (Offset time.offsetMinutes)

            else
                Err time.offsetMinutes
        )
