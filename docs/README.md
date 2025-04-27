# timezone-abbreviation

This Elm package provides time zone name abbreviations from the [IANA Time Zone Database][tzdb] for using with [`elm/time`][elmtime]:

```elm
import Time
import TimeZone
import TimeZone.Abbreviation exposing (Abbreviation(..))

Time.millisToPosix 1743465600000
    |> TimeZone.Abbreviation.forZone
        ( Time.Name "Europe/London"
          -- Using zone offset data from `timezone-data`
        , TimeZone.europe__london ()
        )
--> Ok (ShortName "BST")
```

See some end-to-end example code in the [`examples/`][examples] folder.

## Installation

This package is not published yet. I'm sharing it for review purposes before an initial release.

Import all the files from the [`docs/`][docs] folder into the [`Elm Documentation Previewer`][docpreview] if you want to preview this package documentation and provide feedback!

## Design Goals

The aim of providing time zone abbreviation data only may seem niche, but it's motivated by the fact that the `Time.Zone` type from `elm/time` doesn't have room for it. This package is expected to be used together with any other `Time.Zone` data source like [`justinmimbs/timezone-data`][timezonedata] and complete it with missing abbreviation data.

One difference to note with other alternatives like JavaScript's [Intl][intldatetime] API is that `Intl.DateTimeFormat` provides abbreviations that are locale-dependent, whereas this package provides abbreviations as they are in the IANA Time Zone Database, also including those that have not been used since 1970, uninhabited indicators and [local mean times][lmt]. Next releases effort may be put into trying to improve the internal representation to reduce the data size, apart from providing updates with more recent versions of the database.

As a summary, you may want to use this package if you want time zone name abbreviations:

- In pure Elm, by using the `elm/time` types.
- That are non-locale-dependent and come strictly from the IANA Time Zone Database.

## Alternatives

These are some alternatives that I'm aware of by looking at other existing Elm packages:

- If you just need displaying the time zone offset, [`time-extra`][timeextra] has [a function to obtain the offset in minutes][timeextraoffset], which then you can format as needed (e.g. `GMT+1` or `UTC+01:00`).
- [`deprecated-time`][deprecated] includes a [`Time.TimeZone.abbreviation`][deprecatedabbreviation] function. However, I don't know details about the abbreviations that it provides, doesn't use the `elm/time` types, and was intended to be used while upgrading a codebase to Elm `0.19`.

And these are some packages that could likely provide abbreviation data, but don't do at the moment:

- [`elm-strftime`][strftime] doesn't implement the `%Z` (i.e. time zone name or abbreviation) format specifier.
- [`cldr`][cldr] has a [`ShortName` and `LongName`][cldroptions] time zone formatting options, but they seem to provide a GMT offset (e.g. `GMT+1` for the short version and `GMT+1:00` for the long one) instead of localized abbreviations.

You can also handle human time formatting in the JavaScript side, where you can get localized abbreviations by using `Intl.DateTimeFormat`, for instance via [custom elements][customelements].

## Learning resources

I found [this page][readiana] to be a useful reading when learning how abbreviations are represented in the IANA Time Zone Database source files.

[tzdb]: https://www.iana.org/time-zones
[examples]: https://github.com/aeqz/timezone-abbreviation/tree/main/examples
[docs]: https://github.com/aeqz/timezone-abbreviation/tree/main/docs
[docpreview]: https://elm-doc-preview.netlify.app/
[elmtime]: https://package.elm-lang.org/packages/elm/time/latest/
[timezonedata]: https://package.elm-lang.org/packages/justinmimbs/timezone-data/latest/
[intldatetime]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl/DateTimeFormat
[lmt]: https://en.wikipedia.org/wiki/Local_mean_time
[timeextra]: https://package.elm-lang.org/packages/justinmimbs/time-extra/latest/Time-Extra
[timeextraoffset]: https://package.elm-lang.org/packages/justinmimbs/time-extra/latest/Time-Extra#toOffset
[deprecated]: https://package.elm-lang.org/packages/isaacseymour/deprecated-time/latest
[deprecatedabbreviation]: https://package.elm-lang.org/packages/isaacseymour/deprecated-time/latest/Time-TimeZone#abbreviation
[strftime]: https://package.elm-lang.org/packages/thaterikperson/elm-strftime/latest/Strftime
[cldr]: https://package.elm-lang.org/packages/enkidatron/elm-cldr/latest
[cldroptions]: https://package.elm-lang.org/packages/enkidatron/elm-cldr/latest/Cldr-Format-Options#NameOption
[customelements]: https://guide.elm-lang.org/interop/custom_elements.html
[readiana]: https://ftp.iana.org/tz/tzdb-2020e/tz-how-to.html
