module Main exposing (main)

import Browser
import Html as H exposing (Html)
import Html.Attributes as A
import Task
import Time exposing (Posix, Zone, ZoneName)
import TimeZone.Abbreviation exposing (Abbreviation(..))


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { zone : Maybe ( ZoneName, Zone )
    , now : Maybe Posix
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( { zone = Nothing, now = Nothing }
    , Task.map2 Tuple.pair Time.getZoneName Time.here
        |> Task.perform GotZone
    )



-- UPDATE


type Msg
    = GotCurrentPosix Posix
    | GotZone ( ZoneName, Zone )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotCurrentPosix now ->
            ( { model | now = Just now }, Cmd.none )

        GotZone zone ->
            ( { model | zone = Just zone }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 1000 GotCurrentPosix



-- VIEW


view : Model -> Html Msg
view model =
    H.div [ A.style "padding" "0.5rem" ]
        [ H.p []
            [ H.text <|
                Maybe.withDefault "Loading..." <|
                    Maybe.map2 formatTime model.zone model.now
            ]
        ]


formatTime : ( ZoneName, Zone ) -> Posix -> String
formatTime ( zoneName, zone ) time =
    String.concat
        [ String.padLeft 2 '0' <| String.fromInt <| Time.toHour zone time
        , ":"
        , String.padLeft 2 '0' <| String.fromInt <| Time.toMinute zone time
        , ":"
        , String.padLeft 2 '0' <| String.fromInt <| Time.toSecond zone time
        , " "
        , TimeZone.Abbreviation.toString <|
            TimeZone.Abbreviation.forZoneUnsafe ( zoneName, zone ) time
        ]
