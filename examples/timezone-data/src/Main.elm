module Main exposing (main)

import Browser
import Dict
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Time exposing (Posix, Zone, ZoneName)
import TimeZone
import TimeZone.Abbreviation


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
    , Cmd.none
    )



-- UPDATE


type Msg
    = GotCurrentPosix Posix
    | NameSelected String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotCurrentPosix now ->
            ( { model | now = Just now }
            , Cmd.none
            )

        NameSelected name ->
            case Dict.get name TimeZone.zones of
                Nothing ->
                    ( { model | zone = Nothing }
                    , Cmd.none
                    )

                Just lazyZone ->
                    ( { model | zone = Just ( Time.Name name, lazyZone () ) }
                    , Cmd.none
                    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 1000 GotCurrentPosix



-- VIEW


view : Model -> Html Msg
view model =
    H.div [ A.style "padding" "0.5rem" ]
        [ H.select [ E.onInput NameSelected ] <|
            (::)
                (H.option
                    [ A.selected (model.zone == Nothing) ]
                    [ H.text "Please select a time zone" ]
                )
            <|
                List.map
                    (\name ->
                        H.option
                            [ A.value name
                            , A.selected (Maybe.map Tuple.first model.zone == Just (Time.Name name))
                            ]
                            [ H.text name ]
                    )
                    (Dict.keys TimeZone.zones)
        , H.p []
            [ H.text <|
                Maybe.withDefault "" <|
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
