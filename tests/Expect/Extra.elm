module Expect.Extra exposing
    ( errWith
    , member
    , offsetAbbreviation
    , okWith
    , shortNameAbbreviation
    )

import Expect exposing (Expectation)
import TimeZone.Abbreviation exposing (Abbreviation(..), Error(..))


shortNameAbbreviation : (String -> Expectation) -> Abbreviation -> Expectation
shortNameAbbreviation expectation abbreviation =
    case abbreviation of
        ShortName n ->
            expectation n

        Offset o ->
            Expect.fail ("Got zone offset " ++ String.fromInt o)

        Uninhabited ->
            Expect.fail "Got -00"

        Lmt ->
            Expect.fail "Got LMT"


offsetAbbreviation : (Int -> Expectation) -> Abbreviation -> Expectation
offsetAbbreviation expectation abbreviation =
    case abbreviation of
        ShortName n ->
            Expect.fail ("Got short name \"" ++ n ++ "\"")

        Offset o ->
            expectation o

        Uninhabited ->
            Expect.fail "Got -00"

        Lmt ->
            Expect.fail "Got LMT"


okWith : (a -> Expectation) -> Result e a -> Expectation
okWith expectation result =
    case result of
        Ok a ->
            expectation a

        Err e ->
            Expect.fail ("Got error " ++ Debug.toString e)


errWith : (e -> Expectation) -> Result e a -> Expectation
errWith expectation result =
    case result of
        Ok a ->
            Expect.fail ("Got ok " ++ Debug.toString a)

        Err e ->
            expectation e


member : List a -> a -> Expectation
member list item =
    if List.member item list then
        Expect.pass

    else
        Expect.fail (Debug.toString list ++ " does not contain " ++ Debug.toString item)
