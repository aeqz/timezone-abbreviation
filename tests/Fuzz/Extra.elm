module Fuzz.Extra exposing (fixedOffsetZone, posix)

import Fuzz exposing (Fuzzer)
import Time exposing (Posix, Zone)


posix : Fuzzer Posix
posix =
    Fuzz.uniformInt 2524608000000
        |> Fuzz.map Time.millisToPosix


fixedOffsetZone : Int -> Fuzzer { offset : Int, zone : Zone }
fixedOffsetZone maxOffset =
    Fuzz.intRange -maxOffset maxOffset
        |> Fuzz.map
            (\offset ->
                { offset = offset
                , zone = Time.customZone offset []
                }
            )
