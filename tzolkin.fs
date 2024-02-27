namespace Maya

type TzolkinNumber = int

[<Struct>]
type TzolkinName =
    | Imix
    | Ik
    | Akbal
    | Kan
    | Chikchan
    | Kimi
    | Manik
    | Lamat
    | Muluk
    | Ok
    | Chuwen
    | Eb
    | Ben
    | Ix
    | Men
    | Kib
    | Kaban
    | Etznab
    | Kawak
    | Ajaw
with
    member day.Number =
        match day with
        | Imix -> 1
        | Ik -> 2
        | Akbal -> 3
        | Kan -> 4
        | Chikchan -> 5
        | Kimi -> 6
        | Manik -> 7
        | Lamat -> 8
        | Muluk -> 9
        | Ok -> 10
        | Chuwen -> 11
        | Eb -> 12
        | Ben -> 13
        | Ix -> 14
        | Men -> 15
        | Kib -> 16
        | Kaban -> 17
        | Etznab -> 18
        | Kawak -> 19
        | Ajaw -> 20

    member day.Description =
        match day with
        | Imix -> "waterlily, crocodile"
        | Ik -> "wind, breath, life force"
        | Akbal -> "darkness, night, early dawn"
        | Kan -> "Net, sacrifice"
        | Chikchan -> "cosmological snake"
        | Kimi -> "death"
        | Manik -> "deer"
        | Lamat -> "Venus, star, ripe(ness), maize seeds"
        | Muluk -> "jade, water, offering"
        | Ok -> "dog"
        | Chuwen -> "howler monkey"
        | Eb -> "rain"
        | Ben -> "green/young maize, seed"
        | Ix -> "jaguar"
        | Men -> "eagle"
        | Kib -> "wax"
        | Kaban -> "earth"
        | Etznab -> "flint"
        | Kawak -> "rain storm"
        | Ajaw -> "lord, ruler, sun"

module Tzolkin =

    let dayOfNumber n =
        match n with
        | 01 -> Imix
        | 02 -> Ik
        | 03 -> Akbal
        | 04 -> Kan
        | 05 -> Chikchan
        | 06 -> Kimi
        | 07 -> Manik
        | 08 -> Lamat
        | 09 -> Muluk
        | 10 -> Ok
        | 11 -> Chuwen
        | 12 -> Eb
        | 13 -> Ben
        | 14 -> Ix
        | 15 -> Men
        | 16 -> Kib
        | 17 -> Kaban
        | 18 -> Etznab
        | 19 -> Kawak
        | 20 -> Ajaw
        |_ -> invalidArg "n" "n must be from 1-20"

