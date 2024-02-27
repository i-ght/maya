namespace Maya

type HaabNumber = int

[<Struct>]
type HaabMonth =
    | Pop
    | Wo
    | Sip
    | Sotz
    | Sek
    | Xul
    | Yaxkin
    | Mol
    | Chen
    | Yax
    | Sak
    | Keh
    | Mak
    | Kankin
    | Muwan
    | Pax
    | Kayab
    | Kumku

module Haab =

    let number month = month |>  function
        | Pop -> 1
        | Wo -> 2
        | Sip -> 3
        | Sotz -> 4
        | Sek -> 5
        | Xul -> 6
        | Yaxkin -> 7
        | Mol -> 8
        | Chen -> 9
        | Yax -> 10
        | Sak -> 11
        | Keh -> 12
        | Mak -> 13
        | Kankin -> 14
        | Muwan -> 15
        | Pax -> 16
        | Kayab -> 17
        | Kumku -> 18

    let meaning month = month |> function
        | Pop -> "mat"
        | Wo -> "black conjunction"
        | Sip -> "red conjunction"
        | Sotz -> "bat"
        | Sek -> "death"
        | Xul -> "dog"
        | Yaxkin -> "new sun"
        | Mol -> "water"
        | Chen -> "black storm"
        | Yax -> "green storm"
        | Sak -> "white storm"
        | Keh -> "red storm"
        | Mak -> "enclosed"
        | Kankin -> "yellow sun"
        | Muwan -> "owl"
        | Pax -> "planting time"
        | Kayab -> "turtle"
        | Kumku -> "granary"


type HaabMonth with
        member month.Meaning = Haab.meaning month
        member month.Number = Haab.number month