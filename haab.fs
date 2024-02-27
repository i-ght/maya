namespace Maya

(*0-19*)
type HaabDay = int

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

    let ofNumber n = n |> function
        | 1 -> Pop
        | 2 -> Wo
        | 3 -> Sip
        | 4 -> Sotz
        | 5 -> Sek
        | 6 -> Xul
        | 7 -> Yaxkin
        | 8 -> Mol
        | 9 -> Chen
        | 10 -> Yax
        | 11 -> Sak
        | 12 -> Keh
        | 13 -> Mak
        | 14 -> Kankin
        | 15 -> Muwan
        | 16 -> Pax
        | 17 -> Kayab
        | 18 -> Kumku
        | _ -> invalidArg "n" "n must be between 1-19"
        
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

    let compute y m d =
        let days =
            Maya.computeDays y m d 
            |> float

        let dayOfHaab = (days - 17.0) % 365.0
        let day = dayOfHaab % 20.0 |> round
        let month = round (floor (dayOfHaab/20.0))
        struct (int day, ofNumber <| int (month + 1.0))


type HaabMonth with
        member month.Meaning = Haab.meaning month
        member month.Number = Haab.number month