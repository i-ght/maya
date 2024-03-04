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
    | Wayeb

type HaabDate = (struct (HaabDay * HaabMonth))

module Haab =

    let ofNumber n = n |> function
        | 01 -> Pop
        | 02 -> Wo
        | 03 -> Sip
        | 04 -> Sotz
        | 05 -> Sek
        | 06 -> Xul
        | 07 -> Yaxkin
        | 08 -> Mol
        | 09 -> Chen
        | 10 -> Yax
        | 11 -> Sak
        | 12 -> Keh
        | 13 -> Mak
        | 14 -> Kankin
        | 15 -> Muwan
        | 16 -> Pax
        | 17 -> Kayab
        | 18 -> Kumku
        | 19 -> Wayeb
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
        | Wayeb -> 19

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
        | Wayeb -> ""

    let compute y m d : HaabDate =
        let days =
            LongCount.days y m d
            |> float

        let daysUntilNewYear =
            match LongCount.chosenEpoch with
            | BC3114 ->
                if days < 17 then
                    days - 348.0
                else
                    17.0
            | CE2012 ->
                if days < 102 then
                    days - 263.0
                else
                    102.0

        let dayOfHaab = (days - daysUntilNewYear) % 365.0
    
        let day = dayOfHaab % 20.0 |> round
        let month = 
            round (floor (dayOfHaab/20.0)) + 1.0
        let m = ofNumber <| int month
        struct (int day, m)

    let ofDate (date: System.DateOnly): HaabDate =
        let (y, m, d) = date.Deconstruct()
        compute y m d


type HaabMonth with
        member month.Meaning = Haab.meaning month
        member month.Number = Haab.number month