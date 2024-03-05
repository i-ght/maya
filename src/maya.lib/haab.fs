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

    let construct (date: LongDate): HaabDate =

        let days = LongDate.daysSinceEpoch date
    
        (*-3113 BCE September 9th, 13.0.0.0.0 4 Ajaw, 8 Kumk’u *)
        (* 2012 12 21 4 Ajaw, 3 K'ank'in*)

        let head =
            match LongDate.Epoch with
            | BC3114 ->    
                (8 + ((number Kumku - 1) * 20))
            | CE2012 ->
                ((3 + ((number Kankin - 1) * 20)))

        let dayOfHaab = (days + head) % 365

        let day =
            dayOfHaab % 20
            |> int
        let month = 
            (dayOfHaab/20) + 1
            |> ofNumber

        struct (day, month)
 
    let ofDate y m d =
        let date = LongDate.construct y m d
        construct date

    let ofDateOnly (date: System.DateOnly): HaabDate =
        let (y, m, d) = date.Deconstruct()
        ofDate y m d

    

type HaabMonth with
        member month.Meaning = Haab.meaning month
        member month.Number = Haab.number month