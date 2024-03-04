namespace Maya

open System

[<Struct>]
type MayaEpoch =
    | BC3114
    | CE2012

module LongCount =

    let mutable epoch =
        BC3114

    let print (date: int list) =
        if List.length date <= 13 then
            let fmt = sprintf "%02i"
            let formatted = List.map fmt date
            let joined = String.concat"." formatted
            printfn "%s" joined

    (*
    Using astronomical events recorded both by Mayan and European astronomers, and historical
    events whose dates were recorded both by Spaniards and civilizations using the Mayan
    calendar, scholars have sought to correlate the Mayan and Gregorian calendars. According to
    the most widely accepted correlation (the “Goodman-Martinez-Thompson Correlation”) the
    current Mayan epoch began on Wednesday 11 August 3113 bce (Gregorian) (September 6, 3114 BCE
    Julian Calendar https://www.timeanddate.com/calendar/mayan.html) which is Julian date
    584,282.5.

    https://www.sizes.com/time/cal_mayan.htm
    *)

    (* 
        *)
    let private epochJd () =
        match epoch with
        | BC3114 -> 584282.5  (*-3113 BCE September 9th, 13.0.0.0.0 4 Ajaw, 8 Kumk’u *)
        | CE2012 -> jd 2012 12 21 (* 2012 12 21 4 Ajaw, 3 K'ank'in*)

(*
    https://en.wikipedia.org/wiki/Positional_notation

    +----------+----------+---------+
    |   Head   |   Tail   |  Days   |
    +----------+----------+---------+
    | 1 kin    | None     |       1 |
    | 1 uinal  | 20 kin   |      20 |
    | 1 tun    | 18 uinal |     360 |
    | 1 katun  | 20 tun   |   7,200 |
    | 1 baktun | 20 katun | 144,000 |
    +----------+----------+---------+

    Thus the Maya developed a modified base 20 system in which a date is represented by a
    number written (in modern Indo-Arabic notation) as: 

    a.b.c.d.e = a(18*20**3) + b(18*20**2) + c(18*20**1) + d(20**1) + e(20**0)

    where the third place value is not 20**2 but 18*20. After the third place, each higher place
    is 20 times the previous place value. So the system breaks only in the third place.

    https://www.maa.org/press/periodicals/convergence/when-a-number-system-loses-uniqueness-the-case-of-the-maya-the-mayan-number-system
*)

    let days y m d =
        let jd = jd y m d
        jd - epochJd ()
        |> int

    let compute y m d =
    
        let totalDays = days y m d

        let rec mayaDigis days index acc =
            let index = index - 1
            let fIndex = float index

            let unitOfDays = 
                match index with
                | index when index >= 2 ->
                    18.0 * 20.0 ** (fIndex - 1.0)
                | _ ->
                    20.0 ** fIndex
                |> int
        
            let struct (daysRemaining, value) =
                (days % unitOfDays, days / unitOfDays)
            
            let acc =
                value :: acc
            
            if index = 0 then
                acc
                |> List.rev
            else 
                mayaDigis
                <| daysRemaining
                <| index
                <| acc

        let placesNeeded = 5
        mayaDigis totalDays placesNeeded []


    let ofDate (date: DateOnly) =
        let (y, m, d) = date.Deconstruct()
        compute y m d
    
    let daysOfDate (date: DateOnly) =
        let (y, m, d) = date.Deconstruct()
        days y m d

    let daysOfDateTime (date: DateTimeOffset) =
        let (date, _time, _offset) =
            date.Deconstruct()
        daysOfDate date