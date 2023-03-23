(*
Credit due: 
- https://maya.nmai.si.edu/sites/all/themes/mayatime/js/calendar-converter/calendar-converter-en.js?rpyhor
- https://maya.nmai.si.edu/calendar/maya-calendar-converter
- https://maya.nmai.si.edu/resources
- https://www.sizes.com/time/cal_mayan.htm
- http://www.madore.org/~david/misc/calendar.html
- https://www.fourmilab.ch/documents/calendar/ 
- https://irp-cdn.multiscreensite.com/7b1aa4fb/files/uploaded/Thirteen%20Baktun%20Final.pdf ~ Thirteen Baktun: A Recalculation of the Calender End and the Length of the World Age by Loren W. Jeffries ©2018
The Long Count calendar keeps track of the days that have passed since the mythical
starting date of the Maya creation, August 11, 3114 BCE.
(September 9, 3114 BCE Julian Calendar) 
*)


open System

module Maya =

    [<Struct>]
    type BlackOrWhite =
        | Black
        | White

    (* credit: https://fx.sauder.ubc.ca/julian.html *)

    let julianCount y m d =
        let y = if y < 0 then y + 1 else y
        let struct (y, m, d) = float y, float m, float d

        let struct (jy, jm) =
            if m > 2 then
                (y, m + 1.0)
            else
                (y - 1.0, m + 13.0)
        
        (* why these numbers? what do they mean? 
        i don't know, i just copy and pasted them *)
        let jd0 = (floor(365.25*jy)+floor(30.6001*jm)+d+1720995.0)

        let jd1 =
            if (d+31.0*(m+12.0*y) >= (15.0+31.0*(10.0+12.0*1582.0))) then
                let ja = floor(0.01*jy)
                jd0 + 2.0-ja+floor(0.25*ja)
            else
                jd0
                
        jd1 - 0.5

    (*
    https://en.wikipedia.org/wiki/Positional_notation

    +----------+----------+---------+
    |   Head   |   Tail   |  Days   |
    +----------+----------+---------+
    | 1 kin    | None     |       1 |
    | 1 uinal  | 20 kin   |      20 |
    | 1 tun    | 18 uinal |     360 |
    | 1 katun  | 20 tun   |   7,200 |(*  *)
    | 1 baktun | 20 katun | 140,000 |
    +----------+----------+---------+

    Thus the Maya developed a modified base 20 system in which a date is represented by a
    number written (in modern Indo-Arabic notation) as: 

    a.b.c.d.e = a(18*20**3) + b(18*20**2) + c(18*20**1) + d(20**1) + e(20**0)

    where the third place value is not 20**2 but 18*20. After the third place, each higher place
    is 20 times the previous place value. So the system breaks only in the third place.

    https://www.maa.org/press/periodicals/convergence/when-a-number-system-loses-uniqueness-the-case-of-the-maya-the-mayan-number-system


    *)

    (*
    type MayaTimeCycle =
        | Kin   
        | Uinal 
        | Tun   
        | Katun
        | Baktun
    with
        member cycle.DayLength =
            match cycle with                 
            | Kin    ->        20.0 0 (* 1 *)
            | Uinal  ->        20.0 1 (* 20 *)
            | Tun    -> 18.0 * 20.0 1 (* 360 *)
            | Katun  -> 18.0 * 20.0 2 (* 7,200 *)
            | Baktun -> 18.0 * 20.0 3 (* 144,000 *)
            |> int
    *)

    let discoPanic<'a> msg = invalidOp<'a> msg

    let print (date: int list) =
        if List.length date <= 13 then
            let fmt = sprintf "%02i"
            let formatted = List.map fmt date
            let joined = String.Join('.', formatted)
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

    let private EPOCH_JDAYS = 584282.5
    (*
    The Babylonian numeral system, base 60, was the first positional system to be 
    developed, and its influence is present today in the way time and angles are counted in 
    tallies related to 60, such as 60 minutes in an hour and 360 degrees in a circle.
    *)
    let private WHEEL_TURN_RADIX = 360
    
    let private RADIX = 20

    let date y m d = 
        
        let julia = julianCount y m d
        let daysSinceConstruct = 
            julia - EPOCH_JDAYS
            |> int

        let padding amt = Array.create amt 0 |> List.ofSeq


        let rec mayaDigis blackOrWhite value digits =
            match struct (value / RADIX, value % RADIX, blackOrWhite, List.length digits) with
            | (0, r, Black, len) when len < 2 ->
                padding (2 - len) @ r :: digits 
            | (0, r, White, len) when len < 1 ->
                padding (1 - len) @ r :: digits 
            | (q, r, _, _) when q = 0 ->
                r :: digits
            | (q, r, _, _) ->
                mayaDigis blackOrWhite q <| r ::digits


    (*
    a.b.c.d.e = a(18*20**3) + b(18*20**2) + c(18*20**1) + d(20**1) + e(20**0)

    where the third place value is not 20**2 but 18*20. After the third place, each higher 
    place is 20 times the previous place value. So the system breaks only in the third place.

    begin from the center;
    clever mayans 

    REMINDER: More cleverness to deconstruct. Baktuns are limited by the value 13. With 12 wheel
    slices and a center point, to form 13.
    *)

        let struct (turnsOfTheWheel,daysRemaining) =
            daysSinceConstruct / WHEEL_TURN_RADIX,
            daysSinceConstruct % WHEEL_TURN_RADIX

        let struct (black, white) =
            [struct(turnsOfTheWheel, Black); struct(daysRemaining, White)]
            |> List.map (function | (i, blackOrWhite) -> mayaDigis blackOrWhite i [])
            |> function | [black; white]-> (black, white)
                        | _ -> discoPanic<struct (int list * int list)> "cease warning"

        black @ white


let argv = Environment.GetCommandLineArgs() 

let struct (y, m, d) =
    if 4 = argv.Length then
        struct (
            int argv[1],
            int argv[2],
            int argv[3]
        )
    else
        let now = DateTimeOffset.Now
        now.Year, now.Month, now.Day


Maya.print <| Maya.date 2012 12 21
Maya.print <| Maya.date -3114 9 6

Maya.print <| Maya.date y m d

exit 0