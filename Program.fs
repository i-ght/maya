(*
**
** 13.0.10.5.11
** 2023 February 18
** The author disclaims copyright to this source code. 
**
** hymn:
** In dense darkness O Mother, Thy formless beauty sparkles;
** Therefore the yogis meditate in a dark mountain cave.
** In the lap of boundless dark, on Mahanirvana's waves upborne
** Peace flows serene and inexhaustible.
**
** Taking the form of the Void, in the robe of darkness wrapped,
** Who art Thou, Mother, seated alone in the shrine of samadhi?
** From the Lotus of Thy fear-scattering Feet flash Thy love's lightnings;
** Thy Spirit-Face shines forth with laughter terrible and loud!
**
** Credit due: 
** - https://maya.nmai.si.edu/sites/all/themes/mayatime/js/calendar-converter/calendar-converter-en.js?rpyhor
** - https://maya.nmai.si.edu/calendar/maya-calendar-converter
** - https://maya.nmai.si.edu/resources
** - https://www.sizes.com/time/cal_mayan.htm
** - http://www.madore.org/~david/misc/calendar.html
** - https://www.fourmilab.ch/documents/calendar/ 
**
** The Long Count calendar keeps track of the days that have passed since the mythical
** starting date of the Maya creation, August 11, 3114 BCE.
** (September 9, 3114 BCE Julian Calendar) 
*)

open System

(* credit: https://fx.sauder.ubc.ca/julian.html *)

let julianCount y m d =
    let y = if y < 0.0m then y + 1.0m else y

    let struct (jy, jm) =
        if m > 2.0m then
            struct (y, m + 1.0m)
        else
            struct (y - 1.0m, m + 13.0m)
    
    let jd0 = (floor(365.25m*jy)+floor(30.6001m*jm)+d+1720995.0m)

    let jd1 =
        if (d+31.0m*(m+12.0m*y) >= (15.0m+31.0m*(10.0m+12.0m*1582.0m))) then
            let ja = floor(0.01m*jy)
            jd0 + 2.0m-ja+floor(0.25m*ja)
        else
            jd0
            
    jd1

type MayaTimeCycle =
    | Kin
    | Uinal
    | Tun
    | Katun
    | Baktun
with
    member cycle.DayLength =
        match cycle with
        | Kin -> pown 20.0m 0
        | Uinal -> pown 20.0m 1
        | Tun -> 18.0m * pown 20.0m 1
        | Katun -> 18.0m * pown 20.0m 2
        | Baktun -> 18.0m * pown 20.0m 3

(*
    Thus the Maya developed a modified base 20 system in which a date is represented by a
    number written (in modern Indo-Arabic notation) as: 

    a.b.c.d.e = a(18×pown 20 3) + b(18×pown 20 2) + c(18×pown 20 1) + d(pown 20 1) + e(pown
    20 0)
*)

let mayaLongCount y m d =
    let julian = julianCount y m d

    ()

let longCount jdn =

    let mutable lng = [|0.0m; 0.0m; 0.0m; 0.0m; 0.0m|]

    let mayaConstructJdn = 584282.5m

    let days = jdn - mayaConstructJdn;

    (*
        +----------+----------+---------+
        |   Head   |   Tail   |  Days   |
        +----------+----------+---------+
        | 1 kin    | None     |       1 |
        | 1 uinal  | 20 kin   |      20 |
        | 1 tun    | 18 uinal |     360 |
        | 1 katun  | 20 tun   |   7,200 |
        | 1 baktun | 20 katun | 140,000 |
        +----------+----------+---------+

    *)

    let mutable longCount = jdn - mayaConstructJdn

    lng.[0] <- longCount / 144000.0m
    longCount <- longCount % 144000.0m;
    
    lng.[1] <- longCount / 7200.0m
    longCount <- longCount % 7200.0m

    lng.[2] <- longCount / 360.0m
    longCount <- longCount % 360.0m

    lng.[3] <- longCount / 20.0m

    lng.[4] <- longCount % 20.0m

    lng |> List.ofSeq

let maya y m d =
    let lng = longCount <| julianCount y m d
    lng |> List.map int

let argv = Environment.GetCommandLineArgs() 

let struct (y, m, d) =
    match argv.Length with
    | 3 ->
        struct(
            Decimal.Parse(argv.[0]),
            Decimal.Parse(argv.[1]),
            Decimal.Parse(argv.[2])
        )
    | _ ->
        let now = DateTimeOffset.Now
        ValueTuple.Create(now.Year, now.Month, now.Day)

printfn "%A" <| maya -3113.0m 9.0m 6.0m

printfn "%A" <| maya y m d


exit 0