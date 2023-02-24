(*
**
** 13.0.10.5.11
** 2023 February 18
** The author disclaims copyright to this source code. 
**
** hymn:
O Kali! Thou hast brushed all difficulties aside.
Vedas and Tantras declare and SIva confirms - wilt Thou not admit this?
If Thou find a cremation ground, O Mother, Thou preferest it to a place set with jewells. 
O Mother! Thou and th Consort are all alike; He desists not fromc rushing hemp leaves.
He who is Thy devotee, Mother, has a different kind of lustre.
He is seldom clad in rags, cubs his body with ashes, and weawrs matted hair.
Thou brought'st me to earth and gavest me untold misery.
Yet I Mutter Kali's name - see how courageous I am!
People broadcast all around - Rama Prasada is Kali's son
Who will understand the secret of Mother's treatment with Her son?
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
    let y = if y < 0.0 then y + 1.0 else y

    let struct (jy, jm) =
        if m > 2.0 then
            struct (y, m + 1.0)
        else
            struct (y - 1.0, m + 13.0)
    
    (* why these numbers? what do they mean? 
       i don't know, i just copy and pasted them *)
    let jd0 = (floor(365.25*jy)+floor(30.6001*jm)+d+1720995.0)

    let jd1 =
        if (d+31.0*(m+12.0*y) >= (15.0+31.0*(10.0+12.0*1582.0))) then
            let ja = floor(0.01*jy)
            jd0 + 2.0-ja+floor(0.25*ja)
        else
            jd0
            
    jd1

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

Thus the Maya developed a modified base 20 system in which a date is represented by a
number written (in modern Indo-Arabic notation) as: 

a.b.c.d.e = a(18*20**3) + b(18*20**2) + c(18*20**1) + d(20**1) + e(20**0)

where the third place value is not 202 but 18×20. After the third place, each higher place
is 20 times the previous place value. So the system breaks only in the third place.


https://www.maa.org/press/periodicals/convergence/when-a-number-system-loses-uniqueness-the-case-of-the-maya-the-mayan-number-system

*)

type MayaTimeCycle =
    | Kin   
    | Uinal 
    | Tun   
    | Katun
    | Baktun
with
    member cycle.DayLength =
        match cycle with                 
        | Kin    ->        20.0 ** 0 (* 1 *)
        | Uinal  ->        20.0 ** 1 (* 20 *)
        | Tun    -> 18.0 * 20.0 ** 1 (* 360 *)
        | Katun  -> 18.0 * 20.0 ** 2 (* 7,200 *)
        | Baktun -> 18.0 * 20.0 ** 3 (* 144,000 *)
        |> int

module Maya =

(*

Using astronomical events recorded both by Mayan and European astronomers, and historical
events whose dates were recorded both by Spaniards and civilizations using the Mayan
calendar, scholars have sought to correlate the Mayan and Gregorian calendars. According to
the most widely accepted correlation (the “Goodman-Martinez-Thompson Correlation”) the
current Mayan epoch began on Wednesday 11 August 3113 bce (Gregorian) (September 9, 3114 BCE
Julian Calendar) which is Julian date 584,282.5.

https://www.sizes.com/time/cal_mayan.htm
*)
    let [<Literal>] Construct = 584282.5
    
    let date y m d = 
        
        let julia = julianCount y m d
        let daysSinceConstruct = 
            julia - Construct
            |> int

        let base' = 20
        let rec digis value digits =
            let struct (q, r) = value / base', value % base'
            if (0 = q) then 
                r :: digits
            else 
                digis q <| r :: digits

        let tun (* ⚫ *) = 360

        let struct (turnsOfTheWheel, daysRemaining) =
            daysSinceConstruct / tun, daysSinceConstruct % tun

        let struct (black, white) = digis turnsOfTheWheel [], digis daysRemaining []
        black @ white

let maya y m d =
    Maya.date y m d

let argv = Environment.GetCommandLineArgs() 

let struct (y, m, d) =
    match argv.Length with
    | 3 ->
        struct (
            int argv[0],
            int argv[1],
            int argv[2]
        )
    | _ ->
        let now = DateTimeOffset.Now
        now.Year, now.Month, now.Day

let mayaOK (date: int list) =
    if date.Length < 5 then
        false
    else 
        match struct (date.[0], date.[1], date.[2], date.[3],date.[4]) with
        | struct (baktun, katun, tun, uinal, kin)
            when List.contains baktun [0..13]
            &&   List.contains katun  [0..19]
            &&   List.contains tun    [0..19]
            &&   List.contains uinal  [0..18]
            &&   List.contains kin    [0..19] -> true
        | _                                   -> false

let printMaya (date: int list) = 
    let fmt = sprintf "%02i"
    let formatted = List.map fmt date
    let joined = String.Join('.', formatted)
    printfn "%s" joined


printMaya <| maya y m d

exit 0