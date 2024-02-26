namespace Maya

module Maya =        

    (* credit: https://fx.sauder.ubc.ca/julian.html *)

    let jd y m d=
        let y =
            if y < 0 then y + 1 else y
        let struct (y, m, d) =
            float y, float m, float d

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
    let [<Literal>] private EpochJd = 584282.5 
        (*julian -3114 9 6*)


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

    let longDate y m d =
        
        let jd = jd y m d

        let totalDays = 
            (jd - EpochJd)
            |> int

        let rec mayaDigis (days: int) (index: int) (acc: int list) =
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
            
            if index = 0 then
                value :: acc
                |> List.rev
            else 
                mayaDigis
                <| daysRemaining
                <| index
                <| value :: acc

        let placesNeeded = 5
        mayaDigis totalDays placesNeeded []

