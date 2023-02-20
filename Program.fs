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
**
** Reading the Maya Calendar
** The Maya calendar system records a series of recurring cycles of time based on the
** movements of the Sun, Moon, and planets. Any given date repeats at cyclic intervals, just 
** as, for example, January 1st in the Gregorian calendar repeats every time the Earth
** completes a revolution around the Sun. A complete Maya Long Count cycle is 5,125 years 
** long. The Maya Long Count system establishes an absolute chronology in which any given
** date is unique, such as December 21, 2012, in the Gregorian system. The Long Count
** calendar keeps track of the days that have passed since the mythical starting date of the
** Maya creation, August 11, 3114 BCE. 
*)

[<Struct>]
type DateKnown =
    | DateKnown
    | DateUnknown

[<Struct>]
type MonthKnown =
    | MonthKnown
    | MonthUnknown

[<Struct>]
type DayAndMonthKnown =
    | DayAndMonthKnown
    | DayAndMonthUnknown

let isDateKnown y m d =

    let monthDayLengthDefinitions =
        Map
            [ (0, 31)
              (1, 29)
              (2, 31)
              (3, 30)
              (4, 31)
              (5, 30)
              (6, 31)
              (7, 31)
              (8, 30)
              (9, 31)
              (10, 30)
              (11, 31) ]

    let monthKnown m =
        if m > 0 && m < monthDayLengthDefinitions.Count then
            MonthKnown
        else
            MonthUnknown

    let know (mdefs: Map<int, int>) y m d =

(*
** Credit due: https://www.mathsisfun.com/leap-years.html
** How to know if it is a Leap Year:
**
** yes Leap Years are any year that can be exactly divided by 4
**                                                           (such as 2016, 2020, 2024, etc)
**   not except if it can be exactly divided by 100, then it isn't (such as 2100, 2200, etc)
**     yes except if it can be exactly divided by 400, then it is (such as 2000, 2400)
**
*)
        let accountForLeap y m d =
            let monthDayLength = mdefs.[m]

            if d > 0 && d <= monthDayLength then
                match (m, d) with
                (*leap accounting *)
                | (1, 29) when y % 4 <> 0 -> DayAndMonthUnknown
                | (1, 29) when y > 1582 && y % 4 = 0 && y % 100 = 0 && y % 400 <> 0 ->
                    DayAndMonthUnknown
                | _ -> DayAndMonthKnown
            else
                DayAndMonthUnknown

        match monthKnown m with
        | MonthKnown -> accountForLeap y m d
        | MonthUnknown -> DayAndMonthUnknown

    match m with
    | m when m <= 0 && m < monthDayLengthDefinitions.Count ->
        match know monthDayLengthDefinitions y m d with
        | DayAndMonthKnown -> DateKnown
        | DayAndMonthUnknown -> DateUnknown
    | _ -> DateUnknown

let i i =
    if i > 0.0m then
        floor i
    else
        ceil i

(* credit: https://fx.sauder.ubc.ca/julian.html 

function julian(Y,M,D,UT) {
   with (Math) {
     if (Y < 0) ++Y;
     if (M > 2) { jy=Y;   jm=M+1;  }
     else       { jy=Y-1; jm=M+13; }
     jd2 = (floor(365.25*jy)+floor(30.6001*jm)+D+1720995);
     if (D+31*(M+12*Y) >= (15+31*(10+12*1582)))
     { ja=floor(0.01*jy);
       jd2 += 2-ja+floor(0.25*ja);
     }
//
//     jd=367*Y-floor(7*(Y+floor((M+9)/12))/4)
//       -floor(3*(floor((Y+(M-9)/7)/100)+1)/4)
//       +floor(275*M/9)+D+1721028.5+UT/24.0;
   }
   return jd2;
}
*)

let julianCount (*1.*) y m d =
    let y = if y < 0.0m then y + 1.0m else y

    let struct (jy, jm) =
        if m > 2.0m then
            struct (y, m + 1.0m)
        else
            struct (y - 1.0m, m + 13.0m)
    
    let jd0 = (floor(365.25m*jy)+floor(30.6001m*jm)+d+1720995.0m)

    let jd1 =
        if (d+31.0m*(m+12.0m*y) >= (15.0m+31.0m*(10.0m+12.0m*1582.0m))) then
            let ja= floor(0.01m*jy)
            2.0m-ja+floor(0.25m*ja)
        else
            jd0
            
    jd1

(*
** credit due: 
** - https://www.sizes.com/time/cal_mayan.htm
** - http://www.madore.org/~david/misc/calendar.html
**
** Using astronomical events recorded both by Mayan and European astronomers, and historical
** events whose dates were recorded both by Spaniards and civilizations using the Mayan 
** calendar, scholars have sought to correlate the Mayan and Gregorian calendars. According
** to the most widely accepted correlation (the “Goodman-Martinez-Thompson Correlation”) the
** current Mayan epoch began on Wednesday 11 August 3113 bce (Gregorian) which is Julian date 584,282.65.
*)
let longCount jdn =

    let mutable lng = [|0.0m; 0.0m; 0.0m; 0.0m; 0.0m|]

    let mayaConstructionJdn = 584282.5m

    let mutable longCount = (*round*) jdn - mayaConstructionJdn

    printfn "long count=%M" longCount


    lng.[0] <- floor longCount / 144000.0m
    longCount <- longCount % 144000.0m;
    
    lng.[1] <- floor longCount / 7200.0m
    longCount <- longCount % 7200.0m

    lng.[2] <- floor longCount / 360.0m
    longCount <- longCount % 360.0m

    lng.[3] <- longCount / 20.0m

    lng.[4] <- longCount % 20.0m

    lng |> List.ofSeq

let maya y m d =
    let lng = longCount <| julianCount y m d
    lng |> List.map int

let now = System.DateTimeOffset.Now

// printfn "%A" 
// <| maya (decimal now.Year) (decimal now.Month) (decimal now.Day)

printfn "%A" <| julianCount -4714.0m 1.0m 1.0m
printfn "%A" <| maya -2023.0m 2.0m 19.0m
