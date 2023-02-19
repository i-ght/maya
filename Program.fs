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
    let monthDefinitinos =
        Map
            [ (0, "January")
              (1, "February")
              (2, "March")
              (3, "April")
              (4, "May")
              (5, "June")
              (6, "July")
              (7, "August")
              (8, "September")
              (9, "October")
              (10, "November")
              (11, "December") ]

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
                DayAndMonthKnown

        match monthKnown m with
        | MonthKnown -> accountForLeap y m d
        | MonthUnknown -> DayAndMonthUnknown

    match m with
    | m when m <= 0 && m < monthDayLengthDefinitions.Count ->
        match know monthDayLengthDefinitions y m d with
        | DayAndMonthKnown -> DateKnown
        | DayAndMonthUnknown -> DateUnknown
    | _ -> DateUnknown

let inty x =
    if x > 0.0m then floor x else ceil x
    |> int

(* 
** credit: https://quasar.as.utexas.edu/BillInfo/JulianDatesG.html 
**
** 1. Express the date as Y M D, where Y is the year, M is the month number (Jan = 1, Feb =
** 2, etc.), and D is the day in the month.
**
** 2. If the month is January or February, subtract 1 from the year to get a new Y, and add
** 12 to the month to get a new M. (Thus, we are thinking of January and February as being
** the 13th and 14th month of the previous year).
**
** 3. Dropping the fractional part of all results of all multiplications and divisions, let
**   A = Y/100
**   B = A/4
**   C = 2-A+B
**   E = 365.25x(Y+4716)
**   F = 30.6001x(M+1)
**   JD= C+D+E+F-1524.5
*)

let julianCount (*1.*) y m d =

    (*2.*)
    let struct (y, m) =
        match m with
        | 1.0m
        | 2.0m -> struct (y - 1.0m, m + 12.0m)
        | _ -> struct (y, m)

    (*3.*)
    let a = y / 100.0m
    let b = a / 4.0m
    let c = 2.0m - a + b
    let e = 365.25m * (y + 4716.0m)
    let f = 30.6001m * (m + 1.0m)

    let z = c + d + e + f - 1524.5m

    let jd = z
    jd

(*
** credit due: 
** - https://www.sizes.com/time/cal_mayan.htm
** - http://www.madore.org/~david/misc/calendar.html
**
** Using astronomical events recorded both by Mayan and European astronomers, and historical
** events whose dates were recorded both by Spaniards and civilizations using the Mayan 
** calendar, scholars have sought to correlate the Mayan and Gregorian calendars. According
** to the most widely accepted correlation (the “Goodman-Martinez-Thompson Correlation”) the
** current Mayan epoch began on Wednesday 11 August 3113 bce (Gregorian) which is Julian
** date 584,282.5.
*)
let longCount jdn =

    let long = [ 0; 0; 0; 0; 0 ]

    let mayaConstructionJdn = 584282.5m

    let longCount = round jdn - mayaConstructionJdn
    ()

let maya m d y =

    ()

printfn "Hello from F#"
