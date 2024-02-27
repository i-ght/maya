﻿(*
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
open Maya
open std
            
let argv = Env.argv ()

let struct (y, m, d) =
    if 4 = argv.Length then
        int argv[1],
        int argv[2],
        int argv[3]
    else
        let (date, _time, _offset) =
            DateTimeOffset.Now.Deconstruct()
        date.Year,
        date.Month,
        date.Day


Maya.print
<| Maya.computeDate y m d

let struct (number, day) =
    Tzolkin.compute y m d
printfn "%i, %A" number day
printfn "%s" day.Meaning
printfn "%s" day.DetailedMeaning

Maya.haab y m d
|> printfn "%A"

exit 0