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
            let ja= floor(0.01m*jy)
            jd0 + 2.0m-ja+floor(0.25m*ja)
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

    let mayaConstructJdn = 584282.5m

    let mutable longCount = jdn - mayaConstructJdn

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

printfn "%A" 
<| maya (decimal now.Year) (decimal now.Month) (decimal now.Day)

printfn "%A" <| maya 2012.0m 12.0m 20.0m
printfn "%A" <| maya 2012.0m 12.0m 21.0m
