namespace Maya

open System

[<AutoOpen>]
module internal Convenience =

    (* credit: https://fx.sauder.ubc.ca/julian.html *)

    let jd y m d =
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