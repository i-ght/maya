namespace Maya

open System

type RoundDate =
    { Tzolkin: TzolkinDate
      Haab: HaabDate 
      Long: LongDate }

 module Round =
 
    let construct date =
        { Tzolkin=Tzolkin.construct date
          Haab=Haab.construct date
          Long=date }

    let str (round: RoundDate) =
        (sprintf "%A" round)
            .Replace("struct ", "")
    

module Maya =

    let long y m d =
        LongDate.construct y m d

    let round date =
        Round.construct date

    let roundOfDate (date: DateOnly) =
        let (y, m, d) = date.Deconstruct()
        let long = LongDate.construct y m d
        round long

    let days date =
        LongDate.daysOfDate date

    let tzolkin date =
        Tzolkin.ofDateOnly date

    let haab date =
        Haab.ofDateOnly date
