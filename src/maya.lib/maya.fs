namespace Maya

open System

type RoundDate =
    { Tzolkin: TzolkinDate
      Haab: HaabDate 
      Long: LongDate }

module Maya =

    let round y m d =
        let long = LongDate.construct y m d
        let tzolkin =
            Tzolkin.construct long
        let haab =
            Haab.construct long
        { Tzolkin=tzolkin; 
          Haab=haab;
          Long=long }

    let roundOfDate (date: DateOnly) =
        let (y, m, d) = date.Deconstruct()
        round y m d


    let days date =
        LongDate.daysOfDate date

    let tzolkin date =
        Tzolkin.ofDateOnly date

    let haab date =
        Haab.ofDateOnly date