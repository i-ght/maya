namespace Maya

type RoundDate =
    { Tzolkin: struct (TzolkinDayName * TzolkinDayName)
      Haab: struct (HaabDay * HaabMonth) }

module Maya =

    let longDate y m d =
        LongCount.compute y m d

    let longDays y m d =
        LongCount.computeDays y m d

    let tzolkin y m d =
        Tzolkin.compute y m d

    let haab y m d =
        Haab.compute y m d