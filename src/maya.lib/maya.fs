namespace Maya

type RoundDate =
    { Tzolkin: struct (TzolkinDayName * TzolkinDayName)
      Haab: struct (HaabDay * HaabMonth) }

module Maya =

    let longDate date =
        LongCount.ofDate date

    let longDays date =
        LongCount.days date

    let tzolkin date =
        Tzolkin.ofDate date

    let haab date =
        Haab.ofDate date