namespace Maya

open System

(* 1-13 *)
type TzolkinNumber = int

[<Struct>]
type TzolkinDayName =
    | Imix
    | Ik
    | Akbal
    | Kan
    | Chikchan
    | Kimi
    | Manik
    | Lamat
    | Muluk
    | Ok
    | Chuwen
    | Eb
    | Ben
    | Ix
    | Men
    | Kib
    | Kaban
    | Etznab
    | Kawak
    | Ajaw

type TzolkinDate = (struct (TzolkinNumber * TzolkinDayName))

module Tzolkin =

    let ofNumber n = n |> function
        | 00 -> Ajaw
        | 01 -> Imix
        | 02 -> Ik
        | 03 -> Akbal
        | 04 -> Kan
        | 05 -> Chikchan
        | 06 -> Kimi
        | 07 -> Manik
        | 08 -> Lamat
        | 09 -> Muluk
        | 10 -> Ok
        | 11 -> Chuwen
        | 12 -> Eb
        | 13 -> Ben
        | 14 -> Ix
        | 15 -> Men
        | 16 -> Kib
        | 17 -> Kaban
        | 18 -> Etznab
        | 19 -> Kawak
        | 20 -> Ajaw
        |_ -> invalidArg "n" "n must be from 1-20"

    let meaning day = day |> function
        | Imix -> "waterlily, crocodile"
        | Ik -> "wind, breath, life force"
        | Akbal -> "darkness, night, early dawn"
        | Kan -> "Net, sacrifice"
        | Chikchan -> "cosmological snake"
        | Kimi -> "death"
        | Manik -> "deer"
        | Lamat -> "Venus, star, ripe(ness), maize seeds"
        | Muluk -> "jade, water, offering"
        | Ok -> "dog"
        | Chuwen -> "howler monkey"
        | Eb -> "rain"
        | Ben -> "green/young maize, seed"
        | Ix -> "jaguar"
        | Men -> "eagle"
        | Kib -> "wax"
        | Kaban -> "earth"
        | Etznab -> "flint"
        | Kawak -> "rain storm"
        | Ajaw -> "lord, ruler, sun"


    let detailedMeaning day = day |> function
        | Imix -> "The etymology of IMOX comes from the word “sea,” “river,” and “lake.” It is the day to ask for a person's return after they have left their home or country. People ask for rain on this day, the nawal of the sea. Also, on this day people ask for the calming of mental and spiritual disorders, climate changes, and problems at home. Those who are born on this day are good workers, intuitive, and creative."
        | Ik -> "The etymology of IQ' comes from the word “wind,” “air,” “spirit,” and “heart of the sky.” It means heavy showers and hurricanes. On this day, people ask for the end of suffering, illnesses, or problems at home. Those who are born on this day are emotional, sociable, considerate, and have a pleasant life."
        | Akbal -> "The etymology of AQ'AB'AL comes from the word “darkness.” It means dawn and hand; it is the light of dawn and darkness. It is a day to ask for light to reach all things. It is the day for feelings of the heart. Those who are born on this day are brave hunters, humble, serious, precise, and can withstand criticism and rejection."
        | Kan -> "The etymology of K'AT comes from the word “net,” “tangle,” or “disentangle.” It is the symbol of fire, burning, fishing net, and net to protect the corncobs. It is the day to ask for a child to be born. Those who are born on this day promote theoretical and practical laws, are good, weak, and gravitate toward the fire."
        | Chikchan -> "The etymology of KAN comes from the word “feathered serpent.” It is the Framer and the Shaper of the Universe. It means justice, truth, and peace. People ask for strength, justice, truth, and peace. Those who are born on this day are strong, skillful, wise, and sincere; psychologists and physicists."
        | Kimi -> "The etymology of KAME comes from the word “death.” It encompasses the good and the bad. It is the nawal of the Sun. It means death and the owl that announces death from the trees. On this day, people ask to be free of bad choices and accidents. Those who are born on this day are strong and suffer a great deal."
        | Manik -> "The etymology of KEJ comes from the word “deer.” It is one of the four means of support of the sky and the Earth. It is the force that carries the destinies of humankind. It represents the four corners of the Earth; in K'iche”: Tojil, Avilix, Aja Bitz, and Majukutaj. Those who are born on this day are defenders of other people and are irritable; they don't get sick so easily, and are strong."
        | Lamat -> "The etymology of Q'ANIL comes from the word “seed” and “yellow.” It means the color of gold and of cane, and the third color of the Sun. It is the symbol of the four seasons of the year. It also symbolizes humans, animals, and plants. Those who are born on this day tend to tell about what they don't have and what they are not. They suffer a lot, are not very strong or resistant, and are always in need of moral support from someone else."
        | Muluk -> "The etymology of TOJ comes from the word “offering,” “fine,” and “payment.” It also comes from “help,” socialize,” “listen,” and “understand.” People ask for strength to avoid mistakes and for the end of their sufferings. It is the nawal of fire. People ask for dark things to come out into the light. Those who are born on this day can be good in psychology, morals, as well as material and social arenas. They can be conciliators, and are self-taught."
        | Ok -> "The etymology of TZ'I' comes from the word “dog.” It represents the five senses of human beings. On this day, people ask to prevent and keep away poverty, misfortune, and bad habits. It is the nawal of material and spiritual justice. People can ask to judge the good and the bad, and to be free of criticism."
        | Chuwen -> "The etymology of B'ATZ' comes from the word “thread.” It symbolizes people's destiny and the continuity with the past. It is the thread of time and the development of humankind and nature. On this day, New Year ceremonies take place to celebrate a new cycle of 260 days. New men and women day keepers, or Ajq'ijab, are initiated on this day. People born on this day will be successful in business, marriage, and throughout their lives. They will be respectable, sociable, intelligent, defenders, and teachers."
        | Eb -> "The etymology of E' comes from the word “path.” On this day a man can ask a woman's hand in marriage. It is a day to ask for the physical and moral well-being of a person. It is the time when the ancestors are ready to listen. People born on this day are sociable, good travelers, and generous."
        | Ben -> "The etymology of AJ' comes from the word “cane plantation” and “milpa.” AJ' means “corn” and it is a rod of virtues of divine power. It is a very meaningful day, a day of triumph. Those who are born on this day are very lucky, calm, and intelligent, although sometimes can be moody."
        | Ix -> "The etymology of I'X comes from the word “tiger.” Its meaning is related to vitality, the Maya altar, and wisdom. It is the sacred name of divinity on Earth and the nawal of domestic animals. Those who are born on this day are strong and vigorous."
        | Men -> "The etymology of TZ'IKIN comes from the word “bird.” It symbolizes the Creator of the Universe, represented by all that is in space. It is also the day of money, the day of business, and of merchants. Those who are born on this day are good, kind, and romantic."
        | Kib -> "The etymology of AJMAQ comes from the word “will.” It is a very special day in which people and the Ajq'ijab spend their time preventing mistakes. It is a day to give thanks for the physical and material well-being of the people and our communities. Those who are born on this day can be temperamental and brave."
        | Kaban -> "The etymology of NO'J comes from the word “wisdom,” “criterion,” “reason,” and “thought.” It is the day to come to an agreement with the Creator. It is the day to join ideas, give advice, and cultivate science. It is the day to ask for a change in a person's negative character and to get good ideas granted towards political and social endeavors. Those who are born on this day are good, prudent, and temperamental; merchants or doctors."
        | Etznab -> "The etymology of TIJAX comes from the word “destiny,” “obsidian knife,” and “spontaneous temptation.” On this day, people ask for protection against social, community, or personal wrongdoing. Those who are born on this day are good and brave; healers. They are also irritable and suffer from gossip, arguments, and accidents, which can be prevented by giving offerings."
        | Kawak -> "The etymology of KAWOQ comes from the word “thunder,” “ants,” and “woman.” It is the day to ask for success for projects and to heal mental illnesses in humans. It is the day to ask for prosperity on Earth. Those who are born on this day are good predictors of the future, and judges."
        | Ajaw -> "The etymology of AJPU' comes from the word “life,” “destiny,” “plants,” and “animals.” It is the Lord Sun. It means hunter, marksman, and walker. On this day, people ask for the fulfillment of the ideas suggested by the community. Those who are born on this day are good, talented, and affectionate, but at the same time, they are hot-tempered and judgmental."

    let number day = day |> function
        | Imix -> 1
        | Ik -> 2
        | Akbal -> 3
        | Kan -> 4
        | Chikchan -> 5
        | Kimi -> 6
        | Manik -> 7
        | Lamat -> 8
        | Muluk -> 9
        | Ok -> 10
        | Chuwen -> 11
        | Eb -> 12
        | Ben -> 13
        | Ix -> 14
        | Men -> 15
        | Kib -> 16
        | Kaban -> 17
        | Etznab -> 18
        | Kawak -> 19
        | Ajaw -> 20

    let construct (date: LongDate): TzolkinDate =
        (*-3113 BCE September 9th, 13.0.0.0.0 4 Ajaw, 8 Kumk’u *)
        (* 2012 12 21 4 Ajaw, 3 K'ank'in*)

        let dayz = LongDate.daysSinceEpoch date

        let n = 
            (dayz + 4) % 13
            |> function
            | 0 -> 13 
            | n -> n

        let name =
            (dayz + number Ajaw) % 20 
            |> ofNumber

        struct (n, name)

    let ofDate y m d =
        let date = LongDate.construct y m d
        construct date

    let ofDateOnly (date: DateOnly): TzolkinDate =
        let (y, m, d) = date.Deconstruct()
        ofDate y m d

    let ofDateTime (dateTime: DateTimeOffset) =
        let (date, _time, _offset) =
            dateTime.Deconstruct()
        ofDateOnly date

type TzolkinDayName with
    member day.Number = Tzolkin.number day
    member day.Meaning = Tzolkin.meaning day
    member day.DetailedMeaning = Tzolkin.detailedMeaning day
    static member OfDateOnly date = Tzolkin.ofDateOnly date
    static member OfDateTime dateTime = Tzolkin.ofDateTime dateTime