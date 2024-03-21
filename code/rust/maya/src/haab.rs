use core::panic;

use crate::long::LongDate;

pub type HaabDay = i64;

#[derive(Debug)]
pub enum HaabMonth {
    Pop,
    Wo,
    Sip,
    Sotz,
    Sek,
    Xul,
    Yaxkin,
    Mol,
    Chen,
    Yax,
    Sak,
    Keh,
    Mak,
    Kankin,
    Muwan,
    Pax,
    Kayab,
    Kumku,
    Wayeb
}

pub type HaabDate = (HaabDay, HaabMonth);

impl HaabMonth {
    pub fn of_number(n: i64) -> HaabMonth {
        match n {
            00 => HaabMonth::Wayeb,
            01 => HaabMonth::Pop,
            02 => HaabMonth::Wo,
            03 => HaabMonth::Sip,
            04 => HaabMonth::Sotz,
            05 => HaabMonth::Sek,
            06 => HaabMonth::Xul,
            07 => HaabMonth::Yaxkin,
            08 => HaabMonth::Mol,
            09 => HaabMonth::Chen,
            10 => HaabMonth::Yax,
            11 => HaabMonth::Sak,
            12 => HaabMonth::Keh,
            13 => HaabMonth::Mak,
            14 => HaabMonth::Kankin,
            15 => HaabMonth::Muwan,
            16 => HaabMonth::Pax,
            17 => HaabMonth::Kayab,
            18 => HaabMonth::Kumku,
            19 => HaabMonth::Wayeb,
            _ => panic!()
        }
    }
    
    pub fn number(&self) -> i64 {
        match self {
            HaabMonth::Pop => 1,
            HaabMonth::Wo => 2,
            HaabMonth::Sip => 3,
            HaabMonth::Sotz => 4,
            HaabMonth::Sek => 5,
            HaabMonth::Xul => 6,
            HaabMonth::Yaxkin => 7,
            HaabMonth::Mol => 8,
            HaabMonth::Chen => 9,
            HaabMonth::Yax => 10,
            HaabMonth::Sak => 11,
            HaabMonth::Keh => 12,
            HaabMonth::Mak => 13,
            HaabMonth::Kankin => 14,
            HaabMonth::Muwan => 15,
            HaabMonth::Pax => 16,
            HaabMonth::Kayab => 17,
            HaabMonth::Kumku => 18,
            HaabMonth::Wayeb => 19,
        }
    }

    pub fn meaning(&self) -> &'static str {
        match self {
            HaabMonth::Pop => "mat",
            HaabMonth::Wo => "black conjunction",
            HaabMonth::Sip => "red conjunction",
            HaabMonth::Sotz => "bat",
            HaabMonth::Sek => "death",
            HaabMonth::Xul => "dog",
            HaabMonth::Yaxkin => "new sun",
            HaabMonth::Mol => "water",
            HaabMonth::Chen => "black storm",
            HaabMonth::Yax => "green storm",
            HaabMonth::Sak => "white storm",
            HaabMonth::Keh => "red storm",
            HaabMonth::Mak => "enclosed",
            HaabMonth::Kankin => "yellow sun",
            HaabMonth::Muwan => "owl",
            HaabMonth::Pax => "planting time",
            HaabMonth::Kayab => "turtle",
            HaabMonth::Kumku => "granary",
            HaabMonth::Wayeb => ""
        }
    }
}

pub fn construct(date: &LongDate) -> HaabDate {
    let days = date.days_since_epoch;
    let head = match date.epoch {
        crate::MayaEpoch::BC3114 =>
            8 + ((HaabMonth::Kumku.number()  - 1) * 20),
        crate::MayaEpoch::CE2012 =>
            3 + ((HaabMonth::Kankin.number() - 1) * 20)
    };
    let dayOfHaab = (days + head) % 365;
    let day = dayOfHaab % 20;
    let month = (dayOfHaab/20) + 1;
    let haabMonth = HaabMonth::of_number(month);
    (day, haabMonth)
}