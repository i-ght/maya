#[derive(Debug, Copy, Clone)]
pub enum MayaEpoch {
    BC3114,
    CE2012
}

type Day=i32;
type Month=i32;
type Year=i32;
type Date=(Day, Month, Year);

#[derive(Debug, Clone)]
pub struct LongDate {
    pub digits: Vec<i64>,
    pub date: Date,
    pub days_since_epoch: i64,
    pub epoch: MayaEpoch
}

pub type TzolkinNumber = i64;

#[derive(Debug)]
pub enum TzolkinDayName {
    Imix = 1,
    Ik,
    Akbal,
    Kan,
    Chikchan,
    Kimi,
    Manik,
    Lamat,
    Muluk,
    Ok,
    Chuwen,
    Eb,
    Ben,
    Ix,
    Men,
    Kib,
    Kaban,
    Etznab,
    Kawak,
    Ajaw
}

pub type TzolkinDate = (TzolkinNumber, TzolkinDayName);

pub type HaabDay = i64;

#[derive(Debug)]
pub enum HaabMonth {
    Pop = 1,
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

#[derive(Debug)]
pub struct RoundDate {
    pub tzolkin: TzolkinDate,
    pub haab: HaabDate,
    pub long: LongDate
}

pub mod long;
pub mod tzolkin;
pub mod haab;
pub mod round;