use haab::HaabDate;
use tzolkin::TzolkinDate;

#[derive(Debug, Copy, Clone)]
pub enum MayaEpoch {
    BC3114,
    CE2012
}

type Day=i32;
type Month=i32;
type Year=i32;
type Date=(Day, Month, Year);

pub mod long;
pub mod tzolkin;
pub mod haab;
pub mod round;