use crate::{haab, tzolkin, LongDate, MayaEpoch, RoundDate};

impl RoundDate {
    pub fn of_longdate(long: LongDate) -> RoundDate {
        RoundDate {
            tzolkin: tzolkin::construct(&long),
            haab: haab::construct(&long),
            long
        }
    }

    pub fn of_date(epoch: MayaEpoch, y: i32, m: i32, d: i32) -> RoundDate {
        let long = LongDate::new(epoch, y, m, d);
        RoundDate::of_longdate(long)
    }
}