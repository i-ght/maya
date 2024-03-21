use crate::{haab, tzolkin, LongDate, MayaEpoch, RoundDate};



impl RoundDate {
    pub fn construct(long: LongDate) -> RoundDate {
        RoundDate {
            tzolkin: tzolkin::construct(&long),
            haab: haab::construct(&long),
            long
        }
    }

    pub fn of_epoch_and_date(epoch: MayaEpoch, y: i32, m: i32, d: i32) -> RoundDate {
        let long = LongDate::construct(epoch, y, m, d);
        RoundDate::construct(long)
    }
}