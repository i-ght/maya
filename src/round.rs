use crate::{haab, tzolkin, LongDate, MayaEpoch, RoundDate};

impl From<&LongDate> for RoundDate {
    fn from(value: &LongDate) -> Self {
        RoundDate(
            tzolkin::compute(value),
            haab::compute(value)
        )
    }
    
}

impl From<LongDate> for RoundDate {
    fn from(value: LongDate) -> Self {
        RoundDate::from(&value)
    }
}

impl RoundDate {
    pub fn from_date(epoch: MayaEpoch, y: i32, m: i32, d: i32) -> RoundDate {
        let long = LongDate::new(epoch, y, m, d);
        RoundDate::from(long)
    }
}