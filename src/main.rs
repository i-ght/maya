use chrono::{Datelike, Local};
use maya::{LongDate, MayaEpoch, RoundDate};

fn main() {
    let now = Local::now();
    let date = now.date_naive();
    let long =
        LongDate::new(
            MayaEpoch::BC3114,
            date.year(),
            date.month0() as i32 + 1,
            date.day0() as i32 + 1
        );
    let round = RoundDate::from(&long);
    
    println!("{:#?}", long);
    println!("{:#?}", round);
    println!("{}", round.0.1.detailed_meaning());
    println!("{}", round.0.1.meaning());
    println!("{}", round.1.1.meaning());
}
