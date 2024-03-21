use maya::{RoundDate, MayaEpoch};

fn main() {
    let round = 
        RoundDate::of_date(MayaEpoch::CE2012, 2024, 03, 21);
    println!("{:#?}", round);
    
}
