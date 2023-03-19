use std::{slice, collections::LinkedList};


fn julian_dayz(y: i32, m: i32, d: i32) -> f64 {
    let y = 
        if y < 0 {
            y + 1
        } else {
            y
        };
    let (y, m, d) = (f64::from(y), f64::from(m), f64::from(d));
    let (jy, jm) =
        if m > 2.0 {
            (y, m + 1.0)
        } else {
            (y - 1.0, m + 13.0)
        };

    let jd0 = (365.25*jy).floor()+(30.6001*jm).floor()+d+1720995.0;

    let jd1 =
    if d+31.0*(m+12.0*y) >= (15.0+31.0*(10.0+12.0*1582.0)) {
        let ja = (0.01*jy).floor();
        jd0 + 2.0-ja+(0.25*ja).floor()
    }
    else {
        jd0
    };
        
    jd1 - 0.5
}

const EPOCH_JDAYZ: f64 = 584282.5;
const WHEEL_TURN_RADIX: i32 = 360;
const RADIX: i32 = 20;

fn maya_digis(julian_day: i32) -> Vec<i32> {

    fn rec(value: i32, digis: &mut LinkedList<i32>) -> Vec<i32> {

        match (value / RADIX, value % RADIX) {
            (q, r) if {q == 0} => {
                digis.push_front(r);
                digis
                    .clone()
                    .into_iter()
                    .collect::<Vec<i32>>()
            },
            (q, r) => {
                digis.push_front(r);
                rec(q, digis)
            }
        }

    }

    rec(
        julian_day,
        &mut LinkedList::new()
    )

}

fn date(y: i32, m: i32, d: i32) -> Vec<i32> {
    let date = Vec::with_capacity(5);
    let julia = julian_dayz(y, m, d);
    let days_since_construct =
        (julia - EPOCH_JDAYZ) as i32;

    let (turns_of_the_wheel, days_remaining) = (
        days_since_construct / WHEEL_TURN_RADIX,
        days_since_construct % WHEEL_TURN_RADIX
    );

    let splices: Vec<Vec<i32>> =
        [turns_of_the_wheel, days_remaining]
            .into_iter()
            .map(maya_digis)
            .collect();
    let (black, white) = (&splices[0], &splices[1]);
    
    
}

fn print(date: &[i32]) {
    let formatted: Vec<String> =
        date.into_iter().map(|digit| format!("${digit:#02x}")).collect();
    let joined = formatted.join(", ");
    println!("${joined}");
}   

fn main() {
    println!("Hello, world!");
}
