extern crate reqwest;
#[macro_use]
extern crate clap;
extern crate serde;
#[macro_use]
extern crate serde_derive;
extern crate serde_json;

use std::str::FromStr;
use std::process::exit;
use std::time::{Duration, SystemTime};

fn main() {

    let matches = clap_app!(lamba_hit =>
                            (version: "0.1.0")
                            (author: "David Barnett")
                            (about: "Tests a url by spamming it")
                            (@arg ASYNC: --async "Enables asynchornise test")
                            (@arg TIMES: -t --times +takes_value "Number of times to test")
                            (@arg URL: +required "Url to test")
                            ).get_matches();
    
    let times = match matches.value_of("TIMES").map_or(Ok(100), |s| u32::from_str(s))  {
        Ok(n) => n,
        Err(_) => {
            println!("ERROR: The times option must be a positive number");
            exit(1);
        },
    };

    let async = matches.is_present("ASYNC");
    let url = matches.value_of("URL").unwrap();

    let now = SystemTime::now();
    if async {
        unimplemented!();
    } else {
        sync_get(url, times);
    }

    print!("Total,");
    print_delta(now.elapsed());
}

fn sync_get(url: &str, times: u32) {

    for i in 0..times {
        let now = SystemTime::now();

        let mut resp = match reqwest::get(url) {
            Ok(r) => r,
            Err(e) => {
                println!("Something went wrong with requesting {}\n{:?}", url, e);
                continue;
            },
        };

        if !resp.status().is_success() {
            println!("Status: {:?}", resp.status());
        }

        let parsed = match resp.json::<Body>() {
            Ok(b) => b,
            Err(e) => {
                println!("Something went wrong with parsing response\n{:?}",  e);
                continue;
            },
        };

        print!("{},{},", i, parsed.duration_seconds * 1000.0);
        print_delta(now.elapsed()); 
        println!();
    }

}

fn to_msec(time: Duration) -> f64 {
    (time.as_secs() as f64) * 1000.0 + (time.subsec_nanos() as f64) / 1_000_000.0
}

fn print_delta(delta: Result<Duration, std::time::SystemTimeError> ) {
    if let Ok(elapsed) = delta {
        print!("{}", to_msec(elapsed));
    } else if let Err(err) = delta {
        println!("Something went wrong, {:?}", err);
    }
}

#[derive(Serialize, Deserialize, Debug)]
struct Body {
    #[serde(rename="durationSeconds")]
    duration_seconds: f64,
    max: u32,
    loops: u32,
}
