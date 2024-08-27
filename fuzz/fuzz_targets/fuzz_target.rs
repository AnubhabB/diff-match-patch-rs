#![no_main]

use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: (&str, &str)| {
    // fuzzed code goes here
    diff_match_patch_rs::fuzz::fuzz(data.0, data.1).unwrap();
});
