extern crate which;

use std::process::Command;
use which::which;

const PATH_PACKAGES: &'static str = "package.json";

fn main() {
    // Install Node packages if necessary.
    println!("cargo:rerun-if-changed={}", PATH_PACKAGES);

    let tool = which("yarn")
        .or_else(|_| which("npm"))
        .expect("Could not find neither `yarn` nor `npm`");

    let status = Command::new(tool).arg("install").status().unwrap();

    assert!(status.success());
}
