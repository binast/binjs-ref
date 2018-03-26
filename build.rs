use std::io::ErrorKind;
use std::process::*;

const PATH_PACKAGES : &'static str = "package.json";

fn main() {
    // Install Node packages if necessary.
    println!("cargo:rerun-if-changed={}", PATH_PACKAGES);

    // First try with `yarn`, if available.
    println!("Bootstrap: Attempting to install dependencies with yarn.");
    match Command::new("yarn")
        .arg("install")
        .spawn()
    {
        Ok(mut process) => {
            process.wait()
                .expect("Bootstrap: Could not complete `yarn install`");
        }
        Err(err) => {
            if let ErrorKind::NotFound = err.kind() {
                println!("Bootstrap: Yarn not available, attempting to install dependencies with npm.");
                // Otherwise, try with `npm`.
                Command::new("npm")
                    .arg("install")
                    .spawn()
                    .expect("Bootstrap: Could not launch either `yarn install` or `npm install`")
                    .wait()
                    .expect("Bootstrap: Could not complete `npm` install");
            } else {
                panic!("Bootstrap: Unexpected error during yarn/npm install: {:?}", err);
            }
        }
    }
}