extern crate env_logger;
#[macro_use]
extern crate log;
extern crate tempdir;

use tempdir::TempDir;

use std::path::Path;
use std::process::Command;

/// A macro used to simplify debugging.
const DEBUG: bool = true;

fn run(mut command: std::process::Command) -> Result<(), std::io::Error> {
    let output = command.output()?;
    if DEBUG || !output.status.success() {
        debug!(target: "test", "Command: {command:?}\n** out:{out}\n** err:{err}",
            command = command,
            out =  String::from_utf8(output.stdout).unwrap_or_else(|_| "<cannot decode output>".to_string()),
            err =  String::from_utf8(output.stderr).unwrap_or_else(|_| "<cannot decode output>".to_string())
        );
    }
    assert!(output.status.success());
    Ok(())
}

fn perform_test(root: &Path, prefix: &str, source: &str) {
    let root = root.join(prefix);
    let path_in_source = std::path::Path::new(source);
    let path_tmp_source = root.join("sample.js");
    let path_out_encoded_1 = root.join("encoded_1");
    let path_in_encoded_1 = root.join("encoded_1").join("sample.binjs");
    let path_out_encoded_2 = root.join("encoded_2");
    let path_in_encoded_2 = root.join("encoded_2").join("sample.binjs");
    let path_decoded = root.join("decoded.js");

    let mut dir_builder = std::fs::DirBuilder::new();
    dir_builder.recursive(true).create(root).unwrap();

    // Copy source file to `path_tmp_source`, to normalize file name.
    std::fs::copy(&path_in_source, &path_tmp_source).unwrap();

    debug!(target: "test", "Encoding to encoded_1/sample.binjs");
    let mut command_1 = Command::new("target/debug/binjs_encode");
    command_1
        .arg("--in")
        .arg(&path_tmp_source)
        .arg("--out")
        .arg(&path_out_encoded_1)
        .args(&["advanced", "entropy"]);
    run(command_1).unwrap();

    debug!(target: "test", "Decoding to decoded.js");
    let mut command_2 = Command::new("target/debug/binjs_decode");
    command_2
        .arg(&path_in_encoded_1)
        .arg(&path_decoded)
        .args(&["advanced", "entropy"]);
    run(command_2).unwrap();

    // Copy again to `path_tmp_source`, to normalize file name.
    std::fs::copy(&path_decoded, &path_tmp_source).unwrap();

    debug!(target: "test", "Re-encoding to encoded_2/sample.binjs");
    let mut command_3 = Command::new("target/debug/binjs_encode");
    command_3
        .arg("--in")
        .arg(&path_tmp_source)
        .arg("--out")
        .arg(&path_out_encoded_2)
        .args(&["advanced", "entropy"]);
    run(command_3).unwrap();

    debug!(target: "test", "Comparing the two decoded files");
    let encoded_1 = std::fs::read(&path_in_encoded_1)
        .unwrap_or_else(|e| panic!("Could not open file {:?}: {:?}", path_in_encoded_1, e));
    let encoded_2 = std::fs::read(&path_in_encoded_2)
        .unwrap_or_else(|e| panic!("Could not open file {:?}: {:?}", path_in_encoded_2, e));
    // First check for distinct lengths, it's a simple error to spot.
    assert_eq!(
        encoded_1.len(),
        encoded_2.len(),
        "Distinct file lengths in test {}",
        prefix
    );
    // Now check for distinct contents.
    assert_eq!(
        encoded_1, encoded_2,
        "Distinct file contents in test {}",
        prefix
    );
}

/// Ensure that `binjs_encode` and `binjs_decode` are built.
fn self_init() {
    for name in &["binjs_encode", "binjs_decode"] {
        let mut command = Command::new("cargo");
        command.args(&["build", "--bin", name]);
        run(command).unwrap_or_else(|e| panic!("Could not build {}: {:?}", name, e));
    }
}

#[test]
fn test_external_baseline() {
    env_logger::init();

    self_init();

    let tmp_dir = TempDir::new("test_external_roundtrip").expect("Could not create test directory");

    println!("Testing with baseline dictionary");
    perform_test(
        tmp_dir.path(),
        "baseline",
        "tests/data/frameworks/angular.1.6.5.min.js",
    );
}
