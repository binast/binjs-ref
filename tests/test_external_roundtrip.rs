extern crate env_logger;
#[macro_use]
extern crate log;
extern crate tempdir;

use tempdir::TempDir;

use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

/// A macro used to simplify debugging.
const DEBUG: bool = true;

/// Utility: run a process, optionally displaying its stdout/stderr.
fn run(mut command: std::process::Command) -> std::io::Result<()> {
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

/// Perform a roundtrip:
///
/// - start from the file at `source`;
/// - encode it to a binary `encoded_1`;
/// - decode the binary;
/// - re-encode the decoded binary `encoded_2`;
/// - compare `encoded_1` and `encoded_2`.
///
/// If `dictionary.is_some()`, use an external dictionary for encoding/decoding.
///
/// Args:
/// - `root` is the root directory for these tests;
/// - `prefix` is the name of the test, used both for logging and to create a subdirectory;
/// - `source` is the path of the file to encode;
/// - `dictionary` is the optional path towards an external dictionary.
fn perform_roundtrip(root: &Path, prefix: &str, source: &str, dictionary: Option<&Path>) {
    let root = root.join(prefix);
    let path_in_source = std::path::Path::new(source);
    let path_tmp_source = root.join("sample.js");
    let path_out_encoded_1 = root.join("encoded_1");
    let path_in_encoded_1 = root.join("encoded_1").join("sample.binjs");
    let path_out_encoded_2 = root.join("encoded_2");
    let path_in_encoded_2 = root.join("encoded_2").join("sample.binjs");
    let path_decoded = root.join("decoded.js");

    let mut dir_builder = fs::DirBuilder::new();
    dir_builder.recursive(true).create(root).unwrap();

    // Copy source file to `path_tmp_source`, to normalize file name.
    fs::copy(&path_in_source, &path_tmp_source).unwrap();

    debug!(target: "test", "Encoding to encoded_1/sample.binjs");
    let mut command_1 = Command::new("target/debug/binjs_encode");
    command_1
        .arg("--in")
        .arg(&path_tmp_source)
        .arg("--out")
        .arg(&path_out_encoded_1)
        .args(&["advanced", "entropy"]);
    if let Some(path) = dictionary {
        command_1.arg("--dictionary").arg(&path);
    }
    run(command_1).unwrap();

    debug!(target: "test", "Decoding to decoded.js");
    let mut command_2 = Command::new("target/debug/binjs_decode");
    command_2
        .arg(&path_in_encoded_1)
        .arg(&path_decoded)
        .args(&["advanced", "entropy"]);
    if let Some(path) = dictionary {
        command_2.arg("--dictionary").arg(&path);
    }
    run(command_2).unwrap();

    // Copy again to `path_tmp_source`, to normalize file name.
    fs::copy(&path_decoded, &path_tmp_source).unwrap();

    debug!(target: "test", "Re-encoding to encoded_2/sample.binjs");
    let mut command_3 = Command::new("target/debug/binjs_encode");
    command_3
        .arg("--in")
        .arg(&path_tmp_source)
        .arg("--out")
        .arg(&path_out_encoded_2)
        .args(&["advanced", "entropy"]);
    if let Some(path) = dictionary {
        command_3.arg("--dictionary").arg(&path);
    }
    run(command_3).unwrap();

    debug!(target: "test", "Comparing the two decoded files");
    let encoded_1 = fs::read(&path_in_encoded_1)
        .unwrap_or_else(|e| panic!("Could not open file {:?}: {:?}", path_in_encoded_1, e));
    let encoded_2 = fs::read(&path_in_encoded_2)
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

/// Generate a dictionary from a source.
///
/// Return the path of the file containing the generated dictionary.
fn init_dictionary(root: &Path, source: &str) -> PathBuf {
    let root = root.join("dictionary");
    let mut command = Command::new("target/debug/binjs_generate_prediction_tables");
    command.arg("--in").arg(source).arg("--out").arg(&root);
    run(command).unwrap();
    return root.join("dict.entropy");
}

/// Ensure that all external binaries used by this test have been built.
fn self_init() {
    for name in &[
        "binjs_encode",
        "binjs_decode",
        "binjs_generate_prediction_tables",
    ] {
        let mut command = Command::new("cargo");
        command.args(&["build", "--bin", name]);
        run(command).unwrap_or_else(|e| panic!("Could not build {}: {:?}", name, e));
    }
}

#[test]
fn test_external_roundtrip() {
    env_logger::init();

    self_init();

    let tmp_dir = TempDir::new("test_external_roundtrip").expect("Could not create test directory");

    println!("Testing with baseline dictionary");
    perform_roundtrip(
        tmp_dir.path(),
        "baseline",
        "tests/data/frameworks/angular.1.6.5.min.js",
        None,
    );

    println!("Testing with generated dictionary");
    // We generate a dictionary, and apply it to a different source file.
    let dictionary_path = init_dictionary(
        tmp_dir.path(),
        "tests/data/frameworks/backbone.1.3.3.min.js",
    );
    perform_roundtrip(
        tmp_dir.path(),
        "external",
        "tests/data/frameworks/bootstrap.4.1.0.min.js",
        Some(&dictionary_path),
    );
}
