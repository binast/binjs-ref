use std::io::Read;
use std::path::PathBuf;
use std::process::Command;

extern crate env_logger;
#[macro_use]
extern crate log;

const DEBUG: bool = true;

fn get_path(components: &[&str]) -> PathBuf {
    let mut root = std::env::temp_dir();
    for component in components {
        root.push(component)
    }
    root
}

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

fn perform_test(prefix: &str, source: &str) {
    let path_in_source = std::path::Path::new(source).to_path_buf();
    let path_tmp_source = get_path(&[prefix, "sample.js"]);
    let path_out_encoded_1 = get_path(&[prefix, "encoded_1"]);
    let path_in_encoded_1 = get_path(&[prefix, "encoded_1", "sample.binjs"]);
    let path_out_encoded_2 = get_path(&[prefix, "encoded_2"]);
    let path_in_encoded_2 = get_path(&[prefix, "encoded_2", "sample.binjs"]);
    let path_decoded = get_path(&[prefix, "decoded.js"]);

    let mut dir_builder = std::fs::DirBuilder::new();
    dir_builder.recursive(true)
        .create(get_path(&[prefix]))
        .unwrap();

    // Copy source file to `path_tmp_source`, to normalize file name.
    std::fs::copy(&path_in_source, &path_tmp_source)
        .unwrap();

    debug!(target: "test", "Encoding to encoded_1/sample.binjs");
    let mut command_1 = Command::new("target/debug/binjs_encode");
    command_1.arg("--in")
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
    std::fs::copy(&path_decoded, &path_tmp_source)
        .unwrap();

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
    let encoded_1 = std::fs::File::open(&path_in_encoded_1)
        .unwrap_or_else(|e| panic!("Could not open file {:?}: {:?}", path_in_encoded_1, e));
    let encoded_2 = std::fs::File::open(&path_in_encoded_2)
        .unwrap_or_else(|e| panic!("Could not open file {:?}: {:?}", path_in_encoded_2, e));
    assert_eq!(encoded_1.metadata().unwrap().len(), encoded_2.metadata().unwrap().len(), "Distinct file lengths in test {}", prefix);
    for (i, (b1, b2)) in encoded_1.bytes().zip(encoded_2.bytes()).enumerate() {
        assert_eq!(b1.unwrap(), b2.unwrap(), "Distinct byte {} in test {}", i, prefix);
    }
}

#[test]
fn test_external_baseline() {
    env_logger::init();

    println!("Testing with baseline dictionary");
    perform_test("baseline", "tests/data/frameworks/angular.1.6.5.min.js");

}