use std::{env, process::Command};

#[test]
fn integration() {
    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    let exe = env!("CARGO_BIN_EXE_jgwt");

    assert!(Command::new("tclsh")
        .current_dir(manifest_dir)
        .args(&["tests/test_helper.tcl", "--bin", &exe])
        .spawn()
        .unwrap()
        .wait()
        .unwrap()
        .success());
}
