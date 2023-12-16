use std::{process::{Command, Stdio, ExitStatusError}, io::Write, fs::read_dir, path::PathBuf, env::current_dir, error::Error};

use delta_null_core_emulator::{Core, memory::{SimpleMemory, Memory}};

pub fn compile_and_execute(code: &str) -> Result<u16, Box<dyn Error>> {
    let code = compile_to_machine_code(code)?;
    let mut core = Core::new(SimpleMemory::with_content(&code));
    
    // Trick to make the core halt when it returns from its entry point function
    core.rp = 0xFFFF;
    core.memory.write(0xFFFF, 0xFFFF).unwrap();

    core.step_until_halted()?;
    Ok(core.gprs[0])
}

pub fn compile_to_machine_code(code: &str) -> Result<Vec<u16>, ExitStatusError> {
    assemble(&compile_to_assembly(code)?)
}

pub fn compile_to_assembly(code: &str) -> Result<String, ExitStatusError> {
    run_just_command_with_stdin("compile", &["-"], code)
}

pub fn assemble(code: &str) -> Result<Vec<u16>, ExitStatusError> {
    Ok(
        run_just_command_with_stdin("assemble", &["-"], code)?
            .split_whitespace()
            .map(|w| u16::from_str_radix(w, 16).unwrap())
            .collect()
    )
}

pub fn run_just_command_with_stdin(task_name: &str, args: &[&str], stdin: &str) -> Result<String, ExitStatusError> {
    let mut cmd = Command::new("just")
        .current_dir(repo_root().unwrap())
        .arg(task_name)
        .args(args)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .unwrap();

    let mut cmd_stdin = cmd.stdin.take().unwrap();
    cmd_stdin.write_all(stdin.as_bytes()).unwrap();
    drop(cmd_stdin);

    let output = cmd.wait_with_output().unwrap();
    let stdout = String::from_utf8_lossy(&output.stdout).into_owned();
    let stderr = String::from_utf8_lossy(&output.stderr).into_owned();
    if !output.status.success() {
        panic!(
            "task {} exited with code {}\n\nstdout:\n{}\n\nstderr:\n{}",
            task_name, output.status.code().unwrap(), stdout, stderr
        )
    }
    Ok(stdout)
}

pub fn repo_root() -> Result<PathBuf, std::io::Error> {
    // Seek upwards until we see the `justfile`
    for ancestor in current_dir()?.ancestors() {
        for file in read_dir(ancestor)? {
            if file?.file_name().to_ascii_lowercase() == "justfile" {
                return Ok(ancestor.to_owned());
            }
        }
    }

    Err(std::io::Error::new(std::io::ErrorKind::NotFound, "no `justfile` found"))
}
