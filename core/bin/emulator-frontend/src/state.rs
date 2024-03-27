use std::{error::Error, fmt::Display};

use delta_null_core_emulator_protocol::{Response, EmulatorState, Request};

use crate::socket::BackendSocket;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExecutionState {
    Break,
    Running,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Menu {
    Normal,
    Memory,
    Command(String),
    Error(String),
}

pub struct ApplicationState {
    pub emulator: EmulatorState,
    pub changes: Vec<String>,
    pub socket: BackendSocket,

    pub execution_state: ExecutionState,
    pub menu: Menu,
}

impl ApplicationState {
    pub fn execute_command(&mut self, command: &str) -> Result<(), Box<dyn Error>> {
        let [command, args@..] = &shell_words::split(command)?[..] else {
            return Ok(()) // empty commands are fine, but don't do anything
        };

        match command.as_str() {
            "core.step" => self.emulator_step(),
            "core.gpr.set" => {
                if args.len() != 2 {
                    return Err(Box::new(CommandError::new("usage: core.reg.set <reg> <value>".to_string())));
                }

                let reg_index =
                    if let Some(index) = args[0].strip_prefix('r') {
                        index.parse()
                    } else {
                        args[0].parse()
                    }?;
                let data =
                    if let Some(hex) = args[1].strip_prefix("0x") {
                        u16::from_str_radix(hex, 16)
                    } else {
                        args[1].parse()
                    }?;

                self.socket.send_request(&Request::SetGPR { index: reg_index, data })?;

                Ok(())
            }

            "mem.clear" => {
                for i in 0..0xFFFF {
                    self.socket.send_request(&Request::SetMainMemory { address: i, data: 0 })?;
                }
                Ok(())
            },
            "mem.load.ascii" => {
                if args.len() != 1 {
                    return Err(Box::new(CommandError::new("usage: mem.load.ascii <file>".to_string())));
                }

                // Load bytes from file
                let mut contents = std::fs::read_to_string(&args[0])?;
                contents.retain(|c| !c.is_whitespace());
                let words = contents.chars()
                    .collect::<Vec<_>>()
                    .chunks(4)
                    .map(|w| u16::from_str_radix(&w.iter().collect::<String>(), 16))
                    .collect::<Result<Vec<_>, _>>()?;

                for (i, word) in words.iter().enumerate() {
                    self.socket.send_request(&Request::SetMainMemory {
                        address: i as u16,
                        data: *word,
                    }).unwrap();
                }

                Ok(())
            }
            
            _ => Err(Box::new(CommandError::new(format!("unknown command: {command}")))),
        }
    }

    pub fn execute_command_from_menu(&mut self, command: &str) -> Result<(), Box<dyn Error>> {
        let result = self.execute_command(command);
        self.menu = Menu::Normal;
        result
    }

    pub fn emulator_step(&mut self) -> Result<(), Box<dyn Error>> {
        // Single-step
        let Response::UpdatedState { state: new_state } = self.socket.send_request(&Request::ExecuteOneInstruction)? else {
            panic!("back-end error")
        };

        // Find changes
        self.changes.clear();
        for (i, (old, new)) in self.emulator.gprs.iter().zip(new_state.gprs).enumerate() {
            if *old != new {
                self.changes.push(format!("r{i}"));
            }
        }
        if self.emulator.ip != new_state.ip { self.changes.push("ip".to_string()); }
        if self.emulator.rp != new_state.rp { self.changes.push("rp".to_string()); }
        if self.emulator.sp != new_state.sp { self.changes.push("sp".to_string()); }
        if self.emulator.ef != new_state.ef { self.changes.push("ef".to_string()); }

        // Swap out state
        self.emulator = new_state;

        Ok(())
    }
}

#[derive(Clone, Debug)]
pub struct CommandError(String);
impl CommandError {
    pub fn new(error: String) -> Self {
        Self(error)
    }
}
impl Display for CommandError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}
impl Error for CommandError {}
