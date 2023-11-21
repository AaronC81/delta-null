use std::error::Error;

use delta_null_core_emulator_protocol::{Response, EmulatorState, Request};

use crate::socket::BackendSocket;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExecutionState {
    Break,
    Running,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Menu {
    Normal,
    Memory,
}

pub struct ApplicationState {
    pub emulator: EmulatorState,
    pub changes: Vec<String>,
    pub socket: BackendSocket,

    pub execution_state: ExecutionState,
    pub menu: Menu,
}

impl ApplicationState {
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
