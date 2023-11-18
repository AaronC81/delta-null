use serde::{Serialize, Deserialize};

/// Describes the current state of the core emulator.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EmulatorState {
    pub gprs: [u16; 8],
    pub ip: u16,
    pub rp: u16,
    pub sp: u16,
    pub ef: u16,
}

/// Possible requests that could be sent from front-end to back-end.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "operation", content = "parameters")]
pub enum Request {
    SetMainMemory {
        address: u16,
        data: u16,
    },
    SetGPR {
        index: u8,
        data: u16,
    },
    ExecuteOneInstruction,
}

/// Possible responses that could be sent from back-end to front-end, after handling a [Request].
/// Any successful operation provides an updated [EmulatorState].
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "status", content = "content")]
pub enum Response {
    Ok {
        state: EmulatorState,
    },
    Err {
        description: String,
    },
}
