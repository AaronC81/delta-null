#![feature(never_type)]

use std::error::Error;

use delta_null_core_emulator::{Core, memory::SimpleMemory};
use delta_null_core_emulator_protocol::{Request, Response, EmulatorState};
use zmq::{Message, Context, Socket};

fn server() -> Result<!, Box<dyn Error>> {
    let ctx = zmq::Context::new();
    let socket = ctx.socket(zmq::REP)?;
    socket.bind("ipc:///tmp/delta-null-emulator.ipc")?;
    let frontend = FrontendSocket::new(ctx, socket);

    let mut emulator = Core::new(SimpleMemory::new());

    loop {
        let mut result = Ok(());
        match frontend.receive_request()? {
            Request::SetMainMemory { address, data }
                => emulator.memory.data[address as usize] = data,
            Request::SetGPR { index, data }
                => emulator.gprs[index as usize] = data,
            Request::ExecuteOneInstruction
                => result = emulator.step(),
            Request::GetState
                => (),
            Request::GetMainMemory { address } => {
                // This doesn't send an updated state, so handle specially
                if let Some(data) = emulator.memory.data.get(address as usize) {
                    frontend.send_response(&Response::Data { data: *data })?;
                } else {
                    frontend.send_response(&Response::Err { description: "out-of-range".to_string() })?;
                }
                continue;
            }
        }

        let response = match result {
            Ok(_) => Response::UpdatedState {
                state: EmulatorState {
                    gprs: emulator.gprs,
                    ip: emulator.ip,
                    rp: emulator.rp,
                    sp: emulator.sp,
                    ef: emulator.ef,
                }
            },
            Err(e) => Response::Err { description: e.to_string(), }
        };

        frontend.send_response(&response)?;
    }
}

pub struct FrontendSocket {
    ctx: Context,
    socket: Socket,
}

impl FrontendSocket {
    pub fn new(ctx: Context, socket: Socket) -> Self {
        Self { ctx, socket }
    }

    pub fn receive_request(&self) -> Result<Request, Box<dyn Error>> {
        let buffer = self.socket.recv_bytes(0)?;
        let request: Request = serde_json::from_slice(&buffer)?;
        Ok(request)
    }

    pub fn send_response(&self, response: &Response) -> Result<(), Box<dyn Error>> {
        let json = serde_json::to_string(response)?;
        self.socket.send(&json, 0)?;
        Ok(())
    }
}

fn main() {
    server().unwrap();
}
