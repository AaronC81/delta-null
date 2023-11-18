#![feature(never_type)]

use std::error::Error;

use delta_null_core_emulator_protocol::{Request, Response};
use zmq::{Context, Socket};

fn client() -> Result<(), Box<dyn Error>> {
    let ctx = zmq::Context::new();

    let socket = ctx.socket(zmq::REQ)?;
    socket.connect("ipc:///tmp/delta-null-emulator.ipc")?;

    let backend = BackendSocket::new(ctx, socket);

    println!("{:?}", backend.send_request(&Request::SetMainMemory { address: 0, data: 0xFFFF })?);
    println!("{:?}", backend.send_request(&Request::ExecuteOneInstruction)?);

    Ok(())
}

pub struct BackendSocket {
    ctx: Context,
    socket: Socket,
}

impl BackendSocket {
    pub fn new(ctx: Context, socket: Socket) -> Self {
        Self { ctx, socket }
    }

    pub fn send_request(&self, request: &Request) -> Result<Response, Box<dyn Error>> {
        // Send
        let json = serde_json::to_string(request)?;
        self.socket.send(&json, 0)?;

        // Receive
        let buffer = self.socket.recv_bytes(0)?;
        let response: Response = serde_json::from_slice(&buffer)?;

        Ok(response)
    }
}

fn main() {
    client().unwrap();
}
