use std::error::Error;

use delta_null_core_emulator_protocol::{Response, Request};
use zmq::{Context, Socket};

#[allow(unused)]
pub struct BackendSocket {
    ctx: Context,
    socket: Socket,
}

impl BackendSocket {
    pub fn connect(endpoint: &str) -> Result<Self, Box<dyn Error>> {
        let ctx = zmq::Context::new();

        let socket = ctx.socket(zmq::REQ)?;
        socket.connect(endpoint)?;
    
        Ok(BackendSocket::new(ctx, socket))
    }

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

    pub fn read_memory(&self, address: u16) -> Result<u16, Box<dyn Error>> {
        let resp = self.send_request(&Request::GetMainMemory { address })?;
        let Response::Data { data } = resp else { unreachable!() };
        Ok(data)
    }
}
