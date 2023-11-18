#![feature(never_type)]
#![feature(const_trait_impl)]

use std::{error::Error, io};

use crossterm::{terminal::{enable_raw_mode, disable_raw_mode, LeaveAlternateScreen, EnterAlternateScreen}, execute, event::{Event, self, KeyCode}};
use delta_null_core_emulator_protocol::{Request, Response, EmulatorState};
use ratatui::{backend::CrosstermBackend, Terminal, Frame, widgets::{Paragraph, Table, Row, Cell, Block, Borders}, text::{Span, Line}, style::{Style, Color, Modifier}, layout::{Constraint, Layout, Direction, Rect}};
use zmq::{Context, Socket};

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
}

fn tui_setup() -> Result<Terminal<CrosstermBackend<io::Stdout>>, Box<dyn Error>> {
    enable_raw_mode()?;
    let mut stdout = io::stdout();
    execute!(stdout, EnterAlternateScreen)?;
    let backend = CrosstermBackend::new(stdout);
    
    Ok(Terminal::new(backend)?)
}

fn main() -> Result<(), Box<dyn Error>> {
    let backend = BackendSocket::connect("ipc:///tmp/delta-null-emulator.ipc")?;
    let mut terminal = tui_setup()?;

    let Response::Ok { mut state } = backend.send_request(&Request::GetState)? else {
        panic!("GetState error")
    };
    let mut changes = vec![];

    loop {
        terminal.draw(|f| draw(f, &state, &changes)).unwrap();

        if let Event::Key(key) = event::read()? {
            #[allow(clippy::single_match)]
            match key.code {
                KeyCode::Char('s') => {
                    // Single-step
                    let Response::Ok { state: new_state } = backend.send_request(&Request::ExecuteOneInstruction)? else {
                        panic!("back-end error")
                    };

                    // Find changes
                    changes.clear();
                    for (i, (old, new)) in state.gprs.iter().zip(new_state.gprs).enumerate() {
                        if *old != new {
                            changes.push(format!("r{i}"));
                        }
                    }
                    if state.ip != new_state.ip { changes.push("ip".to_string()); }
                    if state.rp != new_state.rp { changes.push("rp".to_string()); }
                    if state.sp != new_state.sp { changes.push("sp".to_string()); }
                    if state.ef != new_state.ef { changes.push("ef".to_string()); }

                    // Swap out state
                    state = new_state;
                }

                KeyCode::Char('x') => {
                    break;
                }
                _ => {}
            };
        }
    }

    disable_raw_mode()?;
    execute!(terminal.backend_mut(), LeaveAlternateScreen)?;
    terminal.show_cursor()?;
    Ok(())
}

fn draw(f: &mut Frame, state: &EmulatorState, changes: &[String]) {
    register_view(f, f.size(), state, changes);
}

fn register_view(f: &mut Frame, rect: Rect, state: &EmulatorState, changes: &[String]) {
    let layout = Layout::default()
        .direction(Direction::Horizontal)
        .constraints([Constraint::Length(9), Constraint::Length(9), Constraint::Min(0)])
        .split(rect);

    f.render_widget(
        gpr_table(state, changes)
            .block(Block::default().borders(Borders::ALL).title("GPRs")), layout[0]
    );
    f.render_widget(
        spr_table(state, changes)
            .block(Block::default().borders(Borders::ALL).title("SPRs")), layout[1]
    );
}

const REGISTER_NAME_STYLE: Style = Style::new().add_modifier(Modifier::DIM);
const REGISTER_VALUE_STYLE: Style = Style::new().add_modifier(Modifier::BOLD);
const REGISTER_CHANGED_VALUE_STYLE: Style = Style::new().add_modifier(Modifier::BOLD).fg(Color::Blue);

fn gpr_table(state: &EmulatorState, changes: &[String]) -> Table<'static> {
    let mut rows = vec![];
    for (i, value) in state.gprs.iter().enumerate() {
        let name = format!("r{i}");
        let value_style =
            if changes.contains(&name) {
                REGISTER_CHANGED_VALUE_STYLE
            } else {
                REGISTER_VALUE_STYLE
            };

        rows.push(Row::new(vec![
            Cell::from(name).style(REGISTER_NAME_STYLE),
            Cell::from(format!("{value:04x}")).style(value_style),
        ]))
    }

    let table = Table::new(rows)
        .widths(&[Constraint::Length(2), Constraint::Length(4)]); 

    table 
}

fn spr_table(state: &EmulatorState, changes: &[String]) -> Table<'static> {
    let sprs = vec![
        ("ip", state.ip, changes.contains(&"ip".to_string())),
        ("rp", state.rp, changes.contains(&"rp".to_string())),
        ("sp", state.sp, changes.contains(&"sp".to_string())),
        ("ef", state.ef, changes.contains(&"ef".to_string())),
    ];

    let mut rows = vec![];
    for (name, value, changed) in sprs {
        rows.push(Row::new(vec![
            Cell::from(name).style(REGISTER_NAME_STYLE),
            Cell::from(format!("{value:04x}"))
                .style(if changed { REGISTER_CHANGED_VALUE_STYLE } else { REGISTER_VALUE_STYLE }),
        ]))
    }

    let table = Table::new(rows)
        .widths(&[Constraint::Length(2), Constraint::Length(4)]); 

    table 
}

