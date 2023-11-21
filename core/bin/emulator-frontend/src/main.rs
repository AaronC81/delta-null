#![feature(never_type)]
#![feature(const_trait_impl)]

use std::{error::Error, io, time::Duration};

use crossterm::{terminal::{enable_raw_mode, disable_raw_mode, LeaveAlternateScreen, EnterAlternateScreen}, execute, event::{Event, self, KeyCode, KeyEvent}};
use delta_null_core_emulator_protocol::{Request, Response, EmulatorState};
use delta_null_core_instructions::{Instruction, Encodable, ToAssembly};
use ratatui::{backend::CrosstermBackend, Terminal, Frame, widgets::{Table, Row, Cell, Block, Borders, Paragraph}, style::{Style, Color, Modifier}, layout::{Constraint, Layout, Direction, Rect}, text::{Line, Span, Text}};

mod socket;
use socket::BackendSocket;

mod state;
use state::{ApplicationState, ExecutionState};

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

    // TEMP - load SOS-blink
    let code = "4300 1219 1A00 1322 1B00 6212 6212 6212 6213 6213 6213 6212 6212 6212 142F 1C00 6214 6214 6214 6214 6214 6214 1701 1F00 21C7 2195 142F 1C00 4000 6214 4000 6214 21D5 6218 2195 142F 1C00 4000 6214 6214 6214 4000 6214 6214 6214 21D5 6218 16FF 1EFF 1733 1F00 4826 5016 5000 6307 6218";
    for (i, word) in code.split_ascii_whitespace().enumerate() {
        backend.send_request(&Request::SetMainMemory {
            address: i as u16,
            data: u16::from_str_radix(word, 16).unwrap()
        }).unwrap();
    }

    let Response::UpdatedState { state: emulator } = backend.send_request(&Request::GetState)? else {
        panic!("GetState error")
    };
    
    let mut state = ApplicationState {
        emulator,
        changes: vec![],
        socket: backend,

        execution_state: ExecutionState::Break,
    };

    loop {
        terminal.draw(|f| { draw(f, &state) }).unwrap();

        if let Event::Key(key) = event::read()? {
            match key.code {
                KeyCode::Char('s') => state.emulator_step()?,

                KeyCode::Char('r') => {
                    state.execution_state = ExecutionState::Running;

                    loop {
                        if event::poll(Duration::from_secs(0))? {
                            if let Event::Key(KeyEvent { code: KeyCode::Char('p'), .. }) = event::read()? {
                                state.execution_state = ExecutionState::Break;
                                break;
                            }
                        }

                        state.emulator_step()?;

                        terminal.draw(|f| draw(f, &state)).unwrap();
                    }
                }

                KeyCode::Char('q') => {
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

fn draw(f: &mut Frame, state: &ApplicationState) {
    draw_top_level(f, f.size(), state)
}

fn draw_top_level(f: &mut Frame, rect: Rect, state: &ApplicationState) {
    let layout = Layout::default()
        .direction(Direction::Vertical)
        .constraints([Constraint::Max(100), Constraint::Min(1)])
        .split(rect);

    draw_core_view(f, layout[0], state);
    draw_controls(f, layout[1], state);
}

fn draw_core_view(f: &mut Frame, rect: Rect, state: &ApplicationState) {
    let layout = Layout::default()
        .direction(Direction::Horizontal)
        .constraints([Constraint::Percentage(50), Constraint::Length(50), Constraint::Min(0)])
        .split(rect);

    draw_instruction_view(f, layout[0], state);
    draw_register_view(f, layout[1], state);
}

fn draw_instruction_view(f: &mut Frame, rect: Rect, state: &ApplicationState) {
    // Fetch instructions around this one
    let mut instruction_data = vec![];
    for i in state.emulator.ip..(state.emulator.ip.saturating_add(rect.height)) {
        instruction_data.push((i, state.socket.read_memory(i).ok()))
    }

    let mut rows = vec![];
    for (addr, encoded_value) in instruction_data {
        // Try to decode
        let disassembled = encoded_value
            .map(|bits| Instruction::decode(bits))
            .flatten()
            .map(|ins| ins.to_assembly());

        rows.push(Row::new(vec![
            Cell::from(if state.emulator.ip == addr { ">" } else { " " }),
            Cell::from(format!("{addr:04x}")).style(REGISTER_NAME_STYLE),
            Cell::from(match encoded_value {
                Some(encoded_value) => format!("{encoded_value:04x}"),
                None => "????".to_string(),
            }).style(REGISTER_VALUE_STYLE),
            Cell::from(disassembled.unwrap_or("????".to_string())),
        ]))
    }

    let table = Table::new(rows)
        .widths(&[Constraint::Length(1), Constraint::Length(4), Constraint::Length(4), Constraint::Min(12)]);

    f.render_widget(
        table.block(Block::default().borders(Borders::ALL).title("Instructions")),
        rect
    );
}

fn draw_register_view(f: &mut Frame, rect: Rect, state: &ApplicationState) {
    let layout = Layout::default()
        .direction(Direction::Horizontal)
        .constraints([Constraint::Length(9), Constraint::Length(9), Constraint::Min(0)])
        .split(rect);

    f.render_widget(
        gpr_table(state)
            .block(Block::default().borders(Borders::ALL).title("GPRs")), layout[0]
    );
    f.render_widget(
        spr_table(state)
            .block(Block::default().borders(Borders::ALL).title("SPRs")), layout[1]
    );
}

const REGISTER_NAME_STYLE: Style = Style::new().add_modifier(Modifier::DIM);
const REGISTER_VALUE_STYLE: Style = Style::new().add_modifier(Modifier::BOLD);
const REGISTER_CHANGED_VALUE_STYLE: Style = Style::new().add_modifier(Modifier::BOLD).fg(Color::Blue);

fn gpr_table(state: &ApplicationState) -> Table<'static> {
    let mut rows = vec![];
    for (i, value) in state.emulator.gprs.iter().enumerate() {
        let name = format!("r{i}");
        let value_style =
            if state.changes.contains(&name) {
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

fn spr_table(state: &ApplicationState) -> Table<'static> {
    let sprs = vec![
        ("ip", state.emulator.ip, state.changes.contains(&"ip".to_string())),
        ("rp", state.emulator.rp, state.changes.contains(&"rp".to_string())),
        ("sp", state.emulator.sp, state.changes.contains(&"sp".to_string())),
        ("ef", state.emulator.ef, state.changes.contains(&"ef".to_string())),
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

fn draw_controls(f: &mut Frame, rect: Rect, state: &ApplicationState) {
    let mut spans = vec![
        Span::styled(match state.execution_state {
            ExecutionState::Break =>   " BREAK ",
            ExecutionState::Running => "  RUN  ",
        }, Style::default().add_modifier(Modifier::REVERSED)),
        Span::from("   "),
    ];

    let available_commands = match state.execution_state {
        ExecutionState::Break => vec![
            "[S]tep",
            "[R]un",
            "[Q]uit",
        ],
        ExecutionState::Running => vec![
            "[P]ause",
        ],
    };
    for command in available_commands {
        spans.push(Span::from(format!("{command}   ")));
    }

    let para = Paragraph::new(Text::from(Line::from(spans)));

    f.render_widget(para, rect);
}
