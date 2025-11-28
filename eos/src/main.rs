use ratatui::{
    Frame,
    crossterm::event::{self, Event, KeyCode},
};

#[derive(Default)]
struct App {
    input: String,
    cursor: usize,
}

impl App {
    fn insert_char(&mut self, c: char) {
        if self.cursor == self.input.len() - 1 {
            self.input.push(c);
        } else {
            self.input.insert(self.cursor, c);
        }

        self.cursor += 1;
    }

    fn render(&self, frame: &mut Frame) {}
}

fn main() -> color_eyre::Result<()> {
    color_eyre::install()?;
    let mut terminal = ratatui::init();
    let mut app = App::default();

    loop {
        terminal.draw(|f| app.render(f))?;
        if let Event::Key(key) = event::read()? {
            match key.code {
                KeyCode::Enter => {
                    if app.input.as_str() == ":quit" {
                        break;
                    }
                }

                KeyCode::Char(c) => {
                    app.insert_char(c);
                }

                _ => {}
            }
        }
    }

    ratatui::restore();
    Ok(())
}
