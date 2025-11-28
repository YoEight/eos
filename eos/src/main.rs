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
        if self.cursor >= self.input.len() {
            self.input.push(c);
            self.cursor = self.input.len() - 1;
        } else {
            self.input.insert(self.cursor, c);
            self.cursor += 1;
        }
    }

    fn move_right(&mut self) {
        if self.cursor >= self.input.len() {
            return;
        }

        self.cursor += 1;
    }

    fn move_left(&mut self) {
        self.cursor = self.cursor.saturating_sub(1);
    }

    fn delete_char(&mut self) {
        if self.cursor == 0 {
            return;
        }

        self.input.remove(self.cursor);

        if self.cursor >= self.input.len() - 1 {
            self.cursor = self.input.len() - 1;
        }
    }

    fn backspace(&mut self) {
        self.delete_char();
        self.cursor = self.cursor.saturating_sub(1);
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

                KeyCode::Left => {
                    app.move_left();
                }

                KeyCode::Right => {
                    app.move_right();
                }

                KeyCode::Backspace => {
                    app.backspace();
                }

                KeyCode::Delete => {
                    app.delete_char();
                }

                _ => {}
            }
        }
    }

    ratatui::restore();
    Ok(())
}
