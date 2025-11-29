use std::io;

use eos_core::evaluate;
use glyph::{Input, Options, in_memory_inputs};

fn main() -> io::Result<()> {
    let options = Options::default()
        .header(include_str!("../etc/header.txt"))
        .author("Yo Eight")
        .version("0.1.0")
        .date("November, 28th 2025");

    let mut inputs = in_memory_inputs(options)?;

    while let Some(input) = inputs.next_input()? {
        match input {
            Input::Exit => break,

            Input::String(expr) => {
                if matches!(expr.as_str(), "exit" | "quit") {
                    break;
                }

                match evaluate(expr.as_str()) {
                    Ok(res) => println!("> {}", res.pretty_print()),
                    Err(e) => println!("ERR: {e}"),
                }
            }

            _ => {}
        }
    }

    Ok(())
}
