use config::prelude::*;

#[derive(Debug, Default, Config)]
struct Colour {
    r: u8,
    g: u8,
    b: u8
}

#[derive(Debug, Default, Config)]
struct Colours {
    background: Colour,
    foreground: Colour
}

#[derive(Debug, Default, Config)]
struct Friend {
    since: u64,
    id: u16,
    best: bool
}

#[derive(Debug, Default, Config)]
struct Example {
    name: String,
    id: u16,
    admin: bool,
    phone: Option<String>,
    colours: std::collections::HashMap<String, Colour>,
    friends: Vec<Friend>
}

fn main() {
    let config = Example::load(None);
    println!("{:#?}", config)
}