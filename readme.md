# config
*Configuration for thee, but not for me!*

An unobtrusive crate for loading config settings from various sources into Rust structs.

Designed to be used with minimal effort while providing all of the features a user will expect.
Any type can be supported by implementing `FromValue`, which most standard library types implement.
A derive macro, `Config`, is provided to implement `FromValue` for structs and enums.

No consideration for speed is made. If your program will be loading configurations or starting often, consider an alternative.

## Features
- Minimal dependencies, small crate footprint.
- Infallible config loading. Start initialised with sane defaults.
- Support most useful standard library types.
- Automatically load config settings from various sources such as config files in conventional paths, environment variables and CLI args.
- Easy to read and write configuration formats.

## Example

See the `examples` directory.

**Example Usage**
```rust
use config::prelude::*;

#[derive(Debug, Config)]
struct Login {
    pub username: String,
    pub password: Option<String>
}
#[derive(Debug, PartialEq, Eq, Hash, Config)]
enum Protocol {
    Https,
    Http
}
#[derive(Debug, Config)]
struct Web {
    pub address: IpAddr,
    pub data_root: PathBuf,
    pub logins: Vec<Login>,
    pub protocols: HashMap<Protocol, u16>,
    pub banned: Vec<IpAddr>
}
impl Default for Web {/* Omitted */}

fn main() {
    let mut config = Web::load("web", None);
    println!("{:#?}", config)
}
```
**Example Config**
```
address: 127.0.0.1
data_root: /run/user/1000/srv
logins: [
    {
        username: admin
        password: Password1
    }
]
protocols: { http: 8000 }

# Clear ban list
banned: none
banned: [192.168.1.35, 192.168.1.63]
```

## Alternatives
- [clap](https://crates.io/crates/clap), a command line argument parser that can do it all
- [Figment](https://crates.io/crates/figment), has a similar premise as this crate but using well-known formats

## License
This project is licensed under the [MIT License](http://opensource.org/licenses/MIT).