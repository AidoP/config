# config
*Configuration for thee, but not for me!*

An unobtrusive crate for loading config settings from various sources into Rust structs.

## Features
- No dependencies, small crate footprint.
- serde-like derive macro without the bells and whistles.
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
    username: String,
    password: Option<String>
}
#[derive(Debug, PartialEq, Eq, Hash, Config)]
enum Protocol {
    Https,
    Http
}
#[derive(Debug, Config)]
struct Web {
    address: IpAddr,
    data_root: PathBuf,
    logins: Vec<Login>,
    protocols: HashMap<Protocol, u16>,
    banned: Vec<IpAddr>
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
