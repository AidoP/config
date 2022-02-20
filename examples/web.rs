use std::{net::{IpAddr, Ipv4Addr, Ipv6Addr}, path::PathBuf, collections::HashMap};
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
impl Default for Web {
    fn default() -> Self {
        Self {
            address: IpAddr::V4(Ipv4Addr::new(127, 0, 0, 1)),
            data_root: "/srv".into(),
            logins: vec![],
            protocols: HashMap::from([
                (Protocol::Http, 80),
                (Protocol::Https, 443)
            ]),
            banned: vec![
                IpAddr::V4(Ipv4Addr::UNSPECIFIED),
                IpAddr::V6(Ipv6Addr::UNSPECIFIED)
            ]
        }
    }
}

fn main() {
    let mut config = Web::load("web", None);
    // Cargo doesn't execute examples from this directory
    config.load_str(include_str!("web.config"), "included web.config", None);
    println!("{:#?}", config)
}