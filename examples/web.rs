use std::{net::{IpAddr, Ipv4Addr, Ipv6Addr}, path::PathBuf, collections::HashMap};
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
    let mut config = Web::load("web");
    // Cargo doesn't execute examples from this directory
    config.load_str(include_str!("web.config"), "included web.config");
    println!("{:#?}", config)
}