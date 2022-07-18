#[derive(Debug)]
pub enum Command {
    FileRead(String),
    Noop,
}

pub fn read_command() -> Command {
    let matches = clap_app!(mamba =>
        (version: "0.1.1\n")
        (author: "Levi Bland <levi.bland@icloud.com>\n")
        (about: "The Mamba Programming Language")
        (@setting ArgRequiredElseHelp)
        (@arg src: -s --src +takes_value "Path of the source file")
    ).get_matches();

    let src_path = matches.value_of("src").map(|s| s.to_string());
    match src_path {
        Some(s) => Command::FileRead(s),
        _ => Command::Noop,
    }
}