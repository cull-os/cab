use std::{
    io::Write as _,
    process,
};

use cab::syntax::{
    self,
};
use cab_syntax::NodeErrorWithContext;
use clap::Parser as _;
use clap_stdin::FileOrStdin;
use clap_verbosity_flag::{
    InfoLevel,
    Verbosity,
};
use yansi::Paint as _;

#[derive(clap::Parser)]
#[command(version, about)]
struct Cli {
    #[command(flatten)]
    verbosity: Verbosity<InfoLevel>,

    #[command(subcommand)]
    command: Command,
}

#[derive(clap::Subcommand, Debug, Clone)]
enum Command {
    /// Various commands related to debugging.
    Dump {
        #[command(subcommand)]
        command: Dump,

        /// The file to dump.
        #[clap(default_value = "-", global = true)]
        file: FileOrStdin,
    },
}

#[derive(clap::Subcommand, Debug, Clone, Copy)]
enum Dump {
    /// Dump the provided file's tokens.
    Token {
        /// If specified, the output will be colored instead of typed.
        #[arg(long, short, global = true)]
        color: bool,
    },

    /// Dump the provided file's syntax.
    Syntax,

    /// Dump the provided file with parentheses to disambiguate.
    Parenthesize,
}

#[tokio::main]
async fn main() -> miette::Result<()> {
    let cli = Cli::parse();

    yansi::whenever(yansi::Condition::TTY_AND_COLOR);

    let mut out = std::io::stdout();

    // Trying to imitate clap to get a consistent experience.
    env_logger::Builder::new()
        .filter_level(cli.verbosity.log_level_filter())
        .format(|buffer, record| {
            let level = match record.level() {
                log::Level::Error => "error:".red().bold(),
                log::Level::Warn => "warn:".yellow().bold(),
                log::Level::Info => "info:".green().bold(),
                log::Level::Debug => "debug:".blue().bold(),
                log::Level::Trace => "trace:".cyan().bold(),
            };

            writeln!(buffer, "{level} {arguments}", arguments = record.args())
        })
        .init();

    match cli.command {
        Command::Dump { file, command } => {
            let contents = file.contents().unwrap_or_else(|error| {
                log::error!("failed to read file: {error}");
                process::exit(1);
            });

            match command {
                Dump::Token { color } => {
                    for (kind, slice) in syntax::tokenize(&contents) {
                        let result = if color {
                            let style = syntax::COLORS[kind as usize];

                            write!(out, "{slice}", slice = slice.paint(style))
                        } else {
                            writeln!(out, "{kind:?} {slice:?}")
                        };

                        result.unwrap_or_else(|error| {
                            log::error!("failed to write to stdout: {error}");
                            process::exit(1);
                        });
                    }
                },

                Dump::Syntax | Dump::Parenthesize => {
                    let parse = syntax::parse::<_, syntax::node::Expression>(
                        syntax::tokenize(&contents),
                        Default::default(),
                    );

                    for error in parse.errors.clone().into_iter() {
                        let error = NodeErrorWithContext::new(contents.clone(), error);
                        println!("{:?}", Into::<miette::Report>::into(error));
                    }

                    if matches!(command, Dump::Syntax) {
                        write!(out, "{syntax:#?}", syntax = parse.syntax)
                    } else if let Ok(node) = parse.result() {
                        syntax::format::parenthesize(&mut out, &node)
                    } else {
                        Ok(())
                    }
                    .unwrap_or_else(|error| {
                        log::error!("failed to write to stdout: {error}");
                        process::exit(1);
                    });
                },
            }
        },
    }
    Ok(())
}
