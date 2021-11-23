use owo_colors::{Color, OwoColorize};
use std::error::Error;
use std::io::Write;

macro_rules! ok {
    ($title:expr, $msg:expr) => {
        $crate::macros::print::<owo_colors::colors::Green>($title, $msg);
    };

    ($title:expr, $msg:expr, $($arg:tt)*) => {
        ok!($title, format!($msg, $($arg)*).as_str())
    };
}

macro_rules! info {
    ($title:expr, $msg:expr) => {
        $crate::macros::print::<owo_colors::colors::Cyan>($title, $msg)
    };

    ($title:expr, $msg:expr, $($arg:tt)*) => {
        info!($title, format!($msg, $($arg)*).as_str())
    };
}

macro_rules! error {
    ($title:expr, $msg:expr) => {
        $crate::macros::print::<owo_colors::colors::Red>($title, $msg)
    };

    ($title:expr, $msg:expr, $($arg:tt)*) => {
        error!($title, format!($msg, $($arg)*).as_str())
    };
}

pub(super) fn print<C: Color>(title: &str, msg: &str) {
    print!("{:<15}", msg.fg::<C>());
    println!(" {}", msg);
}
