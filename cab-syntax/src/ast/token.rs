use std::{
    fmt,
    num,
};

use crate::syntax::{
    self,
    Kind::*,
};

pub trait Token {
    fn can_cast(from: syntax::Kind) -> bool;

    fn cast(from: syntax::Token) -> Option<Self>
    where
        Self: Sized;

    fn syntax(&self) -> &syntax::Token;
}

macro_rules! token {
    (
        #[from($kind:ident)]
        $(#[$meta:meta])*
        struct $name:ident;
    ) => {
        $(#[$meta])*
        #[derive(Debug, Clone, PartialEq, Eq, Hash)]
        pub struct $name(pub syntax::Token);

        impl fmt::Display for $name {
            fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
                fmt::Display::fmt(self.syntax(), formatter)
            }
        }

        impl Token for $name {
            fn can_cast(kind: syntax::Kind) -> bool {
                kind == $kind
            }

            fn cast(from: syntax::Token) -> Option<Self> {
                Self::can_cast(from.kind()).then_some(Self(from))
            }

            fn syntax(&self) -> &syntax::Token {
                &self.0
            }
        }
    };
}

token! { #[from(TOKEN_WHITESPACE)] struct Whitespace; }

token! { #[from(TOKEN_COMMENT)] struct Comment; }

impl Comment {
    pub fn text(&self) -> &str {
        let raw = self.syntax().text();
        let start_index = raw.find(|c| c != '#').unwrap_or(raw.len());

        // Assumes well formed comment.
        match start_index {
            0 => unreachable!(),
            1..=2 => &raw[start_index..],
            3.. => &raw[start_index..raw.len() - start_index],
        }
    }
}

token! { #[from(TOKEN_INTEGER)] struct Integer; }

impl Integer {
    pub fn value(&self) -> Result<i64, num::ParseIntError> {
        self.syntax().text().parse()
    }
}

token! { #[from(TOKEN_FLOAT)] struct Float; }

impl Float {
    pub fn value(&self) -> Result<f64, num::ParseFloatError> {
        self.syntax().text().parse()
    }
}

token! { #[from(TOKEN_PATH)] struct Path; }

token! { #[from(TOKEN_IDENTIFIER_CONTENT)] struct IdentifierContent; }

token! { #[from(TOKEN_STRING_CONTENT)] struct StringContent; }

token! { #[from(TOKEN_ISLAND_CONTENT)] struct IslandContent; }
