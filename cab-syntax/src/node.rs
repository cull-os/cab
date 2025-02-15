//! [`Node`] definitions.
use std::{
    collections::VecDeque,
    mem,
    ops,
};

use cab_report::Report;
use cab_text::{
    IntoSpan,
    Span,
};
use paste::paste;
use smallvec::SmallVec;

use crate::{
    Kind::{
        self,
        *,
    },
    Token,
    red,
    token::{
        self,
        ContentPart,
    },
};

macro_rules! reffed {
    (
        $(#[$attribute:meta])*
        pub enum $name:ident $(<$($ident:ident $(: $bound:path)?),+>)? {
            $(
                $(#[$variant_attribute:meta])*
                $variant:ident($type:ty)
            ),* $(,)?
        }
    ) => {
        paste! {
            $(#[$attribute])*
            pub enum $name $(<$($ident $(: $bound)?),+>)? {
                $(
                    $(#[$variant_attribute])*
                    $variant($type)
                ),*
            }

            impl$(<$($ident $(: $bound)?),+>)? $name $(<$($ident),+>)? {
                pub fn as_ref(&self) -> [<$name Ref>]<'_$(, $($ident),+)?> {
                    match self {
                        $(
                            Self::$variant(v) => [<$name Ref>]::$variant(v)
                        ),*
                    }
                }
            }

            $(#[$attribute])*
            #[derive(Copy)]
            pub enum [<$name Ref>]<'a $(, $($ident $(: $bound)?),+)?> {
                $(
                    $(#[$variant_attribute])*
                    $variant(&'a $type)
                ),*
            }
        }
    };
}

macro_rules! node {
    (
        #[from($kind:ident)]
        struct $name:ident;
    ) => {
        #[derive(Debug, Clone, PartialEq, Eq, Hash)]
        #[repr(transparent)]
        pub struct $name(red::Node);

        impl ops::Deref for $name {
            type Target = red::Node;

            fn deref(&self) -> &Self::Target {
                &self.0
            }
        }

        impl<'a> TryFrom<&'a red::Node> for &'a $name {
            type Error = ();

            fn try_from(node: &'a red::Node) -> Result<Self, ()> {
                if node.kind() != $kind {
                    return Err(());
                }

                // SAFETY: `node` is an immutable reference and Self is a &red::Node with
                // #[repr(transparent)].
                Ok(unsafe { mem::transmute::<&red::Node, Self>(node) })
            }
        }

        impl TryFrom<red::Node> for $name {
            type Error = ();

            fn try_from(node: red::Node) -> Result<Self, ()> {
                if node.kind() != $kind {
                    return Err(());
                }

                Ok(Self(node))
            }
        }

        impl $name {
            pub const KIND: Kind = $kind;
        }
    };

    (
        #[from($($variant:ident),* $(,)?)]
        enum $name:ident;
    ) => {
        reffed! {
            #[derive(Debug, Clone, PartialEq, Eq, Hash)]
            pub enum $name {
                $($variant($variant),)*
            }
        }

        impl ops::Deref for $name {
            type Target = red::Node;

            fn deref(&self) -> &Self::Target {
                match self {
                    $(Self::$variant(node) => &**node,)*
                }
            }
        }

        impl TryFrom<red::Node> for $name {
            type Error = ();

            fn try_from(node: red::Node) -> Result<Self, ()> {
                Ok(match node.kind() {
                    $($variant::KIND => Self::$variant($variant::try_from(node)?),)*
                    _ => return Err(()),
                })
            }
        }

        $(
            impl From<$variant> for $name {
                fn from(from: $variant) -> Self {
                    Self::$variant(from)
                }
            }

            impl TryFrom<$name> for $variant {
                type Error = ();

                fn try_from(from: $name) -> Result<Self, ()> {
                    if let $name::$variant(node) = from {
                        Ok(node)
                    } else {
                        Err(())
                    }
                }
            }
        )*

        paste! {
            impl ops::Deref for [<$name Ref>]<'_> {
                type Target = red::Node;

                fn deref(&self) -> &Self::Target {
                    match self {
                        $(Self::$variant(node) => &**node,)*
                    }
                }
            }

            impl<'a> TryFrom<&'a red::Node> for [<$name Ref>]<'a> {
                type Error = ();

                fn try_from(node: &'a red::Node) -> Result<Self, ()> {
                    Ok(match node.kind() {
                        $($variant::KIND => Self::$variant(<&$variant>::try_from(node)?),)*
                        _ => return Err(()),
                    })
                }
            }

            $(
                impl<'a> From<&'a $variant> for [<$name Ref>]<'a> {
                    fn from(from: &'a $variant) -> Self {
                        Self::$variant(from)
                    }
                }

                impl<'a> TryFrom<[<$name Ref>]<'a>> for &'a $variant {
                    type Error = ();

                    fn try_from(from: [<$name Ref>]<'a>) -> Result<Self, ()> {
                        if let [<$name Ref>]::$variant(node) = from {
                            Ok(node)
                        } else {
                            Err(())
                        }
                    }
                }
            )*
        }
    };
}

macro_rules! get_token {
    ($name:ident -> $($skip:literal @)? Option<$kind:ident>) => {
        pub fn $name(&self) -> Option<&red::Token> {
            self.children_with_tokens()
                .filter_map(red::ElementRef::into_token)
                $(.skip($skip))?
                .find(|token| token.kind() == $kind)
        }
    };

    ($name:ident -> $($skip:literal @)? $kind:ident) => {
        pub fn $name(&self) -> &red::Token {
            self.children_with_tokens()
                .filter_map(red::ElementRef::into_token)
                $(.skip($skip))?
                .find(|token| token.kind() == $kind)
                .expect("node must have a token child")
        }
    };
}

macro_rules! get_node {
    ($name:ident -> $($skip:literal @)? Option<$type:ty>) => {
        pub fn $name(&self) -> Option<$type> {
            self.children()
                .filter_map(|node| <$type>::try_from(node).ok())
                $(.skip($skip))?
                .next()
        }
    };

    ($name:ident -> $($skip:literal @)? $type:ty) => {
        pub fn $name(&self) -> $type {
            self.children()
                .filter_map(|node| <$type>::try_from(node).ok())
                $(.skip($skip))?
                .next()
                .expect("node must have a matching node child")
        }
    };
}

// EXPRESSION

node! {
    #[from(
        Error,
        Parenthesis,
        List,
        AttributeList,
        PrefixOperation,
        InfixOperation,
        SuffixOperation,
        Path,
        Identifier,
        SString,
        Rune,
        Island,
        Number,
        IfThen,
        IfIs,
    )] enum Expression;
}

impl<'a> ExpressionRef<'a> {
    pub fn validate(self, to: &mut Vec<Report>) {
        match self {
            Self::Parenthesis(parenthesis) => parenthesis.validate(to),
            Self::List(list) => list.validate(to),
            Self::AttributeList(attribute_list) => attribute_list.validate(to),
            Self::PrefixOperation(operation) => operation.validate(to),
            Self::InfixOperation(operation) => operation.validate(to),
            Self::SuffixOperation(operation) => operation.validate(to),
            Self::Path(path) => path.validate(to),
            Self::Identifier(identifier) => identifier.validate(to),
            Self::SString(string) => string.validate(to),
            Self::Rune(rune) => rune.validate(to),
            Self::Island(island) => island.validate(to),
            Self::IfThen(if_else) => if_else.validate(to),
            Self::IfIs(if_is) => if_is.validate(to),

            Self::Error(_) | Self::Number(_) => {},
        }
    }

    pub fn validate_pattern(self, to: &mut Vec<Report>) {
        match self {
            Self::Parenthesis(parenthesis) => {
                parenthesis.expression().validate_pattern(to);
            },

            Self::List(list) => {
                for item in list.items() {
                    item.validate_pattern(to);
                }
            },

            // All attribute lists are valid patterns.
            // TODO: Add warns for right-side expression that
            // statically never use the attribute list made the scope.
            Self::AttributeList(attribute_list) => attribute_list.validate(to),

            Self::InfixOperation(operation)
                if let InfixOperator::ImplicitApply | InfixOperator::Apply = operation.operator() =>
            {
                operation.left().validate(to);
                operation.right().validate_pattern(to);
            },

            Self::InfixOperation(operation) if let InfixOperator::Pipe = operation.operator() => {
                operation.left().validate_pattern(to);
                operation.right().validate(to);
            },

            Self::InfixOperation(operation) if let InfixOperator::Construct = operation.operator() => {
                operation.left().validate_pattern(to);
                operation.right().validate_pattern(to);
            },

            Self::InfixOperation(operation) if let InfixOperator::Select = operation.operator() => {
                match operation.left() {
                    ExpressionRef::Identifier(identifier) => {
                        identifier.validate(to);
                    },

                    invalid => {
                        to.push(
                            Report::error("invalid select pattern")
                                .primary(invalid.span(), "left operand must be an identifier"),
                        )
                    },
                }

                operation.right().validate_pattern(to);
            },

            Self::InfixOperation(operation) if let InfixOperator::All | InfixOperator::Any = operation.operator() => {
                operation.left().validate_pattern(to);
                operation.right().validate_pattern(to);
            },

            Self::InfixOperation(operation) if let InfixOperator::This = operation.operator() => {
                operation.validate_left(to);
                operation.right().validate_pattern(to);
            },

            _ => {
                let mut report = Report::error("arithmetic patterns cannot have multiple binds");

                let mut bind_spans = SmallVec::new();
                self.validate_pattern_arithmetic(&mut bind_spans, to);

                for bind_span in bind_spans {
                    report.push_primary(bind_span, "bind");
                }

                if report.labels.len() > 1 {
                    to.push(report);
                }
            },
        }
    }

    pub fn validate_pattern_arithmetic(self, bind_spans: &mut SmallVec<Span, 4>, reports: &mut Vec<Report>) {
        match self {
            ExpressionRef::Parenthesis(parenthesis) => {
                parenthesis.expression().validate_pattern(reports);
            },

            ExpressionRef::InfixOperation(operation)
                if let InfixOperator::Addition
                | InfixOperator::Subtraction
                | InfixOperator::Multiplication
                | InfixOperator::Power
                | InfixOperator::Division = operation.operator() =>
            {
                operation.left().validate_pattern_arithmetic(bind_spans, reports);
                operation.right().validate_pattern_arithmetic(bind_spans, reports);
            },

            ExpressionRef::InfixOperation(_) => {
                reports.push(
                    Report::error("non-arithmetic infix operators are not valid patterns").primary(self.span(), "here"),
                );
            },

            ExpressionRef::Identifier(identifier) => {
                identifier.validate(reports);
                bind_spans.push(identifier.span());
            },

            ExpressionRef::SString(string) => {
                string.validate(reports);
            },

            ExpressionRef::Number(number) => {
                number.validate(reports);
            },

            _ => {
                reports.push(Report::error("invalid pattern").primary(
                    self.span(),
                    format!("{kind} is not a valid pattern element", kind = self.kind()),
                ));
            },
        }
    }

    pub fn same_items(self) -> impl Iterator<Item = ExpressionRef<'a>> {
        gen move {
            let mut expressions = VecDeque::from([self]);

            while let Some(expression) = expressions.pop_back() {
                match expression {
                    ExpressionRef::InfixOperation(operation) if let InfixOperator::Same = operation.operator() => {
                        expressions.push_front(operation.left());
                        expressions.push_front(operation.right());
                    },

                    ExpressionRef::SuffixOperation(operation) if let SuffixOperator::Same = operation.operator() => {
                        expressions.push_front(operation.left());
                    },

                    normal => yield normal,
                }
            }
        }
    }
}

// ERROR

node! { #[from(NODE_ERROR)] struct Error; }

// PARENTHESIS

node! { #[from(NODE_PARENTHESIS)] struct Parenthesis; }

impl Parenthesis {
    get_token! { token_left_parenthesis -> TOKEN_LEFT_PARENTHESIS }

    get_node! { expression -> ExpressionRef<'_> }

    get_token! { token_right_parenthesis -> Option<TOKEN_RIGHT_PARENTHESIS> }

    pub fn validate(&self, to: &mut Vec<Report>) {
        self.expression().validate(to);
    }
}

// LIST

node! { #[from(NODE_LIST)] struct List; }

impl List {
    get_token! { token_left_bracket -> TOKEN_LEFT_BRACKET }

    get_node! { expression -> Option<ExpressionRef<'_>> }

    get_token! { token_right_bracket -> Option<TOKEN_RIGHT_BRACKET> }

    /// Returns all items of the list.
    pub fn items(&self) -> impl Iterator<Item = ExpressionRef<'_>> {
        self.expression().into_iter().flat_map(ExpressionRef::same_items)
    }

    pub fn validate(&self, to: &mut Vec<Report>) {
        if let Some(ExpressionRef::InfixOperation(operation)) = self.expression()
            && operation.operator() == InfixOperator::Sequence
        {
            to.push(
                Report::error("inner expression of list cannot be sequence")
                    .primary(operation.span(), "consider parenthesizing this"),
            );
        }

        for item in self.items() {
            item.validate(to);
        }
    }
}

// ATTRIBUTE LIST

node! { #[from(NODE_ATTRIBUTE_LIST)] struct AttributeList; }

impl AttributeList {
    get_token! { left_curlybrace_token -> TOKEN_LEFT_CURLYBRACE }

    get_node! { expression -> Option<ExpressionRef<'_>> }

    get_token! { right_curlybrace_token -> Option<TOKEN_RIGHT_CURLYBRACE> }

    /// Returns all entries of the attribute list.
    pub fn entries(&self) -> impl Iterator<Item = ExpressionRef<'_>> {
        self.expression().into_iter().flat_map(ExpressionRef::same_items)
    }

    pub fn validate(&self, to: &mut Vec<Report>) {
        for entry in self.entries() {
            match entry {
                ExpressionRef::InfixOperation(operation) if let InfixOperator::Sequence = operation.operator() => {
                    to.push(
                        Report::error("unexpected sequence operator inside attribute list").primary(
                            operation.span(),
                            "sequence operator has lower binding power and will consume everything",
                        ),
                    );
                },

                ExpressionRef::InfixOperation(operation) if let InfixOperator::Bind = operation.operator() => {
                    operation.validate(to);
                },

                ExpressionRef::Identifier(identifier) => {
                    identifier.validate(to);
                },

                invalid => to.push(Report::error("invalid attribute").primary(invalid.span(), "here")),
            }
        }
    }
}

// PREFIX OPERATION

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PrefixOperator {
    Swwallation, // Get it?
    Negation,

    Not,

    Try,
}

impl TryFrom<Kind> for PrefixOperator {
    type Error = ();

    fn try_from(from: Kind) -> Result<Self, ()> {
        Ok(match from {
            TOKEN_PLUS => Self::Swwallation,
            TOKEN_MINUS => Self::Negation,

            TOKEN_EXCLAMATIONMARK => Self::Not,

            TOKEN_QUESTIONMARK => Self::Try,

            _ => return Err(()),
        })
    }
}

impl PrefixOperator {
    /// Returns the binding power of this operator.
    pub fn binding_power(self) -> ((), u16) {
        match self {
            Self::Swwallation | Self::Negation => ((), 165),
            Self::Not => ((), 145),
            Self::Try => ((), 125),
        }
    }
}

node! { #[from(NODE_PREFIX_OPERATION)] struct PrefixOperation; }

impl PrefixOperation {
    get_node! { right -> 0 @ ExpressionRef<'_> }

    /// Returns the operator token of this operation.
    pub fn operator_token(&self) -> &red::Token {
        self.children_with_tokens()
            .filter_map(|child| child.into_token())
            .find(|token| PrefixOperator::try_from(token.kind()).is_ok())
            .unwrap()
    }

    /// Returns the operator of this operation.
    pub fn operator(&self) -> PrefixOperator {
        self.children_with_tokens()
            .filter_map(|child| child.into_token())
            .find_map(|token| PrefixOperator::try_from(token.kind()).ok())
            .unwrap()
    }

    pub fn validate(&self, to: &mut Vec<Report>) {
        self.right().validate(to);
    }
}

// INFIX OPERATION

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum InfixOperator {
    Same,
    Sequence,

    ImplicitApply,
    Apply,
    Pipe,

    Concat,
    Construct,

    Select,
    Update,

    LessOrEqual,
    Less,
    MoreOrEqual,
    More,

    Equal,
    NotEqual,

    And,
    Or,
    Implication,

    All,
    Any,

    Addition,
    Subtraction,
    Multiplication,
    Power,
    Division,

    This,
    Lambda,
    Bind,
}

impl TryFrom<Kind> for InfixOperator {
    type Error = ();

    fn try_from(from: Kind) -> Result<Self, ()> {
        Ok(match from {
            TOKEN_COMMA => Self::Same,
            TOKEN_SEMICOLON => Self::Sequence,

            kind if kind.is_argument() => Self::ImplicitApply,
            TOKEN_LESS_PIPE => Self::Apply,
            TOKEN_PIPE_MORE => Self::Pipe,

            TOKEN_PLUS_PLUS => Self::Concat,
            TOKEN_COLON => Self::Construct,

            TOKEN_PERIOD => Self::Select,
            TOKEN_SLASH_SLASH => Self::Update,

            TOKEN_LESS_EQUAL => Self::LessOrEqual,
            TOKEN_LESS => Self::Less,
            TOKEN_MORE_EQUAL => Self::MoreOrEqual,
            TOKEN_MORE => Self::More,

            TOKEN_EQUAL_EQUAL => Self::Equal,
            TOKEN_EXCLAMATION_EQUAL => Self::NotEqual,

            TOKEN_AMPERSAND_AMPERSAND => Self::And,
            TOKEN_PIPE_PIPE => Self::Or,
            TOKEN_MINUS_MORE => Self::Implication,

            TOKEN_AMPERSAND => Self::All,
            TOKEN_PIPE => Self::Any,

            TOKEN_PLUS => Self::Addition,
            TOKEN_MINUS => Self::Subtraction,
            TOKEN_ASTERISK => Self::Multiplication,
            TOKEN_CARET => Self::Power,
            TOKEN_SLASH => Self::Division,

            TOKEN_AT => Self::This,
            TOKEN_EQUAL_GREATER => Self::Lambda,
            TOKEN_COLON_EQUAL => Self::Bind,

            _ => return Err(()),
        })
    }
}

impl InfixOperator {
    /// Returns the binding power of this operator.
    pub fn binding_power(self) -> (u16, u16) {
        match self {
            Self::Select => (205, 200),
            Self::ImplicitApply => (190, 195),

            Self::Concat => (180, 185),

            Self::Multiplication | Self::Division => (170, 175),
            Self::Power => (175, 170),

            // PrefixOperator::Swallation | PrefixOperator::Negation
            Self::Addition | Self::Subtraction => (150, 155),
            // PrefixOperator::Not
            Self::Update => (130, 135),

            Self::LessOrEqual | Self::Less | Self::MoreOrEqual | Self::More /* | PrefixOperator::Try */ => {
                (120, 125)
            },

            Self::Construct => (115, 110),

            Self::Equal | Self::NotEqual => (105, 100),

            Self::And | Self::All => (95, 90),
            Self::Or | Self::Any => (85, 80),
            Self::Implication => (75, 70),

            Self::Pipe => (60, 65),
            Self::Apply => (65, 60),

            Self::This => (55, 50),
            Self::Lambda => (45, 40),
            Self::Bind => (35, 30),

            Self::Same => (25, 20),
            Self::Sequence => (15, 10),
        }
    }

    /// Whether if this operator actually owns a token. Not owning a token means
    /// that the operator doesn't actually "exist".
    pub fn is_token_owning(self) -> bool {
        self != Self::ImplicitApply
    }
}

node! { #[from(NODE_INFIX_OPERATION)] struct InfixOperation; }

impl InfixOperation {
    get_node! { left -> 0 @ ExpressionRef<'_> }

    get_node! { right -> 1 @ ExpressionRef<'_> }

    /// Returns the operator token of this operation.
    pub fn operator_token(&self) -> Option<&'_ red::Token> {
        self.children_with_tokens()
            .filter_map(|child| child.into_token())
            .find(|token| InfixOperator::try_from(token.kind()).is_ok())
    }

    /// Returns the operator of this operation.
    pub fn operator(&self) -> InfixOperator {
        self.children_with_tokens()
            .filter_map(|child| child.into_token())
            .find_map(|token| InfixOperator::try_from(token.kind()).ok())
            .unwrap_or(InfixOperator::ImplicitApply)
    }

    pub fn validate(&self, to: &mut Vec<Report>) {
        match self.operator() {
            InfixOperator::Same => {
                let mut same_operator = None;

                for item in ExpressionRef::InfixOperation(self).same_items() {
                    if let ExpressionRef::InfixOperation(operation) = &item
                        && let operator @ (InfixOperator::Lambda | InfixOperator::Bind) = operation.operator()
                    {
                        let same_operator = same_operator.get_or_insert(operator);

                        if *same_operator != operator {
                            to.push(
                                Report::error("invalid same-operand")
                                    .primary(item.span(), "operand")
                                    .help(
                                        "all same-operands must be of matching type: either all lambdas or all binds",
                                    ),
                            );
                            continue;
                        }

                        operation.validate(to);
                    } else {
                        to.push(
                            Report::error("invalid same-operand")
                                .primary(item.span(), "operand")
                                .help("all same-operands must either be lambdas or binds"),
                        );
                    }
                }
            },

            InfixOperator::This => {
                self.validate_left(to);
                self.right().validate(to);
            },

            InfixOperator::Lambda | InfixOperator::Bind => {
                self.left().validate_pattern(to);
                self.right().validate(to);
            },

            operator => {
                let expressions = &[self.left(), self.right()];

                for expression in expressions {
                    expression.validate(to);
                }

                let (InfixOperator::Apply | InfixOperator::Pipe) = operator else {
                    return;
                };

                for expression in expressions {
                    if let ExpressionRef::InfixOperation(operation) = expression
                        && let child_operator @ (InfixOperator::Apply | InfixOperator::Pipe) = operation.operator()
                        && child_operator != operator
                    {
                        to.push(
                            Report::error("application and pipe operators do not associate")
                                .secondary(self.span(), "this")
                                .primary(operation.span(), "does not associate with this"),
                        );
                    }
                }
            },
        }
    }

    /// Asserts that this node is a this-expression and validates the left
    /// expression.
    pub fn validate_left(&self, to: &mut Vec<Report>) {
        assert_eq!(self.operator(), InfixOperator::This);

        match self.left() {
            ExpressionRef::Identifier(identifier) => {
                identifier.validate(to);
            },

            invalid => {
                to.push(
                    Report::error("left operand of a this-expression must be an identifier")
                        .primary(invalid.span(), "left operand"),
                );
            },
        }
    }
}

// SUFFIX OPERATION

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SuffixOperator {
    Same,
    Sequence,
}

impl TryFrom<Kind> for SuffixOperator {
    type Error = ();

    fn try_from(from: Kind) -> Result<Self, ()> {
        match from {
            TOKEN_COMMA => Ok(Self::Same),
            TOKEN_SEMICOLON => Ok(Self::Sequence),

            _ => Err(()),
        }
    }
}

node! { #[from(NODE_SUFFIX_OPERATION)] struct SuffixOperation; }

impl SuffixOperation {
    get_node! { left -> 0 @ ExpressionRef<'_> }

    /// Returns the operator token of this operation.
    pub fn operator_token(&self) -> &'_ red::Token {
        self.children_with_tokens()
            .filter_map(|child| child.into_token())
            .find(|token| SuffixOperator::try_from(token.kind()).is_ok())
            .unwrap()
    }

    /// Returns the operator of this operation.
    pub fn operator(&self) -> SuffixOperator {
        self.children_with_tokens()
            .filter_map(|child| child.into_token())
            .find_map(|token| SuffixOperator::try_from(token.kind()).ok())
            .unwrap()
    }

    pub fn validate(&self, to: &mut Vec<Report>) {
        self.left().validate(to);
    }
}

// INTERPOLATION

reffed! {
    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub enum InterpolatedPart<T: Token> {
        Delimiter(red::Token),
        Content(T),
        Interpolation(Interpolation),
    }
}

// TODO: Find ouy why the hell s/&self/self/ doesn't work. This should be Copy.
impl<T: Token> InterpolatedPartRef<'_, T> {
    pub fn is_delimiter(&self) -> bool {
        matches!(self, Self::Delimiter(_))
    }

    pub fn span(&self) -> Span {
        match self {
            Self::Delimiter(delimiter) => delimiter.span(),
            Self::Content(content) => content.span(),
            Self::Interpolation(interpolation) => interpolation.span(),
        }
    }
}

node! { #[from(NODE_INTERPOLATION)] struct Interpolation; }

impl Interpolation {
    get_token! { interpolation_start_token -> TOKEN_INTERPOLATION_START }

    get_node! { expression -> 0 @ ExpressionRef<'_> }

    get_token! { interpolation_end_token -> Option<TOKEN_INTERPOLATION_END> }

    pub fn validate(&self, to: &mut Vec<Report>) {
        self.expression().validate(to);
    }
}

macro_rules! parted {
    ($content_type:ty) => {
        pub fn parts(&self) -> impl Iterator<Item = InterpolatedPartRef<'_, $content_type>> {
            self.children_with_tokens().map(|child| {
                match child {
                    red::ElementRef::Token(token) => {
                        if let Ok(token) = <&$content_type>::try_from(token) {
                            InterpolatedPartRef::Content(token)
                        } else {
                            InterpolatedPartRef::Delimiter(token)
                        }
                    },

                    red::ElementRef::Node(node) => {
                        InterpolatedPartRef::Interpolation(
                            <&Interpolation>::try_from(node)
                                .expect("child node of a parted element wasn't an interpolation"),
                        )
                    },
                }
            })
        }
    };
}

// PATH

node! { #[from(NODE_PATH)] struct Path; }

impl Path {
    parted!(token::PathContent);

    pub fn validate(&self, to: &mut Vec<Report>) {
        for part in self.parts() {
            if let InterpolatedPartRef::Interpolation(interpolation) = part {
                interpolation.validate(to);
            }
        }
    }
}

// IDENTIFIER

node! { #[from(NODE_IDENTIFIER)] struct IdentifierQuoted; }

impl IdentifierQuoted {
    parted!(token::Content);

    pub fn validate(&self, to: &mut Vec<Report>) {
        let mut report = Report::error("invalid identifier");
        let mut reported_control_character = false;

        for part in self.parts() {
            match part {
                InterpolatedPartRef::Content(content) => {
                    content.parts(&mut report).count();

                    let text = content.text();

                    if !reported_control_character && text.chars().any(char::is_control) {
                        reported_control_character = true;
                        report.push_primary(content.span(), "contains control characters");
                        report.push_help(
                            "quoted identifiers cannot contain control characters (non-escaped newlines, tabs, ...)",
                        );
                    }
                },

                InterpolatedPartRef::Interpolation(interpolation) => {
                    interpolation.validate(to);
                },

                _ => {},
            }
        }

        if !report.is_empty() {
            to.push(report);
        }
    }
}

reffed! {
    /// An identifier value.
    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub enum IdentifierValue {
        /// A plain identifier backed by a [`token::Identifier`].
        Plain(token::Identifier),
        /// A quoted identifier backed by a [`IdentifierQuoted`].
        Quoted(IdentifierQuoted),
    }
}

node! { #[from(NODE_IDENTIFIER)] struct Identifier; }

impl Identifier {
    /// Returns the value of this identifier. A value may either be a
    /// [`token::Identifier`] or a [`IdentifierQuoted`].
    pub fn value(&self) -> IdentifierValueRef<'_> {
        if let Some(token) = self
            .first_token()
            .and_then(|token| <&token::Identifier>::try_from(token).ok())
        {
            return IdentifierValueRef::Plain(token);
        }

        if let Ok(quoted) = <&IdentifierQuoted>::try_from(&**self) {
            return IdentifierValueRef::Quoted(quoted);
        }

        unreachable!("identifier node did not have an identifier or identifier starter token")
    }

    pub fn validate(&self, to: &mut Vec<Report>) {
        if let IdentifierValueRef::Quoted(quoted) = self.value() {
            quoted.validate(to);
        }
    }
}

// STRING

node! { #[from(NODE_STRING)] struct SString; }

impl SString {
    parted!(token::Content);

    // What a behemoth. And the sad part is I can't figure out a way to make this
    // simpler.
    pub fn validate(&self, to: &mut Vec<Report>) {
        let mut report = Report::error("invalid string");
        let mut reported_mixed_indentation = false;

        let mut parts = self
            .parts()
            .scan(0, |index, part| {
                let value = *index;

                match part {
                    InterpolatedPartRef::Delimiter(_) => {},

                    _ => *index += 1,
                }

                Some((value, part))
            })
            .peekable();

        let mut string_is_multiline = false;
        let mut string_first_line_span = None;
        let mut string_last_line_span = None;

        let mut indentation: Option<char> = None;

        let mut previous_span = None;
        while let Some((part_index, part)) = parts.next() {
            let mut part_is_multiline = true;
            let part_is_first = part_index == 0;
            let part_is_last = parts.peek().is_none_or(|(_, part)| part.is_delimiter());

            match &part {
                InterpolatedPartRef::Interpolation(interpolation) => {
                    interpolation.validate(to);

                    let span = interpolation.span();

                    if part_is_first {
                        string_first_line_span = Some(span);
                    } else if part_is_last {
                        string_last_line_span = Some(span);
                    }
                },

                InterpolatedPartRef::Content(content) => {
                    content.parts(&mut report).count();

                    let text = content.text();

                    let mut lines = text.split('\n').enumerate().peekable();
                    while let Some((line_index, line)) = lines.next() {
                        let line_is_first = line_index == 0;
                        let line_is_last = lines.peek().is_none();

                        let line_is_firstest = part_is_first && line_is_first;
                        let line_is_lastest = part_is_last && line_is_last;

                        if line_is_first && line_is_last {
                            part_is_multiline = false;
                        }

                        if line_is_firstest {
                            if !line.trim().is_empty() {
                                string_first_line_span = Some(Span::at(content.span().start, line.trim_end().len()));
                            } else if text.trim().is_empty()
                                && let Some((_, part)) = parts.peek()
                                && !part.is_delimiter()
                            {
                                string_first_line_span = Some(part.span());
                            }
                        } else if line_is_lastest {
                            if !line.trim().is_empty() {
                                let last_line_length = line.trim_start().len();

                                string_last_line_span = Some(Span::at_end(content.span().end, last_line_length));
                            } else if !part_is_multiline && let Some(span) = previous_span {
                                string_last_line_span = Some(span);
                            }
                        }

                        #[allow(clippy::nonminimal_bool)]
                        if
                        // Ignore firstest and lastest lines.
                        !(line_is_firstest || line_is_lastest)
                            // Ignore lines right after an interpolation end.
                            && !(previous_span.is_some() && line_is_first)
                        {
                            for c in line.chars() {
                                if !c.is_whitespace() {
                                    break;
                                }

                                let Some(indentation) = indentation else {
                                    indentation = Some(c);
                                    continue;
                                };

                                if !reported_mixed_indentation && indentation != c {
                                    reported_mixed_indentation = true;
                                    report.push_primary(
                                        self.span(),
                                        "strings cannot mix different kinds of whitespace in indentation",
                                    );
                                }
                            }
                        }
                    }
                },

                _ => {},
            }

            if !part.is_delimiter() {
                previous_span = Some(part.span());
            }

            if part_is_multiline {
                string_is_multiline = true;
            }
        }

        if string_is_multiline {
            for span in [string_first_line_span, string_last_line_span].into_iter().flatten() {
                report.push_primary(span, "multiline strings' first and last lines must be empty");
            }
        }

        if !report.is_empty() {
            to.push(report);
        }
    }
}

// RUNE

node! { #[from(NODE_RUNE)] struct Rune; }

impl Rune {
    parted!(token::Content);

    pub fn validate(&self, to: &mut Vec<Report>) {
        let mut report = Report::error("invalid rune");
        let mut reported_too_long = false;
        let mut reported_control_character = false;
        let mut reported_interpolation = false;

        let mut got_content = false;

        for part in self.parts() {
            match part {
                InterpolatedPartRef::Content(content) => {
                    let text = content.text();

                    if !reported_too_long && {
                        let mut parts = content.parts(&mut report);

                        match (parts.next().unwrap(), parts.next()) {
                            (ContentPart::Literal(text), None) if text.chars().count() == 1 => false,
                            (ContentPart::Escape(_), None) => false,

                            _ => true,
                        }
                    } {
                        reported_too_long = true;
                        report.push_primary(content.span(), "invalid rune literal length");
                    }

                    if !reported_control_character && text.chars().any(char::is_control) {
                        reported_control_character = true;
                        report.push_primary(
                            content.span(),
                            "runes cannot contain control characters (non-escaped newlines, tabs, ...)",
                        );
                    }
                },

                InterpolatedPartRef::Interpolation(interpolation) if !reported_interpolation => {
                    reported_interpolation = true;
                    report.push_primary(interpolation.span(), "runes cannot contain interpolation");
                },

                _ => continue,
            }

            got_content = true;
        }

        if !got_content {
            report.push_primary(self.span(), "runes cannot be empty");
        }

        if !report.is_empty() {
            to.push(report);
        }
    }
}

// ISLAND

node! { #[from(NODE_ISLAND)] struct Island; }

impl Island {
    parted!(token::Content);

    pub fn validate(&self, to: &mut Vec<Report>) {
        let mut report = Report::error("invalid island");
        let mut reported_control_character = false;

        for part in self.parts() {
            match part {
                InterpolatedPartRef::Content(content) => {
                    content.parts(&mut report).count();

                    let text = content.text();

                    if !reported_control_character && text.chars().any(char::is_control) {
                        reported_control_character = true;
                        report.push_primary(content.span(), "here");
                        report.push_tip("islands cannot contain control characters (non-escaped newlines, tabs, ...)");
                    }
                },

                InterpolatedPartRef::Interpolation(interpolation) => {
                    interpolation.validate(to);
                },

                _ => {},
            }
        }

        if !report.is_empty() {
            to.push(report)
        }
    }
}

// NUMBER

reffed! {
    /// A Number value. May either be a [`token::Integer`] or a [`token::Float`].
    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub enum NumberValue {
        Integer(token::Integer),
        Float(token::Float),
    }
}

node! { #[from(NODE_NUMBER)] struct Number; }

impl Number {
    /// Returns the underlying value of this number.
    pub fn value(&self) -> NumberValueRef<'_> {
        if let Some(token) = self
            .first_token()
            .and_then(|token| <&token::Integer>::try_from(token).ok())
        {
            return NumberValueRef::Integer(token);
        }

        if let Some(token) = self
            .first_token()
            .and_then(|token| <&token::Float>::try_from(token).ok())
        {
            return NumberValueRef::Float(token);
        }

        unreachable!()
    }

    #[allow(clippy::ptr_arg)]
    pub fn validate(&self, _to: &mut Vec<Report>) {}
}

// IF THEN

node! { #[from(NODE_IF_THEN)] struct IfThen; }

impl IfThen {
    get_token! { if_token -> TOKEN_LITERAL_IF }

    get_node! { condition -> 0 @ ExpressionRef<'_> }

    get_token! { then_token -> TOKEN_LITERAL_THEN }

    get_node! { consequence -> 1 @ ExpressionRef<'_> }

    get_token! { else_token -> Option<TOKEN_LITERAL_ELSE> }

    get_node! { alternative -> 2 @ Option<ExpressionRef<'_>> }

    pub fn validate(&self, to: &mut Vec<Report>) {
        self.condition().validate(to);
        self.consequence().validate(to);

        if let Some(alternative) = self.alternative() {
            alternative.validate(to);
        }
    }
}

// IF IS

node! { #[from(NODE_IF_IS)] struct IfIs; }

impl IfIs {
    get_token! { if_token -> TOKEN_LITERAL_IF }

    get_node! { expression -> 0 @ ExpressionRef<'_> }

    get_token! { is_token -> TOKEN_LITERAL_IS }

    get_node! { patterns -> 1 @ ExpressionRef<'_> }

    pub fn validate(&self, to: &mut Vec<Report>) {
        let mut report = Report::error("invalid if-is");

        self.expression().validate(to);

        for item in self.patterns().same_items() {
            match item {
                ExpressionRef::InfixOperation(operation) if let InfixOperator::Lambda = operation.operator() => {
                    operation.validate(to);
                },

                invalid => {
                    report.push_primary(invalid.span(), "invalid branch");
                    report.push_help("all if-is branches must be lambdas");
                },
            }
        }

        if !report.is_empty() {
            to.push(report);
        }
    }
}
