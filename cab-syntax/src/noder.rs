use std::result;

use enumset::{
    EnumSet,
    enum_set,
};
use miette::{
    Diagnostic,
    SourceOffset,
    SourceSpan,
};
use peekmore::{
    PeekMore as _,
    PeekMoreIterator as PeekMore,
};
use thiserror::Error;

use crate::{
    Kind::{
        self,
        *,
    },
    RowanNode,
    node::{
        self,
    },
};

/// A parse result that contains a [`rowan::SyntaxNode`],
/// contains a [`node::Node`] if it was successfully created,
/// and a list of [`NodeError`]s.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Parse<N: node::Node> {
    /// The underlying [`rowan::SyntaxNode`].
    pub syntax: RowanNode,

    /// The [`node::Node`], if it was successfully created.
    pub node: Option<N>,

    /// Errors encountered during parsing.
    pub errors: Vec<NodeError>,
}

impl<N: node::Node> Parse<N> {
    /// Returns [`Ok`] with the [`node::Node`] node if there are no errors,
    /// returns [`Err`] with the list of errors obtained from
    /// [`Self::errors`] otherwise.
    pub fn result(self) -> result::Result<N, Vec<NodeError>> {
        if self.errors.is_empty() {
            Ok(self.node.unwrap())
        } else {
            Err(self.errors)
        }
    }
}

/// Options related to parsing.
#[derive(Debug, Clone)]
pub struct ParseOptions {
    /// Whether to deduplicate errors. For every error with the same range, the
    /// first one will be kept.
    ///
    /// This means that the following string will only produce a single error,
    /// instead of two:
    ///
    /// ```txt
    /// [{ a := (b]
    ///           ^ expected ')', found ']'
    ///           ^ expected '}', found ']' (omitted if option is set)
    /// ```
    pub deduplicate_errors: bool,
}

impl Default for ParseOptions {
    fn default() -> Self {
        ParseOptions {
            deduplicate_errors: true,
        }
    }
}

/// Parses a token iterator and returns a [`Parse`].
///
/// Parsing will always fail if the given [`node::Node`] type is not an
/// expression as we cannot parse sub expressions.
pub fn parse<'a, I: Iterator<Item = (Kind, &'a str)>, N: node::Node>(
    tokens: I,
    options: ParseOptions,
) -> Parse<N> {
    let mut noder = Noder::new(tokens);

    noder.node(NODE_ROOT, |this| {
        this.node_expression(EnumSet::empty());

        if let Some(unexpected) = this.peek() {
            let start = this.offset;

            this.node(NODE_ERROR, |this| this.next_direct_while(|_| true));
            this.errors.push(NodeError::Unexpected {
                got: Some(unexpected),
                expected: EnumSet::empty(),
                at: rowan::TextRange::new(start, this.offset),
            });
        }
    });

    let syntax = RowanNode::new_root(noder.builder.finish());
    let node = N::cast(syntax.first_child().unwrap());
    let mut errors = noder.errors;

    // Handle unexpected node type. Happens when you
    // `parse::<node::Foo>(...)` and the input
    // contains a non-Foo expression.
    if node.is_none() {
        let node = syntax.first_child().unwrap();

        errors.push(NodeError::Unexpected {
            got: Some(node.kind()),
            expected: N::kind(),
            at: node.text_range(),
        })
    }

    if options.deduplicate_errors {
        let mut last_text_range = None;

        errors.retain(move |error| {
            let NodeError::Unexpected { at, .. } = error else {
                return true;
            };

            if last_text_range != Some(at.start()) {
                last_text_range = Some(at.start());
                true
            } else {
                false
            }
        })
    }

    // TODO: Temporary hack to check the string validation.
    if errors.is_empty()
        && syntax
            .first_child()
            .and_then(|child| child.first_token())
            .map(|token| token.kind())
            == Some(TOKEN_STRING_START)
    {
        use rowan::ast::AstNode as _;

        let sstring = node::SString::cast(syntax.first_child().unwrap()).unwrap();

        let contents: Vec<_> = sstring
            .parts()
            .filter_map(|part| {
                if let node::InterpolationPart::Content(content) = part {
                    Some(content)
                } else {
                    None
                }
            })
            .collect();

        if let Err(errors2) = crate::token::Content::normalized(&contents) {
            errors.extend(errors2);
        }
    }

    Parse {
        syntax,
        node,
        errors,
    }
}

const EXPRESSION_TOKENS: EnumSet<Kind> = enum_set!(
    TOKEN_LEFT_PARENTHESIS
        | TOKEN_LEFT_BRACKET
        | TOKEN_LEFT_CURLYBRACE
        | TOKEN_INTEGER
        | TOKEN_FLOAT
        | TOKEN_LITERAL_IF
        | TOKEN_PATH
        | TOKEN_IDENTIFIER
        | TOKEN_IDENTIFIER_START
        | TOKEN_STRING_START
        | TOKEN_ISLAND_START
);

const IDENTIFIER_TOKENS: EnumSet<Kind> = enum_set!(TOKEN_IDENTIFIER | TOKEN_IDENTIFIER_START);

#[derive(Debug, Clone, Error, Diagnostic)]
#[error("")]
pub struct NodeErrorWithContext {
    #[source_code]
    pub source_code: String,
    #[label("This bit here")]
    pub span: SourceSpan,
    #[source]
    pub node_error: NodeError,
}

impl NodeErrorWithContext {
    pub fn new(source_code: String, node_error: NodeError) -> NodeErrorWithContext {
        let span = node_error.span();
        NodeErrorWithContext {
            source_code,
            span: SourceSpan::new(SourceOffset::from(span.0), span.1 - span.0),
            node_error,
        }
    }
}

/// A node error.
#[derive(Debug, Clone, PartialEq, Eq, Error, Diagnostic)]
pub enum NodeError {
    /// An error that happens when the parsed expression is not a valid pattern.
    #[error("Invalid Pattern: {got:?}")]
    InvalidPattern {
        /// The node that was not a valid pattern.
        got: Option<Kind>,
        /// The range that contains the invalid node.
        at: rowan::TextRange,
    },

    #[error("Invalid String Like: {reason:?}")]
    // An error that happens when a stringlike contains invalid escapes or is formatted wrongly.
    InvalidStringlike {
        reason: &'static str,
        at: rowan::TextRange,
    },

    #[error("Unexpected: expected {expected:?}, got {}", match got {Some(x) => x.to_string(), None => "EOL".to_string()})]
    /// An error that happens when the noder was not expecting a particular
    /// token or node.
    Unexpected {
        /// The token that was not expected. This being [`None`] means that the
        /// end of the file was reached.
        got: Option<Kind>,
        /// The expected token. This being [`EnumSet::empty()`] means that the
        /// end of the file was expected, but there were leftovers.
        expected: EnumSet<Kind>,
        /// The range that contains the unexpected token sequence.
        at: rowan::TextRange,
    },
}

impl NodeError {
    fn span(&self) -> (usize, usize) {
        use NodeError::*;
        match self {
            InvalidPattern { at, .. } => (at.start().into(), at.end().into()),
            Unexpected { at, .. } => (at.start().into(), at.end().into()),
            InvalidStringlike { at, .. } => (at.start().into(), at.end().into()),
        }
    }
}

type Result<T> = result::Result<T, NodeError>;

struct Noder<'a, I: Iterator<Item = (Kind, &'a str)>> {
    builder: rowan::GreenNodeBuilder<'a>,

    tokens: PeekMore<I>,
    errors: Vec<NodeError>,

    offset: rowan::TextSize,
}

impl<'a, I: Iterator<Item = (Kind, &'a str)>> Noder<'a, I> {
    fn new(tokens: I) -> Self {
        Self {
            builder: rowan::GreenNodeBuilder::new(),

            tokens: tokens.peekmore(),
            errors: Vec::new(),

            offset: 0.into(),
        }
    }

    fn checkpoint(&mut self) -> rowan::Checkpoint {
        self.next_while_trivia();
        self.builder.checkpoint()
    }

    fn node<T>(&mut self, kind: Kind, closure: impl FnOnce(&mut Self) -> T) -> T {
        self.builder.start_node(kind.into());

        let result = closure(self);

        self.builder.finish_node();
        result
    }

    fn node_failable<K>(&mut self, kind: Kind, closure: impl FnOnce(&mut Self) -> Result<K>) {
        let start_of_node = self.checkpoint();

        if let Err(error) = self.node(kind, closure) {
            self.node_from(start_of_node, NODE_ERROR, |_| {});
            self.errors.push(error);
        }
    }

    fn node_from<T>(
        &mut self,
        checkpoint: rowan::Checkpoint,
        kind: Kind,
        closure: impl FnOnce(&mut Self) -> T,
    ) -> T {
        self.builder.start_node_at(checkpoint, kind.into());

        let result = closure(self);

        self.builder.finish_node();
        result
    }

    fn node_failable_from<K>(
        &mut self,
        checkpoint: rowan::Checkpoint,
        kind: Kind,
        closure: impl FnOnce(&mut Self) -> Result<K>,
    ) {
        if let Err(error) = self.node_from(checkpoint, kind, closure) {
            self.node_from(checkpoint, NODE_ERROR, |_| {});
            self.errors.push(error);
        }
    }

    fn peek_direct(&mut self) -> Option<Kind> {
        self.tokens.peek().map(|&(kind, _)| kind)
    }

    fn peek_direct_expecting(&mut self, expected: EnumSet<Kind>) -> Result<Kind> {
        self.peek_direct().ok_or(NodeError::Unexpected {
            got: None,
            expected,
            at: rowan::TextRange::empty(self.offset),
        })
    }

    fn peek_nth(&mut self, n: usize) -> Option<Kind> {
        let mut peek_index: usize = 0;
        let mut index: usize = 0;

        loop {
            let &(kind, _) = self.tokens.peek_nth(peek_index)?;

            if index >= n && !kind.is_trivia() {
                return Some(kind);
            }

            peek_index += 1;

            if !kind.is_trivia() {
                index += 1;
            }
        }
    }

    fn peek(&mut self) -> Option<Kind> {
        self.peek_nth(0)
    }

    fn peek_expecting(&mut self, expected: EnumSet<Kind>) -> Result<Kind> {
        self.peek().ok_or(NodeError::Unexpected {
            got: None,
            expected,
            at: rowan::TextRange::empty(self.offset),
        })
    }

    fn next_direct(&mut self) -> Result<Kind> {
        match self.tokens.next() {
            Some((kind, slice)) => {
                self.offset += rowan::TextSize::of(slice);
                self.builder.token(kind.into(), slice);

                Ok(kind)
            },

            None => {
                Err(NodeError::Unexpected {
                    got: None,
                    expected: EnumSet::empty(),
                    at: rowan::TextRange::empty(self.offset),
                })
            },
        }
    }

    fn next_direct_while(&mut self, mut predicate: impl FnMut(Kind) -> bool) {
        while self.peek_direct().is_some_and(&mut predicate) {
            self.next_direct().unwrap();
        }
    }

    fn next_while_trivia(&mut self) {
        self.next_direct_while(Kind::is_trivia)
    }

    fn next(&mut self) -> Result<Kind> {
        self.next_while_trivia();
        self.next_direct()
    }

    fn next_if(&mut self, expected: Kind) -> bool {
        let condition = self.peek() == Some(expected);

        if condition {
            self.next().unwrap();
        }

        condition
    }

    fn next_while(&mut self, mut predicate: impl FnMut(Kind) -> bool) {
        while self.peek().is_some_and(&mut predicate) {
            self.next().unwrap();
        }
    }

    fn next_expect(
        &mut self,
        expected: EnumSet<Kind>,
        until: EnumSet<Kind>,
    ) -> Result<Option<Kind>> {
        let expected_at = self.checkpoint();

        match self.peek_expecting(expected)? {
            next if expected.contains(next) => self.next().map(Some),

            unexpected => {
                let start = self.offset;

                self.node_from(expected_at, NODE_ERROR, |this| {
                    let boundary = until | expected;

                    this.next_while(|next| !boundary.contains(next));
                });

                let error = NodeError::Unexpected {
                    got: Some(unexpected),
                    expected,
                    at: rowan::TextRange::new(start, self.offset),
                };

                if let Some(next) = self.peek() {
                    self.errors.push(error);

                    if expected.contains(next) {
                        self.next().map(Some)
                    } else {
                        Ok(None)
                    }
                } else {
                    Err(error)
                }
            },
        }
    }

    fn node_parenthesis(&mut self, until: EnumSet<Kind>) {
        self.node_failable(NODE_PARENTHESIS, |this| {
            this.next_expect(
                TOKEN_LEFT_PARENTHESIS.into(),
                until | EXPRESSION_TOKENS | TOKEN_RIGHT_PARENTHESIS,
            )?;

            this.node_expression(until | TOKEN_RIGHT_PARENTHESIS);

            this.next_expect(TOKEN_RIGHT_PARENTHESIS.into(), until)
        });
    }

    fn node_list(&mut self, until: EnumSet<Kind>) {
        self.node_failable(NODE_LIST, |this| {
            this.next_expect(
                TOKEN_LEFT_BRACKET.into(),
                until | EXPRESSION_TOKENS | TOKEN_RIGHT_BRACKET,
            )?;

            if this.peek() != Some(TOKEN_RIGHT_BRACKET) {
                this.node_expression(until | TOKEN_RIGHT_BRACKET);
            }

            this.next_expect(TOKEN_RIGHT_BRACKET.into(), until)
        });
    }

    fn node_attribute_list(&mut self, until: EnumSet<Kind>) {
        self.node_failable(NODE_ATTRIBUTE_LIST, |this| {
            this.next_expect(
                TOKEN_LEFT_CURLYBRACE.into(),
                until | EXPRESSION_TOKENS | TOKEN_RIGHT_CURLYBRACE,
            )?;

            if this.peek() != Some(TOKEN_RIGHT_CURLYBRACE) {
                this.node_expression(until | TOKEN_RIGHT_CURLYBRACE);
            }

            this.next_expect(TOKEN_RIGHT_CURLYBRACE.into(), until)
        });
    }

    fn node_path(&mut self) {
        self.node_failable(NODE_PATH, |this| {
            loop {
                match this.peek_direct() {
                    Some(TOKEN_PATH) => {
                        this.next_direct().unwrap();
                    },

                    Some(TOKEN_INTERPOLATION_START) => {
                        this.node_interpolation()?;
                    },

                    _ => break Ok(()),
                }
            }
        });
    }

    fn node_identifier(&mut self, until: EnumSet<Kind>) {
        if self.peek() == Some(TOKEN_IDENTIFIER_START) {
            self.node_stringlike();
        } else {
            self.node_failable(NODE_IDENTIFIER, |this| {
                this.next_expect(IDENTIFIER_TOKENS, until)
            });
        }
    }

    fn node_stringlike(&mut self) {
        let start_of_stringlike = self.checkpoint();

        let (node, end) = match self.next() {
            Ok(TOKEN_IDENTIFIER_START) => (NODE_IDENTIFIER, TOKEN_IDENTIFIER_END),
            Ok(TOKEN_STRING_START) => (NODE_STRING, TOKEN_STRING_END),
            Ok(TOKEN_ISLAND_START) => (NODE_ISLAND, TOKEN_ISLAND_END),
            _ => unreachable!(),
        };

        self.node_failable_from(start_of_stringlike, node, |this| {
            loop {
                match this.peek_direct_expecting(TOKEN_CONTENT | end)? {
                    TOKEN_CONTENT => {
                        this.next_direct().unwrap();
                    },

                    TOKEN_INTERPOLATION_START => {
                        this.node_interpolation()?;
                    },

                    other if other == end => {
                        this.next_direct().unwrap();
                        break Ok(());
                    },

                    _ => {
                        // Sometimes recoverably parsing interpolation leaves us unwanted tokens. It
                        // is not worth it trying to node it correctly without a big rewrite, so
                        // just consume them.
                        this.next_direct().unwrap();
                    },
                }
            }
        });
    }

    fn node_interpolation(&mut self) -> Result<()> {
        self.node(NODE_INTERPOLATION, |this| {
            this.next_expect(TOKEN_INTERPOLATION_START.into(), EnumSet::empty())?;

            this.node_expression(TOKEN_INTERPOLATION_END.into());

            this.next_expect(TOKEN_INTERPOLATION_END.into(), EnumSet::empty())?;

            Ok(())
        })
    }

    fn node_number(&mut self, until: EnumSet<Kind>) {
        self.node_failable(NODE_NUMBER, |this| {
            this.next_expect(TOKEN_INTEGER | TOKEN_FLOAT, until)
        });
    }

    fn node_if(&mut self, until: EnumSet<Kind>) {
        let is_binding_power = node::InfixOperator::Sequence.binding_power().0 + 1;
        let then_else_binding_power = node::InfixOperator::Same.binding_power().0 + 1;

        let start_of_if = self.checkpoint();

        let differentiator_token = try {
            self.next_expect(
                TOKEN_LITERAL_IF.into(),
                until
                    | EXPRESSION_TOKENS
                    | TOKEN_LITERAL_IS
                    | TOKEN_LITERAL_THEN
                    | TOKEN_LITERAL_ELSE,
            )?;

            self.node_expression_binding_power(
                then_else_binding_power,
                until | TOKEN_LITERAL_IS | TOKEN_LITERAL_THEN | TOKEN_LITERAL_ELSE,
            );

            self.next_expect(
                TOKEN_LITERAL_IS | TOKEN_LITERAL_THEN,
                until | TOKEN_LITERAL_ELSE,
            )?
        };

        match differentiator_token {
            Ok(Some(TOKEN_LITERAL_IS)) => {
                self.node_from(start_of_if, NODE_IF_IS, |this| {
                    this.node_expression_binding_power(is_binding_power, until)
                })
            },

            Ok(Some(TOKEN_LITERAL_THEN)) => {
                self.node_failable_from(start_of_if, NODE_IF_ELSE, |this| {
                    this.node_expression_binding_power(
                        then_else_binding_power,
                        until | TOKEN_LITERAL_ELSE,
                    );

                    if this.next_if(TOKEN_LITERAL_ELSE) {
                        this.node_expression_binding_power(then_else_binding_power, until);
                    }

                    Ok(())
                });
            },

            // Scummy? Probably. But let's just assume this is an if.
            error => self.node_failable_from(start_of_if, NODE_IF_ELSE, |_| error),
        }
    }

    fn node_expression_single(&mut self, until: EnumSet<Kind>) {
        match self.peek_expecting(EXPRESSION_TOKENS) {
            Ok(TOKEN_LEFT_PARENTHESIS) => self.node_parenthesis(until),

            Ok(TOKEN_LEFT_BRACKET) => self.node_list(until),

            Ok(TOKEN_LEFT_CURLYBRACE) => self.node_attribute_list(until),

            Ok(TOKEN_PATH) => self.node_path(),

            Ok(next) if IDENTIFIER_TOKENS.contains(next) => self.node_identifier(until),

            Ok(TOKEN_STRING_START | TOKEN_ISLAND_START) => self.node_stringlike(),

            Ok(TOKEN_INTEGER | TOKEN_FLOAT) => self.node_number(until),

            Ok(TOKEN_LITERAL_IF) => self.node_if(until),

            unexpected => {
                self.next_while_trivia();
                let start = self.offset;

                self.node(NODE_ERROR, |this| {
                    // Consume until the next token is either the limit, an expression token or
                    // an operator.
                    this.next_while(|kind| {
                        !(until.contains(kind)
                            || EXPRESSION_TOKENS.contains(kind)
                            || node::PrefixOperator::try_from(kind).is_ok()
                            || node::InfixOperator::try_from(kind)
                                .is_ok_and(|operator| operator.is_token_owning())
                            || node::SuffixOperator::try_from(kind).is_ok())
                    });
                });

                self.errors.push(match unexpected {
                    Ok(next) => {
                        NodeError::Unexpected {
                            got: Some(next),
                            expected: EXPRESSION_TOKENS,
                            at: rowan::TextRange::new(start, self.offset),
                        }
                    },

                    Err(error) => error,
                });
            },
        }
    }

    fn node_expression_binding_power(&mut self, minimum_power: u16, until: EnumSet<Kind>) {
        let start_of_expression = self.checkpoint();

        if let Some(operator) = self
            .peek()
            .and_then(|kind| node::PrefixOperator::try_from(kind).ok())
        {
            let ((), right_power) = operator.binding_power();

            self.node(NODE_PREFIX_OPERATION, |this| {
                this.next().unwrap();
                this.node_expression_binding_power(right_power, until);
            });
        } else {
            self.node_expression_single(until);
        }

        while let Some(operator) = self
            .peek()
            .and_then(|kind| node::InfixOperator::try_from(kind).ok())
        {
            let (left_power, right_power) = operator.binding_power();
            if left_power < minimum_power {
                break;
            }

            let operator_token = operator.is_token_owning().then(|| self.next().unwrap());

            // Handle suffix-able infix operators. Not for purely suffix operators.
            if operator_token.is_some()
                && node::SuffixOperator::try_from(operator_token.unwrap()).is_ok()
                && self
                    .peek()
                    .is_none_or(|kind| !EXPRESSION_TOKENS.contains(kind))
            {
                self.node_from(start_of_expression, NODE_SUFFIX_OPERATION, |_| {});
            } else {
                self.node_from(start_of_expression, NODE_INFIX_OPERATION, |this| {
                    this.node_expression_binding_power(right_power, until);
                });
            }
        }
    }

    fn node_expression(&mut self, until: EnumSet<Kind>) {
        self.node_expression_binding_power(0, until);
    }
}
