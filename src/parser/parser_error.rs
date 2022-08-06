use crate::tokenizer::token::TokenType;

/// When an invalid sequence of tokens is passed to the [super::Parser], it will
/// return a [ParserError].
#[derive(Debug)]
pub enum ParserError {
	/// Expected one thing but got something else. The type of thing that is
	/// being expected can be anything (not necessarily a token).
	ExpectedButFound(String, String),
	/// Expected something but reached the end of the tokens.
	ExpectedButReachedEnd(String),
	/// A different token type was expected.
	UnexpectedToken(usize, TokenType),
	// A directive was opened, but not closed (For example: `{{# if` ).
	UnclosedDirective,
}

impl std::fmt::Display for ParserError {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::ExpectedButFound(s1, s2) => write!(f, "expected: {s1}, but found: {s2}"),
			Self::ExpectedButReachedEnd(s) => write!(f, "expected: {s}, but reached the end"),
			Self::UnexpectedToken(token_location, token_type) => write!(
				f,
				"unexpected token at character {token_location}, got: {:?}",
				token_type
			),
			Self::UnclosedDirective => write!(f, "directive was not closed"),
		}
	}
}

impl std::error::Error for ParserError {}
