use crate::{
	parser::{node::NodeContent, parser_error::ParserError},
	tokenizer::tokenizer_error::TokenizerError,
};

/// An error that is return by [super::evaluator::EvaluatorBuilder] when an
/// invalid input passed.
#[derive(Debug)]
pub enum EvaluatorBuilderError {
	ExpectedOneOf(usize, Vec<String>),
	TokenizerError(TokenizerError),
	ParserError(ParserError),
	UnexpectedNodeContent(String),
}

impl std::fmt::Display for EvaluatorBuilderError {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::ExpectedOneOf(location, nodes) => write!(
				f,
				"unexpected node at {location}, expected one of: {:?}",
				nodes
			),
			Self::TokenizerError(tokenizer_error) => std::fmt::Display::fmt(&tokenizer_error, f),
			Self::ParserError(parser_error) => std::fmt::Display::fmt(&parser_error, f),
			Self::UnexpectedNodeContent(s) => {
				write!(f, "unexpected node content {:?}", s)
			}
		}
	}
}

impl std::error::Error for EvaluatorBuilderError {
	fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
		match self {
			Self::TokenizerError(tokenizer_error) => Some(tokenizer_error),
			Self::ParserError(parser_error) => Some(parser_error),
			_ => None,
		}
	}
}

impl From<TokenizerError> for EvaluatorBuilderError {
	fn from(tokenizer_error: TokenizerError) -> Self {
		Self::TokenizerError(tokenizer_error)
	}
}

impl From<ParserError> for EvaluatorBuilderError {
	fn from(parser_error: ParserError) -> Self {
		Self::ParserError(parser_error)
	}
}
