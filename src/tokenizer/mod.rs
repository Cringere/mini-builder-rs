//! Tokenization of a string.
//!
//! - [Tokenizer] - Performs the tokenization.
//! - [TokenizerOptions] - Various configurations for the [Tokenizer] for
//!                        example alternative symbols for some tokens.
//! - [Token] - A struct that describes a token - it contains the text of the
//!             token along with its type.
//! - [TokenType] - The type of the token.
//! - [TokenizerError] - The error that the [Tokenizer] will throw in case of
//!                      an error.
//!
//! # Example
//! ```rust
//! use mini_builder_rs::tokenizer::{Tokenizer, TokenizerOptions};
//!
//! let source = "is 1 bigger than 2? {{ 1 > 2 ? 'yes' : 'no' }}";
//! let tokens = Tokenizer::new(source, TokenizerOptions::default())
//!	    .tokenize()
//!	    .unwrap();
//! // tokens will be a vector of `Token<'a>`s where `'a` is the lifetime of
//! // `source`
//! ```

pub mod token;
pub mod tokenizer_error;

use std::collections::HashMap;

use hmap::hmap;
use lazy_static::lazy_static;
use regex::Regex;

use self::{
	token::{Token, TokenType},
	tokenizer_error::TokenizerError,
};

/// Used to configure the [Tokenizer].
#[derive(Debug, Clone)]
pub struct TokenizerOptions {
	open: String,
	close: String,
	pound: String,
	at: String,
}

impl TokenizerOptions {
	// TODO: proper error
	pub fn new(
		open: impl ToString,
		close: impl ToString,
		pound: impl ToString,
		at: impl ToString,
	) -> Result<Self, ()> {
		let open = open.to_string();
		let close = close.to_string();
		let pound = pound.to_string();
		let at = at.to_string();

		if open.is_empty() || close.is_empty() || pound.is_empty() {
			Err(())
		} else {
			Ok(Self {
				open,
				close,
				pound,
				at,
			})
		}
	}
}

impl std::default::Default for TokenizerOptions {
	fn default() -> Self {
		Self::new("{{", "}}", "#", "@").unwrap()
	}
}

/// Given a string slice, tokenizes it to a vector of [Token]s.
pub struct Tokenizer<'a> {
	/// The whole text that is being tokenized.
	_source: &'a str,
	/// The part of the text that hasn't been tokenized yet.
	left: &'a str,
	/// An index to a character of [_source]. Where [left] begins.
	location: usize,
	/// Options
	options: TokenizerOptions,
	// The parsed tokens.
	tokens: Vec<Token<'a>>,
	// Vector of all the tokens that have a specific symbol.
	static_symbol_tokens: Vec<(String, TokenType)>,
	// Identifiers with a specific name.
	reserved_words: HashMap<&'static str, TokenType>,
	// used for lookahead
	recorded: Vec<(usize, &'a str)>,
}

impl<'a> Tokenizer<'a> {
	/// Constructs a new tokenizer.
	pub fn new(source: &'a str, options: TokenizerOptions) -> Self {
		// create the vector of static tokens
		let mut static_symbol_tokens = [
			("?", TokenType::QuestionMark),
			(":", TokenType::Colon),
			(",", TokenType::Comma),
			(".", TokenType::Dot),
			("(", TokenType::LBracket),
			(")", TokenType::RBracket),
			("[", TokenType::SquareLBracket),
			("]", TokenType::SquareRBracket),
			("+", TokenType::Plus),
			("-", TokenType::Minus),
			("*", TokenType::Star),
			("/", TokenType::Slash),
			("||", TokenType::Or),
			("&&", TokenType::And),
			("!", TokenType::Not),
			("=", TokenType::Assign),
			(&options.open, TokenType::Open),
			(&options.close, TokenType::Close),
			(&options.pound, TokenType::Pound),
			(&options.at, TokenType::At),
			("==", TokenType::Equals),
			("!=", TokenType::NotEquals),
			(">", TokenType::GreaterThan),
			("<", TokenType::SmallerThan),
			(">=", TokenType::GreaterEquals),
			("<=", TokenType::SmallerEquals),
		]
		.map(|(key, value)| (key.to_string(), value))
		.into_iter()
		.collect::<Vec<_>>();
		static_symbol_tokens.sort_by_key(|item| item.0.len());
		static_symbol_tokens.reverse();

		let reserved_words = hmap! (
			"if" => TokenType::If,
			"else" => TokenType::Else,
			"elif" => TokenType::Elif,
			"for" => TokenType::For,
			"None" => TokenType::NoneLiteral
		);

		Self {
			_source: source,
			left: source,
			location: 0,
			options,
			tokens: Vec::new(),
			static_symbol_tokens,
			reserved_words,
			recorded: Vec::new(),
		}
	}

	fn advance(&mut self, len: usize) {
		self.left = &self.left[len..];
		self.location += len;
	}

	fn record(&mut self) {
		self.recorded.push((self.location, self.left));
	}

	fn restore(&mut self) {
		(self.location, self.left) = self.recorded.pop().unwrap();
	}

	fn push_and_advance(&mut self, tt: TokenType, len: usize) {
		if len > 0 {
			self.tokens
				.push(Token::new(tt, &self.left[..len], self.location));
			self.advance(len);
		}
	}

	/// Tokenizes the string
	pub fn tokenize(mut self) -> Result<Vec<Token<'a>>, TokenizerError> {
		loop {
			// find the next directive
			if let Some(l) = self.left.find(&self.options.open) {
				// push all the text before the directive as source
				self.push_and_advance(TokenType::Source, l);

				// tokenize the directive
				loop {
					if self.next_directive_token()? {
						break;
					}
				}
			} else {
				// there are no more directives left, push the rest as source and exit
				self.push_and_advance(TokenType::Source, self.left.len());
				break;
			}
		}

		Ok(self.tokens)
	}

	/// Tokenizes a token that is inside a directive. Returns true if the end of
	/// either the source or the directive was reached.
	fn next_directive_token(&mut self) -> Result<bool, TokenizerError> {
		// Regex of tokens that are defined with patterns
		lazy_static! {
			static ref IDENTIFIER_RE: Regex = Regex::new(r"^([_a-zA-Z][_a-zA-Z0-9]*)").unwrap();
			static ref NUMERICAL_LITERAL_RE: Regex = Regex::new(r"^([0-9]+)").unwrap();
			static ref FILE_PATH_RE: Regex = Regex::new(r"^\s*([0-9a-zA-z_\./]+)").unwrap();
		}

		if self.left.is_empty() {
			return Ok(true);
		}

		// ignore whitespaces
		if self.left.starts_with("\n\r") {
			self.advance(2);
			return Ok(false);
		}
		if self.left.starts_with(" ") || self.left.starts_with("\t") || self.left.starts_with("\n")
		{
			self.advance(1);
			return Ok(false);
		}

		// try finding a static token
		let found = self.static_symbol_tokens.iter().find_map(|(s, tt)| {
			if self.left.starts_with(s) {
				Some((*tt, s.len()))
			} else {
				None
			}
		});

		// if a static token was found, return it and check if its a close token
		if let Some((tt, len)) = found {
			self.push_and_advance(tt, len);

			// if found a `Close` token, then notify the caller the directive has ended
			if tt == TokenType::Close {
				return Ok(true);
			}

			// if a found an `At` token, then the following token must be a file name
			if tt == TokenType::At {
				// if found a match, push the file path token
				if let Some(captures) = FILE_PATH_RE.captures(self.left) {
					// get the entire matched text
					if let Some(m0) = captures.get(0) {
						// the length of the entire matched text
						let whole_length = m0.as_str().len();

						// get the captured text
						if let Some(m) = captures.get(1) {
							// the length of the captured text
							let len = m.as_str().len();

							// advance the whitespace prior to the match
							self.advance(whole_length - len);

							// push the file path and return
							self.push_and_advance(TokenType::FilePath, len);
							return Ok(false);
						}
					}
				}

				// else return an error
				return Err(TokenizerError::MustHaveFilePathAfterAt(
					self.location,
					self.options.at.clone(),
				));
			}

			return Ok(false);
		}

		// check string literals
		if self.left.starts_with("'") {
			let start_location = self.location;

			// match `'` as many times as possible - extract the delimiter
			let start_len = self.left.len();
			self.left = self.left.trim_start_matches("'");
			let delimiter_len = start_len - self.left.len();
			let delimiter = "'".repeat(delimiter_len);

			// match characters until the
			self.record();
			let mut string_len = 0;
			while !self.left.starts_with(&delimiter) {
				// if no more characters are left, return an unclosed string
				// error
				if self.left.len() == 0 {
					return Err(TokenizerError::UnclosedString(start_len));
				}
				self.advance(1);
				string_len += 1;
			}
			self.restore();

			// extract the content of the string literal and push it
			self.push_and_advance(TokenType::StringLiteral, string_len);

			// advance past the delimiter
			self.advance(delimiter_len);
			return Ok(false);
		}

		// check numerical literals
		if let Some(captures) = NUMERICAL_LITERAL_RE.captures(self.left) {
			let l = captures.get(0).unwrap().as_str().len();
			self.push_and_advance(TokenType::NumericalLiteral, l);
			return Ok(false);
		}

		// check identifiers
		if let Some(captures) = IDENTIFIER_RE.captures(self.left) {
			let s = captures.get(0).unwrap().as_str();
			let l = s.len();

			let (tt, l) = if let Some(tt) = self.reserved_words.get(s) {
				(*tt, l)
			} else {
				(TokenType::Identifier, l)
			};

			self.push_and_advance(tt, l);

			return Ok(false);
		}

		// if no token was pushed then an error has occurred
		return Err(TokenizerError::UnexpectedCharacter(self.location));
	}
}

#[cfg(test)]
mod tests {
	use crate::tokenizer::{TokenType, Tokenizer, TokenizerOptions};

	use super::Token;

	fn compare_tokens(tokens: &[Token], target_tokens: &[(TokenType, Option<&str>)]) {
		for (a, b) in tokens.into_iter().zip(target_tokens.into_iter()) {
			assert_eq!(a.tt, b.0, "testing: {} {:?}", a.content, b.1);
			if b.1.is_some() {
				assert_eq!(a.content, b.1.unwrap());
			}
		}
	}

	#[test]
	fn test_01() {
		let tokens = Tokenizer::new("abc{{abc}}", TokenizerOptions::default())
			.tokenize()
			.unwrap();

		let target_tokens = vec![
			(TokenType::Source, None),
			(TokenType::Open, None),
			(TokenType::Identifier, Some("abc")),
			(TokenType::Close, None),
		];

		compare_tokens(&tokens, &target_tokens);
	}

	#[test]
	fn test_02() {
		let tokens = Tokenizer::new("abc{{abc}}abc", TokenizerOptions::default())
			.tokenize()
			.unwrap();

		let target_tokens = vec![
			(TokenType::Source, None),
			(TokenType::Open, None),
			(TokenType::Identifier, Some("abc")),
			(TokenType::Close, None),
			(TokenType::Source, Some("abc")),
		];

		compare_tokens(&tokens, &target_tokens);
	}

	#[test]
	fn test_03() {
		let tokens = Tokenizer::new("{{>>>= ==<<<=}}", TokenizerOptions::default())
			.tokenize()
			.unwrap();

		let target_tokens = vec![
			// {{
			(TokenType::Open, None),
			// >>
			(TokenType::GreaterThan, None),
			(TokenType::GreaterThan, None),
			// >=
			(TokenType::GreaterEquals, None),
			// ==
			(TokenType::Equals, None),
			// <<
			(TokenType::SmallerThan, None),
			(TokenType::SmallerThan, None),
			// <=
			(TokenType::SmallerEquals, None),
			// }}
			(TokenType::Close, None),
		];

		compare_tokens(&tokens, &target_tokens);
	}

	#[test]
	fn test_04() {
		let tokens = Tokenizer::new(
			"{{ a > b ? 'item 1' : 'item 2' }}",
			TokenizerOptions::default(),
		)
		.tokenize()
		.unwrap();

		let target_tokens = vec![
			// {{
			(TokenType::Open, None),
			// a > b
			(TokenType::Identifier, Some("a")),
			(TokenType::GreaterThan, None),
			(TokenType::Identifier, Some("b")),
			// ?
			(TokenType::QuestionMark, None),
			// 'item 1' : 'item 2'
			(TokenType::StringLiteral, Some("item 1")),
			(TokenType::Colon, None),
			(TokenType::StringLiteral, Some("item 2")),
			// }}
			(TokenType::Close, None),
		];

		compare_tokens(&tokens, &target_tokens);
	}

	#[test]
	fn test_05() {
		let tokens = Tokenizer::new(
			"{{@ file(arg0 = arg, arg1 = 'txt', arg2 = 123 > num, arg3 = ''_'a'_'')}}",
			TokenizerOptions::default(),
		)
		.tokenize()
		.unwrap();

		let target_tokens = vec![
			// {{
			(TokenType::Open, None),
			// @ file
			(TokenType::At, None),
			(TokenType::FilePath, Some("file")),
			// file(
			(TokenType::LBracket, None),
			// arg0 = arg,
			(TokenType::Identifier, Some("arg0")),
			(TokenType::Assign, None),
			(TokenType::Identifier, Some("arg")),
			(TokenType::Comma, None),
			// arg1 = 'txt',
			(TokenType::Identifier, Some("arg1")),
			(TokenType::Assign, None),
			(TokenType::StringLiteral, Some("txt")),
			(TokenType::Comma, None),
			// arg2 = 123 > num,
			(TokenType::Identifier, Some("arg2")),
			(TokenType::Assign, None),
			(TokenType::NumericalLiteral, Some("123")),
			(TokenType::GreaterThan, None),
			(TokenType::Identifier, Some("num")),
			(TokenType::Comma, None),
			//  arg3 = ''_'a'_''
			(TokenType::Identifier, Some("arg3")),
			(TokenType::Assign, None),
			(TokenType::StringLiteral, Some("_'a'_")),
			// )}}
			(TokenType::RBracket, None),
			(TokenType::Close, None),
		];

		compare_tokens(&tokens, &target_tokens);
	}
}
