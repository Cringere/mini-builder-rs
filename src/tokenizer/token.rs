#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenType {
	/// anything that is not a directive
	Source,
	/// reserved symbols - characters
	QuestionMark,
	Colon,
	Comma,
	Dot,
	LBracket,
	RBracket,
	SquareLBracket,
	SquareRBracket,
	/// reserved symbols - arithmetics
	Plus,
	Minus,
	Star,
	Slash,
	/// reserved symbols - logical operators
	Or,
	And,
	Not,
	/// reserved symbols - assignments
	Assign,
	/// reserved symbols - directive definers
	Open,
	Close,
	Pound,
	At,
	/// reserved symbols - comparisons
	Equals,
	NotEquals,
	GreaterThan,
	SmallerThan,
	GreaterEquals,
	SmallerEquals,
	/// reserved words
	If,
	Else,
	Elif,
	For,
	/// identifiers and literals
	Identifier,
	FilePath,
	StringLiteral,
	NumericalLiteral,
	NoneLiteral,
}

impl TokenType {
	pub fn is_comparison_token(&self) -> bool {
		match *self {
			Self::Equals
			| Self::NotEquals
			| Self::GreaterThan
			| Self::SmallerThan
			| Self::GreaterEquals
			| Self::SmallerEquals => true,
			_ => false,
		}
	}
}

pub struct Token<'a> {
	pub tt: TokenType,
	pub content: &'a str,
	pub location: usize,
}

impl<'a> Token<'a> {
	pub fn new(tt: TokenType, content: &'a str, location: usize) -> Self {
		Self {
			tt,
			content: content,
			location,
		}
	}
}
