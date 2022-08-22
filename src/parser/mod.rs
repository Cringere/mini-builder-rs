//! Converts a sequence of tokens into an AST (abstract syntax tree).
//!
//! The AST defines how tokens are interpreted and how they relate to others.
//! For example consider the following tokens: `1 + 2 * 3`.
//! The parser will use its hardcoded rules of order of operations to create
//! the following tree:
//!           +
//!          / \
//!         1   *
//!            / \
//!           2   3
//! The same principles apply to more complicated examples as well.
//! Keep in mind that the AST isn't necessarily a binary tree!
//!
//! # [Node]
//! The nodes of the AST are defined using the [Node] struct. There are three
//! types of nodes:
//! - Source - The source node holds a string slice to the source text (the
//!            text that was used to create the tokens).
//! - Expression - The expression node holds an expression - a sequence of
//!                operations that return a [Value].
//! - Control Flow - Nodes that decide which nodes are executed and which are
//!                  not.
//!
//! # Example
//! ```rust
//! use mini_builder_rs::{
//!     tokenizer::{Tokenizer, TokenizerOptions},
//! 	parser::Parser,
//! };
//!
//! let source = "is 1 bigger than 2? {{ 1 > 2 ? 'yes' : 'no' }}";
//! let tokens = Tokenizer::new(source, TokenizerOptions::default())
//! 	.tokenize()
//! 	.unwrap();
//! let tokens = tokens.as_slice();
//! let nodes = Parser::new(tokens).parse().unwrap();
//! // nodes is the root of the parsed AST
//! ```

pub mod expression;
pub mod node;
pub mod parser_error;

use crate::{
	tokenizer::token::{Token, TokenType},
	value::Value,
};

use self::{
	expression::{Expression, Expressions, NamedExpressions},
	node::{Node, Nodes},
	parser_error::ParserError,
};

type ExpressionResult<'a> = Result<Expression<'a>, ParserError>;
type NodeResult<'a> = Result<Node<'a>, ParserError>;
pub type NodesResult<'a> = Result<Nodes<'a>, ParserError>;

/// Creates an AST from a sequence of tokens
pub struct Parser<'a, 'b> {
	tokens: &'b [Token<'a>],
	i: usize,
}

impl<'a, 'b> Parser<'a, 'b> {
	pub fn new(tokens: &'b [Token<'a>]) -> Self {
		Self { tokens, i: 0 }
	}

	/// Produces a `ParserError::UnexpectedToken` from the current token
	fn unexpected_token_error(&self) -> ParserError {
		let token = &self.tokens[self.i];
		ParserError::UnexpectedToken(token.location, token.tt)
	}

	/// Consumes and returns a token. If there are non returns None.
	fn have_token(&mut self) -> Option<&'b Token<'a>> {
		let ret = self.tokens.get(self.i);
		self.advance();
		ret
	}

	/// Returns the type of the next token, if there are no more, returns None
	fn peek_type(&self) -> Option<TokenType> {
		self.tokens.get(self.i).map(|t| t.tt.clone())
	}

	/// Consumes the next token only if it is of the type `tt`
	fn have_specific_type(&mut self, tt: TokenType) -> bool {
		if let Some(peeked_tt) = self.peek_type() {
			if peeked_tt == tt {
				self.advance();
				true
			} else {
				false
			}
		} else {
			false
		}
	}

	/// Returns an error if the next token is not of the expected type
	fn expect_type(&self, tt: TokenType) -> Result<(), ParserError> {
		if let Some(peeked_tt) = self.peek_type() {
			if peeked_tt == tt {
				Ok(())
			} else {
				Err(ParserError::ExpectedButFound(
					format!("{:?}", tt),
					format!("{:?}", peeked_tt),
				))
			}
		} else {
			Err(ParserError::ExpectedButReachedEnd(format!("{:?}", tt)))
		}
	}

	/// Consumes the next token and returns its content
	fn have_content(&mut self) -> &'a str {
		self.i += 1;
		self.tokens[self.i - 1].content
	}

	/// Returns the token's type, if reached the end returns an error
	fn peek_type_expect(&self) -> Result<TokenType, ParserError> {
		self.peek_type()
			.ok_or(ParserError::ExpectedButReachedEnd(format!("")))
	}

	/// Consumes the next token
	fn advance(&mut self) {
		self.i += 1;
	}

	/// Helper function for parsing binary expression
	fn parse_binary_expression<F, G>(
		&mut self,
		op: TokenType,
		parse_sub_expression: F,
		package: G,
	) -> ExpressionResult<'a>
	where
		F: Fn(&mut Self) -> ExpressionResult<'a>,
		G: Fn(Expressions) -> Expression,
	{
		// parse the current seen expression
		let expr = parse_sub_expression(self)?;

		// if the operation symbol is the next token, parse that following expression
		// and package them as the desired expression.
		// parsing: <expr> [<symbol> <expr>]*
		if Some(op) == self.peek_type() {
			let mut expressions = vec![Box::new(expr)];
			while self.have_specific_type(op) {
				expressions.push(Box::new(parse_sub_expression(self)?));
			}
			Ok(package(expressions))
		} else {
			Ok(expr)
		}
	}

	/// Helper function for parsing binary expression where two operations have the same order of operations
	fn parse_binary_expression_2<'e, F, G>(
		&mut self,
		op_a: TokenType,
		op_b: TokenType,
		parse_sub_expression: F,
		package: G,
	) -> ExpressionResult<'a>
	where
		F: Fn(&mut Self) -> ExpressionResult<'a>,
		G: Fn(Expressions<'a>, Expressions<'a>) -> Expression<'a>,
	{
		// parse the current seen expression
		let expr = parse_sub_expression(self)?;

		// if the operation symbol is the next token, parse that following expression
		// and package them as the desired expression.
		// parsing: <expr> [<symbol> <expr>]*
		if let Some(peeked_tt) = self.peek_type() {
			if peeked_tt == op_a || peeked_tt == op_b {
				let mut expressions_a = vec![Box::new(expr)];
				let mut expressions_b = vec![];
				// parse [<symbol> <expr>]*
				loop {
					match self.peek_type() {
						Some(tt) => {
							if tt == op_a {
								self.advance();
								expressions_a.push(Box::new(parse_sub_expression(self)?));
							} else if tt == op_b {
								self.advance();
								expressions_b.push(Box::new(parse_sub_expression(self)?));
							} else {
								break;
							}
						}
						_ => break,
					}
				}
				Ok(package(expressions_a, expressions_b))
			} else {
				Ok(expr)
			}
		} else {
			Ok(expr)
		}
	}

	fn parser_named_argument(&mut self) -> Result<Box<(&'a str, Expression<'a>)>, ParserError> {
		// the name of the argument
		self.expect_type(TokenType::Identifier)?;
		let name = self.have_content();

		// =
		self.expect_type(TokenType::Assign)?;
		self.advance();

		// the expression (will produce the value of the argument)
		let expression = self.parse_expression()?;

		Ok(Box::new((name, expression)))
	}

	fn parse_named_arguments(
		&mut self,
	) -> Result<Vec<Box<(&'a str, Expression<'a>)>>, ParserError> {
		self.expect_type(TokenType::LBracket)?;
		self.advance();

		if self.have_specific_type(TokenType::RBracket) {
			// empty arguments
			Ok(Vec::new())
		} else {
			// allocate buffer and parse the first argument
			let mut args: NamedExpressions = Vec::new();
			args.push(self.parser_named_argument()?);

			// parse the rest of the arguments: [,arg = value]*
			loop {
				if let Some(peeked_tt) = self.peek_type() {
					match peeked_tt {
						// ) - reached the end of the arguments
						TokenType::RBracket => {
							self.advance();
							break;
						}
						// identifier - parse an argument
						TokenType::Comma => {
							self.advance();
							args.push(self.parser_named_argument()?);
						}
						_ => return Err(self.unexpected_token_error()),
					}
				} else {
					return Err(ParserError::ExpectedButReachedEnd(format!(
						"identifier or ','"
					)));
				}
			}

			Ok(args)
		}
	}

	fn parse_arguments(&mut self) -> Result<Vec<Box<Expression<'a>>>, ParserError> {
		self.expect_type(TokenType::LBracket)?;
		self.advance();

		if self.have_specific_type(TokenType::RBracket) {
			// empty arguments
			Ok(Vec::new())
		} else {
			// allocate buffer and parse the first argument
			let mut args: Expressions = Vec::new();
			args.push(Box::new(self.parse_expression()?));

			// parse the rest of the arguments (expressions)
			loop {
				if let Some(peeked_tt) = self.peek_type() {
					match peeked_tt {
						// ) - reached the end of the arguments
						TokenType::RBracket => {
							self.advance();
							break;
						}
						// expression - parse an argument
						TokenType::Comma => {
							self.advance();
							args.push(Box::new(self.parse_expression()?));
						}
						_ => return Err(self.unexpected_token_error()),
					}
				} else {
					return Err(ParserError::ExpectedButReachedEnd(format!(
						"expression or ','"
					)));
				}
			}

			Ok(args)
		}
	}

	fn parse_builder(&mut self) -> ExpressionResult<'a> {
		// at
		self.expect_type(TokenType::At)?;
		self.advance();

		// builder name
		self.expect_type(TokenType::FilePath)?;
		let name = self.have_content();

		// if see a `(`, parse arguments, else assume there are non
		let args = if Some(TokenType::LBracket) == self.peek_type() {
			self.parse_named_arguments()?
		} else {
			Vec::new()
		};

		Ok(Expression::Builder(name, args))
	}

	fn parse_function_call_or_variable(&mut self) -> ExpressionResult<'a> {
		// name
		self.expect_type(TokenType::Identifier)?;
		let name = self.have_content();

		// if the name is followed by an open bracket then this is a function call
		if Some(TokenType::LBracket) == self.peek_type() {
			// (arg0 = value0, arg1 = value1, ...)
			let args = self.parse_arguments()?;
			Ok(Expression::FunctionCall(name, args))
		} else {
			Ok(Expression::Variable(name))
		}
	}

	fn parse_unary_expression(
		&mut self,
		seen_negation: bool,
		seen_not: bool,
	) -> ExpressionResult<'a> {
		if let Some(peeked_tt) = self.peek_type() {
			match peeked_tt {
				// call to another file
				TokenType::At => self.parse_builder(),
				// function call or a variable
				TokenType::Identifier => self.parse_function_call_or_variable(),
				// negate and not
				TokenType::Minus if !seen_negation => {
					self.advance();
					Ok(Expression::Negate(Box::new(
						self.parse_unary_expression(true, seen_not)?,
					)))
				}
				TokenType::Not if !seen_not => {
					self.advance();
					Ok(Expression::Not(Box::new(
						self.parse_unary_expression(seen_negation, true)?,
					)))
				}
				// expression inside brackets
				TokenType::LBracket => {
					self.advance();
					let expression = self.parse_expression()?;
					self.expect_type(TokenType::RBracket)?;
					self.advance();
					Ok(expression)
				}
				// list
				TokenType::SquareLBracket => {
					self.advance();
					if Some(TokenType::SquareRBracket) == self.peek_type() {
						// empty
						self.advance();
						Ok(Expression::ListValue(Vec::new()))
					} else {
						// first expression
						let mut expressions = vec![Box::new(self.parse_expression()?)];

						// rest of the expression
						loop {
							if Some(TokenType::SquareRBracket) == self.peek_type() {
								self.advance();
								break;
							}
							self.expect_type(TokenType::Comma)?;
							self.advance();
							expressions.push(Box::new(self.parse_expression()?));
						}

						Ok(Expression::ListValue(expressions))
					}
				}
				// literals
				TokenType::StringLiteral => Ok(Expression::Value(Value::Text(
					self.have_content().to_string(),
				))),
				TokenType::NumericalLiteral => Ok(Expression::Value(Value::Number(
					self.have_content().parse::<f32>().unwrap(),
				))),
				TokenType::NoneLiteral => Ok(Expression::Value(Value::None)),
				// if none of the above matched, then the token type is invalid
				_ => Err(self.unexpected_token_error()),
			}
		} else {
			Err(ParserError::ExpectedButReachedEnd(format!("expression")))
		}
	}

	fn parse_index_of(&mut self) -> ExpressionResult<'a> {
		let mut expression = self.parse_unary_expression(false, false)?;
		while Some(TokenType::SquareLBracket) == self.peek_type() {
			self.advance();
			expression =
				Expression::IndexOf(Box::new(expression), Box::new(self.parse_expression()?));
			self.expect_type(TokenType::SquareRBracket)?;
			self.advance();
		}
		Ok(expression)
	}

	fn parse_multiplicative_expression(&mut self) -> ExpressionResult<'a> {
		self.parse_binary_expression_2(
			TokenType::Star,
			TokenType::Slash,
			Self::parse_index_of,
			|expressions_a, expressions_b| Expression::Multiplicative(expressions_a, expressions_b),
		)
	}

	fn parse_additive_expression(&mut self) -> ExpressionResult<'a> {
		self.parse_binary_expression_2(
			TokenType::Plus,
			TokenType::Minus,
			Self::parse_multiplicative_expression,
			|expressions_a, expressions_b| Expression::Additive(expressions_a, expressions_b),
		)
	}

	fn parse_comparison_expression(&mut self) -> ExpressionResult<'a> {
		// parse the current seen expression
		let expr = self.parse_additive_expression()?;

		if let Some(tt) = self.peek_type() {
			if tt.is_comparison_token() {
				self.advance();
				let rhs = self.parse_additive_expression()?;
				return Ok(Expression::Comparison(Box::new(expr), tt, Box::new(rhs)));
			}
		}

		Ok(expr)
	}

	fn parse_or_expression(&mut self) -> ExpressionResult<'a> {
		self.parse_binary_expression(
			TokenType::Or,
			Self::parse_comparison_expression,
			|expressions| Expression::Or(expressions),
		)
	}

	fn parse_and_expression(&mut self) -> ExpressionResult<'a> {
		self.parse_binary_expression(TokenType::And, Self::parse_or_expression, |expressions| {
			Expression::And(expressions)
		})
	}

	fn parse_ternary(&mut self) -> ExpressionResult<'a> {
		let expression = self.parse_and_expression()?;

		// if the expression is followed by a question mark, then it is a ternary
		if let Some(TokenType::QuestionMark) = self.peek_type() {
			// consume the question mark
			self.advance();

			// parse the first expression
			let a = self.parse_expression()?;

			// parse `:`
			self.expect_type(TokenType::Colon)?;
			self.advance();

			// prase the second expression and return
			let b = self.parse_expression()?;
			Ok(Expression::Ternary(
				Box::new(expression),
				Box::new(a),
				Box::new(b),
			))
		} else {
			Ok(expression)
		}
	}

	fn parse_expression(&mut self) -> ExpressionResult<'a> {
		self.parse_ternary()
	}

	fn see_assignment(&self) -> bool {
		if let (Some(TokenType::Identifier), Some(TokenType::Assign)) = (
			self.tokens.get(self.i).map(|t| t.tt),
			self.tokens.get(self.i + 1).map(|t| t.tt),
		) {
			true
		} else {
			false
		}
	}

	fn parse_assignment(&mut self) -> (&'a str, ExpressionResult<'a>) {
		let name = self.have_content();
		if !self.have_specific_type(TokenType::Assign) {
			todo!("error - expected =")
		}
		(name, self.parse_expression())
	}

	fn parse_directive(&mut self) -> NodeResult<'a> {
		self.expect_type(TokenType::Open)?;
		self.advance();

		// check if the directive is a function
		let location = self.i;
		match self.peek_type_expect()? {
			TokenType::Pound => {
				self.advance();

				let location = self.i;

				match self.peek_type_expect()? {
					TokenType::If => {
						// consume the `if` token
						self.advance();

						// parse the expression
						let expression = self.parse_expression()?;

						// expect and consume the `Close` token
						self.expect_type(TokenType::Close)?;
						self.advance();
						Ok(Node::if_node(location, expression))
					}
					TokenType::Elif => {
						// consume the `elif` token
						self.advance();

						// parse the expression
						let expression = self.parse_expression()?;

						// expect and consume the `Close` token
						self.expect_type(TokenType::Close)?;
						self.advance();

						Ok(Node::elif(location, expression))
					}
					TokenType::Else => {
						// consume the `Else` token
						self.advance();

						// expect and consume the `Close` token
						self.expect_type(TokenType::Close)?;
						self.advance();

						Ok(Node::else_node(location))
					}
					TokenType::For => {
						// consume the token
						self.advance();

						// name of the variable that will hold the list's values
						self.expect_type(TokenType::Identifier)?;
						let name = self.have_content();

						// the 'in' symbol
						self.expect_type(TokenType::Colon)?;
						self.advance();

						// the list
						let l = self.parse_expression()?;

						// expect and consume the `Close` token
						self.expect_type(TokenType::Close)?;
						self.advance();

						// construct and return
						Ok(Node::for_node(location, name, l))
					}
					TokenType::Close => {
						// consume the token and return
						self.advance();
						Ok(Node::end(location))
					}
					_ => Err(self.unexpected_token_error()),
				}
			}
			_ => {
				// parse either an assignment or an expression
				let node = if self.see_assignment() {
					let (name, expression) = self.parse_assignment();
					Node::assignment(location, name, expression?)
				} else {
					Node::expression(location, self.parse_expression()?)
				};

				// consume the `Close` token and return
				self.expect_type(TokenType::Close)?;
				self.advance();
				Ok(node)
			}
		}
	}

	pub fn parse(mut self) -> NodesResult<'a> {
		let mut nodes = Vec::new();
		while let Some(tt) = self.peek_type() {
			match tt {
				TokenType::Source => {
					nodes.push(Box::new(Node::source_from_token(
						self.have_token().unwrap(),
					)));
				}
				TokenType::Open => nodes.push(Box::new(self.parse_directive()?)),
				_ => return Err(self.unexpected_token_error()),
			}
		}
		Ok(nodes)
	}
}

#[cfg(test)]
mod tests {
	use crate::{
		parser::Parser,
		tokenizer::{Tokenizer, TokenizerOptions},
	};

	#[test]
	fn test_01() {
		let source = r"{{#}}";
		let tokens = Tokenizer::new(source, TokenizerOptions::default())
			.tokenize()
			.unwrap();
		let tokens = tokens.as_slice();
		let nodes = Parser::new(tokens).parse().unwrap();
		println!("{:?}", nodes);
	}

	#[test]
	fn test_02() {
		let source = r"{{#if a + b - c > a * b / c || d / (e + f) < d * (e - f)}} {{#}}";
		let tokens = Tokenizer::new(source, TokenizerOptions::default())
			.tokenize()
			.unwrap();
		let tokens = tokens.as_slice();
		let nodes = Parser::new(tokens).parse().unwrap();
		println!("{:?}", nodes);
	}

	#[test]
	fn test_03() {
		let source =
			r"{{@ file0()}} {{@ file1(arg0 = value0)}} {{@ file2(arg0 = value0, arg1 = value1)}}";
		let tokens = Tokenizer::new(source, TokenizerOptions::default())
			.tokenize()
			.unwrap();
		let tokens = tokens.as_slice();
		let nodes = Parser::new(tokens).parse().unwrap();
		println!("{:?}", nodes);
	}

	#[test]
	fn test_04() {
		let source = "{{a > b || c ? a : (a < b ? b : 0)}}";
		let tokens = Tokenizer::new(source, TokenizerOptions::default())
			.tokenize()
			.unwrap();
		let tokens = tokens.as_slice();
		let nodes = Parser::new(tokens).parse().unwrap();
		println!("{:?}", nodes);
	}

	#[test]
	fn test_05() {
		let source = "{{-a > -b || !c || !d}}";
		let tokens = Tokenizer::new(source, TokenizerOptions::default())
			.tokenize()
			.unwrap();
		let tokens = tokens.as_slice();
		let nodes = Parser::new(tokens).parse().unwrap();
		println!("{:?}", nodes);
	}

	#[test]
	fn test_06() {
		let source = "{{# if a > b}} a > b {{# elif a < b}} a < b {{# else}} a = b {{#}}";
		let tokens = Tokenizer::new(source, TokenizerOptions::default())
			.tokenize()
			.unwrap();
		let tokens = tokens.as_slice();
		let nodes = Parser::new(tokens).parse().unwrap();
		println!("{:?}", nodes);
	}

	#[test]
	fn test_07() {
		let source = "{{ a > b || b > c ? @ option_a(a = a, b = b, c = c) : @ option_b()}}";
		let tokens = Tokenizer::new(source, TokenizerOptions::default())
			.tokenize()
			.unwrap();
		let tokens = tokens.as_slice();
		let nodes = Parser::new(tokens).parse().unwrap();
		println!("{:?}", nodes);
	}

	#[test]
	fn test_08() {
		let source =
			"{{ (a > b ? [[0, 1], [1, 2], [2, 3]] : [['a', 'b'], ['c', 'd'], ['e', 'f']])[1][0] }}";
		let tokens = Tokenizer::new(source, TokenizerOptions::default())
			.tokenize()
			.unwrap();
		let tokens = tokens.as_slice();
		let nodes = Parser::new(tokens).parse().unwrap();
		println!("{:?}", nodes);
	}
}
