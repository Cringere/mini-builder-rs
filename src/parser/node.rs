use crate::tokenizer::token::Token;

use super::expression::Expression;

pub type Nodes<'a> = Vec<Box<Node<'a>>>;

/// The data that the [Node] holds. Different Nodes hold different types of data.
#[derive(Debug)]
pub enum NodeContent<'a> {
	Source(&'a str),
	Expression(Expression<'a>),
	If(Expression<'a>),
	For(&'a str, Expression<'a>),
	Elif(Expression<'a>),
	Else,
	End,
}

/// [Node] contains some data, and the location (in the tokenized text) of the
/// first token it used.
#[derive(Debug)]
pub struct Node<'a> {
	pub location: usize,
	pub content: NodeContent<'a>,
}

impl<'a> Node<'a> {
	pub fn new(location: usize, content: NodeContent<'a>) -> Self {
		Self { location, content }
	}

	pub fn _source(location: usize, source: &'a str) -> Self {
		Self::new(location, NodeContent::Source(source))
	}

	pub fn source_from_token(token: &Token<'a>) -> Self {
		Self::new(token.location, NodeContent::Source(token.content))
	}

	pub fn expression(location: usize, expression: Expression<'a>) -> Self {
		Self::new(location, NodeContent::Expression(expression))
	}

	pub fn if_node(location: usize, expression: Expression<'a>) -> Self {
		Self::new(location, NodeContent::If(expression))
	}

	pub fn for_node(location: usize, var_name: &'a str, expression: Expression<'a>) -> Self {
		Self::new(location, NodeContent::For(var_name, expression))
	}

	pub fn elif(location: usize, expression: Expression<'a>) -> Self {
		Self::new(location, NodeContent::Elif(expression))
	}

	pub fn else_node(location: usize) -> Self {
		Self::new(location, NodeContent::Else)
	}

	pub fn end(location: usize) -> Self {
		Self::new(location, NodeContent::End)
	}
}
