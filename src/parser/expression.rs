use crate::{tokenizer::token::TokenType, value::Value};

/// Sequence of expressions.
pub type Expressions<'a> = Vec<Box<Expression<'a>>>;

/// Named sequence of expressions.
pub type NamedExpressions<'a> = Vec<Box<(&'a str, Expression<'a>)>>;

/// Expressions are trees of operations that can be evaluated into a [Value].
#[derive(Debug, Clone)]
pub enum Expression<'a> {
	// arithmetics
	Additive(Expressions<'a>, Expressions<'a>),
	Multiplicative(Expressions<'a>, Expressions<'a>),
	Negate(Box<Expression<'a>>),
	// logic
	Or(Expressions<'a>),
	And(Expressions<'a>),
	Not(Box<Expression<'a>>),
	// compare
	Comparison(Box<Expression<'a>>, TokenType, Box<Expression<'a>>),
	// value
	Value(Value),
	Variable(&'a str),
	ListValue(Expressions<'a>),
	// TODO: rename this
	Builder(&'a str, NamedExpressions<'a>),
	FunctionCall(&'a str, Expressions<'a>),
	IndexOf(Box<Expression<'a>>, Box<Expression<'a>>),
	// control flow
	Ternary(
		Box<Expression<'a>>,
		Box<Expression<'a>>,
		Box<Expression<'a>>,
	),
}
