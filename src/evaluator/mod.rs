//! Transformation of the AST into [Block]s that can be evaluated into strings.
//!
//! ASTs can't be directly evaluated because the trees they build describe the
//! order of operations of expressions, not control flow. They have no notion
//! of scoping and branching.
//! The Control flow is described by a tree of [Block]s
//!
//! # [Block]
//! A [Block] can either hold a sequence of [super::parser::Node] are evaluated
//! in order, or a sequence of [Block]s which are used for branching.
//!
//! # [EvaluationContext]
//! Anything data that expressions reference that is not literals is stored in a
//! context. This includes variables, functions, and builders.
//!
//! # [evaluator::Evaluator] And [evaluator::EvaluatorBuilder]
//! An [evaluator::Evaluator] holds a sequence of [Block]s.
//! The [evaluator::EvaluatorBuilder] takes a sequence of [crate::parser::Node]s
//! and constructs an [evaluator::Evaluator].
//!
//! # [contained_evaluator::ContainedEvaluator]
//! [evaluator::Evaluator] has two lifetime bounds which is inconvenient in some
//! cases. [contained_evaluator::ContainedEvaluator] is a self referential
//! struct that eliminates these lifetime bounds and can be much more easily
//! used.
//!
//! # [Dependencies]
//! This struct holds all the items the evaluator references such as variables,
//! functions, and builders. This information is used by some of the components
//! of [crate::builder]
//!
//! # Example
//! ```rust
//! let source = "is a greater than b? {{ a > b ? 'yes' : 'no' }}";
//! // tokenize, parse, and build the evaluator
//! let tokens = Tokenizer::new(source, TokenizerOptions::default())
//!     .tokenize()
//!     .unwrap();
//! let nodes = Parser::new(tokens.as_slice()).parse().unwrap();
//! let mut evaluator_builder = EvaluatorBuilder::new(nodes.as_slice());
//! let evaluator = evaluator_builder.build_evaluator().unwrap();
//!
//! // prepare the variables
//! let global_variables = HashMap::new();
//! let local_variables = vec![HashMap::from_iter(
//!     [
//!         ("a".to_string(), Value::Number(2.0)),
//!         ("b".to_string(), Value::Number(1.0)),
//!     ]
//!     .into_iter(),
//! )];
//! let functions = HashMap::new();
//! let builders = HashMap::<String, Evaluator>::new();
//!
//! // create the context and evaluate
//! let mut context = EvaluationContext::new(
//!     &global_variables,
//!     local_variables,
//!     &builders,
//!     &functions,
//!     Default::default(),
//! );
//! println!("{}", evaluator.evaluate(&mut context));
//! // the output will be: `is a greater than b? yes`
//! ```

pub mod block;
pub mod contained_evaluator;
pub mod dependencies;
pub mod evaluation_context;
pub mod evaluator;
pub mod evaluator_builder_error;

use std::{borrow::Borrow, collections::HashMap};

use hmap::hmap;

use crate::{parser::expression::Expression, tokenizer::token::TokenType, value::Value};

use self::{
	block::Block,
	dependencies::Dependencies,
	evaluation_context::{ContextLocation, EvaluationContext},
};

pub type Variables = HashMap<String, Value>;
pub type ValueFunction = Box<dyn Fn(&[Value]) -> Value + 'static>;

/// Every evaluator should have function to evaluate itself with a context,
/// a dependencies getter.
pub trait Evaluate {
	fn evaluate<'ea>(&self, context: &mut EvaluationContext<'ea, impl Evaluate>) -> String;

	fn get_dependencies(&self) -> &Dependencies;
}

/// Evaluates an expression in a context and returns its value.
pub fn evaluate_expression<'c>(
	context: &mut EvaluationContext<'c, impl Evaluate>,
	expression: &Expression,
) -> Value {
	match expression {
		// arithmetic
		Expression::Additive(add, subtract) => {
			// convert to iterators
			let mut add = add.iter();
			let mut subtract = subtract.iter();

			// take the first value from add if exists, otherwise from subtract
			let mut value = match add.next() {
				Some(expression) => evaluate_expression(context, expression.borrow()),
				None => evaluate_expression(context, subtract.next().unwrap().borrow()),
			};

			// add remaining
			for expression in add {
				value = value.add(&evaluate_expression(context, expression.borrow()));
			}

			// subtract remaining
			for expression in subtract {
				value = value.sub(&evaluate_expression(context, expression.borrow()));
			}

			value
		}
		Expression::Multiplicative(multiply, divide) => {
			// convert to iterators
			let mut multiply = multiply.iter();
			let mut divide = divide.iter();

			// take the first value from multiply if exists, otherwise from divide
			let mut value = match multiply.next() {
				Some(expression) => evaluate_expression(context, expression.borrow()),
				None => evaluate_expression(context, divide.next().unwrap().borrow()),
			};

			// multiply remaining
			for expression in multiply {
				value = value.mul(&evaluate_expression(context, expression.borrow()));
			}

			// divide remaining
			for expression in divide {
				value = value.div(&evaluate_expression(context, expression.borrow()));
			}

			value
		}
		Expression::Negate(expression) => evaluate_expression(context, expression.borrow()).neg(),
		// logic
		Expression::Or(expressions) => expressions
			.iter()
			.map(|expression| evaluate_expression(context, expression.borrow()))
			.reduce(|acc, x| acc.or(&x))
			.unwrap(),
		Expression::And(expressions) => expressions
			.iter()
			.map(|expression| evaluate_expression(context, expression.borrow()))
			.reduce(|acc, x| acc.and(&x))
			.unwrap(),
		Expression::Not(expression) => evaluate_expression(context, expression.borrow()).not(),
		// compare
		Expression::Comparison(lhs, op, rhs) => {
			let lhs = evaluate_expression(context, lhs.borrow());
			let rhs = evaluate_expression(context, rhs.borrow());
			match op {
				TokenType::SmallerThan => lhs.less_than(&rhs),
				TokenType::SmallerEquals => lhs.less_equals(&rhs),
				TokenType::GreaterThan => lhs.greater_than(&rhs),
				TokenType::GreaterEquals => lhs.greater_than(&rhs),
				TokenType::Equals => lhs.equals(&rhs),
				TokenType::NotEquals => lhs.not_equals(&rhs),
				_ => Value::None,
			}
		}
		// value
		Expression::Value(value) => value.clone(),
		Expression::Variable(name) => context.get_variable_value(*name).unwrap_or(Value::None),
		Expression::ListValue(expressions) => Value::List(
			expressions
				.iter()
				.map(|expression| evaluate_expression(context, expression))
				.collect(),
		),
		Expression::Builder(name, args) => {
			if let Some(builder) = context.get_evaluator(name) {
				// evaluate the arguments
				let mut local_variables = HashMap::new();
				for b in args {
					let (name, expression) = b.borrow();
					local_variables
						.insert(name.to_string(), evaluate_expression(context, expression));
				}

				// create a new context with the evaluated arguments
				if let Ok(mut context) =
					context.spawn_new(vec![local_variables], ContextLocation::template(name))
				{
					// evaluate the builder and return the output text
					Value::Text(builder.evaluate(&mut context))
				} else {
					// if the new context could not be created, return an empty
					// string
					Value::text("")
				}
			} else {
				Value::Text(String::new())
			}
		}
		Expression::FunctionCall(name, args) => {
			if let Some(function) = context.get_function(name) {
				let args: Vec<Value> = args
					.iter()
					.map(|arg| evaluate_expression(context, arg))
					.collect::<Vec<_>>();
				function(&args)
			} else {
				Value::None
			}
		}
		Expression::IndexOf(expression, index_expression) => {
			let expression_value = evaluate_expression(context, expression);
			let index_value = evaluate_expression(context, index_expression);
			if let Value::List(l) = expression_value {
				if let Value::Number(i) = index_value {
					// TODO: round check
					let i = i.round() as usize;
					// TODO: is there a better way to move out a value
					return l.into_iter().skip(i).next().unwrap();
				}
			}

			Value::None
		}
		// control flow
		Expression::Ternary(condition, a, b) => {
			if evaluate_expression(context, condition.borrow()).bool_or_false() {
				evaluate_expression(context, a.borrow())
			} else {
				evaluate_expression(context, b.borrow())
			}
		}
	}
}

/// Calls [evaluate_block] in order and concatenates the output
pub fn evaluate_blocks<'a, 'b, 'c>(
	context: &mut EvaluationContext<'c, impl Evaluate>,
	blocks: &[Box<Block<'a, 'b>>],
) -> String {
	blocks
		.iter()
		.map(|block| evaluate_block(context, block.borrow()))
		.collect::<Vec<_>>()
		.join("")
}

/// Evaluates a block in a context and returns the evaluated text.
pub fn evaluate_block<'a, 'b, 'c>(
	context: &mut EvaluationContext<'c, impl Evaluate>,
	block: &Block,
) -> String {
	match block {
		Block::Source(s) => s.to_string(),
		Block::Expression(expression) => evaluate_expression(context, *expression).to_string(),
		Block::Assignment(name, _tt, expression) => {
			let value = evaluate_expression(context, *expression);
			context.assign_local_variable(name, value);
			String::new()
		}
		Block::If(if_block) => {
			let mut s = None;

			// iterate over all the if arms
			for (expression, blocks) in &if_block.if_blocks {
				if evaluate_expression(context, expression).bool_or_false() {
					s = Some(evaluate_blocks(context, &blocks));
					break;
				}
			}

			// if none of the if arms were triggered, evaluate the else block, otherwise return the already evaluated value
			if let Some(s) = s {
				s
			} else {
				if let Some(else_blocks) = &if_block.else_blocks {
					// evaluate all the blocks into strings and join
					evaluate_blocks(context, &else_blocks)
				} else {
					String::new()
				}
			}
		}
		Block::For(for_block) => {
			let value = evaluate_expression(context, for_block.expression);
			let var_name = for_block.var_name;
			if let Value::List(l) = value {
				// vector to hold all the evaluated blocks' outputs
				let mut outs = Vec::new();

				// iterate over each value of list
				for value in l {
					// add the for loop's variable
					context.push_variables(hmap!(
						var_name.to_string() => value
					));

					// evaluate and push each of the for loop's inner blocks to the output
					for block in &for_block.blocks {
						outs.push(evaluate_block(context, block));
					}

					// remove the for loop's variable
					context.pop_variables();
				}

				outs.join("")
			} else {
				String::new()
			}
		}
	}
}

/// Helper function to list all the dependencies an expression references and
/// add them to the dependencies.
pub fn list_dependencies_expression(expression: &Expression, dependencies: &mut Dependencies) {
	match expression {
		Expression::Additive(a, b) | Expression::Multiplicative(a, b) => {
			for expression in a.iter().chain(b.iter()) {
				list_dependencies_expression(expression.borrow(), dependencies);
			}
		}
		Expression::Negate(a) | Expression::Not(a) => {
			list_dependencies_expression(a.borrow(), dependencies);
		}
		Expression::Or(a) | Expression::And(a) => {
			for expression in a {
				list_dependencies_expression(expression.borrow(), dependencies);
			}
		}
		Expression::Comparison(a, _, b) => {
			list_dependencies_expression(a.borrow(), dependencies);
			list_dependencies_expression(b.borrow(), dependencies);
		}
		Expression::Variable(name) => {
			dependencies.add_variable(name);
		}
		Expression::Builder(name, args) => {
			dependencies.add_builder(name);
			for arg in args {
				let (_, expression) = arg.borrow();
				list_dependencies_expression(expression.borrow(), dependencies);
			}
		}
		Expression::FunctionCall(name, args) => {
			dependencies.add_function(name);
			for arg in args {
				list_dependencies_expression(arg, dependencies);
			}
		}
		Expression::Ternary(a, b, c) => {
			list_dependencies_expression(a.borrow(), dependencies);
			list_dependencies_expression(b.borrow(), dependencies);
			list_dependencies_expression(c.borrow(), dependencies);
		}
		_ => {}
	}
}

/// Helper function to list all the dependencies a block references and
/// add them to the dependencies.
pub fn list_dependencies_block(block: &Block, dependencies: &mut Dependencies) {
	match block {
		Block::Expression(expression) | Block::Assignment(.., expression) => {
			list_dependencies_expression(expression, dependencies)
		}
		Block::If(if_block) => {
			// if arms
			for (expression, blocks) in if_block.if_blocks.iter() {
				list_dependencies_expression(expression, dependencies);
				for block in blocks.iter() {
					list_dependencies_block(block, dependencies);
				}
			}

			// optional else arm
			if let Some(else_block) = &if_block.else_blocks {
				for block in else_block.iter() {
					list_dependencies_block(block.borrow(), dependencies);
				}
			}
		}
		Block::For(for_block) => {
			for block in &for_block.blocks {
				list_dependencies_block(&block, dependencies);
			}
		}
		Block::Source(_) => {}
	}
}

#[cfg(test)]
mod tests_blocks {
	use crate::{
		evaluator::evaluator::EvaluatorBuilder,
		parser::Parser,
		tokenizer::{Tokenizer, TokenizerOptions},
	};

	fn test(source: &str) {
		let tokens = Tokenizer::new(source, TokenizerOptions::default())
			.tokenize()
			.unwrap();
		let nodes = Parser::new(tokens.as_slice()).parse().unwrap();
		let mut evaluator_builder = EvaluatorBuilder::new(nodes.as_slice());
		let blocks = evaluator_builder.build();
		println!("{:?}", blocks);
	}

	#[test]
	fn test_01() {
		test("abc {{a + 1}} def");
	}

	#[test]
	fn test_02() {
		test("{{# if a > b}} a > b {{#}}");
	}

	#[test]
	fn test_03() {
		test("{{# if a > b}} a > b {{# else }} a <= b {{#}}");
	}

	#[test]
	fn test_04() {
		test("{{# if a > b}} a > b {{# elif a < b }} a < b {{#}}");
	}

	#[test]
	fn test_05() {
		test("{{# if a > b}} part 1 {{# if c > d}} c > d {{# else }} c <= d {{#}} {{# elif a < b }} a < b {{#}}");
	}

	#[test]
	fn test_06() {
		test("{{# if a}} {{b ? @ file1(a = a, b = b) : @ file1(a = a, b = 0)}} {{#}}");
	}
}

#[cfg(test)]
mod tests_evaluator {
	use std::collections::HashMap;

	use hmap::hmap;

	use crate::{
		evaluator::evaluation_context::{ContextLocation, EvaluationContextWarnings},
		parser::Parser,
		tokenizer::{Tokenizer, TokenizerOptions},
		value::Value,
	};

	use super::{
		evaluation_context::EvaluationContext,
		evaluator::{Evaluator, EvaluatorBuilder},
		Evaluate,
	};

	fn test(
		source: &str,
		global_variables: Vec<(&str, Value)>,
		builders: HashMap<String, Evaluator>,
	) -> String {
		// tokenize, parse, and build the evaluation blocks
		let tokens = Tokenizer::new(source, TokenizerOptions::default())
			.tokenize()
			.unwrap();
		let nodes = Parser::new(tokens.as_slice()).parse().unwrap();
		let mut evaluator_builder = EvaluatorBuilder::new(nodes.as_slice());
		let evaluator = evaluator_builder.build_evaluator().unwrap();

		// prepare the variables
		let global_variables = global_variables
			.into_iter()
			.map(|(k, v)| (k.to_string(), v));
		let global_variables = HashMap::from_iter(global_variables);

		let functions = HashMap::new();

		// create the context and evaluate
		let mut context = EvaluationContext::new(
			&global_variables,
			vec![],
			&builders,
			&functions,
			Default::default(),
			EvaluationContextWarnings::default(),
			ContextLocation::source("main"),
		);
		let out = evaluator.evaluate(&mut context);
		println!("{}", out);
		out
	}

	#[test]
	fn test_01() {
		test("abc {{ 1 + 2 }} def", vec![], HashMap::new());
	}

	#[test]
	fn test_02() {
		test(
			"result: {{ 1 > 2 ? 'item 1' : 'item 2' }}",
			vec![],
			HashMap::new(),
		);
	}

	#[test]
	fn test_03() {
		test("result: {{ unknown_variable }}", vec![], HashMap::new());
	}

	#[test]
	fn test_04() {
		test(
			"a > b = {{a}} > {{b}} = {{ a > b }}",
			vec![("a", Value::Number(1.0)), ("b", Value::Number(2.0))],
			HashMap::new(),
		);
	}

	#[test]
	fn test_05() {
		test(
			"a is {{ a > b ? 'bigger than' : a < b ? 'smaller than' : 'equal to'}} b",
			vec![("a", Value::Number(1.0)), ("b", Value::Number(2.0))],
			HashMap::new(),
		);
	}

	#[test]
	fn test_06() {
		test(
			"a is {{#if a > b}}bigger than{{# elif a < b}}smaller than{{#else}}equal to{{#}} b",
			vec![("a", Value::Number(1.0)), ("b", Value::Number(1.0))],
			HashMap::new(),
		);
	}

	#[test]
	fn test_07() {
		// create a template
		let source = "saying hi to: {{name}}";
		// tokenize, parse, and build the evaluation blocks
		let tokens = Tokenizer::new(source, TokenizerOptions::default())
			.tokenize()
			.unwrap();
		let nodes = Parser::new(tokens.as_slice()).parse().unwrap();
		let mut evaluator_builder = EvaluatorBuilder::new(nodes.as_slice());
		let evaluator = evaluator_builder.build_evaluator().unwrap();

		test(
			"{{a > b ? @ say_hi(name = 'name ' + a) : @ say_hi(name = 'name ' + b)}}",
			vec![("a", Value::Number(1.0)), ("b", Value::Number(2.0))],
			hmap!(
				format!("say_hi") => evaluator
			),
		);
	}

	#[test]
	fn test_08() {
		let source = "{{a != None ? a : 'default'}}";
		assert_eq!(
			&test(source, vec![("a", Value::text("var-a"))], HashMap::new()),
			"var-a"
		);
		assert_eq!(&test(source, vec![], HashMap::new()), "default");
	}
}

#[cfg(test)]
mod tests_contained_evaluator {
	use std::collections::HashMap;

	use crate::evaluator::{
		contained_evaluator::ContainedEvaluator,
		evaluation_context::{ContextLocation, EvaluationContext, EvaluationContextWarnings},
		Evaluate,
	};

	#[test]
	fn test_01() {
		// prepare the variables
		let global_variables = HashMap::new();
		let local_variables = vec![];
		let builders = HashMap::<_, ContainedEvaluator>::new();
		let functions = HashMap::new();

		// create the context and evaluate
		let mut context = EvaluationContext::new(
			&global_variables,
			local_variables,
			&builders,
			&functions,
			Default::default(),
			EvaluationContextWarnings::default(),
			ContextLocation::source("main"),
		);

		// create the evaluator and evaluate
		let evaluator = ContainedEvaluator::from_string("1 + 2 = {{ 1 + 2 }}".to_string()).unwrap();
		println!("{:?}", std::ptr::addr_of!(evaluator));
		println!("{}", evaluator.evaluator.evaluate(&mut context));
		let _a = 0;
		{
			let moved = evaluator;
			println!("{:?}", std::ptr::addr_of!(moved));
			println!("{}", moved.evaluator.evaluate(&mut context));
		}
	}
}
