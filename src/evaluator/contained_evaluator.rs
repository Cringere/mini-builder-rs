use std::pin::Pin;

use crate::{
	parser::{
		node::{Node, Nodes},
		Parser,
	},
	tokenizer::{Tokenizer, TokenizerOptions},
};

use super::{
	evaluation_context::EvaluationContext,
	evaluator::{Evaluator, EvaluatorBuilder},
	evaluator_builder_error::EvaluatorBuilderError,
	Evaluate,
};

/// A self referential struct that has no external lifetimes dependencies.
pub struct ContainedEvaluator<'a> {
	// pins
	_source: Pin<Box<String>>,
	_nodes: Pin<Box<Nodes<'a>>>,
	// slices
	_source_slice: &'a str,
	_nodes_slice: &'a [Box<Node<'a>>],
	// evaluator
	pub evaluator: Evaluator<'a, 'a>,
}

impl<'a> ContainedEvaluator<'a> {
	pub fn from_string(source: String) -> Result<Self, EvaluatorBuilderError> {
		// source
		let source = Box::pin(source);
		let source_slice = unsafe {
			std::str::from_utf8_unchecked(std::slice::from_raw_parts(source.as_ptr(), source.len()))
		};

		// tokenize
		let tokens =
			Box::pin(Tokenizer::new(source_slice, TokenizerOptions::default()).tokenize()?);
		let tokens_slice =
			unsafe { std::slice::from_raw_parts(tokens.as_slice().as_ptr(), tokens.len()) };

		// parse
		let nodes = Box::pin(Parser::new(tokens_slice).parse()?);
		let nodes_slice =
			unsafe { std::slice::from_raw_parts(nodes.as_slice().as_ptr(), nodes.len()) };

		// evaluator
		let mut evaluator_builder = EvaluatorBuilder::new(nodes_slice);
		let evaluator = evaluator_builder.build_evaluator()?;

		Ok(Self {
			// pins
			_source: source,
			_nodes: nodes,
			// slices
			_source_slice: source_slice,
			_nodes_slice: nodes_slice,
			evaluator,
		})
	}
}

impl<'a> Evaluate for ContainedEvaluator<'a> {
	fn evaluate<'ea>(&self, context: &mut EvaluationContext<'ea, impl Evaluate>) -> String {
		self.evaluator.evaluate(context)
	}

	fn get_dependencies(&self) -> &super::Dependencies {
		self.evaluator.get_dependencies()
	}
}
