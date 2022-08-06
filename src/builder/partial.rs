//! A partial is a combination of an [crate::evaluator::evaluator::Evaluator]
//! and specified [super::ReadWriteTarget]

use std::path::PathBuf;

use crate::evaluator::{
	contained_evaluator::ContainedEvaluator, dependencies::Dependencies,
	evaluation_context::EvaluationContext, evaluator_builder_error::EvaluatorBuilderError,
	Evaluate,
};

use super::targets::{ReadWriteTarget, Target};

pub struct Partial<'a> {
	pub read_write_target: ReadWriteTarget,
	pub evaluator: ContainedEvaluator<'a>,
}

impl<'a> Partial<'a> {
	pub fn new(read_write_target: ReadWriteTarget) -> Result<Self, EvaluatorBuilderError> {
		let evaluator = ContainedEvaluator::from_string(read_write_target.read.read())?;
		Ok(Self {
			read_write_target,
			evaluator,
		})
	}

	pub fn from_file<P: Into<PathBuf>>(
		path: P,
		write_target: Target,
	) -> Result<Self, EvaluatorBuilderError> {
		let read_write_target = ReadWriteTarget {
			read: Target::External(path.into()),
			write: write_target,
		};
		Self::new(read_write_target)
	}

	pub fn from_string(
		content: String,
		write_target: Target,
	) -> Result<Self, EvaluatorBuilderError> {
		let read_write_target = ReadWriteTarget {
			read: Target::Local(content),
			write: write_target,
		};
		Self::new(read_write_target)
	}

	pub fn reload(&mut self) {
		// TODO: what to do with this error?
		self.evaluator =
			ContainedEvaluator::from_string(self.read_write_target.read.read()).unwrap();
	}

	pub fn evaluate_and_store<'ea, 'eb, 'ec>(
		&mut self,
		context: &mut EvaluationContext<'ea, impl Evaluate>,
	) {
		self.read_write_target
			.write
			.write(self.evaluator.evaluate(context));
	}
}

impl<'a> Evaluate for Partial<'a> {
	fn evaluate<'ea, 'eb, 'ec>(
		&self,
		context: &mut EvaluationContext<'ea, impl Evaluate>,
	) -> String {
		self.evaluator.evaluate(context)
	}

	fn get_dependencies(&self) -> &Dependencies {
		self.evaluator.get_dependencies()
	}
}
