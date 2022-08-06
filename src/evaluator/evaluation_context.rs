use std::collections::HashMap;

use crate::value::Value;

use super::{Evaluate, ValueFunction, Variables};

#[derive(Clone)]
/// Specifies which file is using the evaluation context.
pub enum ContextLocation {
	/// (name of the partial, is it a source)
	SourceOrTemplate(String, bool),
	/// Variables file
	Variables,
}

impl ContextLocation {
	pub fn partial(name: impl ToString, is_source: bool) -> Self {
		Self::SourceOrTemplate(name.to_string(), is_source)
	}

	pub fn source(name: impl ToString) -> Self {
		Self::partial(name, true)
	}

	pub fn template(name: impl ToString) -> Self {
		Self::partial(name, false)
	}
}

/// Specifies under which circumstances the evaluator should log a warning.
#[derive(Clone, Copy)]
pub struct EvaluationContextWarnings {
	pub max_depth_reached: bool,
	pub variable_not_found: bool,
	pub function_not_found: bool,
	pub evaluator_not_found: bool,
}

impl std::default::Default for EvaluationContextWarnings {
	fn default() -> Self {
		Self {
			max_depth_reached: true,
			variable_not_found: true,
			function_not_found: true,
			evaluator_not_found: true,
		}
	}
}

/// Context that is used in the evaluation process of a block.
pub struct EvaluationContext<'c, E: Evaluate> {
	// variables
	global_variables: &'c Variables,
	local_variables: Vec<Variables>,
	// functions
	evaluators: &'c HashMap<String, E>,
	functions: &'c HashMap<String, ValueFunction>,
	// recursion
	depth: usize,
	max_depth: usize,
	// warnings
	warnings: EvaluationContextWarnings,
	location_stack: Vec<ContextLocation>,
}

impl<'c, E: Evaluate> EvaluationContext<'c, E> {
	pub fn new(
		global_variables: &'c Variables,
		local_variables: Vec<Variables>,
		builders: &'c HashMap<String, E>,
		functions: &'c HashMap<String, Box<dyn Fn(&[Value]) -> Value + 'static>>,
		max_depth: usize,
		warnings: EvaluationContextWarnings,
		context_location: ContextLocation,
	) -> Self {
		Self {
			global_variables,
			local_variables,
			evaluators: builders,
			functions,
			depth: 0,
			max_depth,
			warnings,
			location_stack: vec![context_location],
		}
	}

	pub fn spawn_new(
		&self,
		local_variables: Vec<Variables>,
		context_location: ContextLocation,
	) -> Result<Self, ()> {
		let depth = self.depth + 1;
		if depth == self.max_depth {
			if self.warnings.max_depth_reached {
				let trace = self.trace();
				let tail_len = 5.min(trace.len());
				let trace = &trace[trace.len() - tail_len..];
				log::warn!("Max depth reached: trace tail: {:?}", trace)
			}
			return Err(());
		}

		let mut location_stack = self.location_stack.clone();
		location_stack.push(context_location);

		Ok(Self {
			global_variables: self.global_variables,
			local_variables,
			evaluators: self.evaluators,
			functions: self.functions,
			depth,
			max_depth: self.max_depth,
			warnings: self.warnings.clone(),
			location_stack,
		})
	}

	fn trace(&self) -> Vec<String> {
		self.location_stack
			.iter()
			.map(|location| match location {
				ContextLocation::SourceOrTemplate(name, is_source) => {
					if *is_source {
						format!("source '{name}'")
					} else {
						format!("template '{name}'")
					}
				}
				ContextLocation::Variables => format!("variables"),
			})
			.collect()
	}

	pub fn get_evaluator(&self, name: &str) -> Option<&'c E> {
		let ret = self.evaluators.get(name);

		// optionally warn if no evaluator was found
		if self.warnings.evaluator_not_found {
			if ret.is_none() {
				log::warn!(
					"No evaluator named `{name}` was found. Trace: {:?}.",
					self.trace()
				);
			}
		}

		ret
	}

	pub fn get_function(&self, name: &str) -> Option<&'c ValueFunction> {
		let ret = self.functions.get(name);

		// if no variable with the given name was found, return `None` and warn
		if self.warnings.function_not_found {
			if ret.is_none() {
				log::warn!(
					"No function named `{name}` was found. Trace: {:?}.",
					self.trace()
				);
			}
		}

		ret
	}

	pub fn get_variable_value(&self, name: &str) -> Option<Value> {
		// first, search the local variables from last to first (inner to outer scopes)
		for variables in self.local_variables.iter().rev() {
			let v = variables.get(name);
			if v.is_some() {
				return v.cloned();
			}
		}

		// then, search the global variables
		let v = self.global_variables.get(name);
		if v.is_some() {
			return v.cloned();
		}

		// if no variable with the given name was found, return `None` and warn
		if self.warnings.variable_not_found {
			log::warn!(
				"No variable named `{name}` was found. Trace: {:?}.",
				self.trace()
			);
		}

		None
	}

	pub fn push_variables(&mut self, variables: Variables) {
		self.local_variables.push(variables);
	}

	pub fn pop_variables(&mut self) {
		self.local_variables.pop();
	}
}
