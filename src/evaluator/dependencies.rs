use std::collections::HashSet;

/// Shallow (non builder recursive) dependencies of an
/// [super::evaluator::Evaluator].
pub struct Dependencies {
	pub variable_dependencies: HashSet<String>,
	pub function_dependencies: HashSet<String>,
	pub builder_dependencies: HashSet<String>,
}

impl Dependencies {
	pub fn empty() -> Self {
		Self {
			variable_dependencies: HashSet::new(),
			function_dependencies: HashSet::new(),
			builder_dependencies: HashSet::new(),
		}
	}

	pub fn add_variable(&mut self, name: impl ToString) {
		self.variable_dependencies.insert(name.to_string());
	}

	pub fn add_function(&mut self, name: impl ToString) {
		self.function_dependencies.insert(name.to_string());
	}

	pub fn add_builder(&mut self, name: impl ToString) {
		self.builder_dependencies.insert(name.to_string());
	}

	pub fn contains_variable(&self, name: &str) -> bool {
		self.variable_dependencies.contains(name)
	}

	pub fn contains_function(&self, name: &str) -> bool {
		self.function_dependencies.contains(name)
	}

	pub fn contains_builder(&self, name: &str) -> bool {
		self.builder_dependencies.contains(name)
	}

	pub fn iter_builder_dependencies(&self) -> impl Iterator<Item = &str> {
		self.builder_dependencies.iter().map(|x| x.as_str())
	}

	pub fn iter_variable_dependencies(&self) -> impl Iterator<Item = &str> {
		self.variable_dependencies.iter().map(|x| x.as_str())
	}
}
