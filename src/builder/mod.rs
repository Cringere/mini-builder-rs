//! Combines the full pipeline of tokenizing, parsing, building nodes with IO.
//!
//! Each of the pipeline's components are available individually. However this
//! crate's goal is to provide a simple struct that users can use by only
//! providing sources, templates, variables and function and let the crate
//! handle the rest. [Builder] is this struct.
//!
//! # IO
//! The [Target] enum specifies where text is read and written to.
//! - [Target::Local] corresponds to reading and writing to a string.
//! - [Target::External] corresponds to reading and writing to a file.
//! When a builder is constructed, a path to two directories `sources` and
//! templates `templates` can be provided. Files under these directories will
//! automatically be loaded with a [Target::External] read.
//! [Target::Local] read files can be added with `add_local_*` functions.
//!
//! # Watching
//! The builder has a `watch` function which will automatically re-read any
//! files under the `sources`, `templates` directories and the `variables` file.
//!
//! # Variables file
//! If a variables files is provided to the builder, it will load its contents
//! as global variables that will be available to all sources and templates.
//! Additionally, under `watch`, the builder will listen to changes in that file
//! and automatically reload its variables and rebuild any sources that depend
//! on these variables.
//!
//! # Examples
//! See the examples directory

pub mod builder_error;
mod partial;
pub mod targets;

use std::{
	borrow::Borrow,
	collections::{HashMap, HashSet},
	path::{Path, PathBuf},
	sync::mpsc,
};

use lazy_static::lazy_static;
use notify::{DebouncedEvent, RecommendedWatcher, RecursiveMode, Watcher};
use regex::Regex;

use crate::{
	builder::targets::Target,
	evaluator::{
		evaluate_expression,
		evaluation_context::{ContextLocation, EvaluationContext, EvaluationContextWarnings},
		evaluator_builder_error::EvaluatorBuilderError,
		Evaluate, ValueFunction, Variables,
	},
	parser::{expression::Expression, node::NodeContent, Parser},
	tokenizer::{Tokenizer, TokenizerOptions},
	value::Value,
};

use self::{builder_error::BuilderError, partial::Partial, targets::ReadWriteTarget};

/// Recursively gets all files from a directory, returning their path and
/// relative name to the root folder.
/// Example: 'root/dir1/file1.txt' -> ['dir1', 'file1.txt']
fn list_files<P: AsRef<Path>>(
	path: P,
	ignore_extensions: bool,
	warn_on_skip: bool,
) -> Vec<(PathBuf, Vec<String>)> {
	fn list_files_rec<P: AsRef<Path>>(
		path: P,
		name_stack: &mut Vec<String>,
		out: &mut Vec<(PathBuf, Vec<String>)>,
		ignore_extensions: bool,
		warn_on_skip: bool,
	) {
		for entry in std::fs::read_dir(path).unwrap() {
			// ensure entry is valid
			let entry = match entry {
				Ok(entry) => entry,
				_ => continue,
			};

			// ensure the name is valid (either file or directory name)
			let name = entry.file_name();
			let name = match name.to_str() {
				Some(name) => name,
				_ => {
					if warn_on_skip {
						log::warn!(
							"Skipping entry {:?} - name could not be converted to string.",
							name
						);
					}
					continue;
				}
			};

			// ensure that the name is ascii
			if !name.is_ascii() {
				if warn_on_skip {
					log::warn!("Skipping entry {name} - name is not ascii.");
				}
				continue;
			}

			// ensure entry has metadata
			let metadata = match entry.metadata() {
				Ok(metadata) => metadata,
				_ => {
					if warn_on_skip {
						log::warn!("Skipping entry {name} - could not extract metadata.");
					}
					continue;
				}
			};

			// handle directories - recurse
			if metadata.is_dir() {
				name_stack.push(name.to_string());
				list_files_rec(
					entry.path(),
					name_stack,
					out,
					ignore_extensions,
					warn_on_skip,
				);
				name_stack.pop();
				continue;
			}

			// handle files
			if metadata.is_file() {
				let path = entry.path();

				// get file name, choosing if to include or
				// exclude file extensions
				let extracted_name = if ignore_extensions {
					path.file_stem()
				} else {
					path.file_name()
				};

				// process the chosen name, if valid add to the
				// name stack and push to the output
				if let Some(extracted_name) = extracted_name {
					if let Some(name) = extracted_name.to_str() {
						let mut name_stack = name_stack.clone();
						name_stack.push(name.to_string());
						out.push((entry.path().to_owned(), name_stack.clone()));
						continue;
					}
				}

				log::error!("Skipping entry {name} - unexpected error");
				continue;
			}

			if warn_on_skip {
				log::warn!("Skipping entry {name} - entry is neither a file or a directory.")
			}
		}
	}

	let mut out = Vec::new();
	let mut name_stack = Vec::new();
	list_files_rec(
		path,
		&mut name_stack,
		&mut out,
		ignore_extensions,
		warn_on_skip,
	);

	out
}

/// Specifies under which circumstances the builder should log a warning.
#[derive(Clone, Copy)]
pub struct BuilderWarnings {
	pub found_cycle: bool,
	pub invalid_variable_file_line: bool,
	pub file_skip: bool,
}

impl std::default::Default for BuilderWarnings {
	fn default() -> Self {
		Self {
			found_cycle: false,
			invalid_variable_file_line: true,
			file_skip: true,
		}
	}
}

#[derive(Clone)]
pub struct BuilderOptions {
	pub max_depth: usize,
	pub remove_generated_dir_content_on_create: bool,
	pub watcher_delays_millis: u64,
	pub builder_warnings: BuilderWarnings,
	pub evaluation_context_warnings: EvaluationContextWarnings,
}

impl std::default::Default for BuilderOptions {
	fn default() -> Self {
		Self {
			max_depth: 50,
			remove_generated_dir_content_on_create: false,
			watcher_delays_millis: 10,
			builder_warnings: BuilderWarnings::default(),
			evaluation_context_warnings: EvaluationContextWarnings::default(),
		}
	}
}

pub struct Builder<'p> {
	// paths
	sources_dir: Option<PathBuf>,
	templates_dir: Option<PathBuf>,
	generated_dir: Option<PathBuf>,
	variables_file: Option<PathBuf>,
	// builders
	sources: HashMap<String, Partial<'p>>,
	templates: HashMap<String, Partial<'p>>,
	// globals
	variables: Variables,
	functions: HashMap<String, ValueFunction>,
	// options
	options: BuilderOptions,
}

impl<'p> Builder<'p> {
	pub fn new(
		sources_dir: Option<PathBuf>,
		templates_dir: Option<PathBuf>,
		generated_dir: Option<PathBuf>,
		variables_file: Option<PathBuf>,
		options: BuilderOptions,
	) -> Result<Self, BuilderError> {
		// helper functions that creates directories and transforms relative
		// paths to global
		fn map_path_dir(path: Option<PathBuf>) -> std::io::Result<Option<PathBuf>> {
			Ok(if let Some(path_buf) = path {
				std::fs::create_dir_all(&path_buf)?;
				Some(std::fs::canonicalize(&path_buf)?)
			} else {
				None
			})
		}

		fn map_path_file(path: Option<PathBuf>) -> std::io::Result<Option<PathBuf>> {
			Ok(if let Some(path_buf) = path {
				let path_buf = std::fs::canonicalize(&path_buf)?;
				if !path_buf.as_path().exists() {
					std::fs::File::create(&path_buf)?;
				}
				Some(path_buf)
			} else {
				None
			})
		}

		// map paths
		let sources_dir = map_path_dir(sources_dir)?;
		let templates_dir = map_path_dir(templates_dir)?;
		let generated_dir = map_path_dir(generated_dir)?;
		let variables_file = map_path_file(variables_file)?;

		// optional - clear generated dir
		if let Some(path) = generated_dir.as_ref() {
			if options.remove_generated_dir_content_on_create {
				std::fs::remove_dir_all(&path)?;
			}
		}

		// helper function, returns true if either a or b is under target
		fn contains_dir(
			target: &Option<PathBuf>,
			a: &Option<PathBuf>,
			b: &Option<PathBuf>,
		) -> bool {
			if let Some(target) = target {
				if let Some(a) = a {
					if a.starts_with(&target) {
						return true;
					}
				}

				if let Some(b) = b {
					if b.starts_with(&target) {
						return true;
					}
				}

				false
			} else {
				false
			}
		}

		// check that none of the directories contain each other
		if contains_dir(&sources_dir, &templates_dir, &generated_dir)
			|| contains_dir(&templates_dir, &sources_dir, &generated_dir)
			|| contains_dir(&generated_dir, &templates_dir, &sources_dir)
		{
			return Err(BuilderError::InvalidDirectories);
		}

		Ok(Self {
			// directories
			sources_dir,
			templates_dir,
			generated_dir,
			variables_file,
			// builders
			sources: HashMap::new(),
			templates: HashMap::new(),
			// global variables
			variables: HashMap::new(),
			functions: HashMap::new(),
			// options
			options,
		})
	}

	/// Helper function for adding either local sources or local templates.
	fn add_local(
		partials: &mut HashMap<String, Partial>,
		name: impl ToString,
		content: impl ToString,
	) -> Result<(), EvaluatorBuilderError> {
		partials.insert(
			name.to_string(),
			Partial::from_string(content.to_string(), Target::Local(String::new()))?,
		);
		Ok(())
	}

	/// Adds a local source.
	pub fn add_local_source(
		&mut self,
		name: impl ToString,
		content: impl ToString,
	) -> Result<&mut Self, EvaluatorBuilderError> {
		Self::add_local(&mut self.sources, name, content)?;
		Ok(self)
	}

	// Adds a local template.
	pub fn add_local_template(
		&mut self,
		name: impl ToString,
		content: impl ToString,
	) -> Result<&mut Self, EvaluatorBuilderError> {
		Self::add_local(&mut self.templates, name, content)?;
		Ok(self)
	}

	/// Helper function for adding either several local sources or templates
	fn add_locals(
		partials: &mut HashMap<String, Partial>,
		items: &[(impl ToString, impl ToString)],
	) -> Result<(), Vec<(String, EvaluatorBuilderError)>> {
		let mut errors = Vec::new();
		for (name, content) in items {
			if let Err(err) = Self::add_local(partials, name.to_string(), content.to_string()) {
				errors.push((name.to_string(), err));
			}
		}

		if errors.len() > 0 {
			Err(errors)
		} else {
			Ok(())
		}
	}

	/// Adds multiple local sources.
	pub fn add_local_sources(
		&mut self,
		items: &[(impl ToString, impl ToString)],
	) -> Result<&mut Self, Vec<(String, EvaluatorBuilderError)>> {
		Self::add_locals(&mut self.sources, items)?;
		Ok(self)
	}

	/// Adds multiple local templates.
	pub fn add_local_templates(
		&mut self,
		items: &[(impl ToString, impl ToString)],
	) -> Result<&mut Self, Vec<(String, EvaluatorBuilderError)>> {
		Self::add_locals(&mut self.templates, items)?;
		Ok(self)
	}

	/// Adds a variable.
	pub fn add_variable(&mut self, name: impl ToString, value: Value) -> &mut Self {
		self.variables.insert(name.to_string(), value);
		self
	}

	/// Adds multiple variables.
	pub fn add_variables(&mut self, variables: &[(String, Value)]) -> &mut Self {
		for (name, value) in variables.into_iter().cloned() {
			self.variables.insert(name, value);
		}
		self
	}

	/// Adds a function.
	pub fn add_function(&mut self, name: impl ToString, function: ValueFunction) -> &mut Self {
		self.functions.insert(name.to_string(), function);
		self
	}

	/// Adds functions.
	pub fn add_functions(&mut self, functions: Vec<(String, ValueFunction)>) -> &mut Self {
		for (name, value) in functions.into_iter() {
			self.functions.insert(name, value);
		}
		self
	}

	/// Recursively checks if a partial or its dependents depend on a variable.
	fn is_source_dependent_on_variables(&self, source_name: &str, variables: &[String]) -> bool {
		fn dfs<'a>(
			variables: &'a [String],
			partials: &'a HashMap<String, Partial>,
			partial_name: &'a str,
			seen: &mut HashSet<&'a str>,
		) -> bool {
			if seen.contains(partial_name) {
				return false;
			}

			seen.insert(partial_name);

			if let Some(partial) = partials.get(partial_name) {
				for variable in variables {
					if partial.get_dependencies().contains_variable(&variable) {
						return true;
					}
				}

				for dep in partial.get_dependencies().iter_builder_dependencies() {
					if dfs(variables, partials, dep, seen) {
						return true;
					}
				}
			}

			false
		}

		if let Some(source) = self.sources.get(source_name) {
			for variable in variables {
				if source.get_dependencies().contains_variable(&variable) {
					return true;
				}
			}

			for partial in source.get_dependencies().iter_builder_dependencies() {
				let mut seen = HashSet::new();
				if dfs(variables, &self.templates, partial, &mut seen) {
					return true;
				}
			}
		}

		false
	}

	/// Recursively checks if a partial or its dependents depend on a partial.
	fn is_source_dependent_on_partial(&self, source_name: &str, partial_name: &str) -> bool {
		fn dfs<'a>(
			partials: &'a HashMap<String, Partial>,
			current_partial_name: &'a str,
			target_partial_name: &'a str,
			seen: &mut HashSet<&'a str>,
		) -> bool {
			if seen.contains(current_partial_name) {
				return false;
			}

			seen.insert(current_partial_name);

			if current_partial_name == target_partial_name {
				return true;
			}

			if let Some(partial) = partials.get(current_partial_name) {
				if partial
					.get_dependencies()
					.contains_builder(target_partial_name)
				{
					return true;
				}

				for dep in partial.get_dependencies().iter_builder_dependencies() {
					if dfs(partials, dep, target_partial_name, seen) {
						return true;
					}
				}
			}

			false
		}

		for root_partial_name in self
			.sources
			.get(source_name)
			.unwrap()
			.get_dependencies()
			.builder_dependencies
			.iter()
		{
			let mut seen = HashSet::new();
			if dfs(&self.templates, root_partial_name, partial_name, &mut seen) {
				return true;
			}
		}

		false
	}

	/// Updates the global variables, and rebuilds all the sources that rely on
	/// them.
	pub fn update_variables<'a>(&mut self, variables: Vec<(String, impl Borrow<Expression<'a>>)>) {
		// create an empty evaluation context
		let global_variables = HashMap::new();
		let builders = HashMap::<_, Partial>::new();
		let functions = HashMap::new();
		let local_variables = vec![];
		let mut context = EvaluationContext::new(
			&global_variables,
			local_variables,
			&builders,
			&functions,
			1,
			self.options.evaluation_context_warnings,
			ContextLocation::Variables,
		);

		// calculate the value of each variable and extract the names
		let mut names = Vec::new();
		for (name, expression) in variables.into_iter() {
			let value = evaluate_expression(&mut context, expression.borrow());
			names.push(name.clone());
			self.variables.insert(name, value);
		}

		// list sources to update
		let mut sources_to_update = Vec::new();
		for name in self.sources.keys().clone().into_iter() {
			if self.is_source_dependent_on_variables(name, names.as_slice()) {
				sources_to_update.push(name.clone());
			}
		}

		// update sources
		for name in sources_to_update {
			self.write_source(&name);
		}
	}

	/// If there exists a circular dependency between templates, it will be
	/// returned.
	fn find_cycle(&mut self) -> Option<Vec<String>> {
		/// Runs dfs cycle search on the templates
		fn dfs(
			templates: &HashMap<String, Partial>,
			partial_name: &str,
			seen: &mut HashSet<String>,
			previously_seen: &mut HashSet<String>,
			cycle: &mut Vec<String>,
			add_to_cycle: &mut bool,
		) -> bool {
			// when a cycle is found, this if statement is triggered
			if seen.contains(partial_name) {
				// indicate to the following templates to add their names to
				// the cycle vec
				*add_to_cycle = true;

				cycle.push(partial_name.to_string());
				return true;
			}

			// the current partial was already checked
			if previously_seen.contains(partial_name) {
				return false;
			}

			// push current partial name to the dfs stack
			seen.insert(partial_name.to_string());

			// globally store visited templates
			previously_seen.insert(partial_name.to_string());

			// iterate over the templates dependent upon and continue
			// searching for cycles
			if let Some(partial) = templates.get(partial_name) {
				for dep in partial.get_dependencies().iter_builder_dependencies() {
					// a cycle was found somewhere inside the call stack
					if dfs(templates, dep, seen, previously_seen, cycle, add_to_cycle) {
						if *add_to_cycle {
							cycle.push(partial_name.to_string());

							// if the current name equals to the first name,
							// then the cycle's beginning was found
							if partial_name.cmp(cycle[0].as_str()) == std::cmp::Ordering::Equal {
								*add_to_cycle = false;
							}
						}
						return true;
					}
				}
			}

			// pop current partial name
			seen.remove(partial_name);

			false
		}

		// starting at each source, run dfs over the templates and try to find a cycle
		let mut previously_seen = HashSet::new(); // global dfs helper
		for source in self.sources.values_mut() {
			for partial_name in source.get_dependencies().iter_builder_dependencies() {
				// local dfs helpers
				let mut seen = HashSet::new();
				let mut cycle = Vec::new();
				let mut add_to_cycle = true;

				// run dfs
				if dfs(
					&self.templates,
					partial_name,
					&mut seen,
					&mut previously_seen,
					&mut cycle,
					&mut add_to_cycle,
				) {
					return Some(cycle);
				}
			}
		}

		// false
		None
	}

	/// If specified by the options calls `find_cycle` and warns in case of a
	/// cycle
	fn check_cycle(&mut self) {
		if self.options.builder_warnings.found_cycle {
			if let Some(cycle) = self.find_cycle() {
				log::warn!(
					"found a circular dependency between the following files: {:?}",
					cycle
				);
			}
		}
	}

	/// Evaluates a source and writes it to its target.
	pub fn write_source(&mut self, name: &str) {
		// create the evaluation context
		let mut context = EvaluationContext::new(
			&self.variables,
			vec![],
			&self.templates,
			&self.functions,
			self.options.max_depth,
			self.options.evaluation_context_warnings,
			ContextLocation::source(name),
		);

		// get the source
		let source = self.sources.get_mut(name).unwrap();

		// evaluate the source, storing its output to its target
		source.evaluate_and_store(&mut context);
	}

	/// Finds and returns a source by its name.
	pub fn get_source(&self, name: &str) -> Option<&Partial> {
		self.sources.get(name)
	}

	/// Rescans and rebuilds the entire project.
	pub fn rebuild(&mut self) {
		// clear
		// self.sources.clear();
		// self.templates.clear();
		// self.variables.clear();

		// rescan
		self.rescan_templates();
		self.rescan_sources();
		self.rescan_variables();

		self.check_cycle();

		// iterate over the sources and generate output
		let sources: Vec<String> = self.sources.keys().cloned().collect();
		for name in sources {
			self.write_source(name.as_str());
		}
	}

	/// Helper function, scans a directory, adding missing partials to the
	/// specified map
	fn rescan(
		scan_path: &Option<PathBuf>,
		write_path: &Option<PathBuf>,
		map: &mut HashMap<String, Partial>,
		ignore_extensions: bool,
		warn_on_skip: bool,
	) {
		if let Some(path) = scan_path.as_ref() {
			for (path, name_stack) in list_files(path, ignore_extensions, warn_on_skip) {
				// join the vec into a string to create a name
				let name = name_stack.clone().join("/");

				// if the name does not exist in the map, create a new partial and add it
				if !map.contains_key(&name) {
					// calculate the write target
					let write = if let Some(write_path) = write_path {
						// join the vec into a path to create the target's path
						let mut target_path = write_path.clone();
						for name in name_stack.into_iter() {
							target_path = target_path.join(name);
						}
						Target::External(target_path)
					} else {
						// if a write path is not give, the partial is assumed to have a local write target
						Target::Local(String::new())
					};

					// TODO: what to do about this error?
					map.insert(
						name,
						Partial::new(ReadWriteTarget {
							read: Target::External(path),
							write,
						})
						.unwrap(),
					);
				}
			}
		}
	}

	/// Rescans the templates directory, parsing and adding all the missing
	/// files.
	fn rescan_templates(&mut self) {
		Self::rescan(
			&self.templates_dir,
			&None,
			&mut self.templates,
			true,
			self.options.builder_warnings.file_skip,
		)
	}

	/// Rescans the sources directory, parsing and adding all the missing files.
	fn rescan_sources(&mut self) {
		Self::rescan(
			&self.sources_dir,
			&self.generated_dir,
			&mut self.sources,
			false,
			self.options.builder_warnings.file_skip,
		)
	}

	fn rescan_variables(&mut self) {
		lazy_static! {
			static ref FILE_PATH_RE: Regex =
				Regex::new(r"^([a-zA-Z_][0-9a-zA-Z_]+)\s*=\s*(.*)\s*").unwrap();
		}

		if let Some(path) = &self.variables_file {
			let contents = std::fs::read_to_string(path).unwrap();

			// iterate over each line and evaluate it
			for (index, line) in contents.lines().enumerate() {
				if let Some(captures) = FILE_PATH_RE.captures(line) {
					if let (Some(name), Some(expression)) = (captures.get(1), captures.get(2)) {
						let name = name.as_str().to_string();
						let expression = expression.as_str();

						// tokenize
						let tokens = match Tokenizer::new(expression, TokenizerOptions::default())
							.tokenize()
						{
							Ok(tokens) => tokens,
							Err(err) => {
								if self.options.builder_warnings.invalid_variable_file_line {
									log::warn!(
										"Could not parse variable {name} on line {index}. Tokenization error: {err}."
									)
								}
								continue;
							}
						};

						// parse
						let nodes = match Parser::new(&tokens).parse() {
							Ok(nodes) => nodes,
							Err(err) => {
								if self.options.builder_warnings.invalid_variable_file_line {
									log::warn!("Could not parse variable {name} on line {index}. Parser error: {err}.")
								}
								continue;
							}
						};

						// ensure there is only one node
						if nodes.len() != 1 {
							if self.options.builder_warnings.invalid_variable_file_line {
								log::warn!("Could not parse variable {name} on line {index}. Expected to have exactly one expression.")
							}
							continue;
						}

						// and it is an expression
						if let NodeContent::Expression(expression) = &nodes[0].content {
							// and its an expression
							self.update_variables(vec![(name, expression)]);
						} else {
							if self.options.builder_warnings.invalid_variable_file_line {
								log::warn!("Could not parse variable {name} on line {index}. Parsed node is not an expression.")
							}
						}
					}
				} else {
					log::warn!("Could not parse on line {index}: invalid syntax.")
				}
			}
		}
	}

	fn find_by_path<P: AsRef<Path>>(map: &HashMap<String, Partial>, path: P) -> Option<String> {
		let path = path.as_ref();
		map.iter().find_map(|(name, p)| {
			if p.read_write_target.read.path_eq(path) {
				Some(name.clone())
			} else {
				None
			}
		})
	}

	/// Notify the system that a partial has been either changed or added,
	/// returns the name of the partial if it was found.
	fn notify_partial<P: AsRef<Path>>(
		map: &mut HashMap<String, Partial>,
		path: P,
	) -> Option<String> {
		if let Some(name) = Self::find_by_path(&map, path.as_ref()) {
			// the path exists, so the partial needs to be updated
			map.get_mut(&name).unwrap().reload();

			Some(name.to_string())
		} else {
			None
		}
	}

	/// Notify the system that either a source or a partial has been either
	/// changed or added.
	fn notify<P: AsRef<Path>>(&mut self, path: P) {
		let path = std::fs::canonicalize(path).unwrap();

		// sources
		if let Some(sources_dir) = self.sources_dir.as_ref() {
			if path.starts_with(sources_dir.as_path()) {
				if let Some(name) = Self::notify_partial(&mut self.sources, &path) {
					self.write_source(&name);
				} else {
					self.rescan_sources();
				}
			}
		}

		// partials
		if let Some(templates_dir) = self.templates_dir.as_ref() {
			if path.starts_with(templates_dir.as_path()) {
				if let Some(name) = Self::notify_partial(&mut self.templates, &path) {
					let sources_names = self.sources.keys().cloned().collect::<Vec<_>>();
					for source in sources_names {
						if self.is_source_dependent_on_partial(&source, &name) {
							self.write_source(&source);
						}
					}
				} else {
					self.rescan_templates();
				}
			}
		}

		// variables
		if let Some(variables_file) = self.variables_file.as_ref() {
			if path.as_path() == variables_file {
				self.rescan_variables();
			}
		}

		self.check_cycle();
	}

	/// Notify the system that either a source or a partial has been deleted.
	fn remove<P: AsRef<Path>>(&mut self, path: P, rebuild: bool) {
		// remove from templates
		Self::find_by_path(&self.templates, path.as_ref())
			.and_then(|name| self.templates.remove(&name));

		// remove from sources
		Self::find_by_path(&self.sources, path.as_ref())
			.and_then(|name| self.sources.remove(&name));

		if rebuild {
			self.rebuild();
		}
	}

	/// Notify the system that either a source or a partial has been renamed.
	fn rename<P: AsRef<Path>, P2: AsRef<Path>>(&mut self, p_from: P, p_to: P2) {
		let p_from = p_from.as_ref();
		let p_to = p_to.as_ref();

		// check if the variables file was renamed
		self.variables_file.as_mut().map(|path| {
			if path == p_from {
				*path = p_to.to_path_buf();
			}
		});

		// check if the sources or templates were renamed
		self.remove(p_from, false);
		self.rescan_templates();
		self.rescan_sources();
		self.rebuild();
	}

	// watching the files and updating them live
	pub fn watch(&mut self) -> notify::Result<()> {
		self.rebuild();

		// create a file watcher and a channel for notifying for events
		let (tx, rx) = mpsc::channel();
		let mut watcher: RecommendedWatcher = Watcher::new(
			tx,
			std::time::Duration::from_millis(self.options.watcher_delays_millis),
		)?;

		// watch the templates and the sources directory
		if let Some(path) = self.templates_dir.as_ref() {
			watcher.watch(path, RecursiveMode::Recursive)?;
		}
		if let Some(path) = self.sources_dir.as_ref() {
			watcher.watch(path, RecursiveMode::Recursive)?;
		}

		// watch the variables file
		if let Some(path) = self.variables_file.as_ref() {
			watcher.watch(path, RecursiveMode::NonRecursive)?;
		}

		// main loop - listen for events and regenerate files as neccessary
		loop {
			match rx.recv() {
				Ok(event) => self.handle_event(event),
				Err(e) => println!("watch error: {:?}", e),
			}
		}
	}

	fn handle_event(&mut self, event: DebouncedEvent) {
		match event {
			DebouncedEvent::NoticeWrite(..) | DebouncedEvent::NoticeRemove(..) => {}
			DebouncedEvent::Create(path_buf) => self.notify(path_buf),
			DebouncedEvent::Write(path_buf) => self.notify(path_buf),
			DebouncedEvent::Chmod(path_buf) => self.notify(path_buf),
			DebouncedEvent::Remove(path_buf) => self.notify(path_buf),
			DebouncedEvent::Rename(path_buf_from, path_buf_to) => {
				self.rename(path_buf_from, path_buf_to)
			}
			DebouncedEvent::Rescan => {}
			DebouncedEvent::Error(..) => {}
		}
	}
}
