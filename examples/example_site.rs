use mini_builder_rs::value::Value;
use simple_logger::SimpleLogger;

static DATA: &[(&str, f32)] = &[
	("file1.txt", 1024.0),
	("file2.txt", 2048.0),
	("file3.txt", 4096.0),
];

fn get_file_size(values: &[Value]) -> Value {
	if let Some(Value::Text(name)) = values.get(0) {
		for (file_name, file_size) in DATA {
			if file_name == name {
				return Value::Number(*file_size);
			}
		}
	}
	Value::None
}

fn main() {
	SimpleLogger::new().init().unwrap();

	mini_builder_rs::builder::Builder::new(
		// sources - the pages that will be generated
		Some("./examples/example_site/sources".into()),
		// templates - the partial files that can be referenced
		//             from sources or other templates
		Some("./examples/example_site/templates".into()),
		// generated - where the transformed sources will be placed
		Some("./examples/example_site/generated".into()),
		// variables - a file containing global variables and their values
		Some("./examples/example_site/variables".into()),
		// see mini_builder_rs::builder::BuilderOptions
		Default::default(),
	)
	.unwrap()
	// variable that will be available for every source and template
	// (unless shadowed)
	.add_variable("title", Value::text("Website Title"))
	.add_variable(
		"files",
		Value::List(DATA.iter().map(|x| Value::text(x.0)).collect::<Vec<_>>()),
	)
	// functions that can be called from every source and template
	.add_function("get_file_size", Box::new(get_file_size) as _)
	// watch for changes in the sources and templates directories,
	// updating the generated directory live
	.watch()
	.unwrap();
}
