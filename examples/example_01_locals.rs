use mini_builder_rs::value::Value;

fn main() {
	// define a source and a template
	let main_page = "page: {{ page }}: {{ @template1(name = 'name1') }}".to_string();
	let template_1 = "saying hi to: {{name}}".to_string();

	// create the builder
	let mut builder =
		mini_builder_rs::builder::Builder::new(None, None, None, None, Default::default()).unwrap();

	// add the source and the template
	builder
		.add_local_source("main", main_page)
		.unwrap()
		.add_local_template("template1", template_1)
		.unwrap();

	// add a variable
	builder.add_variable("page", Value::text("Main Page"));

	// build
	builder.rebuild();

	// print the generated output
	println!(
		"{}",
		builder
			// get source by a specified name
			.get_source("main")
			.unwrap()
			// get the write target, in this case a string
			.read_write_target
			.write
			// read from the write target
			.read()
	)
}
