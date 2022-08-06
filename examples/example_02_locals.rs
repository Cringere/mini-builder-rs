use mini_builder_rs::value::Value;

fn add(values: &[Value]) -> Value {
	values.into_iter().fold(Value::None, |acc, x| &acc + x)
}

fn main() {
	// define a source and a template
	let main_page = "source: {{ @template1 }}".to_string();
	let template_1 = "template 1: {{ @template2(a = 10) }}".to_string();
	let template_2 =
		"template 2: doing some calculations: 1 + a + b = {{ add(1, a, b) }}".to_string();

	// create the builder
	let mut builder =
		mini_builder_rs::builder::Builder::new(None, None, None, None, Default::default()).unwrap();

	// add sources, templates, and variables
	builder
		.add_local_source("main", main_page)
		.unwrap()
		.add_local_templates(&[("template1", &template_1), ("template2", &template_2)])
		.unwrap()
		.add_variable("b", Value::Number(100.0))
		.add_function("add", Box::new(add) as _);

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
