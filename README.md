# mini-builder-rs
A static website generator.

Programming languages use functions to avoid rewriting similar logic.
HTML doesn't have a similar features. This crate provides a simple language
that can be inserted into HTML (or really any) files in order to
programmatically generate new files.

For example, consider the following HTML:
```html
<section>
    <a href="page 1" class="selected">Page1</a>  
    <a href="page 2" class="">Page2</a>  
    <a href="page 3" class="">Page3</a>  
</section>
```
Without a static website generator, this html snippet would have to be copied
and pasted into each of the pages with a minor change of which `a` tag will have
the `selected` class. However with a static website generator, this snippet
could be stored in a file for example `navigation.html` and then reused in every
page. For example:

`navigation.html`
```html
<section>
    <a href="page 1" class="{{ if page == 'page 1' ? 'selected' : ''}}">Page1</a>  
    <a href="page 2" class="{{ if page == 'page 2' ? 'selected' : ''}}">Page2</a>  
    <a href="page 3" class="{{ if page == 'page 3' ? 'selected' : ''}}">Page3</a>  
</section>
```

`page1.html`
```html
...
{{@ navigation(page = 'page 1')}}
...
```

# Examples
For some code examples check out the `examples` directory.

# Builder
The `Builder` handles 
```rust
mini_builder_rs::builder::Builder::new(
	// sources - the source for the pages that will be generated
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

...

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
```

# Sources and Templates
Sources the files that correspond to the output files. Templates are files that
are used in order by sources or other templates to generate text. For example if
the sources are `page_1.html`, `page_2.html` and the templates are
`navigation.html`, and `footer.html`, then after the site has finished, the
output will be the expanded versions of only `page_1.html` and `page_2.html`.

Templates are called with the syntax `@ template(key1 = value1, ...)`.

The name of the template is either given explicitly when passed as a string,
or is the path of the file relative to the templates directory when it loaded
from a file ignoring extensions (for example the template
`template/sub_dir1/temp1.html` name would be `sub_dir1/tmp1`).

# Variables and Functions
Values are defined with the following enum:
```rust
pub enum Value {
    Number(f32),
    Bool(bool),
    Text(String),
    List(Vec<Value>),
    None,
}
```
Functions are defined by the type: `Box<dyn Fn(&[Value]) -> Value + 'static>`.

## Directives
A directive is a piece of code with the pattern `{{...}}` that adds logic to
plain files. Control flow directives such as `if` statements use a slightly
different pattern: `{{# ...}}`. The language used inside the directives can be
summarized in a few examples:
* Expressions: 
```html
<!-- if the variables `a` and `b` are defined, they will be added and returned
-->
<p> a + b = {{ a + b }} </p>
```
* If statements:
```html
{{# if page == 'page 1' }}
    <!-- if the variable `page` exists and equals to 'page 1' this section will
	be evaluated  -->
    ...
{{# elif page == 'page 2'}}
    <!-- elif is short for else if -->
    ...
{{# else }}
    ....
{{#}}
```
* For loops:
```html
<!-- the for each loop will be evaluated if the variable `files` exists and is
a list -->
{{# for file in files}}
    <h2>{{ file }}</h2>
{{#}}
```
* Ternary:
```html
{# if a != None && b != None}}
    <p>a is{{ a > b ? 'greater than : a < b ? 'smaller than' : 'equals to'}} b</p>
{{# else}}
    <p>a or b are none</p>
{{#}}
```
* Templates:
```html
<!-- Templates are not control flow, they are expressions and therefore can be
with other expressions. if no variables are passed to a template then both the
syntaxes `@ template` and `@ template()` are valid. -->
{{ use_pretty_header ? @ pretty_header : @ normal_header}}

<!-- call the `footer` template with a single variable called
`use_pretty_footer` whose value equals to the variable `use_pretty_header`
value's -->
{{@ footer(use_pretty_footer = use_pretty_header)}}
```
