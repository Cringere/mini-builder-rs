#[derive(Debug)]
pub enum TokenizerError {
	UnexpectedCharacter(usize),
	MustHaveFilePathAfterAt(usize, String),
}

impl std::fmt::Display for TokenizerError {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::UnexpectedCharacter(i) => write!(f, "Unexpected character at index {i}."),
			Self::MustHaveFilePathAfterAt(i, at) => write!(
				f,
				"A file path must follow the `At`({at}) symbol. Error at index {i}."
			),
		}
	}
}

impl std::error::Error for TokenizerError {}
