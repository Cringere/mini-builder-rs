#[derive(Debug)]
pub enum BuilderError {
	IoError(std::io::Error),
	InvalidDirectories,
}

impl std::fmt::Display for BuilderError {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::IoError(err) => write!(f, "{}", err),
			Self::InvalidDirectories => write!(
				f,
				"Invalid directories, ensure that none of the directories contain each other."
			),
		}
	}
}

impl std::error::Error for BuilderError {
	fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
		match self {
			Self::IoError(err) => Some(err.clone()),
			Self::InvalidDirectories => None,
		}
	}
}

impl From<std::io::Error> for BuilderError {
	fn from(err: std::io::Error) -> Self {
		Self::IoError(err)
	}
}
