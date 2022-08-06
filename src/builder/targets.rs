//!
use std::{
	io::Write,
	path::{Path, PathBuf},
};

pub enum Target {
	Local(String),
	External(PathBuf),
}

impl Target {
	pub fn is_local(&self) {
		matches!(self, Self::Local(..));
	}

	pub fn is_external(&self) {
		matches!(self, Self::External(..));
	}

	pub fn path_eq<P: AsRef<Path>>(&self, path: P) -> bool {
		match self {
			Self::External(p) => p == path.as_ref(),
			_ => false,
		}
	}

	pub fn read(&self) -> String {
		match &self {
			Target::Local(s) => s.clone(),
			Target::External(p) => std::fs::read_to_string(p).unwrap(),
		}
	}

	pub fn write(&mut self, content: String) {
		match self {
			Target::Local(s) => *s = content,
			Target::External(p) => {
				std::fs::create_dir_all(p.parent().unwrap()).unwrap();
				let mut file = std::fs::File::create(p).unwrap();
				file.write(content.as_bytes()).unwrap();
			}
		}
	}
}

pub struct ReadWriteTarget {
	pub read: Target,
	pub write: Target,
}
