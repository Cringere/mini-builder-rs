//! The data types and their interactions that are used in the language

use std::borrow::Borrow;

use approx::{relative_eq, relative_ne};

// TODO: epsilon compare, should text be `Cow` to save on copies?
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
	Number(f32),
	Bool(bool),
	Text(String),
	List(Vec<Value>),
	None,
}

impl Value {
	// constructors
	pub fn text(text: impl ToString) -> Value {
		Value::Text(text.to_string())
	}

	// general
	pub fn is_none(&self) -> bool {
		match self {
			Self::None => true,
			_ => false,
		}
	}

	pub fn bool_or_false(&self) -> bool {
		match self {
			Self::Bool(b) => *b,
			_ => false,
		}
	}

	// arithmetics
	pub fn add(&self, other: &Self) -> Self {
		if let Self::List(l1) = self {
			let mut l1 = l1.clone();
			if let Self::List(l2) = other {
				let l2 = l2.clone();
				l1.extend(l2);
			} else {
				l1.push(other.clone());
			}
			Self::List(l1)
		} else if let Self::List(l2) = other {
			let mut l2 = l2.clone();
			l2.insert(0, self.clone());
			Self::List(l2)
		} else {
			match self {
				Self::Number(a) => match other {
					Self::Number(b) => Self::Number(*a + *b),
					Self::Bool(b) => Self::Number(*a + if *b { 1.0 } else { 0.0 }),
					Self::Text(b) => Self::Text(format!("{a}{b}")),
					Self::None => self.clone(),
					_ => Self::None,
				},
				Self::Bool(a) => match other {
					Self::Number(b) => Self::Number(if *a { 1.0 } else { 0.0 } + *b),
					Self::Text(b) => {
						Self::Text(format!("{}{b}", if *a { "true" } else { "false" }))
					}
					_ => Self::None,
				},
				Self::Text(a) => match other {
					Self::Number(b) => Self::Text(format!("{a}{b}")),
					Self::Bool(b) => {
						Self::Text(format!("{a}{}", if *b { "true" } else { "false" }))
					}
					Self::Text(b) => Self::Text(format!("{a}{b}")),
					Self::None => self.clone(),
					_ => Self::None,
				},
				Self::None => other.clone(),
				_ => Self::None,
			}
		}
	}

	pub fn sub(&self, other: &Self) -> Self {
		match self {
			Self::Number(a) => match other {
				Self::Number(b) => Self::Number(*a - *b),
				_ => Self::None,
			},
			_ => Self::None,
		}
	}

	pub fn mul(&self, other: &Self) -> Self {
		match self {
			Self::Number(a) => match other {
				Self::Number(b) => Self::Number(*a * *b),
				_ => Self::None,
			},
			_ => Self::None,
		}
	}

	pub fn div(&self, other: &Self) -> Self {
		match self {
			Self::Number(a) => match other {
				Self::Number(b) => Self::Number(*a / *b),
				_ => Self::None,
			},
			_ => Self::None,
		}
	}

	pub fn neg(&self) -> Self {
		match self {
			Self::Number(a) => Self::Number(-*a),
			_ => Self::None,
		}
	}

	// logic
	pub fn not(&self) -> Self {
		match self {
			Self::Bool(a) => Self::Bool(!*a),
			_ => Self::None,
		}
	}

	fn logic_helper(&self, other: &Self, op: impl Fn(bool, bool) -> bool) -> Value {
		match self {
			Self::Bool(a) => match other {
				Self::Bool(b) => Self::Bool(op(*a, *b)),
				_ => Self::None,
			},
			_ => Self::None,
		}
	}

	pub fn or(&self, other: &Self) -> Self {
		Self::logic_helper(&self, other, |a, b| a || b)
	}

	pub fn and(&self, other: &Self) -> Self {
		Self::logic_helper(&self, other, |a, b| a && b)
	}

	// comparisons
	fn compare_numbers(&self, other: &Self, op: impl Fn(f32, f32) -> bool) -> Value {
		match self {
			Self::Number(a) => match other {
				Self::Number(b) => Self::Bool(op(*a, *b)),
				_ => Self::None,
			},
			_ => Self::None,
		}
	}

	pub fn less_than(&self, other: &Self) -> Self {
		self.compare_numbers(other, |a, b| a < b)
	}

	pub fn less_equals(&self, other: &Self) -> Self {
		self.compare_numbers(other, |a, b| a <= b)
	}

	pub fn greater_than(&self, other: &Self) -> Self {
		self.compare_numbers(other, |a, b| a > b)
	}

	pub fn greater_equals(&self, other: &Self) -> Self {
		self.compare_numbers(other, |a, b| a >= b)
	}

	pub fn equals(&self, other: &Self) -> Self {
		if let (Self::Number(a), Self::Number(b)) = (self, other) {
			return Self::Bool(relative_eq!(*a, *b));
		}

		if let (Self::Bool(a), Self::Bool(b)) = (self, other) {
			return Self::Bool(a == b);
		}

		if let (Self::Text(a), Self::Text(b)) = (self, other) {
			return Self::Bool(a == b);
		}

		if self.is_none() && other.is_none() {
			return Self::Bool(true);
		}

		if self.is_none() || other.is_none() {
			return Self::Bool(false);
		}

		Self::None
	}

	pub fn not_equals(&self, other: &Self) -> Self {
		if let (Self::Number(a), Self::Number(b)) = (self, other) {
			return Self::Bool(!relative_ne!(*a, *b));
		}

		if let (Self::Bool(a), Self::Bool(b)) = (self, other) {
			return Self::Bool(a != b);
		}

		if let (Self::Text(a), Self::Text(b)) = (self, other) {
			return Self::Bool(a != b);
		}

		if self.is_none() && other.is_none() {
			return Self::Bool(false);
		}

		if self.is_none() || other.is_none() {
			return Self::Bool(true);
		}

		Self::None
	}
}

impl<B: Borrow<Value>> std::ops::Add<B> for &Value {
	type Output = Value;

	fn add(self, rhs: B) -> Self::Output {
		Value::add(&self, rhs.borrow())
	}
}

impl ToString for Value {
	fn to_string(&self) -> String {
		match self {
			Self::Number(a) => a.to_string(),
			Self::Bool(a) => a.to_string(),
			Self::Text(a) => a.to_string(),
			Self::List(l) => format!(
				"[{}]",
				l.iter()
					.map(|v| v.to_string())
					.collect::<Vec<_>>()
					.join(", ")
			),
			Self::None => String::new(),
		}
	}
}
