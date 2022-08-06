use crate::parser::expression::Expression;

pub type Blocks<'a, 'b> = Vec<Box<Block<'a, 'b>>>;

/// A block that is produced by [super::evaluator::EvaluatorBuilder] and can be
/// evaluated using a [super::evaluation_context::EvaluationContext].
#[derive(Debug)]
pub enum Block<'a, 'b> {
	Source(&'b str),
	Expression(&'a Expression<'b>),
	If(Box<IfBlock<'a, 'b>>),
	For(Box<ForBlock<'a, 'b>>),
}

/// If-Else chain control flow block.
#[derive(Debug)]
pub struct IfBlock<'a, 'b> {
	pub if_blocks: Vec<(&'a Expression<'b>, Blocks<'a, 'b>)>,
	pub else_blocks: Option<Blocks<'a, 'b>>,
}

impl<'a, 'b> IfBlock<'a, 'b> {
	pub fn new(
		if_blocks: Vec<(&'a Expression<'b>, Blocks<'a, 'b>)>,
		else_blocks: Option<Blocks<'a, 'b>>,
	) -> Self {
		Self {
			if_blocks,
			else_blocks,
		}
	}
}

/// For-Each control flow block.
#[derive(Debug)]
pub struct ForBlock<'a, 'b> {
	pub var_name: &'b str,
	pub expression: &'a Expression<'b>,
	pub blocks: Blocks<'a, 'b>,
}

impl<'a, 'b> ForBlock<'a, 'b> {
	pub fn new(var_name: &'b str, expression: &'a Expression<'b>, blocks: Blocks<'a, 'b>) -> Self {
		Self {
			var_name,
			expression,
			blocks,
		}
	}
}
