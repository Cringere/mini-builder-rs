use crate::parser::{expression::Expression, node::Node, node::NodeContent};

use super::{
	block::{Block, Blocks, ForBlock, IfBlock},
	evaluate_blocks,
	evaluation_context::EvaluationContext,
	evaluator_builder_error::EvaluatorBuilderError,
	list_dependencies_block, Dependencies, Evaluate,
};

pub type BlocksResult<'a, 'b> = Result<Blocks<'a, 'b>, EvaluatorBuilderError>;
pub type IfBlockResult<'a, 'b> = Result<IfBlock<'a, 'b>, EvaluatorBuilderError>;
pub type EvaluatorResult<'a, 'b> = Result<Evaluator<'a, 'b>, EvaluatorBuilderError>;

pub struct EvaluatorBuilder<'a, 'b> {
	nodes: &'a [Box<Node<'b>>],
	i: usize,
}

impl<'a, 'b> EvaluatorBuilder<'a, 'b> {
	pub fn new(nodes: &'a [Box<Node<'b>>]) -> Self {
		Self { nodes, i: 0 }
	}

	fn advance(&mut self) {
		self.i = (self.i + 1).min(self.nodes.len());
	}

	fn regress(&mut self) {
		if self.i > 0 {
			self.i -= 1;
		}
	}

	fn peek(&mut self) -> Option<&'a Box<Node<'b>>> {
		if self.i < self.nodes.len() {
			Some(&self.nodes[self.i])
		} else {
			None
		}
	}

	fn next(&mut self) -> Option<&'a Box<Node<'b>>> {
		let ret = self.peek();
		self.advance();
		ret
	}

	fn build_if(&mut self, first_expression: &'a Expression<'b>) -> IfBlockResult<'a, 'b> {
		let mut if_blocks = vec![(first_expression, Vec::new())];
		let mut else_blocks = None;
		while let Some(n) = self.next() {
			match &n.content {
				NodeContent::Elif(expression) => {
					// push an empty `if_block` with the new expression
					if_blocks.push((expression, Vec::new()));
				}
				NodeContent::Else => {
					// add the final blocks in the if-else chain and break
					let blocks = self.build_blocks(true, false)?;
					else_blocks = Some(blocks);
					break;
				}
				NodeContent::End => break,
				_ => {
					self.regress();

					// add blocks to the current `if`
					let blocks = self.build_blocks(false, true)?;
					if_blocks.last_mut().unwrap().1.extend(blocks.into_iter());
				}
			}
		}
		Ok(IfBlock::new(if_blocks, else_blocks))
	}

	fn build_blocks(&mut self, consume_end: bool, expect_else: bool) -> BlocksResult<'a, 'b> {
		let mut blocks = Vec::new();
		while let Some(n) = self.next() {
			match &n.content {
				NodeContent::Source(source) => blocks.push(Box::new(Block::Source(source))),
				NodeContent::Expression(expression) => {
					blocks.push(Box::new(Block::Expression(expression)))
				}
				NodeContent::If(expression) => {
					let if_block = self.build_if(expression)?;
					blocks.push(Box::new(Block::If(Box::new(if_block))));
				}
				NodeContent::For(var_name, expression) => {
					let inner_blocks = self.build_blocks(true, false)?;
					let for_block = ForBlock::new(var_name, expression, inner_blocks);
					blocks.push(Box::new(Block::For(Box::new(for_block))));
				}
				NodeContent::End => {
					if !consume_end {
						self.regress();
					}
					break;
				}
				NodeContent::Else if expect_else => {
					self.regress();
					break;
				}
				NodeContent::Elif(..) if expect_else => {
					self.regress();
					break;
				}
				_ => todo!("error"),
			}
		}
		Ok(blocks)
	}

	pub fn build(&mut self) -> BlocksResult<'a, 'b> {
		self.build_blocks(true, false)
	}

	pub fn build_evaluator(&mut self) -> EvaluatorResult<'a, 'b> {
		Ok(Evaluator::new(self.build()?))
	}
}

pub struct Evaluator<'a, 'b> {
	blocks: Blocks<'a, 'b>,
	// shallow dependencies - non builder recursive
	dependencies: Dependencies,
}

impl<'a, 'b> Evaluator<'a, 'b> {
	fn new(blocks: Blocks<'a, 'b>) -> Self {
		// calculate dependencies
		let mut dependencies = Dependencies::empty();
		for block in &blocks {
			list_dependencies_block(block, &mut dependencies);
		}

		Self {
			blocks,
			dependencies,
		}
	}
}

impl<'a, 'b> Evaluate for Evaluator<'a, 'b> {
	fn evaluate<'ea>(&self, context: &mut EvaluationContext<'ea, impl Evaluate>) -> String {
		evaluate_blocks(context, &self.blocks)
	}

	fn get_dependencies(&self) -> &Dependencies {
		&self.dependencies
	}
}
