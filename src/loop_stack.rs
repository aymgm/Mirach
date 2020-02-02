
use symbol::Symbol;
use super::llvm;

pub struct LoopStackElem {
    pub name: Option<Symbol>,
    pub begin_bb: llvm::BasicBlock,
    pub end_bb: llvm::BasicBlock,
}
