use super::{llvm, MirErr};

pub struct FuncStackElem {
    pub func: llvm::Value,
    pub alloca_bb: llvm::BasicBlock,
}


pub struct FuncStack(Vec<FuncStackElem>);
impl FuncStack {
    pub fn new()->Self {
        FuncStack(Vec::new())
    }
    pub fn get_current(&mut self)->Option<&mut FuncStackElem> {
        self.0.last_mut()
    }
    pub fn push(&mut self, el: FuncStackElem) {
        self.0.push(el)
    }
    pub fn pop(&mut self)->Option<FuncStackElem> {
        self.0.pop()
    }
    pub fn insert_alloca(&mut self, builder: &mut llvm::Builder, current_bb: &llvm::BasicBlock, ty: &llvm::Type)->Result<llvm::Value, MirErr> {
        let fse = self.get_current().ok_or(MirErr::LocalVarOnGlobal)?;
        let alloca_bb = &fse.alloca_bb;
        builder.set_position_at_end_of(alloca_bb);
        let ret = builder.alloca(ty, "");
        builder.set_position_at_end_of(current_bb);
        Ok(ret)
    }
}