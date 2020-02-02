

use std::collections::HashMap;
use symbol::Symbol;
use super::{llvm_wrapper, MirErr,};

pub enum ScopedValue {
    Const(llvm_wrapper::Value),
    Var(llvm_wrapper::Type, llvm_wrapper::Value),
}

pub struct ScopeStack(Vec<HashMap<Symbol, ScopedValue>>);
impl ScopeStack {
    pub fn new()->Self {
        ScopeStack(vec![HashMap::new()])
    }

    pub fn leave_scope(&mut self) -> Result<(), MirErr> {
        if self.0.is_empty() {
            return Err(MirErr::LeavingStaticScope)
        }
        self.0.pop();
        return Ok(())
    }
    pub fn enter_scope(&mut self) {
        self.0.push(HashMap::new());
    }

    pub fn insert(&mut self, sym: Symbol, v: ScopedValue) {
        if let Some(last) = self.0.last_mut() {
            last.insert(sym, v);
        }
    }

    pub fn get_value(&self, b: &mut llvm_wrapper::Builder, sym: Symbol)->Option<llvm_wrapper::Value> {
        for hm in self.0.iter().rev() {
            if let Some(v) = hm.get(&sym) {
                return match v {
                    ScopedValue::Const(c) => Some(*c),
                    ScopedValue::Var(t, p) => {
                        //let pt = llvm_wrapper::Type::ptr(*t);
                        //let v = b.load_with_type(&pt, p, "");
                        let v = b.load_with_type(t, p, "");
                        //let v = b.load(p, "");
                        Some(v)
                    }
                }
                //return Some(*v);
            }
        }
        None
    }

    pub fn set_value(&self, b: &mut llvm_wrapper::Builder, sym: Symbol, val: &llvm_wrapper::Value)->Result<llvm_wrapper::Value, MirErr> {
        for hm in self.0.iter().rev() {
            if let Some(v) = hm.get(&sym) {
                return match v {
                    ScopedValue::Const(c) => Err(MirErr::ConstNotModifiable(sym)),
                    ScopedValue::Var(t, p) => {
                        //let pt = llvm_wrapper::Type::ptr(*t);
                        //let v = b.load_with_type(&pt, p, "");
                        let v = b.store(p, val);
                        //let v = b.load(p, "");
                        Ok(v)
                    }
                }
                //return Some(*v);
            }
        }
        Err(MirErr::SymbolNotFound(sym))
    }

    pub fn clear(&mut self) {
        self.0.clear();
    }
}
