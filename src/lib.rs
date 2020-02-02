#![allow(unused)] // TODO: Remove me

extern crate symbol;
extern crate thiserror;

use std::collections::HashMap;
use symbol::Symbol;
use thiserror::Error;

mod llvm_wrapper;
use llvm_wrapper as llvm;

mod scope_stack;
use scope_stack::{ScopeStack, ScopedValue};

mod func_stack;
use func_stack::{FuncStackElem, FuncStack};

mod loop_stack;
use loop_stack::LoopStackElem;

#[cfg(test)]
mod unit_tests;

pub type Meta = (u32, u32, u32);

#[derive(Error, Debug)]
pub enum MirErr {
    #[error("Cannot create local variable at global scope")]
    LocalVarOnGlobal,

    #[error("`{0}` is not modifiable")]
    ConstNotModifiable(Symbol),

    #[error("Symbol `{0}` is not found. ")]
    SymbolNotFound(Symbol),

    #[error("Cannot get outer scope than static scope")]
    LeavingStaticScope,

    #[error("Must have value")]
    MustHaveValue,
}

#[derive(Debug)]
pub struct MirTy {
    ty: Symbol,
    args: Vec<MirTy>
}

pub type Mir = Box<MirRaw>;

#[derive(Debug)]
pub enum MirRaw {
    Unit(Meta),
    Bool(bool, Meta),
    I64(i64, Meta),
    U64(u64, Meta),
    F32(f32, Meta),
    F64(f64, Meta),
    StrLit(String, Meta),
    DefConst(Symbol, Mir, Meta),
    DefVar(Symbol, MirTy, Mir, Meta),
    StoreVar(Symbol, Mir, Meta),
    Load(Symbol, Meta),
    IAdd(Mir, Mir, Meta),
    ISub(Mir, Mir, Meta),
    IMul(Mir, Mir, Meta),
    IDiv(Mir, Mir, bool, Meta),
    IMod(Mir, Mir, bool, Meta),
    IEq(Mir, Mir, Meta),
    INeq(Mir, Mir, Meta),
    ILt(Mir, Mir, bool, Meta),
    ILte(Mir, Mir, bool, Meta),
    IGt(Mir, Mir, bool, Meta),
    IGte(Mir, Mir,bool,  Meta),

    FAdd(Mir, Mir, Meta),
    FSub(Mir, Mir, Meta),
    FMul(Mir, Mir, Meta),
    FDiv(Mir, Mir, Meta),
    FMod(Mir, Mir, Meta),

    LAnd(Mir, Mir, Meta),
    LOr(Mir, Mir, Meta),
    LNot(Mir, Meta),

    If(Mir, Mir, Mir, Meta),
    IfVoid(Vec<(Mir, Mir)>, Meta),
    While{label: Option<Symbol>, cond: Mir, body: Mir, meta: Meta},
    DoWhile{label: Option<Symbol>, cond: Mir, body: Mir, meta: Meta},
    Loop(Option<Symbol>, Mir, Meta),
    Continue(Option<Symbol>, Meta),
    Break(Option<Symbol>, Meta),
    Fun{name: Symbol, ty: MirTy, params: Vec<Symbol>, body: Mir, meta: Meta},
    DeclExternFun{name: Symbol, ty: MirTy, meta: Meta},
    Call(Mir, Vec<Mir>, Meta),
    Ret(Mir, Meta),
    RetUnit(Meta),
    Seq(Vec<Mir>, Meta),
}

pub struct Generator {
    ctx: llvm::Context,
    m: llvm::Module,
    b: llvm::Builder, 
    scope_stack: ScopeStack,
    func_stack: FuncStack,
    loop_stack: Vec<LoopStackElem>,
}

impl Generator {
    fn new(mod_name: String)->Self {
        use llvm::*;
        let ctx = Context::new();
        let m = Module::new(mod_name, &ctx);
        let b = Builder::new(&ctx);
        let scope_stack = ScopeStack::new();
        let func_stack = FuncStack::new();
        let loop_stack = Vec::new();
        Self{ctx, m, b, scope_stack, func_stack, loop_stack}
    }

    fn dispose(&mut self) {
        self.b.dispose();
        self.m.dispose();
        self.ctx.dispose();
        self.scope_stack.clear();
    }
    fn dump_llvm_ir(&mut self) {
        self.m.dump();
    }
    fn print_llvm_ir_to_file(&mut self, file: String) {
        self.m.print_to_file(file);
    }
    fn verify_module(&self){
        self.m.verify();
    }
    fn meta2str(meta: Meta)->String {
        format!("f{}l{}c{}", meta.0, meta.1, meta.2)
        //"".into()
    }

    fn gen_ty(&mut self, ty: &MirTy)->llvm::Type {
        match ty.ty.as_str() {
            "Unit" => llvm::Type::void(&self.ctx),
            "Bool" => llvm::Type::int1(&self.ctx),
            "I8" => llvm::Type::int8(&self.ctx),
            "I64" => llvm::Type::int64(&self.ctx),
            "F32" => llvm::Type::float(&self.ctx),
            "F64" => llvm::Type::double(&self.ctx),
            "Fun" => {
                let mut it = ty.args.iter();
                let ret_ty = it.next().expect("illegal function parameter");
                let param_ty = it.map(|x| self.gen_ty(x)).collect::<Vec<_>>();
                llvm::Type::function(self.gen_ty(ret_ty), &param_ty, false)
            },
            "FunVarArgs" => {
                let mut it = ty.args.iter();
                let ret_ty = it.next().expect("illegal function parameter");
                let param_ty = it.map(|x| self.gen_ty(x)).collect::<Vec<_>>();
                llvm::Type::function(self.gen_ty(ret_ty), &param_ty, true)
            },
            "Ptr" => {
                let t = if ty.args.is_empty() {
                    llvm::Type::void(&self.ctx)
                } else {
                    self.gen_ty(&ty.args[0])
                };
                llvm::Type::ptr(t)
            },
            _ => unimplemented!()
        }
    }
    fn gen(&mut self, mir: Mir)->Result<Option<llvm::Value>, MirErr> {
        use llvm::*;
        match *mir {
            MirRaw::Unit(_) => panic!("Unit can't be compiled"),
            MirRaw::Bool(b, _) => Ok(Some(Value::int1(&self.ctx, b))),
            MirRaw::I64(i, _) => Ok(Some(Value::int64(&self.ctx, i))),
            MirRaw::StrLit(s, _) => Ok(Some(self.b.global_string_ptr(s, "".into()))),
            MirRaw::DefConst(n, v, _) => {
                let v = self.gen(v)?.ok_or(MirErr::MustHaveValue)?;
                self.scope_stack.insert(n, ScopedValue::Const(v));
                Ok(Some(v))
            },
            MirRaw::DefVar(n, t, v, _) => {
                let v = self.gen(v)?.ok_or(MirErr::MustHaveValue)?;
                let t = self.gen_ty(&t);
                let cbb = self.b.get_current_basic_block();
                let m = self.func_stack.insert_alloca(&mut self.b, &cbb, &t).unwrap();
                self.scope_stack.insert(n, ScopedValue::Var(t, m));
                let r = self.scope_stack.set_value(&mut self.b, n, &v)?;
                Ok(Some(r))
            },
            MirRaw::StoreVar(n, v, _) => {
                let v = self.gen(v)?.ok_or(MirErr::MustHaveValue)?;
                let r = self.scope_stack.set_value(&mut self.b, n, &v)?;
                Ok(Some(r))
            },
            MirRaw::Load(n, _) => Ok(Some(self.scope_stack.get_value(&mut self.b, n).expect("variable not found"))),
            MirRaw::IAdd(l, r, m) => {
                let l = self.gen(l)?.ok_or(MirErr::MustHaveValue)?;
                let r = self.gen(r)?.ok_or(MirErr::MustHaveValue)?;
                Ok(Some(self.b.op_iadd(&l, &r, "")))
            },
            MirRaw::ISub(l, r, m) => {
                let l = self.gen(l)?.ok_or(MirErr::MustHaveValue)?;
                let r = self.gen(r)?.ok_or(MirErr::MustHaveValue)?;
                Ok(Some(self.b.op_isub(&l, &r, "")))
            },
            MirRaw::IMul(l, r, m) => {
                let l = self.gen(l)?.ok_or(MirErr::MustHaveValue)?;
                let r = self.gen(r)?.ok_or(MirErr::MustHaveValue)?;
                Ok(Some(self.b.op_imul(&l, &r, "")))
            },
            MirRaw::IDiv(l, r, u, m) => {
                let l = self.gen(l)?.ok_or(MirErr::MustHaveValue)?;
                let r = self.gen(r)?.ok_or(MirErr::MustHaveValue)?;
                Ok(Some(self.b.op_idiv(&l, &r, u,"")))
            },
            MirRaw::IEq(l, r, m) => {
                let l = self.gen(l)?.ok_or(MirErr::MustHaveValue)?;
                let r = self.gen(r)?.ok_or(MirErr::MustHaveValue)?;
                Ok(Some(self.b.op_ieq(&l, &r, "")))
            },
            MirRaw::INeq(l, r, m) => {
                let l = self.gen(l)?.ok_or(MirErr::MustHaveValue)?;
                let r = self.gen(r)?.ok_or(MirErr::MustHaveValue)?;
                Ok(Some(self.b.op_ineq(&l, &r, "")))
            },
            MirRaw::IGt(l, r, u, m) => {
                let l = self.gen(l)?.ok_or(MirErr::MustHaveValue)?;
                let r = self.gen(r)?.ok_or(MirErr::MustHaveValue)?;
                Ok(Some(self.b.op_igt(&l, &r, u,"")))
            },
            MirRaw::IGte(l, r, u, m) => {
                let l = self.gen(l)?.ok_or(MirErr::MustHaveValue)?;
                let r = self.gen(r)?.ok_or(MirErr::MustHaveValue)?;
                Ok(Some(self.b.op_igte(&l, &r, u,"")))
            },
            MirRaw::ILt(l, r, u, m) => {
                let l = self.gen(l)?.ok_or(MirErr::MustHaveValue)?;
                let r = self.gen(r)?.ok_or(MirErr::MustHaveValue)?;
                Ok(Some(self.b.op_ilt(&l, &r, u,"")))
            },
            MirRaw::ILte(l, r, u, m) => {
                let l = self.gen(l)?.ok_or(MirErr::MustHaveValue)?;
                let r = self.gen(r)?.ok_or(MirErr::MustHaveValue)?;
                Ok(Some(self.b.op_ilte(&l, &r, u,"")))
            },
            MirRaw::LAnd(l, r, m) => {
                let f = self.func_stack.get_current().expect("LAnd instruction must be in a function").func;
                let bb0 = self.b.get_current_basic_block();
                let bb1 = self.ctx.append_basic_block_after(&f, &bb0, format!("and_{}_rhs", Self::meta2str(m)));
                let bb2 = self.ctx.append_basic_block_after(&f, &bb1, format!("and_{}_end", Self::meta2str(m)));
                self.b.set_position_at_end_of(&bb0);
                let l = self.gen(l)?.ok_or(MirErr::MustHaveValue)?;
                self.b.cond_br(&l, &bb1, &bb2);
                self.b.set_position_at_end_of(&bb1);
                let r = self.gen(r)?.ok_or(MirErr::MustHaveValue)?;
                self.b.br(&bb2);
                self.b.set_position_at_end_of(&bb2);
                let mut phi = self.b.phi(&Type::int1(&self.ctx), "");
                phi.add_incomings(&vec![bb0, bb1], &vec![l, r]);
                Ok(Some(phi.get_value()))
            },
            MirRaw::LOr(l, r, m) => {
                let f = self.func_stack.get_current().expect("LOr instruction must be in a function").func;
                let bb0 = self.b.get_current_basic_block();
                let bb1 = self.ctx.append_basic_block_after(&f, &bb0, format!("or_{}_rhs", Self::meta2str(m)));
                let bb2 = self.ctx.append_basic_block_after(&f, &bb1, format!("or_{}_end", Self::meta2str(m)));
                self.b.set_position_at_end_of(&bb0);
                let l = self.gen(l)?.ok_or(MirErr::MustHaveValue)?;
                self.b.cond_br(&l, &bb2, &bb1);
                self.b.set_position_at_end_of(&bb1);
                let r = self.gen(r)?.ok_or(MirErr::MustHaveValue)?;
                self.b.br(&bb2);
                self.b.set_position_at_end_of(&bb2);
                let mut phi = self.b.phi(&Type::int1(&self.ctx), "");
                phi.add_incomings(&vec![bb0, bb1], &vec![l, r]);
                Ok(Some(phi.get_value()))
            },
            MirRaw::LNot(x, m) => {
                let x = self.gen(x)?.ok_or(MirErr::MustHaveValue)?;
                Ok(Some(self.b.op_lnot(&x, "")))
            },
            MirRaw::If(cond, tbody, fbody, m) => {
                let f = self.func_stack.get_current().expect("If instruction must be in a function").func;
                let bb0 = self.b.get_current_basic_block();
                let bb1 = self.ctx.append_basic_block_after(&f, &bb0, format!("if_{}_true", Self::meta2str(m)));
                let bb2 = self.ctx.append_basic_block_after(&f, &bb1, format!("if_{}_false", Self::meta2str(m)));
                let bb3 = self.ctx.append_basic_block_after(&f, &bb2, format!("if_{}_end", Self::meta2str(m)));

                self.b.set_position_at_end_of(&bb0);
                let cond = self.gen(cond)?.expect("condition expression can't be unit");
                self.b.cond_br(&cond, &bb1, &bb2);

                self.b.set_position_at_end_of(&bb1);
                let tbody = self.gen(tbody)?.expect("true branch of if can't be unit");
                self.b.br(&bb3);

                self.b.set_position_at_end_of(&bb2);
                let fbody = self.gen(fbody)?.expect("false branch of if can't be unit");
                self.b.br(&bb3);

                self.b.set_position_at_end_of(&bb3);
                let mut phi = self.b.phi(&tbody.get_type(), "");
                phi.add_incomings(&vec![bb1, bb2], &vec![tbody, fbody]);
                Ok(Some(phi.get_value()))
            },
            MirRaw::IfVoid(bs, m) => {
                let len = bs.len();
                let f = &self.func_stack.get_current().expect("IfVoid insruction must be in a function").func.clone();
                let mut bbs = Vec::with_capacity(len);
                let bb_begin = self.b.get_current_basic_block();
                let bb_end = self.ctx.append_basic_block(&f, format!("if_{}_end", Self::meta2str(m)));
                for i in 0..len {
                    let bb0 = self.ctx.insert_basic_block_before(&bb_end, format!("if_{}_block{}_begin", Self::meta2str(m), i));
                    let bb1 = if i != len - 1 {
                        self.ctx.insert_basic_block_before(&bb_end, format!("if_{}_block{}_end", Self::meta2str(m), i))
                    } else {
                        bb_end.clone()
                    };
                    bbs.push((bb0, bb1));
                }
                self.b.set_position_at_end_of(&bb_begin);
                for x in  bs.into_iter().enumerate() {
                    let (i, (b0, b1)) = x;
                    let (bb0, bb1) = &bbs[i];                    
                    let cond = self.gen(b0)?.ok_or(MirErr::MustHaveValue)?;
                    self.b.cond_br(&cond, &bb0, &bb1);
                    self.b.set_position_at_end_of(&bb0);
                    self.gen(b1)?;
                    self.b.br(&bb_end);
                    self.b.set_position_at_end_of(&bb1);
                }

                Ok(None)
            },

            MirRaw::Loop(l, b, m) => {
                let f = self.func_stack.get_current().expect("Loop instruction must be in a function").func;
                let bb_current = self.b.get_current_basic_block();
                let bb_begin = self.ctx.append_basic_block_after(&f, &bb_current, format!("loop_{}_begin", Self::meta2str(m)));
                let bb_end = self.ctx.append_basic_block_after(&f, &bb_begin, format!("loop_{}_end", Self::meta2str(m)));
                self.b.set_position_at_end_of(&bb_begin);
                self.loop_stack.push(LoopStackElem {name: l, begin_bb: bb_begin.clone(), end_bb: bb_end.clone()});
                self.scope_stack.enter_scope();
                self.gen(b)?;
                if self.loop_stack.is_empty() || self.loop_stack.last().unwrap().name != l {
                    panic!("loop stack is in unexpected state")
                }
                self.scope_stack.leave_scope()?;
                self.loop_stack.pop();
                self.b.br(&bb_begin);
                self.b.set_position_at_end_of(&bb_end);
                Ok(None)
            },

            MirRaw::Break(l, m) => {
                if self.loop_stack.is_empty() {
                    panic!("break is not in a loop")
                }
                if let Some(n) = l {
                    let mut iter = self.loop_stack.iter().rev();
                    while let Some(LoopStackElem{name: loop_label, begin_bb: _, end_bb: e}) = iter.next() {
                        if let Some(n2) = loop_label {
                            if &n == n2 {
                                self.b.br(&e);
                                return Ok(None);
                            }
                        }
                    }
                    panic!(format!("loop labeled {} is not found", n))
                } else {
                    let e = self.loop_stack.last().unwrap();
                    self.b.br(&e.end_bb);
                    return Ok(None);
                }
            },

            MirRaw::Fun{name, ty, params, body, meta} => {
                let ty = self.gen_ty(&ty);
                let f = self.m.add_function(name.as_str(), ty);
                self.scope_stack.insert(name, ScopedValue::Const(f.clone()));
                self.scope_stack.enter_scope();

                // create allocation bb and entry bb
                let alloc = self.ctx.append_basic_block(&f, "alloc");
                let entry = self.ctx.append_basic_block(&f, "entry");
                self.b.set_position_at_end_of(&alloc);
                self.b.set_position_at_end_of(&entry);
                self.func_stack.push(FuncStackElem{func: f.clone(), alloca_bb: alloc.clone()});

                // push aguments to scope stack
                let mut i: u32 = 0;
                for p in params {
                    let v = f.get_param(i);
                    self.scope_stack.insert(p, ScopedValue::Const(v)); // TODO: mutable parameter
                    i += 1;
                }

                // generate body
                self.gen(body)?;

                // insert br inst to the end of alloc bb in order not to let alloc bb be empty
                let current_bb = self.b.get_current_basic_block();
                self.b.set_position_at_end_of(&alloc);
                self.b.br(&entry);
                self.b.set_position_at_end_of(&current_bb);

                // return 
                self.func_stack.pop();
                self.scope_stack.leave_scope()?;
                Ok(Some(f))
            },
            MirRaw::DeclExternFun{name, ty, meta} => {
                let ty = self.gen_ty(&ty);
                
                let f = self.m.add_function(name.as_str(), ty);
                self.scope_stack.insert(name, ScopedValue::Const(f.clone()));
                Ok(Some(f))
            },
            MirRaw::Ret(val, meta) => {
                let v = self.gen(val)?.ok_or(MirErr::MustHaveValue)?;
                Ok(Some(self.b.ret(&v)))
            },
            MirRaw::RetUnit(meta) => {
                Ok(Some(self.b.ret_void()))
            },
            MirRaw::Call(fun, args, meta) => {
                let mut a = vec!();
                for x in args.into_iter() {
                    a.push(self.gen(x)?.ok_or(MirErr::MustHaveValue)?)
                }
                let f = self.gen(fun)?.ok_or(MirErr::MustHaveValue)?;
                let name = "";
                Ok(Some(self.b.call(&f, &a, name)))
            },
            MirRaw::Seq(es, meta) => {
                if es.is_empty() {panic!("Seq never empty"); }
                let mut r = None;
                for e in es {
                    r = self.gen(e)?;
                }
                Ok(r)
            },
            _ => unimplemented!()
        }
    }
}

#[macro_export]
macro_rules! ast {
    ( $id:ident : $($arg:expr),* ) => (
        Mir::new(MirRaw::$id($($arg),*, (0,line!(),column!())))
    );
    ( $id:ident # $($name:ident : $arg:expr),* ) => {
        Mir::new(MirRaw::$id{$($name: $arg),*, meta: (0,line!(),column!())})
    };
    ( $($arg:expr);* ) => {
        Mir::new(MirRaw::Seq(vec![$($arg),*], (0,line!(),column!())))
    };
    //($($t:tt)*) => {$($t)*};
}

#[macro_export]
macro_rules! ty {
    ([ $id:ident ]) => {
        MirTy{ty: stringify!($id).into(), args: vec![]};
    };
    ([$id:ident : $($t:tt),* ]) => {
        MirTy{ty: stringify!($id).into(), args: vec![$(ty!($t)),*]};
    };
}

