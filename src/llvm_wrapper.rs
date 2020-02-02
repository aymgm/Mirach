extern crate llvm_sys as llvm;
extern crate libc;

use std::ops::Deref;
use std::{mem, ptr};
use std::ffi::CString;
use llvm::prelude::*;
use llvm::core::*;
use llvm::target::*;
use llvm::target_machine::*;
use llvm::analysis::{LLVMVerifierFailureAction, LLVMVerifyModule, LLVMVerifyFunction};

#[derive(Debug)]
pub struct Context {
    pub context: LLVMContextRef
}
impl Context {
    pub fn new()->Context {
        unsafe {
            Context{context: LLVMContextCreate()}
        }
    }

    pub fn append_basic_block<StrT: Into<Vec<u8>>>(&mut self, fun: &Value, name: StrT)->BasicBlock {
        let cname = CString::new(name).expect("CString::new failed");
        unsafe {
            BasicBlock {
                basic_block: LLVMAppendBasicBlockInContext(self.context, fun.value, cname.as_ptr())
            }
        }
    }

    pub fn insert_basic_block_before<StrT: Into<Vec<u8>>>(&mut self, pos: &BasicBlock, name: StrT)->BasicBlock {
        let cname = CString::new(name).expect("CString::new failed");
        unsafe {
            BasicBlock {
                basic_block: LLVMInsertBasicBlockInContext(self.context, pos.basic_block, cname.as_ptr())
            }
        }
    }

    pub fn append_basic_block_after<StrT: Into<Vec<u8>>>(&mut self, fun: &Value, pos: &BasicBlock, name: StrT)->BasicBlock {
        let cname = CString::new(name).expect("CString::new failed");
        unsafe {
            let next_bb = LLVMGetNextBasicBlock(pos.basic_block);
            if next_bb == std::ptr::null_mut() {
                BasicBlock {
                    basic_block: LLVMAppendBasicBlockInContext(self.context, fun.value, cname.as_ptr())
                }
            } else {
                BasicBlock {
                    basic_block: LLVMInsertBasicBlockInContext(self.context, next_bb, cname.as_ptr())
                }
            }
        }
    }
    pub fn last_basic_block(fun: &Value)->BasicBlock {
        unsafe {
            BasicBlock {
                basic_block: LLVMGetLastBasicBlock(fun.value)
            }
        }
    }

    pub fn debug_info(&self, line: u32, col: u32)->MetaData {
        unsafe {
            MetaData {
                meta_data: llvm_sys::debuginfo::LLVMDIBuilderCreateDebugLocation(self.context, line as libc::c_uint, col as libc::c_uint, std::ptr::null_mut(), std::ptr::null_mut())
            }
        }
    }
    pub fn dispose(&mut self) {
        unsafe { LLVMContextDispose(self.context); }
    }
}

#[derive(Debug)]
pub struct Module {
    pub module: LLVMModuleRef
}

impl Module {
    pub fn new<StrT: Into<Vec<u8>>>(name: StrT, context: &Context)->Module {
        let cname = CString::new(name).expect("CString::new failed");
        unsafe {
            let raw_mod = LLVMModuleCreateWithNameInContext(cname.as_ptr(), context.context);
            Module {module: raw_mod}
        }
    }

    pub fn add_function<StrT: Into<Vec<u8>>>(&mut self, name: StrT, ty: Type)->Value {
        let cname = CString::new(name).expect("CString::new failed");
        unsafe{
            let ret = LLVMAddFunction(self.module, cname.as_ptr(), ty.type_);
            Value {value: ret}
        }
    }

    pub fn dump(&self) {
        unsafe {
            LLVMDumpModule(self.module);
        }
    }

    pub fn print_to_file<StrT: Into<Vec<u8>>>(&self, file: StrT) {
        let cfile = CString::new(file).expect("CString::new failed");
        unsafe {
            let is_err = LLVMPrintModuleToFile(self.module, cfile.as_ptr(), std::ptr::null_mut());
            if is_err != 0 {
                panic!("failed to print module to file")
            }
        }
    }

    pub fn verify(&self) {
        unsafe {
            LLVMVerifyModule(self.module, LLVMVerifierFailureAction::LLVMAbortProcessAction, std::ptr::null_mut());
        }
    }

    pub fn dispose(&self) {
        unsafe {LLVMDisposeModule(self.module);}
    }
}

impl Drop for Module {
    fn drop(&mut self) {
        // unsafe {LLVMDisposeModule(self.module);}
    }
}

#[derive(Debug)]
pub struct Builder {
    pub builder: LLVMBuilderRef
}
impl Builder {
    pub fn new(context: &Context)->Builder {
        unsafe {
            Builder{ builder: LLVMCreateBuilderInContext(context.context) }
        }
    }

    pub fn dispose(&mut self) {
        unsafe {LLVMDisposeBuilder(self.builder);}
    }

    pub fn set_debug_info(&mut self, file: u32, line: u32, col: u32) {
        unimplemented!();
    }

    pub fn get_current_basic_block(&self)->BasicBlock {
        unsafe {
            BasicBlock {basic_block: LLVMGetInsertBlock(self.builder)}
        }
    }
    pub fn set_position_at_end_of(&mut self, bb: &BasicBlock) {
        unsafe {
            LLVMPositionBuilderAtEnd(self.builder, bb.basic_block);
        }
    }

    pub fn br(&mut self, dest: &BasicBlock)->Value {
        unsafe {
            Value {
                value: LLVMBuildBr(self.builder, dest.basic_block)
            }
        }
    }

    pub fn cond_br(&mut self, cond: &Value, true_clause: &BasicBlock, false_clause: &BasicBlock)->Value {
        unsafe {
            Value {
                value: LLVMBuildCondBr(self.builder, cond.value, true_clause.basic_block, false_clause.basic_block)
            }
        }
    }

    pub fn phi<StrT>(&mut self, ty: &Type, name: StrT)->Phi where StrT: Into<Vec<u8>> {
        let cname = CString::new(name).expect("CString::new failed");
        unsafe {
            Phi {
                value: LLVMBuildPhi(self.builder, ty.type_, cname.as_ptr())
            }
        }
    }


    pub fn ret(&mut self, val: &Value)->Value {
        unsafe {
            Value {
                value: LLVMBuildRet(self.builder, val.value)
            }
        }
    }

    pub fn ret_void(&mut self)->Value {
        unsafe {
            Value {
                value: LLVMBuildRetVoid(self.builder)
            }
        }
    }

    pub fn alloca<StrT: Into<Vec<u8>>>(&mut self, ty: &Type, name: StrT)->Value {
        let cname = CString::new(name).expect("CString::new failed");
        unsafe {
            Value {
                value: LLVMBuildAlloca(self.builder, ty.type_, cname.as_ptr())
            }
        }
    }

    pub fn load<StrT: Into<Vec<u8>>>(&mut self, ptr_val: &Value, name: StrT)->Value {
        let cname = CString::new(name).expect("CString::new failed");
        unsafe {
            Value {
                value: LLVMBuildLoad(self.builder, ptr_val.value, cname.as_ptr())
            }
        }
    }

    pub fn load_with_type<StrT: Into<Vec<u8>>>(&mut self, ty: &Type, ptr_val: &Value, name: StrT)->Value {
        let cname = CString::new(name).expect("CString::new failed");
        unsafe {
            Value {
                value: LLVMBuildLoad2(self.builder, ty.type_, ptr_val.value, cname.as_ptr())
            }
        }
    }

    pub fn store(&mut self, ptr_val: &Value, val: &Value)->Value {
        unsafe {
            Value{
                value: LLVMBuildStore(self.builder, val.value, ptr_val.value)
            }
        }
    }
    pub fn global_string_ptr<StrT: Into<Vec<u8>>>(&mut self, val: StrT, name: StrT)->Value {
        let cval = CString::new(val).expect("CString::new failed");
        let cname = CString::new(name).expect("CString::new failed");
        unsafe {
            Value {
                value: LLVMBuildGlobalString(self.builder, cval.as_ptr(), cname.as_ptr())
            }
        }
    }

    pub fn call<StrT: Into<Vec<u8>>>(&mut self, fun: &Value, args: &Vec<Value>, name: StrT)->Value {
        let cname = CString::new(name).expect("CString::new failed");
        let n = args.len() as u32;
        let mut cargs = args.iter().map(|x|{x.value}).collect::<Vec<LLVMValueRef>>();
        let p = cargs.as_mut_ptr();
        unsafe {
            Value {
                value: LLVMBuildCall(self.builder, fun.value, p, n, cname.as_ptr())
            }
        }
    }

    pub fn get_struct_elem<StrT: Into<Vec<u8>>>(&self, struct_: &Value, idx: u32, name: StrT)->Value {
        let cname = CString::new(name).expect("CString::new failed");
        unsafe {
            Value { value: LLVMBuildExtractValue(self.builder, struct_.value, idx, cname.as_ptr()) }
        }
    }

    pub fn get_array_elem<StrT: Into<Vec<u8>>>(&self, array: &Value, idx: &Value, name: StrT)->Value {
        let cname = CString::new(name).expect("CString::new failed");
        unsafe {
            Value { value: LLVMBuildExtractElement(self.builder, array.value, idx.value, cname.as_ptr()) }
        }
    }

    pub fn set_struct_elem<StrT: Into<Vec<u8>>>(&self, struct_: &Value, idx: u32, elem: &Value, name: StrT)->Value {
        let cname = CString::new(name).expect("CString::new failed");
        unsafe {
            Value { value: LLVMBuildInsertValue(self.builder, struct_.value, elem.value, idx, cname.as_ptr()) }
        }
    }

    pub fn set_array_elem<StrT: Into<Vec<u8>>>(&self, array: &Value, idx: &Value, name: StrT)->Value {
        let cname = CString::new(name).expect("CString::new failed");
        unsafe {
            Value { value: LLVMBuildExtractElement(self.builder, array.value, idx.value, cname.as_ptr()) }
        }
    }

    pub fn op_iadd<StrT: Into<Vec<u8>>>(&mut self, lhs: &Value, rhs: &Value, name: StrT)->Value {
        let cname = CString::new(name).expect("CString::new failed");
        unsafe {
            Value {
                value: LLVMBuildAdd(self.builder, lhs.value, rhs.value, cname.as_ptr())
            }
        }
    }

    pub fn op_isub<StrT: Into<Vec<u8>>>(&mut self, lhs: &Value, rhs: &Value, name: StrT)->Value {
        let cname = CString::new(name).expect("CString::new failed");
        unsafe {
            Value {
                value: LLVMBuildSub(self.builder, lhs.value, rhs.value, cname.as_ptr())
            }
        }
    }
    pub fn op_imul<StrT: Into<Vec<u8>>>(&mut self, lhs: &Value, rhs: &Value, name: StrT)->Value {
        let cname = CString::new(name).expect("CString::new failed");
        unsafe {
            Value {
                value: LLVMBuildMul(self.builder, lhs.value, rhs.value, cname.as_ptr())
            }
        }
    }

    pub fn op_idiv<StrT: Into<Vec<u8>>>(&mut self, lhs: &Value, rhs: &Value, is_unsigned: bool, name: StrT)->Value {
        let cname = CString::new(name).expect("CString::new failed");
        unsafe {
            Value {
                value: if is_unsigned {
                    LLVMBuildUDiv(self.builder, lhs.value, rhs.value, cname.as_ptr())
                } else {
                   LLVMBuildSDiv(self.builder, lhs.value, rhs.value, cname.as_ptr()) 
                }
            }
        }
    }
    pub fn op_imod<StrT: Into<Vec<u8>>>(&mut self, lhs: &Value, rhs: &Value, is_unsigned: bool, name: StrT)->Value {
        let cname = CString::new(name).expect("CString::new failed");
        unsafe {
            Value {
                value: if is_unsigned {
                    LLVMBuildURem(self.builder, lhs.value, rhs.value, cname.as_ptr())
                } else {
                    LLVMBuildSRem(self.builder, lhs.value, rhs.value, cname.as_ptr())
                }
            }
        }
    }

    pub fn op_lnot<StrT: Into<Vec<u8>>>(&mut self, arg: &Value, name: StrT)->Value {
        let cname = CString::new(name).expect("CString::new failed");
        unsafe {
            Value {
                value: LLVMBuildNot(self.builder, arg.value, cname.as_ptr())
            }
        }
    }
    pub fn op_ieq<StrT: Into<Vec<u8>>>(&mut self, lhs: &Value, rhs: &Value, name: StrT)->Value {
        let cname = CString::new(name).expect("CString::new failed");
        unsafe {
            Value {
                value: LLVMBuildICmp(self.builder, llvm::LLVMIntPredicate::LLVMIntEQ, lhs.value , rhs.value, cname.as_ptr())
            }
        }
    }
    pub fn op_ineq<StrT: Into<Vec<u8>>>(&mut self, lhs: &Value, rhs: &Value, name: StrT)->Value {
        let cname = CString::new(name).expect("CString::new failed");
        unsafe {
            Value {
                value: LLVMBuildICmp(self.builder, llvm::LLVMIntPredicate::LLVMIntNE, lhs.value , rhs.value, cname.as_ptr())
            }
        }
    }
    pub fn op_ilt<StrT: Into<Vec<u8>>>(&mut self, lhs: &Value, rhs: &Value, is_unsigned: bool, name: StrT)->Value {
        let cname = CString::new(name).expect("CString::new failed");
        unsafe {
            Value {
                value: if is_unsigned {
                    LLVMBuildICmp(self.builder, llvm::LLVMIntPredicate::LLVMIntULT, lhs.value , rhs.value, cname.as_ptr())
                } else {
                    LLVMBuildICmp(self.builder, llvm::LLVMIntPredicate::LLVMIntSLT, lhs.value , rhs.value, cname.as_ptr())
                }
            }
        }
    }
    pub fn op_ilte<StrT: Into<Vec<u8>>>(&mut self, lhs: &Value, rhs: &Value, is_unsigned: bool, name: StrT)->Value {
        let cname = CString::new(name).expect("CString::new failed");
        unsafe {
            Value {
                value: if is_unsigned {
                    LLVMBuildICmp(self.builder, llvm::LLVMIntPredicate::LLVMIntULE, lhs.value , rhs.value, cname.as_ptr())
                } else {
                    LLVMBuildICmp(self.builder, llvm::LLVMIntPredicate::LLVMIntSLE, lhs.value , rhs.value, cname.as_ptr())
                }
            }
        }
    }
    pub fn op_igt<StrT: Into<Vec<u8>>>(&mut self, lhs: &Value, rhs: &Value, is_unsigned: bool, name: StrT)->Value {
        let cname = CString::new(name).expect("CString::new failed");
        unsafe {
            Value {
                value: if is_unsigned {
                    LLVMBuildICmp(self.builder, llvm::LLVMIntPredicate::LLVMIntUGT, lhs.value , rhs.value, cname.as_ptr())
                } else {
                    LLVMBuildICmp(self.builder, llvm::LLVMIntPredicate::LLVMIntSGT, lhs.value , rhs.value, cname.as_ptr())
                }
            }
        }
    }
    pub fn op_igte<StrT: Into<Vec<u8>>>(&mut self, lhs: &Value, rhs: &Value, is_unsigned: bool, name: StrT)->Value {
        let cname = CString::new(name).expect("CString::new failed");
        unsafe {
            Value {
                value: if is_unsigned {
                    LLVMBuildICmp(self.builder, llvm::LLVMIntPredicate::LLVMIntUGE, lhs.value , rhs.value, cname.as_ptr())
                } else {
                    LLVMBuildICmp(self.builder, llvm::LLVMIntPredicate::LLVMIntSGE, lhs.value , rhs.value, cname.as_ptr())
                }
            }
        }
    }
}

impl Drop for Builder {
    fn drop(&mut self) {
        //unsafe {LLVMDisposeBuilder(self.builder);}
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Type {
    pub type_: LLVMTypeRef
}
impl Type {
    pub fn void(context: &Context)->Type {
        unsafe {
            Type { type_: LLVMVoidTypeInContext(context.context) }
        }
    }
    pub fn int1(context: &Context)->Type {
        unsafe {
            Type { type_: LLVMInt1TypeInContext(context.context) }
        }
    }
    pub fn int8(context: &Context)->Type {
        unsafe {
            Type { type_: LLVMInt8TypeInContext(context.context) }
        }
    }
    pub fn int16(context: &Context)->Type {
        unsafe {
            Type { type_: LLVMInt16TypeInContext(context.context) }
        }
    }
    pub fn int32(context: &Context)->Type {
        unsafe {
            Type { type_: LLVMInt32TypeInContext(context.context) }
        }
    }
    pub fn int64(context: &Context)->Type {
        unsafe {
            Type { type_: LLVMInt64TypeInContext(context.context) }
        }
    }
    pub fn float(context: &Context)->Type {
        unsafe {
            Type { type_: LLVMFloatTypeInContext(context.context) }
        }
    }
    pub fn double(context: &Context)->Type {
        unsafe {
            Type { type_: LLVMDoubleTypeInContext(context.context) }
        }
    }
    pub fn ptr(ty: Type)->Type {
        unsafe {
            Type { type_: LLVMPointerType(ty.type_, 0) }
        }
    }
    pub fn label(context: &Context)->Type {
        unsafe {
            Type { type_: LLVMLabelTypeInContext(context.context) }
        }
    }
    pub fn function<IterT: Deref<Target=[Type]>>(return_type: Type, param_types: &IterT, is_vararg: bool)->Type {
        let n = param_types.len() as u32;
        let mut params = param_types.iter().map(|x|{x.type_}).collect::<Vec<LLVMTypeRef>>();
        let p = params.as_mut_ptr();
        unsafe {
            Type { type_: LLVMFunctionType(return_type.type_, p, if is_vararg {0} else {n}, if is_vararg {1} else {0}) }
        }
    }

    pub fn struct_<IterT: Deref<Target=[Type]>>(context: &Context, elem_types: &IterT, is_packed: bool)->Type {
        let n = elem_types.len() as u32;
        let mut elems = elem_types.iter().map(|x|{x.type_}).collect::<Vec<LLVMTypeRef>>();
        let p = elems.as_mut_ptr();
        unsafe {
            Type { type_: LLVMStructTypeInContext(context.context, p, n, if is_packed {1} else {0}) }
        }
    }

    pub fn dump(&self) {
        unsafe {
            LLVMDumpType(self.type_);
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Value {
    pub value: LLVMValueRef
}

impl Value {
    pub fn dump(&self) {
        unsafe {
            LLVMDumpValue(self.value);
        }
    }

    pub fn get_param(&self, pos: u32)->Value {
        unsafe {
            Value {value: LLVMGetParam(self.value, pos)}
        }
    }

    pub fn int1(context: &Context, b: bool)->Value {
        unsafe {
            Value {value: LLVMConstInt(Type::int1(context).type_, if b {1} else {0} as u64, 1)}
        }
    }
    pub fn int8(context: &Context, i: i8)->Value {
        unsafe {
            Value {value: LLVMConstInt(Type::int8(context).type_, i as u64, 1)}
        }
    }
    pub fn int16(context: &Context, i: i16)->Value {
        unsafe {
            Value {value: LLVMConstInt(Type::int16(context).type_, i as u64, 1)}
        }
    }
    pub fn int32(context: &Context, i: i32)->Value {
        unsafe {
            Value {value: LLVMConstInt(Type::int32(context).type_, i as u64, 1)}
        }
    }
    pub fn int64(context: &Context, i: i64)->Value {
        unsafe {
            Value {value: LLVMConstInt(Type::int64(context).type_, i as u64, 1)}
        }
    }

    pub fn get_type(&self)->Type {
        unsafe {
            Type { type_: LLVMTypeOf(self.value) }
        }
    }
}


#[derive(Debug, Clone, Copy)]
pub struct Phi {
    pub value: LLVMValueRef
}

impl Phi {
    pub fn add_incomings(&mut self, blocks: &Vec<BasicBlock>, values: &Vec<Value>) {
        if blocks.len() != values.len() {
            panic!("Phi#incomings' argument vectors must have same length")
        }
        let l = blocks.len() as libc::c_uint;
        let mut bv = blocks.into_iter().map(|x|x.basic_block).collect::<Vec<_>>();
        let mut vv = values.into_iter().map(|x|x.value).collect::<Vec<_>>();
        let mut b = bv.as_mut_ptr();
        let mut v = vv.as_mut_ptr();
        unsafe {
            LLVMAddIncoming(self.value, v, b, l)
        }
    }

    pub fn add_incoming(&mut self, block: &BasicBlock, value: &Value) {
        let mut b = [block.basic_block];
        let mut v = [value.value];
        unsafe {
            LLVMAddIncoming(self.value, v.as_mut_ptr(), b.as_mut_ptr(), 1);
        } 
    }

    pub fn get_value(&self)->Value {
        Value{
            value: self.value
        }
    }
}

#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub basic_block: LLVMBasicBlockRef
}

impl BasicBlock {
    pub fn move_before(&mut self, pos: &BasicBlock) {
        unsafe {
            LLVMMoveBasicBlockBefore(self.basic_block, pos.basic_block)
        }
    }
    pub fn move_after(&mut self, pos: &BasicBlock) {
        unsafe {
            LLVMMoveBasicBlockAfter(self.basic_block, pos.basic_block)
        }
    }
}

#[derive(Debug)]
pub struct MetaData {
    pub meta_data: LLVMMetadataRef
}
impl MetaData {
}

#[derive(Debug)]
pub struct TargetMachine {
    pub target_machine: LLVMTargetMachineRef
}

impl TargetMachine {
    pub fn new<StrT: Into<Vec<u8>>>(triple: StrT, cpu: StrT, features: StrT, opt_level: LLVMCodeGenOptLevel, reloc_mode: LLVMRelocMode, code_model: LLVMCodeModel)->TargetMachine {
        unsafe {
            let ctriple = CString::new(triple).expect("CString::new failed");
            let ccpu = CString::new(cpu).expect("CString::new failed");
            let cfeatures = CString::new(features).expect("CString::new failed");
            LLVM_InitializeAllTargets();
            LLVM_InitializeAllAsmPrinters();

            let mut target = mem::MaybeUninit::uninit().assume_init();
            let mut err = mem::MaybeUninit::uninit().assume_init();
            
            let is_ok = LLVMGetTargetFromTriple(ctriple.as_ptr(), &mut target, &mut err);
            if is_ok != 0 {
                let err = CString::from_raw(err);
                println!("{:?}", &err);
                LLVMDisposeMessage(err.into_raw());
            }
            let tm = LLVMCreateTargetMachine(target, ctriple.as_ptr(), ccpu.as_ptr(), cfeatures.as_ptr(), opt_level, reloc_mode, code_model);
            TargetMachine{ target_machine: tm }
        }
    }
    pub fn new_native<StrT: Into<Vec<u8>>>(cpu: StrT, features: StrT, opt_level: LLVMCodeGenOptLevel, reloc_mode: LLVMRelocMode, code_model: LLVMCodeModel)->TargetMachine {
        unsafe {
            let ccpu = CString::new(cpu).expect("CString::new failed");
            let cfeatures = CString::new(features).expect("CString::new failed");
            LLVM_InitializeNativeTarget();
            LLVM_InitializeNativeAsmPrinter();

            let triple = LLVMGetDefaultTargetTriple();
            let mut target = mem::MaybeUninit::uninit().assume_init();
            let mut err = mem::MaybeUninit::uninit().assume_init();
            
            let is_ok = LLVMGetTargetFromTriple(triple, &mut target, &mut err);
            if is_ok != 0 {
                let err = CString::from_raw(err);
                println!("{:?}", &err);
                LLVMDisposeMessage(err.into_raw());
            }
            let tm = LLVMCreateTargetMachine(target, triple, ccpu.as_ptr(), cfeatures.as_ptr(), opt_level, reloc_mode, code_model);
            LLVMDisposeMessage(triple);
            TargetMachine{ target_machine: tm }
        }
    }

    pub fn new_host(opt_level: LLVMCodeGenOptLevel, reloc_mode: LLVMRelocMode, code_model: LLVMCodeModel)->TargetMachine {
        unsafe {
            let cpu = CString::from_raw(LLVMGetHostCPUName());
            let feats = CString::from_raw(LLVMGetHostCPUFeatures());
            let tm = Self::new_native(cpu.as_bytes(), feats.as_bytes(), opt_level, reloc_mode, code_model);
            LLVMDisposeMessage(cpu.into_raw());
            LLVMDisposeMessage(feats.into_raw());
            tm
        }
    }


    pub fn compile_to_file<StrT: Into<Vec<u8>>>(&self, module: &Module, file_name: StrT, file_type: LLVMCodeGenFileType)->Result<(), &str> {
        unsafe {
            let cfname = CString::new(file_name).expect("CString::new failed");
            let is_err = LLVMTargetMachineEmitToFile(self.target_machine, module.module, cfname.into_raw(), file_type, ptr::null_mut());
            if is_err != 0 {
                return Err("init");
            }
            Ok(())
        }
    }
    pub fn dispose(&mut self) {
        unsafe{ LLVMDisposeTargetMachine(self.target_machine); }
    }
}
