use super::*;

fn run(name: String, mir: Mir)-> String {
    use std::process::*;
    use llvm_sys::target_machine::*;
    use llvm_wrapper::TargetMachine;
    let test_data_dir = "testdata";
    let prefix = if cfg!(target_os = "windows") {format!("{}\\", test_data_dir)} else {format!("{}/", test_data_dir)};
    let obj_path = format!("{}{}.o", &prefix, &name);
    let exe_suffix = if cfg!(target_os = "windows") {".exe"} else {""};
    let exe_path = format!("{}{}{}", &prefix, &name, exe_suffix);
    let ll_path = format!("{}{}.ll", &prefix, &name);
    let mut g = Generator::new(name);
    let v = g.gen(mir);

    g.verify_module();
    g.print_llvm_ir_to_file(ll_path);
    let tm = TargetMachine::new_host(LLVMCodeGenOptLevel::LLVMCodeGenLevelAggressive, LLVMRelocMode::LLVMRelocPIC, LLVMCodeModel::LLVMCodeModelDefault);

    tm.compile_to_file(&g.m, obj_path.clone(), LLVMCodeGenFileType::LLVMObjectFile).expect("failed to comile");

    let compiled = Command::new("gcc")
                    .arg(obj_path)
                    .arg("-o")
                    .arg(&exe_path)
                    .output()
                    .expect("failed to compile");
    if compiled.status.code().expect("gcc is killed by signal") != 0 {
        let err = String::from_utf8(compiled.stderr).expect("illegal charactor in stderr");
        eprintln!("compile error:\n{}", err);
        return "".into();
    }
    let result = Command::new(exe_path)
                    .output()
                    .expect("failed to compile");
    if result.status.code().expect("execution is killed by signal") != 0 {
        let err = String::from_utf8(result.stderr).expect("illegal charactor in stderr");
        eprintln!("executin error:\n{}", err);
        "".into()
    } else {
        String::from_utf8(result.stdout).expect("illegal charactor in the result output")
    }
}
#[test]
fn hello_world() {
    let a = ast![
        ast!{
            DeclExternFun #
            name: "printf".into(),
            ty: ty!([FunVarArgs: [Unit], [Ptr: [I8]]])
        };
        ast!{
            Fun #
            name: "main".into(),
            ty: ty!([Fun: [I64]]),
            params: vec![],
            body: ast![
                ast!(Call : ast!(Load: "printf".into()), vec![ast!(StrLit: "Hello, World!\n".into())]);
                ast!(Ret: ast!(I64: 0))
            ]
        }
    ];
    assert_eq!(run("hw".into(), a).trim(), String::from("Hello, World!"));
}

#[test]
fn add() {
    let a = ast![
        ast!{
            DeclExternFun #
            name: "printf".into(),
            ty: ty!([FunVarArgs: [Unit], [Ptr: [I8]]])
        };
        ast!{
            Fun #
            name: "add".into(),
            ty: ty!([Fun: [I64], [I64], [I64]]),
            params: vec!["x".into(), "y".into()],
            body: ast![
                ast!(DefConst : "z".into(),
                    ast!(IAdd:
                        ast!(Load: "x".into()),
                        ast!(Load: "y".into())
                    )
                );
                ast!(Ret: ast!(Load: "z".into()))
            ]
        };
        ast!{
            Fun #
            name: "main".into(),
            ty: ty!([Fun: [I64]]),
            params: vec![],
            body: ast![
                ast!(DefConst : "z".into(),
                    ast!(Call : ast!(Load: "add".into()), vec![ast!(I64: 2), ast!(I64: 2)])
                );
                ast!(Call : ast!(Load: "printf".into()), vec![ast!(StrLit: "%d\n".into()), ast!(Load: "z".into())]);
                ast!(Ret: ast!(I64: 0))
            ]
        }
    ];
    assert_eq!(run("add".into(), a).trim(), String::from("4"));
}

#[test]
fn variable() {
    let a = ast![
        ast!{
            DeclExternFun #
            name: "printf".into(),
            ty: ty!([FunVarArgs: [Unit], [Ptr: [I8]]])
        };
        ast!{
            Fun #
            name: "variable".into(),
            ty: ty!([Fun: [I64], [I64]]),
            params: vec!["x".into()],
            body: ast![
                ast!(DefVar : "t".into(), ty!([I64]), ast!(Load: "x".into()));
                
                ast!(StoreVar: "t".into(),
                    ast!(IMul:
                        ast!(Load: "t".into()),
                        ast!(I64: 2)
                    )
                );
                
                ast!(Ret: ast!(Load: "t".into()))
            ]
        };
        ast!{
            Fun #
            name: "main".into(),
            ty: ty!([Fun: [I64]]),
            params: vec![],
            body: ast![
                ast!(DefConst : "ret".into(),
                    ast!(Call : ast!(Load: "variable".into()), vec![ast!(I64: 21)])
                );
                ast!(Call : ast!(Load: "printf".into()), vec![ast!(StrLit: "%d\n".into()), ast!(Load: "ret".into())]);
                ast!(Ret: ast!(I64: 0))
            ]
        }
    ];
    assert_eq!(run("variable".into(), a).trim(), String::from("42"));
}

#[test]
fn if_void() {
    let a = ast![
        ast!{
            DeclExternFun #
            name: "printf".into(),
            ty: ty!([FunVarArgs: [Unit], [Ptr: [I8]]])
        };
        ast!{
            Fun #
            name: "main".into(),
            ty: ty!([Fun: [I64]]),
            params: vec![],
            body: ast![
                ast!(IfVoid : vec![
                    (
                        ast!(Bool: false),
                        ast!(Call : ast!(Load: "printf".into()), vec![ast!(StrLit: "%d\n".into()), ast!(I64: 43i64)])
                    ),
                    (
                        ast!(Bool: true),
                        ast!(Call : ast!(Load: "printf".into()), vec![ast!(StrLit: "%d\n".into()), ast!(I64: 42i64)])
                    ),
                ]);
                ast!(Ret: ast!(I64: 0))
            ]
        }
    ];
    assert_eq!(run("if_void".into(), a).trim(), String::from("42"));
}

#[test]
fn if_expression() {
    let a = ast![
        ast!{
            DeclExternFun #
            name: "printf".into(),
            ty: ty!([FunVarArgs: [Unit], [Ptr: [I8]]])
        };
        ast!{
            Fun #
            name: "main".into(),
            ty: ty!([Fun: [I64]]),
            params: vec![],
            body: ast![
                ast!(DefConst : "ret".into(),
                    ast!(If :
                        ast!(ILte: ast!(I64: 1), ast!(I64: 2), false),
                        ast!(I64: 42),
                        ast!(I64: 43)
                    )
                );
                ast!(Call : ast!(Load: "printf".into()), vec![
                    ast!(StrLit: "%d\n".into()),
                    ast!(Load: "ret".into())
                ]);
                ast!(Ret: ast!(I64: 0))
            ]
        }
    ];
    assert_eq!(run("if_expression".into(), a).trim(), String::from("42"));
}

#[test]
fn land() {
    let a = ast![
        ast!{
            DeclExternFun #
            name: "printf".into(),
            ty: ty!([FunVarArgs: [Unit], [Ptr: [I8]]])
        };
        ast!{
            Fun #
            name: "main".into(),
            ty: ty!([Fun: [I64]]),
            params: vec![],
            body: ast![
                ast!(IfVoid : vec![
                    (
                        ast!(LAnd: ast!(Bool: false), ast!(Bool: false)),
                        ast!(Call : ast!(Load: "printf".into()), vec![ast!(StrLit: "a".into())])
                    ),
                ]);
                ast!(IfVoid : vec![
                    (
                        ast!(LAnd: ast!(Bool: false), ast!(Bool: true)),
                        ast!(Call : ast!(Load: "printf".into()), vec![ast!(StrLit: "b".into())])
                    ),
                ]);
                ast!(IfVoid : vec![
                    (
                        ast!(LAnd: ast!(Bool: true), ast!(Bool: false)),
                        ast!(Call : ast!(Load: "printf".into()), vec![ast!(StrLit: "c".into())])
                    ),
                ]);
                ast!(IfVoid : vec![
                    (
                        ast!(LAnd: ast!(Bool: true), ast!(Bool: true)),
                        ast!(Call : ast!(Load: "printf".into()), vec![ast!(StrLit: "d".into())])
                    ),
                ]);

                ast!(Call : ast!(Load: "printf".into()), vec![ast!(StrLit: "\n".into())]);
                ast!(Ret: ast!(I64: 0))
            ]
        }
    ];
    assert_eq!(run("land".into(), a).trim(), String::from("d"));
}

#[test]
fn lor() {
    let a = ast![
        ast!{
            DeclExternFun #
            name: "printf".into(),
            ty: ty!([FunVarArgs: [Unit], [Ptr: [I8]]])
        };
        ast!{
            Fun #
            name: "main".into(),
            ty: ty!([Fun: [I64]]),
            params: vec![],
            body: ast![
                ast!(IfVoid : vec![
                    (
                        ast!(LOr: ast!(Bool: false), ast!(Bool: false)),
                        ast!(Call : ast!(Load: "printf".into()), vec![ast!(StrLit: "a".into())])
                    ),
                ]);
                ast!(IfVoid : vec![
                    (
                        ast!(LOr: ast!(Bool: false), ast!(Bool: true)),
                        ast!(Call : ast!(Load: "printf".into()), vec![ast!(StrLit: "b".into())])
                    ),
                ]);
                ast!(IfVoid : vec![
                    (
                        ast!(LOr: ast!(Bool: true), ast!(Bool: false)),
                        ast!(Call : ast!(Load: "printf".into()), vec![ast!(StrLit: "c".into())])
                    ),
                ]);
                ast!(IfVoid : vec![
                    (
                        ast!(LOr: ast!(Bool: true), ast!(Bool: true)),
                        ast!(Call : ast!(Load: "printf".into()), vec![ast!(StrLit: "d".into())])
                    ),
                ]);

                ast!(Call : ast!(Load: "printf".into()), vec![ast!(StrLit: "\n".into())]);
                ast!(Ret: ast!(I64: 0))
            ]
        }
    ];
    assert_eq!(run("lor".into(), a).trim(), String::from("bcd"));
}


#[test]
fn recursion() {
    let a = ast![
        ast!{
            DeclExternFun #
            name: "printf".into(),
            ty: ty!([FunVarArgs: [Unit], [Ptr: [I8]]])
        };

        ast!{
            Fun #
            name: "sum".into(),
            ty: ty!([Fun: [I64], [I64]]),
            params: vec!["x".into()],
            body: ast![
                ast!(Ret :
                    ast!(If :
                        ast!(IEq: ast!(Load: "x".into()), ast!(I64: 0)),
                        ast!(I64: 0),
                        ast!(IAdd:
                            ast!(Call : ast!(Load: "sum".into()), vec![
                                ast!(ISub : ast!(Load: "x".into()), ast!(I64 : 1))
                            ]),
                            ast!(Load : "x".into())
                        )
                    )
                )
            ]
        };

        ast!{
            Fun #
            name: "main".into(),
            ty: ty!([Fun: [I64]]),
            params: vec![],
            body: ast![
                ast!(Call : ast!(Load: "printf".into()), vec![
                    ast!(StrLit: "%d\n".into()),
                    ast!(Call : ast!(Load: "sum".into()), vec![ast!(I64: 10)])
                ]);
                ast!(Ret: ast!(I64: 0))
            ]
        }
    ];
    assert_eq!(run("recursion".into(), a).trim(), String::from("55"));
}