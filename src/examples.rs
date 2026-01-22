use std::{
    path::PathBuf,
    process::{self, Stdio},
};

use rand::{Rng, rngs::ThreadRng};

use crate::{
    compile_ast_with_ctx,
    mono_ir::{
        Ast, MatchCase, Var,
        pattern::Pattern,
        types::{AstCtx, AstTy, EnumCase, EnumDef},
    },
    run_and_check_output,
};

pub fn test_ast<S: ToString>(ast: UAst, prog_name: S) {
    test_ast_with_ctx(ast, prog_name, Default::default());
}

pub fn test_ast_with_ctx<S: ToString>(ast: UAst, prog_name: S, ctx: AstCtx) {
    let prog_name = prog_name.to_string();
    let p = PathBuf::from(prog_name.clone());
    let _ = std::fs::remove_file(&p);
    compile_ast_with_ctx(ast, prog_name.clone(), ctx);
    assert!(std::fs::exists(&p).is_ok());
    process::Command::new(prog_name.clone())
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .spawn()
        .unwrap()
        .wait()
        .unwrap()
        .exit_ok()
        .unwrap();
    run_and_check_output(
        "valgrind",
        &[&prog_name[..]],
        "All heap blocks were freed -- no leaks are possible",
    )
    .unwrap();
}

#[cfg(test)]
mod tests {
    use crate::examples::*;
    #[test]
    fn test_1() {
        test_ast(test_ast_1(), "test_dir/test_ast_1")
    }
    #[test]
    fn test_2() {
        test_ast(test_ast_2(), "test_dir/test_ast_2")
    }
    #[test]
    fn test_3() {
        test_ast(test_ast_3(), "test_dir/test_ast_3")
    }
    #[test]
    fn test_4() {
        test_ast(test_ast_4(), "test_dir/test_ast_4")
    }
    #[test]
    fn test_5() {
        test_ast(test_ast_5(), "test_dir/test_ast_5")
    }
    #[test]
    fn test_6() {
        test_ast(test_ast_6(), "test_dir/test_ast_6")
    }
    #[test]
    fn test_7() {
        test_ast(test_ast_7(), "test_dir/test_ast_7")
    }
    #[test]
    fn test_8() {
        test_ast(test_ast_8(), "test_dir/test_ast_8")
    }
    #[test]
    fn test_9() {
        test_ast(test_ast_9(), "test_dir/test_ast_9")
    }
    #[test]
    fn test_fact() {
        test_ast(fact_ast(), "test_dir/test_fact")
    }

    #[test]
    fn test_list() {
        let (ast, ctx) = list_test();
        test_ast_with_ctx(ast, "test_dir/list_test", ctx);
    }

    #[test]
    fn test_list2() {
        let (ast, ctx) = list_test2();
        test_ast_with_ctx(ast, "test_dir/list_test2", ctx);
    }

    #[test]
    fn test_list3() {
        let (ast, ctx) = list_test3();
        test_ast_with_ctx(ast, "test_dir/list_test3", ctx);
    }
}

type UAst = Ast<()>;

#[allow(unused)]
fn test_ast_1() -> UAst {
    let x = Var::fresh();
    let y = Var::fresh();
    UAst::lambda(
        x,
        AstTy::int(),
        UAst::lambda(
            y,
            AstTy::int(),
            UAst::app(
                UAst::app(
                    UAst::native("add"),
                    UAst::seq(
                        UAst::app(UAst::native("print_int"), UAst::var(x)),
                        UAst::var(x),
                    ),
                ),
                UAst::var(y),
            ),
        ),
    )
}

#[allow(unused)]
fn test_ast_2() -> UAst {
    let x = Var::fresh();
    let y = Var::fresh();
    UAst::app(
        UAst::app(
            UAst::lambda(
                x,
                AstTy::int(),
                UAst::lambda(
                    y,
                    AstTy::int(),
                    UAst::app(
                        UAst::app(
                            UAst::native("add"),
                            UAst::seq(
                                UAst::app(UAst::native("print_int"), UAst::var(x)),
                                UAst::var(x),
                            ),
                        ),
                        UAst::var(y),
                    ),
                ),
            ),
            UAst::int(5),
        ),
        UAst::int(6),
    )
}

#[allow(unused)]
fn test_ast_3() -> UAst {
    let x = Var::fresh();
    let y = Var::fresh();
    let z = Var::fresh();
    let wrapper = UAst::lambda(
        x,
        AstTy::Int,
        UAst::lambda(
            y,
            AstTy::Int,
            UAst::lambda(
                z,
                AstTy::fun(AstTy::Int, AstTy::Int),
                UAst::app(
                    UAst::app(UAst::native("add"), UAst::app(UAst::var(z), UAst::var(x))),
                    UAst::app(UAst::var(z), UAst::var(y)),
                ),
            ),
        ),
    );

    let a = Var::fresh();

    let print_ret = UAst::lambda(
        a,
        AstTy::Int,
        UAst::seq(
            UAst::app(UAst::native("print_int"), UAst::var(a)),
            UAst::var(a),
        ),
    );

    UAst::app(
        UAst::app(UAst::app(wrapper, UAst::int(69)), UAst::int(420)),
        print_ret,
    )
}

#[allow(unused)]
fn test_ast_4() -> UAst {
    UAst::app(UAst::native("print_int"), test_ast_3())
}

#[allow(unused)]
fn test_ast_5() -> UAst {
    let x = Var::fresh();

    UAst::let_in(
        x,
        AstTy::Int,
        UAst::int(5),
        UAst::app(
            UAst::native("print_int"),
            UAst::app(UAst::app(UAst::native("add"), UAst::int(64)), UAst::var(x)),
        ),
    )
}

#[allow(unused)]
fn test_ast_6() -> UAst {
    let x = Var::fresh();

    UAst::let_in(
        x,
        AstTy::fun(AstTy::Int, AstTy::Tuple(vec![])),
        UAst::native("print_int"),
        UAst::app(UAst::var(x), UAst::int(420)),
    )
}

#[allow(unused)]
fn test_ast_7() -> UAst {
    UAst::app(
        UAst::native("print_string"),
        UAst::string("Hello, World !\n"),
    )
}

#[allow(unused)]
fn test_ast_8() -> UAst {
    let x = Var::fresh();

    UAst::let_in(
        x,
        AstTy::fun(AstTy::Int, AstTy::Tuple(vec![])),
        UAst::native("print_int"),
        UAst::app(UAst::var(x), UAst::int(420)),
    )
}

#[allow(unused)]
fn test_ast_9() -> UAst {
    let x = Var::fresh();
    let print_is_zero = UAst::lambda(
        x,
        AstTy::Int,
        UAst::app(
            UAst::native("print_string"),
            UAst::seq(
                UAst::app(UAst::native("print_int"), UAst::var(x)),
                UAst::ifte(
                    UAst::var(x),
                    UAst::string(" is not zero\n"),
                    UAst::string(" is zero\n"),
                ),
            ),
        ),
    );
    let f = Var::fresh();
    UAst::let_in(
        f,
        AstTy::fun(AstTy::Int, AstTy::Tuple(vec![])),
        print_is_zero,
        UAst::seq(
            UAst::app(UAst::var(f), UAst::int(420)),
            UAst::app(UAst::var(f), UAst::int(0)),
        ),
    )
}

#[allow(unused)]
fn fact_ast() -> UAst {
    let fact = Var::fresh();
    let n = Var::fresh();
    let int = AstTy::Int;
    UAst::let_in(
        fact,
        AstTy::fun(int.clone(), int.clone()),
        UAst::lambda(
            n,
            int,
            UAst::ifte(
                UAst::var(n),
                UAst::app(
                    UAst::app(UAst::native("mul"), UAst::var(n)),
                    UAst::app(
                        UAst::var(fact),
                        UAst::app(UAst::app(UAst::native("add"), UAst::int(-1)), UAst::var(n)),
                    ),
                ),
                UAst::int(1),
            ),
        ),
        UAst::seq(
            UAst::app(
                UAst::native("print_int"),
                UAst::app(UAst::var(fact), UAst::int(6)),
            ),
            UAst::app(UAst::native("print_string"), UAst::string("\n")),
        ),
    )
}

#[allow(unused)]
fn fact_bench() -> UAst {
    let fact = Var::fresh();
    let loo = Var::fresh();
    let n = Var::fresh();
    let res = Var::fresh();
    UAst::let_in(
        fact,
        AstTy::fun(AstTy::Int, AstTy::Int),
        UAst::lambda(
            n,
            AstTy::Int,
            UAst::ifte(
                UAst::var(n),
                UAst::app(
                    UAst::app(UAst::native("mul"), UAst::var(n)),
                    UAst::app(
                        UAst::var(fact),
                        UAst::app(UAst::app(UAst::native("add"), UAst::int(-1)), UAst::var(n)),
                    ),
                ),
                UAst::int(1),
            ),
        ),
        UAst::let_in(
            loo,
            AstTy::fun(AstTy::Int, AstTy::Tuple(vec![])),
            UAst::lambda(
                n,
                AstTy::Int,
                UAst::ifte(
                    UAst::var(n),
                    UAst::let_in(
                        res,
                        AstTy::Int,
                        UAst::app(
                            UAst::var(fact),
                            UAst::app(UAst::native("random_int"), UAst::int(10)),
                        ),
                        UAst::seq(
                            UAst::seq(
                                UAst::app(UAst::native("print_int"), UAst::var(res)),
                                UAst::app(UAst::native("print_string"), UAst::string("\n")),
                            ),
                            UAst::app(
                                UAst::var(loo),
                                UAst::app(
                                    UAst::app(UAst::native("add"), UAst::int(-1)),
                                    UAst::var(n),
                                ),
                            ),
                        ),
                    ),
                    UAst::app(UAst::native("print_string"), UAst::string("Done !\n")),
                ),
            ),
            UAst::app(UAst::var(loo), UAst::int(100000)),
        ),
    )
}

#[allow(unused)]
fn list_test() -> (UAst, AstCtx) {
    let mut ctx = AstCtx::default();
    ctx.natives.insert(
        "print_lst".into(),
        AstTy::fun(AstTy::named("lst"), AstTy::Tuple(vec![])),
    );
    ctx.types.insert(
        "lst".into(),
        EnumDef {
            name: "lst".into(),
            cases: vec![
                EnumCase {
                    cons_name: "Nil".into(),
                    arg: None,
                },
                EnumCase {
                    cons_name: "Cons".into(),
                    arg: Some(AstTy::Tuple(vec![AstTy::Int, AstTy::named("lst")])),
                },
            ],
        },
    );
    let ast = UAst::app(UAst::native("print_lst"), UAst::cons("lst", "Nil", None));

    (ast, ctx)
}

#[allow(unused)]
fn list_test2() -> (UAst, AstCtx) {
    let mut ctx = AstCtx::default();
    ctx.natives.insert(
        "print_lst".into(),
        AstTy::fun(AstTy::named("lst"), AstTy::Tuple(vec![])),
    );
    ctx.types.insert(
        "lst".into(),
        EnumDef {
            name: "lst".into(),
            cases: vec![
                EnumCase {
                    cons_name: "Nil".into(),
                    arg: None,
                },
                EnumCase {
                    cons_name: "Cons".into(),
                    arg: Some(AstTy::Tuple(vec![AstTy::Int, AstTy::named("lst")])),
                },
            ],
        },
    );
    let constr = |name: &str, val| UAst::cons("lst", name, val);
    let cons = |val, old| constr("Cons", Some(UAst::tuple(vec![val, old])));
    let nil = || constr("Nil", None);
    let ast = UAst::app(
        UAst::native("print_lst"),
        cons(UAst::int(420), cons(UAst::int(69), nil())),
    );

    (ast, ctx)
}

#[allow(unused)]
fn list_test3() -> (UAst, AstCtx) {
    let mut ctx = AstCtx::default();
    ctx.natives.insert(
        "print_lst".into(),
        AstTy::fun(AstTy::named("lst"), AstTy::Tuple(vec![])),
    );
    ctx.types.insert(
        "lst".into(),
        EnumDef {
            name: "lst".into(),
            cases: vec![
                EnumCase {
                    cons_name: "Nil".into(),
                    arg: None,
                },
                EnumCase {
                    cons_name: "Cons".into(),
                    arg: Some(AstTy::Tuple(vec![AstTy::Int, AstTy::named("lst")])),
                },
            ],
        },
    );
    let constr = |name: &str, val| UAst::cons("lst", name, val);
    let cons = |val, old| constr("Cons", Some(UAst::tuple(vec![val, old])));
    let nil = || constr("Nil", None);
    let add_cons = Var::fresh();
    let l = Var::fresh();
    let ast = UAst::let_in(
        add_cons,
        AstTy::fun(AstTy::named("lst"), AstTy::named("lst")),
        UAst::lambda(l, AstTy::named("lst"), cons(UAst::int(123), UAst::var(l))),
        UAst::app(
            UAst::native("print_lst"),
            UAst::app(UAst::var(add_cons), UAst::app(UAst::var(add_cons), nil())),
        ),
    );

    (ast, ctx)
}

#[allow(unused)]
fn match_ast() -> (UAst, AstCtx) {
    let mut ctx = AstCtx::default();
    ctx.types.insert(
        "val".into(),
        EnumDef {
            name: "val".into(),
            cases: vec![
                EnumCase {
                    cons_name: "Int".into(),
                    arg: Some(AstTy::Int),
                },
                EnumCase {
                    cons_name: "Str".into(),
                    arg: Some(AstTy::String),
                },
            ],
        },
    );
    ctx.natives.insert(
        "print_int".into(),
        AstTy::fun(AstTy::Int, AstTy::Tuple(vec![])),
    );
    ctx.natives.insert(
        "print_string".into(),
        AstTy::fun(AstTy::String, AstTy::Tuple(vec![])),
    );
    let print_val = Var::fresh();
    let v = Var::fresh();
    let int_val = Var::fresh();
    let str_val = Var::fresh();
    let ast = UAst::let_in(
        print_val,
        AstTy::fun(AstTy::named("val"), AstTy::Tuple(vec![])),
        UAst::lambda(
            v,
            AstTy::named("val"),
            UAst::match_with(
                UAst::var(v),
                vec![
                    MatchCase {
                        pat: Pattern::Cons {
                            enum_name: "val".into(),
                            cons: "Int".into(),
                            arg: Some(Box::new(Pattern::Symbol(int_val, AstTy::Int))),
                        },
                        expr: UAst::app(UAst::native("print_int"), UAst::var(int_val)),
                    },
                    MatchCase {
                        pat: Pattern::Cons {
                            enum_name: "val".into(),
                            cons: "Str".into(),
                            arg: Some(Box::new(Pattern::Symbol(str_val, AstTy::String))),
                        },
                        expr: UAst::app(UAst::native("print_string"), UAst::var(str_val)),
                    },
                ],
            ),
        ),
        UAst::seq(
            UAst::seq(
                UAst::app(
                    UAst::var(print_val),
                    UAst::cons("val", "Int", Some(UAst::int(123))),
                ),
                UAst::app(UAst::native("print_string"), UAst::string("\n")),
            ),
            UAst::app(
                UAst::var(print_val),
                UAst::cons("val", "Str", Some(UAst::string("Hello, World !\n"))),
            ),
        ),
    );

    (ast, ctx)
}

#[allow(unused)]
fn list_test4() -> (UAst, AstCtx) {
    let mut ctx = AstCtx::default();
    ctx.natives.insert(
        "print_lst".into(),
        AstTy::fun(AstTy::named("lst"), AstTy::Tuple(vec![])),
    );
    ctx.types.insert(
        "lst".into(),
        EnumDef {
            name: "lst".into(),
            cases: vec![
                EnumCase {
                    cons_name: "Nil".into(),
                    arg: None,
                },
                EnumCase {
                    cons_name: "Cons".into(),
                    arg: Some(AstTy::Tuple(vec![AstTy::Int, AstTy::named("lst")])),
                },
            ],
        },
    );
    let constr = |name: &str, val| UAst::cons("lst", name, val);
    let cons = |val, old| constr("Cons", Some(UAst::tuple(vec![val, old])));
    let nil = || constr("Nil", None);
    let rev = Var::fresh();
    let l = Var::fresh();
    let aux = Var::fresh();
    let l2 = Var::fresh();
    let acc = Var::fresh();
    let hd = Var::fresh();
    let tl = Var::fresh();
    let ast = UAst::let_in(
        rev,
        AstTy::fun(AstTy::named("lst"), AstTy::named("lst")),
        UAst::let_in(
            aux,
            AstTy::fun(
                AstTy::named("lst"),
                AstTy::fun(AstTy::named("lst"), AstTy::named("lst")),
            ),
            UAst::lambda(
                acc,
                AstTy::named("lst"),
                UAst::lambda(
                    l2,
                    AstTy::named("lst"),
                    UAst::match_with(
                        UAst::var(l2),
                        vec![
                            MatchCase {
                                pat: Pattern::cons("lst", "Nil", None),
                                expr: UAst::var(acc),
                            },
                            MatchCase {
                                pat: Pattern::cons(
                                    "lst",
                                    "Cons",
                                    Some(Pattern::tuple(vec![
                                        Pattern::symb(hd, AstTy::Int),
                                        Pattern::symb(tl, AstTy::named("lst")),
                                    ])),
                                ),
                                expr: UAst::app(
                                    UAst::app(UAst::var(aux), cons(UAst::var(hd), UAst::var(acc))),
                                    UAst::var(tl),
                                ),
                            },
                        ],
                    ),
                ),
            ),
            UAst::app(UAst::var(aux), nil()),
        ),
        UAst::app(
            UAst::native("print_lst"),
            UAst::app(
                UAst::var(rev),
                cons(UAst::int(123), cons(UAst::int(456), nil())),
            ),
        ),
    );

    (ast, ctx)
}

#[allow(unused)]
fn list_test5() -> (UAst, AstCtx) {
    let mut ctx = AstCtx::default();
    ctx.natives.insert(
        "print_lst".into(),
        AstTy::fun(AstTy::named("lst"), AstTy::Tuple(vec![])),
    );
    ctx.natives.insert(
        "print_string".into(),
        AstTy::fun(AstTy::String, AstTy::Tuple(vec![])),
    );
    ctx.types.insert(
        "lst".into(),
        EnumDef {
            name: "lst".into(),
            cases: vec![
                EnumCase {
                    cons_name: "Nil".into(),
                    arg: None,
                },
                EnumCase {
                    cons_name: "Cons".into(),
                    arg: Some(AstTy::Tuple(vec![AstTy::Int, AstTy::named("lst")])),
                },
            ],
        },
    );
    let constr = |name: &str, val| UAst::cons("lst", name, val);
    let cons = |val, old| constr("Cons", Some(UAst::tuple(vec![val, old])));
    let nil = || constr("Nil", None);
    let mut rng = rand::rng();
    fn create_random_list(i: usize, rng: &mut ThreadRng) -> UAst {
        if i == 0 {
            UAst::cons("lst", "Nil", None)
        } else {
            UAst::cons(
                "lst",
                "Cons",
                Some(UAst::tuple(vec![
                    UAst::int(rng.random()),
                    create_random_list(i - 1, rng),
                ])),
            )
        }
    }

    let mut create_random_list = |i: usize| create_random_list(i, &mut rng);

    let rev = Var::fresh();
    let l = Var::fresh();
    let aux = Var::fresh();
    let l2 = Var::fresh();
    let acc = Var::fresh();
    let hd = Var::fresh();
    let tl = Var::fresh();
    let l3 = Var::fresh();
    let ast = UAst::let_in(
        rev,
        AstTy::fun(AstTy::named("lst"), AstTy::named("lst")),
        UAst::lambda(
            l,
            AstTy::named("lst"),
            UAst::let_in(
                aux,
                AstTy::fun(
                    AstTy::named("lst"),
                    AstTy::fun(AstTy::named("lst"), AstTy::named("lst")),
                ),
                UAst::lambda(
                    l2,
                    AstTy::named("lst"),
                    UAst::lambda(
                        acc,
                        AstTy::named("lst"),
                        UAst::match_with(
                            UAst::var(l2),
                            vec![
                                MatchCase {
                                    pat: Pattern::cons("lst", "Nil", None),
                                    expr: UAst::var(acc),
                                },
                                MatchCase {
                                    pat: Pattern::cons(
                                        "lst",
                                        "Cons",
                                        Some(Pattern::tuple(vec![
                                            Pattern::symb(hd, AstTy::Int),
                                            Pattern::symb(tl, AstTy::named("lst")),
                                        ])),
                                    ),
                                    expr: UAst::app(
                                        UAst::app(UAst::var(aux), UAst::var(tl)),
                                        cons(UAst::var(hd), UAst::var(acc)),
                                    ),
                                },
                            ],
                        ),
                    ),
                ),
                UAst::app(UAst::app(UAst::var(aux), UAst::var(l)), nil()),
            ),
        ),
        UAst::let_in(
            l3,
            AstTy::named("lst"),
            create_random_list(150),
            UAst::seq(
                UAst::seq(
                    UAst::app(UAst::native("print_lst"), UAst::var(l3)),
                    UAst::app(UAst::native("print_string"), UAst::string("\n")),
                ),
                UAst::app(
                    UAst::native("print_lst"),
                    UAst::app(UAst::var(rev), UAst::var(l3)),
                ),
            ),
        ),
    );

    (ast, ctx)
}

fn get_loo<F>(body: F) -> UAst
where
    F: FnOnce(Var) -> UAst,
{
    let loo = Var::fresh();
    let n = Var::fresh();
    let f = Var::fresh();
    UAst::let_in(
        loo,
        AstTy::fun(
            AstTy::Int,
            AstTy::fun(
                AstTy::fun(AstTy::Tuple(vec![]), AstTy::Tuple(vec![])),
                AstTy::Tuple(vec![]),
            ),
        ),
        UAst::lambda(
            n,
            AstTy::Int,
            UAst::lambda(
                f,
                AstTy::fun(AstTy::Tuple(vec![]), AstTy::Tuple(vec![])),
                UAst::ifte(
                    UAst::var(n),
                    UAst::seq(
                        UAst::app(UAst::var(f), UAst::tuple(vec![])),
                        UAst::app(
                            UAst::app(
                                UAst::var(loo),
                                UAst::app(
                                    UAst::app(UAst::native("add"), UAst::int(-1)),
                                    UAst::var(n),
                                ),
                            ),
                            UAst::var(f),
                        ),
                    ),
                    UAst::tuple(vec![]),
                ),
            ),
        ),
        body(loo),
    )
}

fn make_random_list_generator() -> UAst {
    let g = Var::fresh();
    let n = Var::fresh();
    let aux = Var::fresh();
    let n2 = Var::fresh();
    let acc = Var::fresh();

    // Define aux at the OUTER level
    UAst::let_in(
        aux,
        AstTy::fun(
            AstTy::Int,
            AstTy::fun(AstTy::named("lst"), AstTy::named("lst")),
        ),
        UAst::lambda(
            n2,
            AstTy::Int,
            UAst::lambda(
                acc,
                AstTy::named("lst"),
                UAst::ifte(
                    UAst::var(n2),
                    UAst::app(
                        UAst::app(
                            UAst::var(aux),
                            UAst::app(UAst::app(UAst::native("add"), UAst::int(-1)), UAst::var(n2)),
                        ),
                        UAst::cons(
                            "lst",
                            "Cons",
                            Some(UAst::tuple(vec![
                                UAst::app(
                                    UAst::app(UAst::native("add"), UAst::int(100)),
                                    UAst::app(UAst::native("random_int"), UAst::int(899)),
                                ),
                                UAst::var(acc),
                            ])),
                        ),
                    ),
                    UAst::var(acc),
                ),
            ),
        ),
        // Now define g, using aux
        UAst::let_in(
            g,
            AstTy::fun(AstTy::Int, AstTy::named("lst")),
            UAst::lambda(
                n,
                AstTy::Int,
                UAst::app(
                    UAst::app(UAst::var(aux), UAst::var(n)),
                    UAst::cons("lst", "Nil", None),
                ),
            ),
            UAst::var(g),
        ),
    )
}

pub fn rev_bench(list_size: i32, loop_n: i32) -> (UAst, AstCtx) {
    let mut ctx = AstCtx::new();
    ctx.types.insert(
        "lst".into(),
        EnumDef {
            name: "lst".into(),
            cases: vec![
                EnumCase {
                    cons_name: "Nil".into(),
                    arg: None,
                },
                EnumCase {
                    cons_name: "Cons".into(),
                    arg: Some(AstTy::Tuple(vec![AstTy::Int, AstTy::named("lst")])),
                },
            ],
        },
    );

    let constr = |name: &str, val| UAst::cons("lst", name, val);
    let cons = |val, old| constr("Cons", Some(UAst::tuple(vec![val, old])));
    let nil = || constr("Nil", None);
    let l = Var::fresh();
    let aux = Var::fresh();
    let l2 = Var::fresh();
    let acc = Var::fresh();
    let hd = Var::fresh();
    let tl = Var::fresh();

    let void_arg = Var::fresh();
    let l4 = Var::fresh();
    let rev = Var::fresh();

    let generate_list = UAst::lambda(
        void_arg,
        AstTy::Tuple(vec![]),
        UAst::let_in(
            l4,
            AstTy::named("lst"),
            UAst::app(make_random_list_generator(), UAst::int(list_size)),
            UAst::seq(
                UAst::seq(
                    UAst::app(UAst::native("print_lst"), UAst::var(l4)),
                    UAst::app(
                        UAst::native("print_lst"),
                        UAst::app(UAst::var(rev), UAst::var(l4)),
                    ),
                ),
                UAst::tuple(vec![]),
            ),
        ),
    );

    let ast = UAst::let_in(
        rev,
        AstTy::fun(AstTy::named("lst"), AstTy::named("lst")),
        UAst::lambda(
            l,
            AstTy::named("lst"),
            UAst::let_in(
                aux,
                AstTy::fun(
                    AstTy::named("lst"),
                    AstTy::fun(AstTy::named("lst"), AstTy::named("lst")),
                ),
                UAst::lambda(
                    l2,
                    AstTy::named("lst"),
                    UAst::lambda(
                        acc,
                        AstTy::named("lst"),
                        UAst::match_with(
                            UAst::var(l2),
                            vec![
                                MatchCase {
                                    pat: Pattern::cons("lst", "Nil", None),
                                    expr: UAst::var(acc),
                                },
                                MatchCase {
                                    pat: Pattern::cons(
                                        "lst",
                                        "Cons",
                                        Some(Pattern::tuple(vec![
                                            Pattern::symb(hd, AstTy::Int),
                                            Pattern::symb(tl, AstTy::named("lst")),
                                        ])),
                                    ),
                                    expr: UAst::app(
                                        UAst::app(UAst::var(aux), UAst::var(tl)),
                                        cons(UAst::var(hd), UAst::var(acc)),
                                    ),
                                },
                            ],
                        ),
                    ),
                ),
                UAst::app(UAst::app(UAst::var(aux), UAst::var(l)), nil()),
            ),
        ),
        get_loo(|loo| UAst::app(UAst::app(UAst::var(loo), UAst::int(loop_n)), generate_list)),
    );
    (ast, ctx)
}
