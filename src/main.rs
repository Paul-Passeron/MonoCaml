#![feature(exit_status_error)]
#![feature(random)]

use std::{
    io::Read,
    iter::once,
    path::PathBuf,
    process::{self, Command, Stdio},
};

use rand::{Rng, rngs::ThreadRng};

use crate::{
    ast::{
        Ast, MatchCase, Var,
        pattern::Pattern,
        types::{AstCtx, AstTy, AstTyped, EnumCase, EnumDef},
    },
    backend::c_backend::ExportC,
    cfg::{FunName, Label, compile::Compiler, var::CfgVar},
};

pub mod ast;
pub mod backend;
pub mod cfg;
pub mod helpers;

#[allow(unused)]
fn test_ast_1() -> Ast {
    let x = Var::fresh();
    let y = Var::fresh();
    Ast::lambda(
        AstTyped::new(x, AstTy::int()),
        Ast::lambda(
            AstTyped::new(y, AstTy::int()),
            Ast::app(
                Ast::app(
                    Ast::native("add"),
                    Ast::seq(Ast::app(Ast::native("print_int"), Ast::var(x)), Ast::var(x)),
                ),
                Ast::var(y),
            ),
        ),
    )
}

#[allow(unused)]
fn test_ast_2() -> Ast {
    let x = Var::fresh();
    let y = Var::fresh();
    Ast::app(
        Ast::app(
            Ast::lambda(
                AstTyped::new(x, AstTy::int()),
                Ast::lambda(
                    AstTyped::new(y, AstTy::int()),
                    Ast::app(
                        Ast::app(
                            Ast::native("add"),
                            Ast::seq(Ast::app(Ast::native("print_int"), Ast::var(x)), Ast::var(x)),
                        ),
                        Ast::var(y),
                    ),
                ),
            ),
            Ast::Int(5),
        ),
        Ast::Int(6),
    )
}

#[allow(unused)]
fn test_ast_3() -> Ast {
    let x = Var::fresh();
    let y = Var::fresh();
    let z = Var::fresh();
    let wrapper = Ast::lambda(
        AstTyped::new(x, AstTy::Int),
        Ast::lambda(
            AstTyped::new(y, AstTy::Int),
            Ast::lambda(
                AstTyped::new(z, AstTy::fun(AstTy::Int, AstTy::Int)),
                Ast::app(
                    Ast::app(
                        Ast::Native("add".into()),
                        Ast::app(Ast::Var(z), Ast::Var(x)),
                    ),
                    Ast::app(Ast::Var(z), Ast::Var(y)),
                ),
            ),
        ),
    );

    let a = Var::fresh();

    let print_ret = Ast::lambda(
        AstTyped::new(a, AstTy::Int),
        Ast::seq(Ast::app(Ast::native("print_int"), Ast::Var(a)), Ast::Var(a)),
    );

    Ast::app(
        Ast::app(Ast::app(wrapper, Ast::Int(69)), Ast::Int(420)),
        print_ret,
    )
}

#[allow(unused)]
fn test_ast_4() -> Ast {
    Ast::app(Ast::native("print_int"), test_ast_3())
}

#[allow(unused)]
fn test_ast_5() -> Ast {
    let x = Var::fresh();

    Ast::let_in(
        x,
        AstTy::Int,
        Ast::Int(5),
        Ast::app(
            Ast::native("print_int"),
            Ast::app(Ast::app(Ast::native("add"), Ast::Int(64)), Ast::Var(x)),
        ),
    )
}

#[allow(unused)]
fn test_ast_6() -> Ast {
    let x = Var::fresh();

    Ast::let_in(
        x,
        AstTy::fun(AstTy::Int, AstTy::Tuple(vec![])),
        Ast::native("print_int"),
        Ast::app(Ast::Var(x), Ast::Int(420)),
    )
}

#[allow(unused)]
fn test_ast_7() -> Ast {
    Ast::app(Ast::native("print_string"), Ast::string("Hello, World !\n"))
}

#[allow(unused)]
fn test_ast_8() -> Ast {
    let x = Var::fresh();

    Ast::let_in(
        x,
        AstTy::fun(AstTy::Int, AstTy::Tuple(vec![])),
        Ast::native("print_int"),
        Ast::app(Ast::Var(x), Ast::Int(420)),
    )
}

#[allow(unused)]
fn test_ast_9() -> Ast {
    let x = Var::fresh();
    let print_is_zero = Ast::lambda(
        AstTyped::new(x, AstTy::Int),
        Ast::app(
            Ast::native("print_string"),
            Ast::seq(
                Ast::app(Ast::native("print_int"), Ast::Var(x)),
                Ast::ifte(
                    Ast::var(x),
                    Ast::string(" is not zero\n"),
                    Ast::string(" is zero\n"),
                ),
            ),
        ),
    );
    let f = Var::fresh();
    Ast::let_in(
        f,
        AstTy::fun(AstTy::Int, AstTy::Tuple(vec![])),
        print_is_zero,
        Ast::seq(
            Ast::app(Ast::Var(f), Ast::Int(420)),
            Ast::app(Ast::Var(f), Ast::Int(0)),
        ),
    )
}

#[allow(unused)]
fn fact_ast() -> Ast {
    let fact = Var::fresh();
    let n = Var::fresh();
    let int = AstTy::Int;
    Ast::let_in(
        fact,
        AstTy::fun(int.clone(), int.clone()),
        Ast::lambda(
            AstTyped::new(n, int),
            Ast::ifte(
                Ast::Var(n),
                Ast::app(
                    Ast::app(Ast::native("mul"), Ast::var(n)),
                    Ast::app(
                        Ast::Var(fact),
                        Ast::app(Ast::app(Ast::native("add"), Ast::Int(-1)), Ast::var(n)),
                    ),
                ),
                Ast::Int(1),
            ),
        ),
        Ast::seq(
            Ast::app(
                Ast::native("print_int"),
                Ast::app(Ast::var(fact), Ast::Int(6)),
            ),
            Ast::app(Ast::native("print_string"), Ast::string("\n")),
        ),
    )
}

#[allow(unused)]
fn fact_bench() -> Ast {
    let fact = Var::fresh();
    let loo = Var::fresh();
    let n = Var::fresh();
    let res = Var::fresh();
    Ast::let_in(
        fact,
        AstTy::fun(AstTy::Int, AstTy::Int),
        Ast::lambda(
            AstTyped::new(n, AstTy::Int),
            Ast::ifte(
                Ast::Var(n),
                Ast::app(
                    Ast::app(Ast::native("mul"), Ast::var(n)),
                    Ast::app(
                        Ast::Var(fact),
                        Ast::app(Ast::app(Ast::native("add"), Ast::Int(-1)), Ast::var(n)),
                    ),
                ),
                Ast::Int(1),
            ),
        ),
        Ast::let_in(
            loo,
            AstTy::fun(AstTy::Int, AstTy::Tuple(vec![])),
            Ast::lambda(
                AstTyped::new(n, AstTy::Int),
                Ast::ifte(
                    Ast::var(n),
                    Ast::let_in(
                        res,
                        AstTy::Int,
                        Ast::app(
                            Ast::var(fact),
                            Ast::app(Ast::native("random_int"), Ast::Int(10)),
                        ),
                        Ast::seq(
                            Ast::seq(
                                Ast::app(Ast::native("print_int"), Ast::var(res)),
                                Ast::app(Ast::native("print_string"), Ast::string("\n")),
                            ),
                            Ast::app(
                                Ast::var(loo),
                                Ast::app(Ast::app(Ast::native("add"), Ast::Int(-1)), Ast::var(n)),
                            ),
                        ),
                    ),
                    Ast::app(Ast::native("print_string"), Ast::string("Done !\n")),
                ),
            ),
            Ast::app(Ast::var(loo), Ast::Int(100000)),
        ),
    )
}

#[allow(unused)]
fn list_test() -> (Ast, AstCtx) {
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
    let ast = Ast::app(Ast::native("print_lst"), Ast::cons("lst", "Nil", None));

    (ast, ctx)
}

#[allow(unused)]
fn list_test2() -> (Ast, AstCtx) {
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
    let constr = |name: &str, val| Ast::cons("lst", name, val);
    let cons = |val, old| constr("Cons", Some(Ast::tuple(vec![val, old])));
    let nil = || constr("Nil", None);
    let ast = Ast::app(
        Ast::native("print_lst"),
        cons(Ast::Int(420), cons(Ast::Int(69), nil())),
    );

    (ast, ctx)
}

#[allow(unused)]
fn list_test3() -> (Ast, AstCtx) {
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
    let constr = |name: &str, val| Ast::cons("lst", name, val);
    let cons = |val, old| constr("Cons", Some(Ast::tuple(vec![val, old])));
    let nil = || constr("Nil", None);
    let add_cons = Var::fresh();
    let l = Var::fresh();
    let ast = Ast::let_in(
        add_cons,
        AstTy::fun(AstTy::named("lst"), AstTy::named("lst")),
        Ast::lambda(
            AstTyped::new(l, AstTy::named("lst")),
            cons(Ast::Int(123), Ast::Var(l)),
        ),
        Ast::app(
            Ast::native("print_lst"),
            Ast::app(Ast::Var(add_cons), Ast::app(Ast::Var(add_cons), nil())),
        ),
    );

    (ast, ctx)
}

#[allow(unused)]
fn match_ast() -> (Ast, AstCtx) {
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
    let ast = Ast::let_in(
        print_val,
        AstTy::fun(AstTy::named("val"), AstTy::Tuple(vec![])),
        Ast::lambda(
            AstTyped::new(v, AstTy::named("val")),
            Ast::match_with(
                Ast::var(v),
                vec![
                    MatchCase {
                        pat: Pattern::Cons {
                            enum_name: "val".into(),
                            cons: "Int".into(),
                            arg: Some(Box::new(Pattern::Symbol(int_val, AstTy::Int))),
                        },
                        expr: Ast::app(Ast::native("print_int"), Ast::var(int_val)),
                    },
                    MatchCase {
                        pat: Pattern::Cons {
                            enum_name: "val".into(),
                            cons: "Str".into(),
                            arg: Some(Box::new(Pattern::Symbol(str_val, AstTy::String))),
                        },
                        expr: Ast::app(Ast::native("print_string"), Ast::var(str_val)),
                    },
                ],
            ),
        ),
        Ast::seq(
            Ast::seq(
                Ast::app(
                    Ast::var(print_val),
                    Ast::cons("val", "Int", Some(Ast::int(123))),
                ),
                Ast::app(Ast::native("print_string"), Ast::string("\n")),
            ),
            Ast::app(
                Ast::var(print_val),
                Ast::cons("val", "Str", Some(Ast::string("Hello, World !\n"))),
            ),
        ),
    );

    (ast, ctx)
}

#[allow(unused)]
fn list_test4() -> (Ast, AstCtx) {
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
    let constr = |name: &str, val| Ast::cons("lst", name, val);
    let cons = |val, old| constr("Cons", Some(Ast::tuple(vec![val, old])));
    let nil = || constr("Nil", None);
    let rev = Var::fresh();
    let l = Var::fresh();
    let aux = Var::fresh();
    let l2 = Var::fresh();
    let acc = Var::fresh();
    let hd = Var::fresh();
    let tl = Var::fresh();
    let ast = Ast::let_in(
        rev,
        AstTy::fun(AstTy::named("lst"), AstTy::named("lst")),
        Ast::let_in(
            aux,
            AstTy::fun(
                AstTy::named("lst"),
                AstTy::fun(AstTy::named("lst"), AstTy::named("lst")),
            ),
            Ast::lambda(
                AstTyped::new(acc, AstTy::named("lst")),
                Ast::lambda(
                    AstTyped::new(l2, AstTy::named("lst")),
                    Ast::match_with(
                        Ast::var(l2),
                        vec![
                            MatchCase {
                                pat: Pattern::cons("lst", "Nil", None),
                                expr: Ast::var(acc),
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
                                expr: Ast::app(
                                    Ast::app(Ast::Var(aux), cons(Ast::Var(hd), Ast::var(acc))),
                                    Ast::Var(tl),
                                ),
                            },
                        ],
                    ),
                ),
            ),
            Ast::app(Ast::Var(aux), nil()),
        ),
        Ast::app(
            Ast::native("print_lst"),
            Ast::app(
                Ast::var(rev),
                cons(Ast::Int(123), cons(Ast::Int(456), nil())),
            ),
        ),
    );

    (ast, ctx)
}

#[allow(unused)]
fn list_test5() -> (Ast, AstCtx) {
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
    let constr = |name: &str, val| Ast::cons("lst", name, val);
    let cons = |val, old| constr("Cons", Some(Ast::tuple(vec![val, old])));
    let nil = || constr("Nil", None);
    let mut rng = rand::rng();
    fn create_random_list(i: usize, rng: &mut ThreadRng) -> Ast {
        if i == 0 {
            Ast::cons("lst", "Nil", None)
        } else {
            Ast::cons(
                "lst",
                "Cons",
                Some(Ast::Tuple(vec![
                    Ast::Int(rng.random()),
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
    let ast = Ast::let_in(
        rev,
        AstTy::fun(AstTy::named("lst"), AstTy::named("lst")),
        Ast::lambda(
            AstTyped::new(l, AstTy::named("lst")),
            Ast::let_in(
                aux,
                AstTy::fun(
                    AstTy::named("lst"),
                    AstTy::fun(AstTy::named("lst"), AstTy::named("lst")),
                ),
                Ast::lambda(
                    AstTyped::new(l2, AstTy::named("lst")),
                    Ast::lambda(
                        AstTyped::new(acc, AstTy::named("lst")),
                        Ast::match_with(
                            Ast::var(l2),
                            vec![
                                MatchCase {
                                    pat: Pattern::cons("lst", "Nil", None),
                                    expr: Ast::var(acc),
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
                                    expr: Ast::app(
                                        Ast::app(Ast::Var(aux), Ast::Var(tl)),
                                        cons(Ast::Var(hd), Ast::var(acc)),
                                    ),
                                },
                            ],
                        ),
                    ),
                ),
                Ast::app(Ast::app(Ast::Var(aux), Ast::var(l)), nil()),
            ),
        ),
        Ast::let_in(
            l3,
            AstTy::named("lst"),
            create_random_list(150),
            Ast::seq(
                Ast::seq(
                    Ast::app(Ast::native("print_lst"), Ast::var(l3)),
                    Ast::app(Ast::native("print_string"), Ast::string("\n")),
                ),
                Ast::app(
                    Ast::native("print_lst"),
                    Ast::app(Ast::var(rev), Ast::var(l3)),
                ),
            ),
        ),
    );

    (ast, ctx)
}

fn get_loo<F>(body: F) -> Ast
where
    F: FnOnce(Var) -> Ast,
{
    let loo = Var::fresh();
    let n = Var::fresh();
    let f = Var::fresh();
    Ast::let_in(
        loo,
        AstTy::fun(
            AstTy::Int,
            AstTy::fun(
                AstTy::fun(AstTy::Tuple(vec![]), AstTy::Tuple(vec![])),
                AstTy::Tuple(vec![]),
            ),
        ),
        Ast::lambda(
            AstTyped::new(n, AstTy::Int),
            Ast::lambda(
                AstTyped::new(f, AstTy::fun(AstTy::Tuple(vec![]), AstTy::Tuple(vec![]))),
                Ast::ifte(
                    Ast::var(n),
                    Ast::seq(
                        Ast::app(Ast::var(f), Ast::Tuple(vec![])),
                        Ast::app(
                            Ast::app(
                                Ast::var(loo),
                                Ast::app(Ast::app(Ast::native("add"), Ast::Int(-1)), Ast::var(n)),
                            ),
                            Ast::var(f),
                        ),
                    ),
                    Ast::Tuple(vec![]),
                ),
            ),
        ),
        body(loo),
    )
}

fn make_random_list_generator() -> Ast {
    let g = Var::fresh();
    let n = Var::fresh();
    let aux = Var::fresh();
    let n2 = Var::fresh();
    let acc = Var::fresh();

    // Define aux at the OUTER level
    Ast::let_in(
        aux,
        AstTy::fun(
            AstTy::Int,
            AstTy::fun(AstTy::named("lst"), AstTy::named("lst")),
        ),
        Ast::lambda(
            AstTyped::new(n2, AstTy::Int),
            Ast::lambda(
                AstTyped::new(acc, AstTy::named("lst")),
                Ast::ifte(
                    Ast::var(n2),
                    Ast::app(
                        Ast::app(
                            Ast::var(aux),
                            Ast::app(Ast::app(Ast::native("add"), Ast::Int(-1)), Ast::var(n2)),
                        ),
                        Ast::cons(
                            "lst",
                            "Cons",
                            Some(Ast::Tuple(vec![
                                Ast::app(
                                    Ast::app(Ast::native("add"), Ast::Int(100)),
                                    Ast::app(Ast::native("random_int"), Ast::Int(899)),
                                ),
                                Ast::var(acc),
                            ])),
                        ),
                    ),
                    Ast::var(acc),
                ),
            ),
        ),
        // Now define g, using aux
        Ast::let_in(
            g,
            AstTy::fun(AstTy::Int, AstTy::named("lst")),
            Ast::lambda(
                AstTyped::new(n, AstTy::Int),
                Ast::app(
                    Ast::app(Ast::var(aux), Ast::var(n)),
                    Ast::cons("lst", "Nil", None),
                ),
            ),
            Ast::var(g),
        ),
    )
}

fn rev_bench(list_size: i32, loop_n: i32) -> (Ast, AstCtx) {
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

    let constr = |name: &str, val| Ast::cons("lst", name, val);
    let cons = |val, old| constr("Cons", Some(Ast::tuple(vec![val, old])));
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

    let generate_list = Ast::lambda(
        AstTyped::new(void_arg, AstTy::Tuple(vec![])),
        Ast::let_in(
            l4,
            AstTy::named("lst"),
            Ast::app(make_random_list_generator(), Ast::int(list_size)),
            Ast::seq(
                Ast::seq(
                    Ast::app(Ast::native("print_lst"), Ast::var(l4)),
                    Ast::app(
                        Ast::native("print_lst"),
                        Ast::app(Ast::var(rev), Ast::var(l4)),
                    ),
                ),
                Ast::Tuple(vec![]),
            ),
        ),
    );

    let ast = Ast::let_in(
        rev,
        AstTy::fun(AstTy::named("lst"), AstTy::named("lst")),
        Ast::lambda(
            AstTyped::new(l, AstTy::named("lst")),
            Ast::let_in(
                aux,
                AstTy::fun(
                    AstTy::named("lst"),
                    AstTy::fun(AstTy::named("lst"), AstTy::named("lst")),
                ),
                Ast::lambda(
                    AstTyped::new(l2, AstTy::named("lst")),
                    Ast::lambda(
                        AstTyped::new(acc, AstTy::named("lst")),
                        Ast::match_with(
                            Ast::var(l2),
                            vec![
                                MatchCase {
                                    pat: Pattern::cons("lst", "Nil", None),
                                    expr: Ast::var(acc),
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
                                    expr: Ast::app(
                                        Ast::app(Ast::Var(aux), Ast::Var(tl)),
                                        cons(Ast::Var(hd), Ast::var(acc)),
                                    ),
                                },
                            ],
                        ),
                    ),
                ),
                Ast::app(Ast::app(Ast::Var(aux), Ast::var(l)), nil()),
            ),
        ),
        get_loo(|loo| Ast::app(Ast::app(Ast::var(loo), Ast::Int(loop_n)), generate_list)),
    );
    (ast, ctx)
}

#[allow(unused)]
fn compile_ast<S: ToString>(ast: Ast, prog_name: S) {
    compile_ast_with_ctx(ast, prog_name, AstCtx::default())
}

fn compile_ast_with_ctx<S: ToString>(ast: Ast, prog_name: S, ctx: AstCtx) {
    let prog_name = prog_name.to_string();
    println!("{ast}");
    let prog = Compiler::compile(ast, ctx);
    println!("{prog}");
    prog.compile(ExportC::new(PathBuf::from(format!("./{prog_name}.c"))))
        .unwrap();

    let mut compile = process::Command::new("gcc");
    compile
        .arg("-o")
        .arg(format!("{prog_name}"))
        .arg("-I.")
        .arg(format!("{prog_name}.c"))
        .arg("runtime.c")
        .arg("-O2");

    println!(
        "[CMD] {}",
        once(format!("{}", compile.get_program().display()))
            .chain(
                compile
                    .get_args()
                    .into_iter()
                    .map(|x| format!("{}", x.display()))
            )
            .collect::<Vec<String>>()
            .join(" ")
    );
    compile.spawn().unwrap().wait().unwrap().exit_ok().unwrap();

    Var::reset();
    CfgVar::reset();
    Label::reset();
    FunName::reset();
}

fn run_and_check_output(program: &str, args: &[&str], expected: &str) -> std::io::Result<bool> {
    let mut child = Command::new(program)
        .args(args)
        .stdout(Stdio::piped())
        .stderr(Stdio::null())
        .spawn()?;

    let mut stdout = String::new();
    child.stdout.take().unwrap().read_to_string(&mut stdout)?;
    child.wait()?;

    Ok(stdout.contains(expected))
}

pub fn test_ast<S: ToString>(ast: Ast, prog_name: S) {
    test_ast_with_ctx(ast, prog_name, Default::default());
}

pub fn test_ast_with_ctx<S: ToString>(ast: Ast, prog_name: S, ctx: AstCtx) {
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
    use super::*;
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

fn main() {
    let (ast, ctx) = rev_bench(100, 100);
    compile_ast_with_ctx(ast, "test", ctx);
}
