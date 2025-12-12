use crate::parsing::{docstring::DocString, location::Location};

pub enum Token<'a> {
    AmperAmper,                   // &&
    Ampersand,                    // &
    And,                          // and
    As,                           // as
    Assert,                       // assert
    Backquote,                    // `
    Bang,                         // !
    Bar,                          // |
    Barbar,                       // ||
    BarRBracket,                  // |]
    Char(char),                   // 'a'        (just an example)
    Class,                        // class
    Colon,                        // :
    ColonColon,                   // ::
    ColonEqual,                   // :=
    ColonGreater,                 // :>
    Comma,                        // ,
    Constraint,                   // constraint
    Do,                           // do
    Done,                         // done
    Dot,                          // .
    DotDot,                       // ..
    DownTo,                       // downto
    Effect,                       // effect
    Else,                         // else
    End,                          // end
    Eof,                          // End of file
    Equal,                        // =
    Exception,                    // exception
    External,                     // external
    False,                        // false
    Float(&'a str, Option<char>), // 42.0
    For,                          // for
    Fun,                          // fun
    Function,                     // function
    Functor,                      // Functor
    Greater,                      // >
    GreaterRBrace,                // >}
    GreaterRBracket,              // >]
    If,                           // if
    In,                           // in
    Include,                      // include
    InfixOp0(&'a str),            // !=         (just an example)
    InfixOp1(&'a str),            // @          (just an example)
    InfixOp2(&'a str),            // +!         (just an example)
    InfixOp3(&'a str),            // land       (just an example)
    InfixOp4(&'a str),            // **         (just an example)
    DotOp(&'a str),               // .+
    LetOp(&'a str),               // let+       (just an example)
    AndOp(&'a str),               // and+       (just and example)
    Inherit,                      // inherit
    Initializer,                  // Initializer
    Int(&'a str, Option<char>),   // 42         (just an example)
    Label(&'a str),               // ~label     (just an example)
    Lazy,                         // lazy
    LBrace,                       // {
    LBraceLess,                   // {<
    LBracketBar,                  // [|
    LBracketLess,                 // [<
    LBracketGreater,              // [>
    LBracketPercent,              // [%
    LBracketPercentPercent,       // [%%
    Less,                         // <
    LessMinus,                    // <-
    Let,                          // let
    Lident,                       // lident     (just an example)
    LParen,                       // (
    LBracketAt,                   // [@
    LBracketAtAt,                 // [@@
    LBracketAtAtAt,               // [@@@
    Match,                        // match
    Method,                       // method
    Minus,                        // -
    MinusDot,                     // -.
    MinusGreater,                 // ->
    Module,                       // module
    Mutable,                      //mutable
    New,                          // new
    NonRec,                       // nonrec
    Object,                       // object
    Of,                           // of
    Open,                         // open
    OptLabel(&'a str),            // ?label:    (just an example)
    Or,                           // or
    Percent,                      // %
    Plus,                         // +
    PlusDot,                      // +.
    PlusEq,                       // +=
    PrefixOp(&'a str),            // !+
    Private,                      // private,
    Question,                     // ?
    Quote,                        // '
    RBrace,                       // }
    RBracket,                     // ]
    Rec,                          // rec
    RParen,                       // )
    Semi,                         // ;
    SemiSemi,                     // ;;
    Hash,                         // #
    HashOp(&'a str),              // ##         (just an example)
    Sig,                          // sig
    Star,                         // *
    String(Box<(&'a str, Location, Option<&'a str>)>),
    QuotedStringExpr(Box<(&'a str, Location, &'a str, Location, Option<&'a str>)>),
    QuotedStringItem(Box<(&'a str, Location, &'a str, Location, Option<&'a str>)>),
    Struct,                            // struct
    Then,                              // then
    Tilde,                             // ~
    To,                                // to
    True,                              // true
    Try,                               // try
    Type,                              // type
    UIdent(&'a str),                   // UIdent (just an example)
    Underscore,                        // _
    Val,                               // val
    Virtual,                           // virtual
    When,                              // when
    While,                             // while
    With,                              // with
    Comment(Box<(&'a str, Location)>), // (*  This is a comment *)
    DocString(DocString),          // (** Documentation *)
    Eol,                               // \\n
}
