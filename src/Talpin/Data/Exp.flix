mod Talpin.Data {

pub enum Exp {
    /// x
    case Var(String)
    /// (e e')
    case App(Exp, Exp)
    /// (lambda (x) e)
    case Abs(String, Exp)
    /// (rec (f x) e)
    case Rec(String, String, Exp)
    /// (let (x e) e')
    case Let(String, Exp, Exp)
    /// (new e)
    case New(Exp)
    /// (get e)
    case Get(Exp)
    /// (set e e')
    case Set(Exp, Exp)

    case BoolCst(Bool)
    case If(Exp, Exp, Exp)
    case IntCst(Int32)
    case Add(Exp, Exp)
    case Sub(Exp, Exp)
    case Eq(Exp, Exp)
}

instance ToString[Exp] {
    pub def toString(e: Exp): String = Exp.toString(e)
}

mod Exp {

    pub def toString(exp: Exp): String = match exp {
        case Var(x) => "${x}"
        case App(e, e1) => "(${e} ${e1})"
        case Abs(x, e) => "(lambda (${x}) ${e})"
        case Rec(f, x, e) => "(rec (${f} ${x}) ${e})"
        case Let(x, e, e1) => "(let (${x} ${e}) ${e1})"
        case New(e) => "(new ${e})"
        case Get(e) => "(get ${e})"
        case Set(e, e1) => "(set ${e} ${e1})"

        case BoolCst(b) => "${b}"
        case If(e, e1, e2) => "(if ${e} ${e1} ${e2})"
        case IntCst(i) => "${i}"
        case Add(e, e1) => "(add ${e} ${e1})"
        case Sub(e, e1) => "(sub ${e} ${e1})"
        case Eq(e, e1) => "(eq ${e} ${e1})"
    }

}

}
