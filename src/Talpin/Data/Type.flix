mod Talpin.Data {

pub enum Type {
    case Unitt
    case Var(String)
    case Ref(Reg, Type)
    case Arrow(Type, Effect, Type)
}

instance ToString[Type] {
    pub def toString(t: Type): String = Type.toString(t)
}

mod Type {
    use Talpin.Data.Effect

    pub def toString(tpe: Type): String = match tpe {
        case Unitt => "unit"
        case Var(s) => "t_${s}"
        case Ref(p, t) => "ref_${p}(${toString(t)})"
        case Arrow(t, Effect.Empty, t1) => "${toString(t)} -> ${toString(t1)}"
        case Arrow(t, ef, t1) => "${toString(t)} ->[${ef}] ${toString(t1)}"
    }

}

}
