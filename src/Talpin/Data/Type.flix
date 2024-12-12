mod Talpin.Data {

pub enum Type {
    case Unitt
    case Var(String)
    case Ref(Reg, Type)
    case Arrow(Type, Effect, Type)

    case Bool
    case Int
}

instance ToString[Type] {
    pub def toString(t: Type): String = Type.toString(t)
}

mod Type {
    use Talpin.Data.Effect
    use Talpin.Data.FreeVarSet
    use Talpin.Data.Reg

    pub def toString(tpe: Type): String = match tpe {
        case Unitt => "unit"
        case Var(x) => "${x}"
        case Ref(p, t) => "ref_${p}(${toString(t)})"
        case Arrow(Arrow(it, ief, it1), Effect.Empty, t1) => "(${toString(Arrow(it, ief, it1))}) -> ${toString(t1)}"
        case Arrow(t, Effect.Empty, t1) => "${toString(t)} -> ${toString(t1)}"
        case Arrow(Arrow(it, ief, it1), ef, t1) => "(${toString(Arrow(it, ief, it1))}) ->[${ef}] ${toString(t1)}"
        case Arrow(t, ef, t1) => "${toString(t)} ->[${ef}] ${toString(t1)}"

        case Bool => "bool"
        case Int => "int"
    }

    pub def fv(tpe: Type): FreeVarSet = match tpe {
        case Unitt => FreeVarSet.empty()
        case Var(x) => FreeVarSet.singleTVar(x)
        case Ref(p, t) => Reg.fv(p) + fv(t)
        case Arrow(t, ef, t1) => fv(t) + Effect.fv(ef) + fv(t1)

        case Bool => FreeVarSet.empty()
        case Int => FreeVarSet.empty()
    }

    pub def simplify(tpe: Type): Type = match tpe {
        case Unitt => Unitt
        case Var(_) => tpe
        case Ref(p, t) => Ref(p, simplify(t))
        case Arrow(t, ef, t1) => Arrow(simplify(t), Effect.simplify(ef), simplify(t1))

        case Bool => Bool
        case Int => Int
    }

}

}
