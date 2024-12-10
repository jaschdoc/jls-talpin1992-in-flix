mod Talpin.Data {

pub enum Value {
    case U
    case Ref(RefLoc)
    case Closure(Id, Exp, Unit -> Env)

    case BoolVal(Bool)
    case IntVal(Int32)
}

instance ToString[Value] {
    pub def toString(v: Value): String = Value.toString(v)
}


mod Value {
    use Talpin.Data.Id
    use Talpin.Data.Exp
    use Talpin.Data.Env
    use Talpin.Data.RefLoc

    pub def getRef(v: Value): RefLoc \ Abort = match v {
        case Ref(l) => l
        case other => Abort.abort("'${other}' is not a ref.")
    }

    pub def getClosure(v: Value): (Id, Exp, Env) \ Abort = match v {
        case Closure(x, e, env) => (x, e, env())
        case other => Abort.abort("'${other}' is not a closure.")
    }

    pub def getBool(v: Value): Bool \ Abort = match v {
        case BoolVal(b) => b
        case other => Abort.abort("'${other}' is not a bool.")
    }

    pub def getInt(v: Value): Int32 \ Abort = match v {
        case IntVal(i) => i
        case other => Abort.abort("'${other}' is not an int.")
    }

    pub def toString(v: Value): String = match v {
        case U => "u"
        case Ref(loc) => "loc_${loc}"
        case Closure(x, e, _env) => "<${x}, ${e}, _>"
        case BoolVal(b) => "${b}"
        case IntVal(i) => "${i}"
    }

}

}
