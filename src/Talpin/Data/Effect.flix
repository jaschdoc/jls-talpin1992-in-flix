mod Talpin.Data {

pub enum Effect {
    case Empty
    case Init(Reg)
    case Read(Reg)
    case Write(Reg)
    case Union(Effect, Effect)
    case Var(String)
}

instance ToString[Effect] {
    pub def toString(ef: Effect): String = Effect.toString(ef)
}

mod Effect {

    pub def toString(ef: Effect): String = match ef {
        case Empty => "Ø"
        case Init(p) => "init(${p})"
        case Read(p) => "read(${p})"
        case Write(p) => "write(${p})"
        case Union(p, p1) => "${toString(p)} ∪ ${toString(p1)}"
        case Var(z) => "e_${z}"
    }

}

}
