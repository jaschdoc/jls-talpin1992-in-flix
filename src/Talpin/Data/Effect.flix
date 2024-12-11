mod Talpin.Data {

pub enum Effect with Eq, Order {
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

instance Add[Effect] {
    pub def add(ef1: Effect, ef2: Effect): Effect = Effect.Union(ef1, ef2)
}

mod Effect {
    use Functor.map
    use Option.getWithDefault

    pub def toString(ef: Effect): String = match ef {
        case Empty => "Ø"
        case Init(p) => "init(${p})"
        case Read(p) => "read(${p})"
        case Write(p) => "write(${p})"
        case Union(p, p1) => "${toString(p)} ∪ ${toString(p1)}"
        case Var(z) => "e_${z}"
    }

    pub def union(l: List[Effect]): Effect =
        l |> List.toNel |> map(Nel.reduceLeft(Union)) |> getWithDefault(Empty)

}

}
