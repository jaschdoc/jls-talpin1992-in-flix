mod Talpin.Data {

pub enum Trace(Set[RuntimeEffect]) with ToString

instance Coerce[Trace] {
    type Out = Set[RuntimeEffect]

    pub def coerce(f: Trace): Set[RuntimeEffect] = Trace.coerce(f)
}

instance Add[Trace] {
    pub def add(f1: Trace, f2: Trace): Trace = Trace.union(f1, f2)
}

mod Trace {
    use Talpin.Data.RuntimeEffect

    pub def empty(): Trace = Set.empty() |> Trace

    pub def singleton(ef: RuntimeEffect): Trace =
        ef |> Set.singleton |> Trace

    pub def insert(ef: RuntimeEffect, f: Trace): Trace =
        f |> coerceMap(Set.insert(ef))

    pub def union(f1: Trace, f2: Trace): Trace =
        Set.union(f1 |> coerce, f2 |> coerce) |> Trace

    pub def coerce(f: Trace): Set[RuntimeEffect] =
        match f { case Trace(s) => s }

    pub def coerceMap(g: Set[RuntimeEffect] -> Set[RuntimeEffect] \ ef, f: Trace): Trace \ ef =
        f |> coerce |> g |> Trace

    pub def remove(ef: RuntimeEffect, f: Trace): Trace = {
        f |> coerceMap(Set.remove(ef))
    }

}

}
