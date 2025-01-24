mod Talpin.Data {

pub enum Constraint(Set[(EVar, Effect)]) with Coerce

instance ToString[Constraint] {
    pub def toString(k: Constraint): String = Constraint.toString(k)
}


instance Add[Constraint] {
    pub def add(k1: Constraint, k2: Constraint): Constraint = Constraint.combine(k1, k2)
}

mod Constraint {
    use Talpin.Data.Effect
    use Talpin.Data.FreeVarSet
    use Talpin.Data.EVar

    pub def empty(): Constraint =
        Constraint(Set.empty())

    pub def isEmpty(k: Constraint): Bool =
        k |> coerce |> Set.isEmpty

    pub def coerce(k: Constraint): Set[(EVar, Effect)] = {
        let Constraint(s) = k;
        s
    }

    pub def constraint(upperBound: {upperBound = EVar}, ef: Effect): Constraint =
        Constraint(Set.singleton((upperBound#upperBound, ef)))

    pub def combine(k1: Constraint, k2: Constraint): Constraint =
        Set.union(coerce(k1), coerce(k2)) |> Constraint

    pub def fv(k: Constraint): FreeVarSet = {
        k |> coerce |> Set.foldLeft(acc -> match (z, ef) -> FreeVarSet.singleEVar(z) + Effect.fv(ef) + acc, FreeVarSet.empty())
    }

    pub def simplify(k: Constraint): Constraint = {
        k |> coerce |> Set.map(match (x, ef) -> (x, Effect.simplify(ef))) |> Constraint
    }

    pub def toString(k: Constraint): String = {
        let inner = k |> coerce |> Set.toList |> List.map(match (x, y) -> "${x} ⊒ ${y}") |> List.join(", ");
        "{${inner}}"
    }

}

}
