mod Talpin.Data {

pub enum Constraint(Set[(String, Effect)])

instance Coerce[Constraint] {
    type Out = Set[(String, Effect)]

    pub def coerce(k: Constraint): Set[(String, Effect)] = Constraint.coerce(k)
}


instance Add[Constraint] {
    pub def add(k1: Constraint, k2: Constraint): Constraint = Constraint.combine(k1, k2)
}


mod Constraint {
    use Talpin.Data.Effect

    pub def empty(): Constraint =
        Constraint(Set.empty())

    pub def coerce(k: Constraint): Set[(String, Effect)] = {
        let Constraint(s) = k;
        s
    }

    pub def constraint(upperBound: {upperBound = String}, ef: Effect): Constraint =
        Constraint(Set.singleton((upperBound#upperBound, ef)))

    pub def combine(k1: Constraint, k2: Constraint): Constraint =
        Set.union(coerce(k1), coerce(k2)) |> Constraint

}

}
