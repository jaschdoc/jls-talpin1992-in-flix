mod Talpin.EffectSolver {
    use Talpin.Data.Constraint
    use Talpin.Data.Subst

    pub def solv(k: Constraint): Subst = {
        let lhss = k |> coerce |> Set.map(match (z, _) -> z);
        let unique = Set.size(lhss) == Set.size(coerce(k));
        if (unique) min(k) else ?notUnique
    }

    def min(k: Constraint): Subst = {
        match Set.minimum(coerce(k)) {
            case None => Subst.id()
            case Some((z, ef)) =>
                let k1 = Set.remove((z, ef), coerce(k)) |> Constraint.Constraint;
                let my = min(k1);
                Subst.compose(inner = my, Subst.singleEVar(z, Subst.appEffect(my)(ef)))
        }
    }

}
