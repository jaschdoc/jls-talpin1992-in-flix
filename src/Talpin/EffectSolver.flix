mod Talpin.EffectSolver {
    use Talpin.Data.Constraint
    use Talpin.Data.Type
    use Talpin.Data.Effect
    use Talpin.Data.Subst

    pub def solv(k: Constraint): Subst = {
        let lhss = k |> coerce |> Set.map(match (z, _) -> z);
        let unique = Set.size(lhss) == Set.size(coerce(k));
        if (unique) min(k) else {
            bug!(coerce(k) |> ToString.toString)
        }
    }

    pub def solvFull(p: (Subst, Type, Effect, Constraint)): (Subst, Type, Effect) = {
        let (s, t, ef, k) = p;
        let s1 = solv(k);
        (Subst.compose(inner = s, s1), Subst.appType(s1)(t), Subst.appEffect(s1)(ef))
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
