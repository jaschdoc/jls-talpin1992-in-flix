mod Talpin.EffectSolver {
    use Talpin.Data.Constraint
    use Talpin.Data.Type
    use Talpin.Data.Effect
    use Talpin.Data.Subst

    use Eq.eq

    pub def solv(k: Constraint): Subst =
        k |> collapse |> min

    pub def solvFull(p: (Subst, Type, Effect, Constraint)): (Subst, Type, Effect) = {
        let (s, t, ef, k) = p;
        let s1 = solv(k);
        (Subst.compose(inner = s, s1), Subst.appType(s1)(t), Subst.appEffect(s1)(ef))
    }

    def collapse(k: Constraint): Constraint = {
        k |> coerce |> Set.toList |> List.groupBy(x -> y -> fst(x) == fst(y)) |> List.map(
            group ->
                match group {
                    case Nil => ?unreachable
                    case (evar, _) :: xs =>
                        (evar, group |> List.map(snd) |> List.foldLeft(Effect.Union, Effect.Empty))
                }
        ) |> List.toSet |> Constraint.Constraint
    }

    def min(k: Constraint): Subst = {
        match Set.minimum(coerce(k)) {
            case None => Subst.id()
            case Some((z, ef)) =>
                let k1 = Set.remove((z, ef), coerce(k)) |> Constraint.Constraint;
                let my = min(k1);
                Subst.singleEVar(z, Subst.appEffect(my)(ef) |> Effect.remove(toRemove = Effect.Var(z))) |> Subst.compose(inner = my)
        }
    }

}
