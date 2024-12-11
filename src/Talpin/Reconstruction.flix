mod Talpin.Reconstruction {
    use Talpin.Data.TyEnv
    use Talpin.Data.Exp
    use Talpin.Data.Subst
    use Talpin.Data.Subst.{appEffect, appEVarOpt, appConstraint, appType, appTyEnv, bindTVarAll, bindRVarAll, bindEVarAll, compose}
    use Talpin.Data.Type
    use Talpin.Data.Effect
    use Talpin.Data.Constraint
    use Talpin.Data.Constraint.constraint
    use Talpin.Data.Reg
    use Util.Util.init
    use Talpin.Data.Exp.expansive

    use Functor.map
    use Foldable.toList
    use ToString.toString

    pub eff FreshName {
        def get(): String
    }

    pub def recon(tEnv: TyEnv, exp: Exp): (Subst, Type, Effect, Constraint) \ {FreshName, KeyNotFound} = match exp {
        case Exp.Var(x) =>
            let {tVars = tVars0, rVars = rVars0, eVars = eVars0, tpe = t, k} = tEnv[x];
            let tVars = tVars0 |> toList |> map(tx -> (tx, freshTVar()));
            let rVars = rVars0 |> toList |> map(tx -> (tx, freshRVar()));
            let eVars = eVars0 |> toList |> map(tx -> (tx, freshEVar()));
            let s = Subst.empty() |> bindTVarAll(tVars) |> bindRVarAll(rVars) |> bindEVarAll(eVars);
            (Subst.id(), appType(s)(t), Effect.Empty, appConstraint(s)(k))
        case Exp.Let(x, e, e1) =>
            let (s, t, ef, k) = recon(tEnv, e);
            if (not expansive(e) and false) {
                ??? // disable for now to simplify
            } else {
                let tEnv1 = appTyEnv(s)(tEnv |> TyEnv.unbind(x)) |> TyEnv.bindType(x, t);
                let (s1, t1, ef1, k1) = recon(tEnv1, e1);
                (compose(inner = s, s1), t1, appEffect(s1)(ef) + ef1, appConstraint(s1)(k) + k1)
            }
        case Exp.Abs(x, e) =>
            let a = freshTVar();
            let zName = FreshName.get();
            let z = Effect.Var(zName);
            let (s, t, ef, k) = recon(tEnv |> TyEnv.unbind(x) |> TyEnv.bindType(x, a), e);
            (s, Type.Arrow(appType(s)(a), z, t), Effect.Empty, k + constraint(upperBound = zName, ef))
        case Exp.Rec(f, x, e) =>
            let a = freshTVar();
            let a1 = freshTVar();
            let zName = FreshName.get();
            let z = Effect.Var(zName);
            // typo in paper, x here is written as n twice
            let tEnv1 = tEnv |> TyEnv.unbind(f) |> TyEnv.unbind(x) |> TyEnv.bindType(f, Type.Arrow(a, z, a1)) |> TyEnv.bindType(x, a);
            let (s, t, ef, k) = recon(tEnv1, e);
            let s1 = ?unify(appType(s)(a1), t);
            let comp = compose(inner = s, s1);
            let zSubst = match appEVarOpt(s, zName) {
                case None => zName
                case Some(Effect.Var(zName1)) => zName1
                case Some(other) => bug!(other |> toString)
            };
            (comp, appType(comp)(Type.Arrow(a, ef, a1)), Effect.Empty, appConstraint(s1)(k + constraint(upperBound = zSubst, ef)))
        case Exp.App(e, e1) =>
            let (s, t,  ef, k) = recon(tEnv, e);
            let (s1, t1, ef1, k1) = recon(tEnv, e1);
            let a = freshTVar();
            let z = freshEVar();
            let s11 = ?unify(appType(s1)(t), Type.Arrow(t1, z, a));
            let ef11 = appEffect(s11)(Effect.union(List#{appEffect(s1)(ef), ef1, z}));
            (compose(inner = compose(inner = s, s1), s11), appType(s11)(a), ef11, appConstraint(s11)(appConstraint(s1)(k) + k1))
        // case Exp.New(_elem) => ???
        // case Exp.Get(_elem) => ???
        // case Exp.Set(_elem0, _elem1) => ???
        // case Exp.BoolCst(_elem) => ???
        // case Exp.If(_elem0, _elem1, _elem2) => ???
        // case Exp.IntCst(_elem) => ???
        // case Exp.Add(_elem0, _elem1) => ???
        // case Exp.Sub(_elem0, _elem1) => ???
        // case Exp.Eq(_elem0, _elem1) => ???
    }

    def freshTVar(): Type \ FreshName = Type.Var(FreshName.get())
    def freshRVar(): Reg \ FreshName = Reg.RegVar(FreshName.get())
    def freshEVar(): Effect \ FreshName = Effect.Var(FreshName.get())

}
