mod Talpin.Reconstruction {
    use Talpin.Data.Constraint
    use Talpin.Data.Constraint.constraint
    use Talpin.Data.Effect
    use Talpin.Data.Exp
    use Talpin.Data.Exp.expansive
    use Talpin.Data.FreeVarSet
    use Talpin.Data.Reg
    use Talpin.Data.Subst
    use Talpin.Data.Subst.{appEffect, appEVarOpt, appConstraint, appReg, appType, appTyEnv, bindTVarAll, bindRVarAll, bindEVarAll, compose}
    use Talpin.Data.TyEnv
    use Talpin.Data.Type
    use Talpin.FreshName
    use Util.Util.init

    use Functor.map
    use Foldable.toList
    use ToString.toString

    pub eff BinderTypes {
        def typed(x: String, t: Type): Unit
    }

    mod BinderTypes {
        use Talpin.Data.Type

        pub def handleWithNothing(f: a -> b \ ef): a -> b \ ef - BinderTypes =
            x -> run f(x) with BinderTypes { def typed(_x, _t, k) = k() }

        pub def handleWithList(f: a -> b \ ef): a -> (b, Vector[(String, Type)]) \ ef - BinderTypes =
            x -> region local {
                let types = MutList.empty(local);
                run {
                    (f(x), MutList.toVector(types))
                } with BinderTypes {
                    def typed(x, t, k) = {
                        MutList.push((x, t), types);
                        k()
                    }
                }
            }
    }

    pub def reconstruct(exp: Exp): (Subst, Type, Effect, Constraint) \ {Abort, BinderTypes, FreshName, KeyNotFound} = {
        let (s, t, ef, k) = recon(TyEnv.empty(), exp);
        (s, Type.simplify(t), Effect.simplify(ef), Constraint.simplify(k))
    }

    def recon(tEnv: TyEnv, exp: Exp): (Subst, Type, Effect, Constraint) \ {Abort, BinderTypes, FreshName, KeyNotFound} = match exp {
        case Exp.Var(x) => reconVar(tEnv, x)
        case Exp.Let(x, e, e1) => reconLet(tEnv, x, e, e1)
        case Exp.Abs(x, e) => reconAbs(tEnv, x, e)
        case Exp.Rec(f, x, e) => reconRec(tEnv, f, x, e)
        case Exp.App(e, e1) => reconApp(tEnv, e, e1)
        case Exp.New(e) => reconNew(tEnv, e)
        case Exp.Get(e) => reconGet(tEnv, e)
        case Exp.Set(e, e1) => reconSet(tEnv, e, e1)

        case Exp.BoolCst(_) => (Subst.id(), Type.Bool, Effect.Empty, Constraint.empty())
        case Exp.If(e, e1, e11) => reconIf(tEnv, e, e1, e11)
        case Exp.IntCst(_) => (Subst.id(), Type.Int, Effect.Empty, Constraint.empty())
        case Exp.Add(e, e1) => reconIntOp(tEnv, e, e1)
        case Exp.Sub(e, e1) => reconIntOp(tEnv, e, e1)
        case Exp.Eq(e, e1) => reconEq(tEnv, e, e1)

        case Exp.Do(ef) => reconDo(tEnv, ef)
        case Exp.Run(e, ef) => reconRun(tEnv, e, ef)
    }

    def reconVar(tEnv: TyEnv, x: String): (Subst, Type, Effect, Constraint) \ {FreshName, KeyNotFound} = {
        let {tVars = tVars0, rVars = rVars0, eVars = eVars0, tpe = t, k} = tEnv[x];
        let tVars = tVars0 |> toList |> map(tx -> (tx, freshTVar()));
        let rVars = rVars0 |> toList |> map(tx -> (tx, freshRVar()));
        let eVars = eVars0 |> toList |> map(tx -> (tx, freshEVar()));
        let s = Subst.empty() |> bindTVarAll(tVars) |> bindRVarAll(rVars) |> bindEVarAll(eVars);
        (Subst.id(), appType(s)(t), Effect.Empty, appConstraint(s)(k))
    }

    def reconLet(tEnv: TyEnv, x: String, e: Exp, e1: Exp): (Subst, Type, Effect, Constraint) \ {Abort, BinderTypes, FreshName, KeyNotFound} = {
        let (s, t, ef, k) = recon(tEnv, e);
        if (not expansive(e) and false) {
            // TODO: add gen
            let vs = Type.fv(t) + Constraint.fv(k) - TyEnv.fv(tEnv);
            let tEnv1 = if (x == "_") appTyEnv(s)(tEnv) else appTyEnv(s)(tEnv |> TyEnv.unbind(x)) |> TyEnv.bind(x, vs, t, k);
            let (s1, t1, ef1, k1) = recon(tEnv1, e1);
            (compose(inner = s, s1), t1, ef1, k1)
        } else {
            BinderTypes.typed(x, t);
            let tEnv1 = if (x == "_") appTyEnv(s)(tEnv) else appTyEnv(s)(tEnv |> TyEnv.unbind(x)) |> TyEnv.bindType(x, t);
            let (s1, t1, ef1, k1) = recon(tEnv1, e1);
            (compose(inner = s, s1), t1, appEffect(s1)(ef) + ef1, appConstraint(s1)(k) + k1)
        }
    }

    def reconAbs(tEnv: TyEnv, x: String, e: Exp): (Subst, Type, Effect, Constraint) \ {Abort, BinderTypes, FreshName, KeyNotFound} = {
        let a = freshTVar();
        let (s, t, ef, k) = recon(tEnv |> TyEnv.unbind(x) |> TyEnv.bindType(x, a), e);
        let zName = "e${FreshName.get()}";
        let z = Effect.Var(zName);
        (s, Type.Arrow(appType(s)(a), z, t), Effect.Empty, k + constraint(upperBound = zName, ef))
    }

    def reconRec(tEnv: TyEnv, f: String, x: String, e: Exp): (Subst, Type, Effect, Constraint) \ {Abort, BinderTypes, FreshName, KeyNotFound} = {
        let a = freshTVar();
        let a1 = freshTVar();
        let zName = "e${FreshName.get()}";
        let z = Effect.Var(zName);
        // PAPER ERROR: x here is written as n twice
        let tEnv1 = tEnv |> TyEnv.unbind(f) |> TyEnv.unbind(x) |> TyEnv.bindType(f, Type.Arrow(a, z, a1)) |> TyEnv.bindType(x, a);
        let (s, t, ef, k) = recon(tEnv1, e);
        let s1 = unify(appType(s)(a1), t);
        let comp = compose(inner = s, s1);
        let zSubst = match appEVarOpt(s, zName) {
            case None => zName
            case Some(Effect.Var(zName1)) => zName1
            case Some(other) => bug!(other |> toString)
        };
        // PAPER ERROR: effect should be the variable, not the effect
        (comp, appType(comp)(Type.Arrow(a, z, a1)), Effect.Empty, appConstraint(s1)(k + constraint(upperBound = zSubst, ef)))
    }


    def reconApp(tEnv: TyEnv, e: Exp, e1: Exp): (Subst, Type, Effect, Constraint) \ {Abort, BinderTypes, FreshName, KeyNotFound} = {
        let (s, t,  ef, k) = recon(tEnv, e);
        let (s1, t1, ef1, k1) = recon(tEnv, e1);
        let a = freshTVar();
        let z = freshEVar();
        let s11 = unify(appType(s1)(t), Type.Arrow(t1, z, a));
        let ef11 = appEffect(s11)(appEffect(s1)(ef) + ef1 + z);
        (compose(inner = compose(inner = s, s1), s11), appType(s11)(a), ef11, appConstraint(s11)(appConstraint(s1)(k) + k1))
    }

    def reconNew(tEnv: TyEnv, e: Exp): (Subst, Type, Effect, Constraint) \ {Abort, BinderTypes, FreshName, KeyNotFound} = {
        let r = freshRVar();
        let (s, t, ef, k) = recon(tEnv, e);
        (s, Type.Ref(r, t), ef + Effect.Init(r), k)
    }

    def reconGet(tEnv: TyEnv, e: Exp): (Subst, Type, Effect, Constraint) \ {Abort, BinderTypes, FreshName, KeyNotFound} = {
        let (s, t, ef, k) = recon(tEnv, e);
        let a = freshTVar();
        let r = freshRVar();
        let s1 = unify(Type.Ref(r, a), t);
        (compose(inner = s, s1), appType(s1)(a), ef + Effect.Read(appReg(s1)(r)), appConstraint(s1)(k))
    }

    def reconSet(tEnv: TyEnv, e: Exp, e1: Exp): (Subst, Type, Effect, Constraint) \ {Abort, BinderTypes, FreshName, KeyNotFound} = {
        let (s, t, ef, k) = recon(tEnv, e);
        let (s1, t1, ef1, k1) = recon(tEnv, e1);
        let r = freshRVar();
        let s11 = unify(Type.Ref(r, t1), appType(s1)(t));
        let ef11 = appEffect(s11)(appEffect(s1)(ef) + ef1 + Effect.Write(r));
        (compose(inner = compose(inner = s, s1), s11), Type.Unitt, ef11, appConstraint(s11)(appConstraint(s1)(k) + k1))
    }

    def reconIf(tEnv: TyEnv, e: Exp, e1: Exp, e11: Exp): (Subst, Type, Effect, Constraint) \ {Abort, BinderTypes, FreshName, KeyNotFound} = {
        let (s, t, ef, k) = recon(tEnv, e);
        let s1 = unify(t, Type.Bool);
        let (s11, t1, ef1, k1) = recon(tEnv, e1);
        let (s111, t11, ef11, k11) = recon(tEnv, e11);
        let s1111 = unify(appType(s111)(t1), t11);
        let comp1And11 = compose(inner = s1, s11);
        let comp111And1111 = compose(inner = s111, s1111);
        let comp1Through1111 = compose(inner = comp1And11, comp111And1111);
        let ef111 = appEffect(comp1Through1111)(ef) + appEffect(comp111And1111)(ef1) + appEffect(s1111)(ef11);
        (compose(inner = s, comp1Through1111), appType(s1111)(t11), ef111, appConstraint(comp1Through1111)(k) + appConstraint(comp111And1111)(k1) + appConstraint(s1111)(k11))
    }

    def reconIntOp(tEnv: TyEnv, e: Exp, e1: Exp): (Subst, Type, Effect, Constraint) \ {Abort, BinderTypes, FreshName, KeyNotFound} = {
        let (s, t, ef, k) = recon(tEnv, e);
        let (s1, t1, ef1, k1) = recon(tEnv, e1);
        let s11 = unify(appType(s1)(t), Type.Int);
        let s111 = unify(appType(s11)(t1), Type.Int);
        let comp11And111 = compose(inner = s11, s111);
        let comp1And11And111 = compose(inner = s1, comp11And111);
        let ef11 = appEffect(comp1And11And111)(ef) + appEffect(comp11And111)(ef1);
        (compose(inner = s, comp1And11And111), Type.Int, ef11, appConstraint(comp1And11And111)(k) + appConstraint(comp11And111)(k1))
    }

    def reconEq(tEnv: TyEnv, e: Exp, e1: Exp): (Subst, Type, Effect, Constraint) \ {Abort, BinderTypes, FreshName, KeyNotFound} = {
        let (s, t, ef, k) = recon(tEnv, e);
        let (s1, t1, ef1, k1) = recon(tEnv, e1);
        let s11 = unify(appType(s1)(t), Type.Int);
        let s111 = unify(appType(s11)(t1), Type.Int);
        let comp = compose(inner = s11, s111);
        let comp1 = compose(inner = s1, comp);
        let ef11 = appEffect(comp1)(ef) + appEffect(comp)(ef1);
        (compose(inner = s, comp1), Type.Bool, ef11, appConstraint(comp1)(k) + appConstraint(comp)(k1))
    }

    def reconDo(_tEnv: TyEnv, ef: String): (Subst, Type, Effect, Constraint) =
        (Subst.id(), Type.Unitt, Effect.Eff(ef), Constraint.empty())

    def reconRun(tEnv: TyEnv, e: Exp, effect: String): (Subst, Type, Effect, Constraint) \ {Abort, BinderTypes, FreshName, KeyNotFound} = {
        let (s, t, _ef, k) = recon(tEnv, e);
        let qName = "e${FreshName.get()}";
        let _q = Effect.Var(qName);
        let z = freshEVar();
        let _union = z + Effect.Eff(effect);
        (s, t, z, k + Abort.abort("run not implemented"))
    }

    def unify(tpe: Type, tpe1: Type): Subst \ Abort = match (tpe, tpe1) {
        case (Type.Unitt, Type.Unitt) => Subst.id()
        case (Type.Var(x), Type.Var(_)) => Subst.singleTVar(x, tpe1)
        case (Type.Var(x), t) if not FreeVarSet.containsTVar(x, Type.fv(t)) => Subst.singleTVar(x, t)
        case (t, Type.Var(x)) if not FreeVarSet.containsTVar(x, Type.fv(t)) => Subst.singleTVar(x, t)
        case (Type.Arrow(ti, Effect.Var(z), tf), Type.Arrow(ti1, Effect.Var(z1), tf1)) =>
            let s = Subst.singleEVar(z, Effect.Var(z1));
            let s1 = unify(appType(s)(ti), appType(s)(ti1));
            let comp = compose(inner = s, s1);
            let s11 = unify(appType(comp)(tf), appType(comp)(tf1));
            compose(inner = comp, s11)
        case (Type.Ref(Reg.RegVar(r), t), Type.Ref(Reg.RegVar(r1), t1)) =>
            let s = Subst.singleRVar(r, Reg.RegVar(r1));
            let s1 = unify(appType(s)(t), appType(s)(t1));
            compose(inner = s, s1)

        case (Type.Bool, Type.Bool) => Subst.id()
        case (Type.Int, Type.Int) => Subst.id()

        case (t, t1) => Abort.abort("${t} !~ ${t1}")
    }

    def freshTVar(): Type \ FreshName = Type.Var("t${FreshName.get()}")

    def freshRVar(): Reg \ FreshName = Reg.RegVar("r${FreshName.get()}")

    def freshEVar(): Effect \ FreshName = Effect.Var("e${FreshName.get()}")

}
