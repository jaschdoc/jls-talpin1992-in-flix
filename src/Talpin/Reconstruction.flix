mod Talpin.Reconstruction {
    use Talpin.Data.Constraint
    use Talpin.Data.Constraint.constraint
    use Talpin.Data.Effect
    use Talpin.Data.Exp
    use Talpin.Data.Exp.expansive
    use Talpin.Data.FreeVarSet
    use Talpin.Data.Reg
    use Talpin.Data.Subst
    use Talpin.Data.SubstApp.app
    use Talpin.Data.SubstBind.bindAll
    use Talpin.Data.Subst.compose
    use Talpin.Data.TyEnv
    use Talpin.Data.Type
    use Talpin.Data.TyScheme
    use Talpin.Data.TVar
    use Talpin.Data.TVar.TVar
    use Talpin.Data.RVar
    use Talpin.Data.RVar.RVar
    use Talpin.Data.EVar
    use Talpin.Data.EVar.EVar
    use Talpin.Reconstruction
    use Talpin.EffectSolver
    use Talpin.Data.Subst
    use Talpin.FreshName
    use Util.Util.init
    use Util.Util

    use Functor.map
    use Foldable.toList
    use ToString.toString

    pub type alias Info = {
        exp = Exp,
        tpe = Type,
        effect = Effect,
        binders = Vector[(String, TyScheme)],
        preEffTpe = Type,
        preEffEffect = Effect,
        constraints = Constraint,
        preEffSubst = Subst,
        preEffBinders = Vector[(String, TyScheme)],
        solvedSubst = Subst
    }

    mod Info {
        use Functor.map
        use Talpin.Data.Constraint
        use Talpin.Data.Subst

        pub def toStrings(i: Info): List[String] = region local {
            use ToString.toString;
            let {
                exp,
                tpe,
                effect,
                binders,
                preEffTpe,
                preEffEffect,
                constraints,
                preEffSubst,
                preEffBinders,
                solvedSubst
            } = i;
            let l = MutList.empty(local);
            def add(title, content, start, end) = l |> MutList.push(Some((title, content, start, end)));
            def addOne(title, content) = l |> MutList.push(Some((title, List#{content}, "", "")));
            def addEmpty() = l |> MutList.push(None);
            addOne("Exp", exp |> toString);
            addOne("Type", tpe |> toString);
            addOne("Effect", effect |> toString);
            add("Binders", binders |> map(match (x, sigma) -> "${x}: ${sigma}") |> Vector.toList, "{", "}");
            addEmpty();
            addOne("(PreEff) Tpe", preEffTpe |> toString);
            addOne("(PreEff) Effect", preEffEffect |> toString);
            add("(PreEff) Constraints", constraints |> Constraint.toStrings, "{", "}");
            add("(PreEff) Subst", preEffSubst |> Subst.toStrings, "[", "]");
            add("(PreEff) Binders", preEffBinders |> map(match (x, sigma) -> "${x}: ${sigma}") |> Vector.toList, "{", "}");
            addEmpty();
            add("(Solved) Subst", solvedSubst |> Subst.toStrings, "[", "]");

            let maxFirst = l |> MutList.map(local, Option.map(match (x, _, _, _) -> String.length(x)) >> Option.getWithDefault(0)) |> MutList.maximum |> Option.getWithDefault(0);
            let indent = String.repeat(maxFirst+6, " ");
            l |> MutList.toList |> List.flatMap(line -> match line {
                case None => List#{""}
                case Some((title, content, start, end)) =>
                    let paddedTitle = String.padRight(maxFirst+2, '.', title);
                    match content {
                        case Nil => List#{"${paddedTitle}: ${start}${end}"}
                        case one :: Nil => List#{"${paddedTitle}: ${start}${one}${end}"}
                        case many => "${paddedTitle}: ${start}" :: map(String.concat(indent), many) ::: List#{String.concat(String.padLeft(maxFirst+2, ' ', ""), "  ${end}")}
                    }
            })
        }
    }

    pub def reconstructInfo(exp: Exp): Info \ {Abort, KeyNotFound} = {
        let ((s, t, ef, k), binders) = run Reconstruction.reconstruct(exp) with FreshName.runWithState with Reconstruction.BinderTypes.runWithList;
        let s1 = EffectSolver.solv(k);
        let finalSubst = s1 |> compose(inner = s);
        {
            exp = exp,
            tpe = app(s1)(t) |> Type.simplify,
            effect = app(s1)(ef) |> Effect.simplify,
            binders = binders |> Vector.map(match (x, xt) -> (x, app(finalSubst)(xt) |> TyScheme.simplify)),

            preEffTpe = t,
            preEffEffect = ef,
            constraints = k,
            preEffSubst = s,
            preEffBinders = binders |> Vector.map(match (x, xt) -> (x, app(s)(xt) |> TyScheme.simplify)),

            solvedSubst = finalSubst |> Subst.simplify
        }
    }

    pub eff BinderTypes {
        def typed(x: String, t: TyScheme): Unit
    }

    mod BinderTypes {
        use Talpin.Data.Type
        use Talpin.Data.TyScheme

        pub def handleWithNothing(f: a -> b \ ef): a -> b \ ef - BinderTypes =
            x -> runWithNothing(() -> f(x))

        pub def runWithNothing(f: Unit -> b \ ef): b \ ef - BinderTypes =
            run f() with handler BinderTypes { def typed(_x, _t, k) = k() }

        pub def handleWithList(f: a -> b \ ef): a -> (b, Vector[(String, TyScheme)]) \ ef - BinderTypes =
            x -> runWithList(() -> f(x))

        pub def runWithList(f: Unit -> b \ ef): (b, Vector[(String, TyScheme)]) \ ef - BinderTypes =
            region local {
                let types = MutList.empty(local);
                run {
                    (f(), MutList.toVector(types))
                } with handler BinderTypes {
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
        let {tVars = tVars0, rVars = rVars0, eVars = eVars0, tpe = t, k} = coerce(tEnv[x]);
        let tVars = tVars0 |> toList |> map(tx -> (tx, Type.Var(freshTVar())));
        let rVars = rVars0 |> toList |> map(tx -> (tx, Reg.RegVar(freshRVar())));
        let eVars = eVars0 |> toList |> map(tx -> (tx, Effect.Var(freshEVar())));
        let s = Subst.empty() |> bindAll(tVars) |> bindAll(rVars) |> bindAll(eVars);
        (Subst.id(), app(s)(t), Effect.Empty, app(s)(k))
    }

    def reconLet(tEnv: TyEnv, x: String, e: Exp, e1: Exp): (Subst, Type, Effect, Constraint) \ {Abort, BinderTypes, FreshName, KeyNotFound} = {
        let (s, t, ef, k) = recon(tEnv, e);
        // don't need to generalize for _xyz let-binders
        if (not expansive(e) and not String.startsWith(prefix = "_", x)) {
            let vs = Type.fv(t) + Constraint.fv(k) - TyEnv.fv(tEnv);
            let scheme = TyScheme.genType(t, k, vs);
            BinderTypes.typed(x, scheme);
            let tEnv1 = app(s)(tEnv |> TyEnv.unbind(x)) |> TyEnv.bind(x, scheme);
            let (s1, t1, ef1, k1) = recon(tEnv1, e1);
            (s1 |> compose(inner = s), t1, ef1, k1)
        } else {
            BinderTypes.typed(x, TyScheme.fromType(t));
            let tEnv1 = app(s)(tEnv |> TyEnv.unbind(x)) |> TyEnv.bindType(x, t);
            let (s1, t1, ef1, k1) = recon(tEnv1, e1);
            (s1 |> compose(inner = s), t1, app(s1)(ef) + ef1, app(s1)(k) + k1)
        }
    }

    def reconAbs(tEnv: TyEnv, x: String, e: Exp): (Subst, Type, Effect, Constraint) \ {Abort, BinderTypes, FreshName, KeyNotFound} = {
        let a = Type.Var(freshTVar());
        let (s, t, ef, k) = recon(tEnv |> TyEnv.unbind(x) |> TyEnv.bindType(x, a), e);
        let zName = freshEVar();
        let z = Effect.Var(zName);
        BinderTypes.typed(x, TyScheme.fromType(a));
        (s, Type.Arrow(app(s)(a), z, t), Effect.Empty, k + constraint(upperBound = zName, ef))
    }

    def reconRec(tEnv: TyEnv, f: String, x: String, e: Exp): (Subst, Type, Effect, Constraint) \ {Abort, BinderTypes, FreshName, KeyNotFound} = {
        let a = Type.Var(freshTVar());
        let a1 = Type.Var(freshTVar());
        let zName = freshEVar();
        let z = Effect.Var(zName);
        // PAPER ERROR: x here is written as n twice
        let tEnv1 = tEnv |> TyEnv.unbind(f) |> TyEnv.unbind(x) |> TyEnv.bindType(f, Type.Arrow(a, z, a1)) |> TyEnv.bindType(x, a);
        let (s, t, ef, k) = recon(tEnv1, e);
        let s1 = unify(app(s)(a1), t);
        let comp = s1 |> compose(inner = s);
        let zSubst = match app(s, zName) {
            case Effect.Var(zName1) => zName1
            case other => bug!(other |> toString)
        };
        // PAPER ERROR: effect should be the variable, not the effect
        (comp, app(comp)(Type.Arrow(a, z, a1)), Effect.Empty, app(s1)(k + constraint(upperBound = zSubst, ef)))
    }


    def reconApp(tEnv: TyEnv, e: Exp, e1: Exp): (Subst, Type, Effect, Constraint) \ {Abort, BinderTypes, FreshName, KeyNotFound} = {
        let (s, t,  ef, k) = recon(tEnv, e);
        let (s1, t1, ef1, k1) = recon(tEnv, e1);
        let a = Type.Var(freshTVar());
        let z = Effect.Var(freshEVar());
        let s11 = unify(app(s1)(t), Type.Arrow(t1, z, a));
        let ef11 = app(s11)(app(s1)(ef) + ef1 + z);
        (s11 |> compose(inner = s1 |> compose(inner = s)), app(s11)(a), ef11, app(s11)(app(s1)(k) + k1))
    }

    def reconNew(tEnv: TyEnv, e: Exp): (Subst, Type, Effect, Constraint) \ {Abort, BinderTypes, FreshName, KeyNotFound} = {
        let r = Reg.RegVar(freshRVar());
        let (s, t, ef, k) = recon(tEnv, e);
        (s, Type.Ref(r, t), ef + Effect.Init(r), k)
    }

    def reconGet(tEnv: TyEnv, e: Exp): (Subst, Type, Effect, Constraint) \ {Abort, BinderTypes, FreshName, KeyNotFound} = {
        let (s, t, ef, k) = recon(tEnv, e);
        let a = Type.Var(freshTVar());
        let r = Reg.RegVar(freshRVar());
        let s1 = unify(Type.Ref(r, a), t);
        (s1 |> compose(inner = s), app(s1)(a), ef + Effect.Read(app(s1)(r)), app(s1)(k))
    }

    def reconSet(tEnv: TyEnv, e: Exp, e1: Exp): (Subst, Type, Effect, Constraint) \ {Abort, BinderTypes, FreshName, KeyNotFound} = {
        let (s, t, ef, k) = recon(tEnv, e);
        let (s1, t1, ef1, k1) = recon(tEnv, e1);
        let r = Reg.RegVar(freshRVar());
        let s11 = unify(Type.Ref(r, t1), app(s1)(t));
        let ef11 = app(s11)(app(s1)(ef) + ef1 + Effect.Write(r));
        (s11 |> compose(inner = s1 |> compose(inner = s)), Type.Unitt, ef11, app(s11)(app(s1)(k) + k1))
    }

    def reconIf(tEnv: TyEnv, e: Exp, e1: Exp, e11: Exp): (Subst, Type, Effect, Constraint) \ {Abort, BinderTypes, FreshName, KeyNotFound} = {
        let (s, t, ef, k) = recon(tEnv, e);
        let s1 = unify(t, Type.Bool);
        let (s11, t1, ef1, k1) = recon(tEnv, e1);
        let (s111, t11, ef11, k11) = recon(tEnv, e11);
        let s1111 = unify(app(s111)(t1), t11);
        let comp1And11 = s11 |> compose(inner = s1);
        let comp111And1111 = s1111 |> compose(inner = s111);
        let comp1Through1111 = comp111And1111 |> compose(inner = comp1And11);
        let ef111 = app(comp1Through1111)(ef) + app(comp111And1111)(ef1) + app(s1111)(ef11);
        (comp1Through1111 |> compose(inner = s), app(s1111)(t11), ef111, app(comp1Through1111)(k) + app(comp111And1111)(k1) + app(s1111)(k11))
    }

    def reconIntOp(tEnv: TyEnv, e: Exp, e1: Exp): (Subst, Type, Effect, Constraint) \ {Abort, BinderTypes, FreshName, KeyNotFound} = {
        let (s, t, ef, k) = recon(tEnv, e);
        let (s1, t1, ef1, k1) = recon(tEnv, e1);
        let s11 = unify(app(s1)(t), Type.Int);
        let s111 = unify(app(s11)(t1), Type.Int);
        let comp11And111 = s111 |> compose(inner = s11);
        let comp1And11And111 = comp11And111 |> compose(inner = s1);
        let ef11 = app(comp1And11And111)(ef) + app(comp11And111)(ef1);
        (comp1And11And111 |> compose(inner = s), Type.Int, ef11, app(comp1And11And111)(k) + app(comp11And111)(k1))
    }

    def reconEq(tEnv: TyEnv, e: Exp, e1: Exp): (Subst, Type, Effect, Constraint) \ {Abort, BinderTypes, FreshName, KeyNotFound} = {
        let (s, t, ef, k) = recon(tEnv, e);
        let (s1, t1, ef1, k1) = recon(tEnv, e1);
        let s11 = unify(app(s1)(t), Type.Int);
        let s111 = unify(app(s11)(t1), Type.Int);
        let comp = s111 |> compose(inner = s11);
        let comp1 = comp |> compose(inner = s1);
        let ef11 = app(comp1)(ef) + app(comp)(ef1);
        (comp1 |> compose(inner = s), Type.Bool, ef11, app(comp1)(k) + app(comp)(k1))
    }

    def reconDo(_tEnv: TyEnv, ef: String): (Subst, Type, Effect, Constraint) =
        (Subst.id(), Type.Unitt, Effect.Eff(ef), Constraint.empty())

    def reconRun(tEnv: TyEnv, e: Exp, effect: String): (Subst, Type, Effect, Constraint) \ {Abort, BinderTypes, FreshName, KeyNotFound} = {
        let (s, t, _ef, k) = recon(tEnv, e);
        let qName = freshEVar();
        let _q = Effect.Var(qName);
        let z = Effect.Var(freshEVar());
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
            let s1 = unify(app(s)(ti), app(s)(ti1));
            let comp = s1 |> compose(inner = s);
            let s11 = unify(app(comp)(tf), app(comp)(tf1));
            s11 |> compose(inner = comp)
        case (Type.Ref(Reg.RegVar(r), t), Type.Ref(Reg.RegVar(r1), t1)) =>
            let s = Subst.singleRVar(r, Reg.RegVar(r1));
            let s1 = unify(app(s)(t), app(s)(t1));
            s1 |> compose(inner = s)

        case (Type.Bool, Type.Bool) => Subst.id()
        case (Type.Int, Type.Int) => Subst.id()

        case (t, t1) => Abort.abort("${t} !~ ${t1}")
    }

    def freshTVar(): TVar \ FreshName = TVar("t${FreshName.get()}")

    def freshRVar(): RVar \ FreshName = RVar("r${FreshName.get()}")

    def freshEVar(): EVar \ FreshName = EVar("e${FreshName.get()}")

}
