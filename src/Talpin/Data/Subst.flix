mod Talpin.Data {

pub enum Subst(Map[TVar, Type], Map[RVar, Reg], Map[EVar, Effect])

instance ToString[Subst] {
    pub def toString(subst: Subst): String = Subst.toString(subst)
}

trait SubstApp[t] {
    type Out: Type
    pub def app(s: Subst, x: t): SubstApp.Out[t]
}

instance SubstApp[TVar] {
    type Out = Type
    pub def app(s: Subst, x: TVar): Type = Subst.appTVar(s, x)
}

instance SubstApp[RVar] {
    type Out = Reg
    pub def app(s: Subst, x: RVar): Reg = Subst.appRVar(s, x)
}

instance SubstApp[EVar] {
    type Out = Effect
    pub def app(s: Subst, x: EVar): Effect = Subst.appEVar(s, x)
}

instance SubstApp[Reg] {
    type Out = Reg
    pub def app(s: Subst, x: Reg): Reg = Subst.appReg(s, x)
}

instance SubstApp[Type] {
    type Out = Type
    pub def app(s: Subst, x: Type): Type = Subst.appType(s, x)
}

instance SubstApp[Effect] {
    type Out = Effect
    pub def app(s: Subst, x: Effect): Effect = Subst.appEffect(s, x)
}

instance SubstApp[Constraint] {
    type Out = Constraint
    pub def app(s: Subst, x: Constraint): Constraint = Subst.appConstraint(s, x)
}

instance SubstApp[TyScheme] {
    type Out = TyScheme
    pub def app(s: Subst, x: TyScheme): TyScheme = Subst.appTyScheme(s, x)
}

instance SubstApp[Map[k, v]] with SubstApp[v] {
    type Out = Map[k, SubstApp.Out[v]]
    pub def app(s: Subst, x: Map[k, v]): Map[k, SubstApp.Out[v]] =
        Map.map(SubstApp.app(s), x)
}

trait SubstBind[t] {
    type Value: Type
    pub def bind(k: t, v: SubstBind.Value[t], s: Subst): Subst
}

instance SubstBind[TVar] {
    type Value = Type
    pub def bind(x: TVar, t: Type, s: Subst): Subst = Subst.bindTVar(x, t, s)
}

instance SubstBind[RVar] {
    type Value = Reg
    pub def bind(x: RVar, p: Reg, s: Subst): Subst = Subst.bindRVar(x, p, s)
}

instance SubstBind[EVar] {
    type Value = Effect
    pub def bind(x: EVar, effect: Effect, s: Subst): Subst = Subst.bindEVar(x, effect, s)
}

mod SubstBind {
    use Talpin.Data.Subst
    use Talpin.Data.SubstBind

    pub def bindAll(l: List[(k, v)], s: Subst): Subst with SubstBind[k] where SubstBind.Value[k] ~ v =
        (s, l) ||> List.foldLeft(acc -> match (x, t) -> SubstBind.bind(x, t, acc))
}

mod Subst {
    use Option.getWithDefault

    use Talpin.Data.Type
    use Talpin.Data.Reg
    use Talpin.Data.Effect
    use Talpin.Data.TyScheme
    use Talpin.Data.TyEnv
    use Talpin.Data.Constraint
    use Talpin.Data.TVar
    use Talpin.Data.RVar
    use Talpin.Data.EVar
    use Talpin.Data.SubstApp.app

    pub def empty(): Subst =
        Subst(Map.empty(), Map.empty(), Map.empty())

    pub def isEmpty(s: Subst): Bool = {
        let Subst(t, r, e) = s;
        Map.isEmpty(t) and Map.isEmpty(r) and Map.isEmpty(e)
    }

    pub def singleTVar(x: TVar, t: Type): Subst =
        empty() |> bindTVar(x, t)

    pub def singleRVar(x: RVar, t: Reg): Subst =
        empty() |> bindRVar(x, t)

    pub def singleEVar(x: EVar, t: Effect): Subst =
        empty() |> bindEVar(x, t)

    pub def id(): Subst =
        empty()

    pub def compose(inner: {inner = Subst}, outer: Subst): Subst = {
        if (isEmpty(inner#inner)) outer
        else if (isEmpty(outer)) inner#inner
        else {
            let Subst(tInner, rInner, eInner) = inner#inner;
            let tMap = tInner |> Map.map(appType(outer));
            let rMap = rInner |> Map.map(appReg(outer));
            let eMap = eInner |> Map.map(appEffect(outer));

            let Subst(tOuter, rOuter, eOuter) = outer;
            // Note that Map.union is left-biased.
            let tMap1 = Map.union(tMap, tOuter);
            let rMap1 = Map.union(rMap, rOuter);
            let eMap1 = Map.union(eMap, eOuter);
            Subst(tMap1, rMap1, eMap1)
        }
    }

    pub def bindTVar(x: TVar, t: Type, subst: Subst): Subst = {
        let Subst(tMap, rMap, eMap) = subst;
        Subst(tMap |> Map.insert(x, t), rMap, eMap)
    }

    pub def bindTVarAll(l: List[(TVar, Type)], subst: Subst): Subst =
        (subst, l) ||> List.foldLeft(acc -> match (x, t) -> bindTVar(x, t, acc))

    pub def unbindTVar(x: TVar, subst: Subst): Subst = {
        let Subst(tMap, rMap, eMap) = subst;
        Subst(tMap |> Map.remove(x), rMap, eMap)
    }

    pub def appTVar(subst: Subst, x: TVar): Type = {
        let Subst(tMap, _, _) = subst;
        Map.get(x, tMap) |> Option.getWithDefault(Type.Var(x))
    }

    pub def bindRVar(x: RVar, p: Reg, subst: Subst): Subst = {
        let Subst(tMap, rMap, eMap) = subst;
        Subst(tMap, rMap |> Map.insert(x, p), eMap)
    }

    pub def bindRVarAll(l: List[(RVar, Reg)], subst: Subst): Subst =
        (subst, l) ||> List.foldLeft(acc -> match (x, t) -> bindRVar(x, t, acc))

    pub def unbindRVar(x: RVar, subst: Subst): Subst = {
        let Subst(tMap, rMap, eMap) = subst;
        Subst(tMap, rMap |> Map.remove(x), eMap)
    }

    pub def appRVar(subst: Subst, x: RVar): Reg = {
        let Subst(_, rMap, _) = subst;
        Map.get(x, rMap) |> Option.getWithDefault(Reg.RegVar(x))
    }

    pub def bindEVar(x: EVar, ef: Effect, subst: Subst): Subst = {
        let Subst(tMap, rMap, eMap) = subst;
        Subst(tMap, rMap, eMap |> Map.insert(x, ef))
    }

    pub def bindEVarAll(l: List[(EVar, Effect)], subst: Subst): Subst =
        (subst, l) ||> List.foldLeft(acc -> match (x, t) -> bindEVar(x, t, acc))

    pub def unbindEVar(x: EVar, subst: Subst): Subst = {
        let Subst(tMap, rMap, eMap) = subst;
        Subst(tMap, rMap, eMap |> Map.remove(x))
    }

    pub def appEVar(subst: Subst, x: EVar): Effect = {
        let Subst(_, _, eMap) = subst;
        Map.get(x, eMap) |> Option.getWithDefault(Effect.Var(x))
    }

    pub def appEVarDirect(subst: Subst, x: EVar): Option[Effect] = {
        let Subst(_, _, eMap) = subst;
        Map.get(x, eMap)
    }

    pub def appType(subst: Subst, tpe: Type): Type = match tpe {
        case Type.Unitt => Type.Unitt
        case Type.Var(x) => app(subst, x)
        case Type.Ref(p, t) => Type.Ref(app(subst, p), app(subst, t))
        case Type.Arrow(t, ef, t1) => Type.Arrow(app(subst, t), app(subst, ef), app(subst, t1))

        case Type.Bool => Type.Bool
        case Type.Int => Type.Int
    }

    pub def appReg(subst: Subst, p: Reg): Reg = match p {
        case Reg.RegConst(_) => p
        case Reg.RegVar(x) => app(subst, x)
    }

    pub def appEffect(subst: Subst, effect: Effect): Effect = match effect {
        case Effect.Empty => Effect.Empty
        case Effect.Init(p) => Effect.Init(app(subst, p))
        case Effect.Read(p) => Effect.Read(app(subst, p))
        case Effect.Write(p) => Effect.Write(app(subst, p))
        case Effect.Union(ef, ef1) => app(subst, ef) + app(subst, ef1)
        case Effect.Var(x) => app(subst, x)

        case Effect.Eff(_) => effect
    }

    pub def appTyEnv(subst: Subst, env: TyEnv): TyEnv =
        TyEnv.map(appTyScheme(subst), env)

    pub def appTyScheme(subst: Subst, scheme: TyScheme): TyScheme = region local {
        let {tVars, rVars, eVars, tpe = t, k} = coerce(scheme);
        let acc = Ref.fresh(local, subst);
        tVars |> Set.forEach(x -> acc |> Ref.transform(unbindTVar(x)));
        rVars |> Set.forEach(x -> acc |> Ref.transform(unbindRVar(x)));
        eVars |> Set.forEach(x -> acc |> Ref.transform(unbindEVar(x)));
        let t1 = app(Ref.get(acc), t);
        let k1 = app(Ref.get(acc), k);
        TyScheme.TyScheme({tpe = t1, k = k1 | coerce(scheme)})
    }

    pub def appConstraint(subst: Subst, k: Constraint): Constraint = {
        k |> coerce |> Set.map(match (x, ef) -> {
            match app(subst, x) {
                case Effect.Var(x1) => (x1, appEffect(subst)(ef))
                case other => bug!(other |> ToString.toString) // paper says it should only be inferred variables, so unreachable
            }
        }) |> Constraint.Constraint
    }

    pub def toString(subst: Subst): String = region local {
        let Subst(ts, rs, efs) = subst;
        let tIt = ts |> Map.iterator(local) |> Iterator.map(match (x, t) -> "  ${x} -> ${t}");
        let rIt = rs |> Map.iterator(local) |> Iterator.map(match (x, r) -> "  ${x} -> ${r}");
        let eIt = efs |> Map.iterator(local) |> Iterator.map(match (x, ef) -> "  ${x} -> ${ef}");
        let inner = (tIt `Iterator.append` rIt `Iterator.append` eIt) |> Iterator.join(",\n");
        "[\n${inner}\n]"
    }

}

}
