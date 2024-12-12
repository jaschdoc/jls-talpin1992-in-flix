mod Talpin.Data {

pub enum Subst(Map[String, Type], Map[String, Reg], Map[String, Effect])

instance ToString[Subst] {
    pub def toString(subst: Subst): String = Subst.toString(subst)
}


mod Subst {
    use Option.getWithDefault

    use Talpin.Data.Type
    use Talpin.Data.Reg
    use Talpin.Data.Effect
    use Talpin.Data.TyScheme
    use Talpin.Data.TyEnv
    use Talpin.Data.Constraint

    pub def empty(): Subst =
        Subst(Map.empty(), Map.empty(), Map.empty())

    pub def isEmpty(s: Subst): Bool = {
        let Subst(t, r, e) = s;
        Map.isEmpty(t) and Map.isEmpty(r) and Map.isEmpty(e)
    }

    pub def singleTVar(x: String, t: Type): Subst =
        empty() |> bindTVar(x, t)

    pub def singleRVar(x: String, t: Reg): Subst =
        empty() |> bindRVar(x, t)

    pub def singleEVar(x: String, t: Effect): Subst =
        empty() |> bindEVar(x, t)

    pub def id(): Subst =
        empty()

    // TODO: swap order
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

    pub def bindTVar(x: String, t: Type, subst: Subst): Subst = {
        let Subst(tMap, rMap, eMap) = subst;
        Subst(tMap |> Map.insert(x, t), rMap, eMap)
    }

    pub def bindTVarAll(l: List[(String, Type)], subst: Subst): Subst =
        (subst, l) ||> List.foldLeft(acc -> match (x, t) -> bindTVar(x, t, acc))

    pub def unbindTVar(x: String, subst: Subst): Subst = {
        let Subst(tMap, rMap, eMap) = subst;
        Subst(tMap |> Map.remove(x), rMap, eMap)
    }

    pub def appTVar(subst: Subst, x: String): Type \ KeyNotFound = {
        let Subst(tMap, _, _) = subst;
        tMap[x]
    }

    pub def appTVarOpt(subst: Subst, x: String): Option[Type] = {
        let Subst(tMap, _, _) = subst;
        Map.get(x, tMap)
    }

    pub def bindRVar(x: String, p: Reg, subst: Subst): Subst = {
        let Subst(tMap, rMap, eMap) = subst;
        Subst(tMap, rMap |> Map.insert(x, p), eMap)
    }

    pub def bindRVarAll(l: List[(String, Reg)], subst: Subst): Subst =
        (subst, l) ||> List.foldLeft(acc -> match (x, t) -> bindRVar(x, t, acc))

    pub def unbindRVar(x: String, subst: Subst): Subst = {
        let Subst(tMap, rMap, eMap) = subst;
        Subst(tMap, rMap |> Map.remove(x), eMap)
    }

    pub def appRVar(subst: Subst, x: String): Reg \ KeyNotFound = {
        let Subst(_, rMap, _) = subst;
        rMap[x]
    }

    pub def appRVarOpt(subst: Subst, x: String): Option[Reg] = {
        let Subst(_, rMap, _) = subst;
        Map.get(x, rMap)
    }

    pub def bindEVar(x: String, ef: Effect, subst: Subst): Subst = {
        let Subst(tMap, rMap, eMap) = subst;
        Subst(tMap, rMap, eMap |> Map.insert(x, ef))
    }

    pub def bindEVarAll(l: List[(String, Effect)], subst: Subst): Subst =
        (subst, l) ||> List.foldLeft(acc -> match (x, t) -> bindEVar(x, t, acc))

    pub def unbindEVar(x: String, subst: Subst): Subst = {
        let Subst(tMap, rMap, eMap) = subst;
        Subst(tMap, rMap, eMap |> Map.remove(x))
    }

    pub def appEVar(subst: Subst, x: String): Effect \ KeyNotFound = {
        let Subst(_, _, eMap) = subst;
        eMap[x]
    }

    pub def appEVarOpt(subst: Subst, x: String): Option[Effect] = {
        let Subst(_, _, eMap) = subst;
        Map.get(x, eMap)
    }

    pub def appType(subst: Subst, tpe: Type): Type = match tpe {
        case Type.Unitt => Type.Unitt
        case Type.Var(x) => appTVarOpt(subst, x) |> getWithDefault(tpe)
        case Type.Ref(p, t) => Type.Ref(appReg(subst, p), appType(subst, t))
        case Type.Arrow(t, ef, t1) => Type.Arrow(appType(subst, t), appEffect(subst, ef), appType(subst, t1))

        case Type.Bool => Type.Bool
        case Type.Int => Type.Int
    }

    pub def appReg(subst: Subst, p: Reg): Reg = match p {
        case Reg.RegConst(_) => p
        case Reg.RegVar(x) => appRVarOpt(subst, x) |> getWithDefault(p)
    }

    pub def appEffect(subst: Subst, effect: Effect): Effect = match effect {
        case Effect.Empty => Effect.Empty
        case Effect.Init(p) => Effect.Init(appReg(subst, p))
        case Effect.Read(p) => Effect.Read(appReg(subst, p))
        case Effect.Write(p) => Effect.Write(appReg(subst, p))
        case Effect.Union(ef, ef1) => appEffect(subst, ef) + appEffect(subst, ef1)
        case Effect.Var(x) => appEVarOpt(subst, x) |> getWithDefault(effect)
    }

    pub def appTyEnv(subst: Subst, env: TyEnv): TyEnv =
        TyEnv.map(appTyScheme(subst), env)

    pub def appTyScheme(subst: Subst, scheme: TyScheme): TyScheme = region local {
        let {tVars, rVars, eVars, tpe = t, k} = scheme;
        let acc = Ref.fresh(local, subst);
        tVars |> Set.forEach(x -> acc |> Ref.transform(unbindTVar(x)));
        rVars |> Set.forEach(x -> acc |> Ref.transform(unbindRVar(x)));
        eVars |> Set.forEach(x -> acc |> Ref.transform(unbindEVar(x)));
        let t1 = appType(Ref.get(acc), t);
        let k1 = appConstraint(Ref.get(acc), k);
        {tpe = t1, k = k1 | scheme}
    }

    pub def appConstraint(subst: Subst, k: Constraint): Constraint = {
        k |> coerce |> Set.map(match (x, ef) -> {
            match appEVarOpt(subst)(x) {
                case None => (x, appEffect(subst)(ef))
                case Some(Effect.Var(x1)) => (x1, appEffect(subst)(ef))
                case Some(other) => bug!(other |> ToString.toString) // paper says it should only be inferred variables, so unreachable
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
