mod Talpin.Data {

pub enum Subst(Map[String, Type], Map[String, Reg], Map[String, Effect])

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

    pub def bindTVar(x: String, t: Type, subst: Subst): Subst = {
        let Subst(tMap, rMap, eMap) = subst;
        Subst(tMap |> Map.insert(x, t), rMap, eMap)
    }

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
        case Effect.Union(ef, ef1) => Effect.Union(appEffect(subst, ef), appEffect(subst, ef1))
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
        let k1 = ?appConstraint(Ref.get(acc), k);
        {tpe = t1, k = k1 | scheme}
    }

}

}
