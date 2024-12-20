mod Talpin.Data {

pub type alias TyScheme = {
    tVars = Set[String],
    rVars = Set[String],
    eVars = Set[String],
    tpe = Type,
    k = Constraint
}

mod TyScheme {
    use Talpin.Data.Type
    use Talpin.Data.Constraint
    use Talpin.Data.FreeVarSet

    pub def fromType(t: Type): TyScheme = {
        tVars = Set.empty(),
        rVars = Set.empty(),
        eVars = Set.empty(),
        tpe = t,
        k = Constraint.empty()
    }

    pub def genType(t: Type, k: Constraint, fv: FreeVarSet): TyScheme ={
        let {tVars, rVars, eVars} = coerce(fv);
        {tVars = tVars, rVars = rVars, eVars = eVars, tpe = t, k = k}
    }

    pub def fv(s: TyScheme): FreeVarSet = {
        let {tVars, rVars, eVars, tpe, k} = s;
        (Type.fv(tpe) + Constraint.fv(k))
        |> FreeVarSet.removeTVars(tVars)
        |> FreeVarSet.removeRVars(rVars)
        |> FreeVarSet.removeEVars(eVars)
    }

    pub def isEmpty(s: TyScheme): Bool = {
        let {tVars, rVars, eVars, tpe = _, k} = s;
        Set.isEmpty(tVars) and Set.isEmpty(rVars) and Set.isEmpty(eVars) and Constraint.isEmpty(k)
    }

    pub def toString(s: TyScheme): String = {
        if (isEmpty(s)) s#tpe |> ToString.toString
        else {
            let {tVars, rVars, eVars, tpe, k} = s;
            let vars = Set.toList(tVars) `List.append` Set.toList(rVars) `List.append` Set.toList(eVars);
            "∀${vars |> List.join(", ")}. ${tpe}   with ${k}"
        }
    }

}

}
