mod Talpin.Data {

pub enum TyScheme({
    tVars = Set[TVar],
    rVars = Set[RVar],
    eVars = Set[EVar],
    tpe = Type,
    k = Constraint
}) with Coerce

mod TyScheme {
    use Coerce.coerce
    use Talpin.Data.Type
    use Talpin.Data.Constraint
    use Talpin.Data.FreeVarSet
    use Talpin.Data.TyScheme.TyScheme

    pub def fromType(t: Type): TyScheme = TyScheme({
        tVars = Set.empty(),
        rVars = Set.empty(),
        eVars = Set.empty(),
        tpe = t,
        k = Constraint.empty()
    })

    pub def genType(t: Type, k: Constraint, fv: FreeVarSet): TyScheme ={
        let {tVars, rVars, eVars} = coerce(fv);
        TyScheme({tVars = tVars, rVars = rVars, eVars = eVars, tpe = t, k = k})
    }

    pub def fv(s: TyScheme): FreeVarSet = {
        let {tVars, rVars, eVars, tpe, k} = coerce(s);
        (Type.fv(tpe) + Constraint.fv(k))
        |> FreeVarSet.removeTVars(tVars)
        |> FreeVarSet.removeRVars(rVars)
        |> FreeVarSet.removeEVars(eVars)
    }

    pub def isEmpty(s: TyScheme): Bool = {
        let {tVars, rVars, eVars, tpe = _, k} = coerce(s);
        Set.isEmpty(tVars) and Set.isEmpty(rVars) and Set.isEmpty(eVars) and Constraint.isEmpty(k)
    }

    pub def toString(s: TyScheme): String = {
        if (isEmpty(s)) coerce(s)#tpe |> ToString.toString
        else {
            let {tVars, rVars, eVars, tpe, k} = coerce(s);
            let vars = List.map(ToString.toString, Set.toList(tVars)) `List.append` List.map(ToString.toString, Set.toList(rVars)) `List.append` List.map(ToString.toString, Set.toList(eVars));
            "∀${vars |> List.join(", ")}. ${tpe}   with ${k}"
        }
    }

}

}
