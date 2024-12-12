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

    pub def fv(s: TyScheme): FreeVarSet = ???

}

}
