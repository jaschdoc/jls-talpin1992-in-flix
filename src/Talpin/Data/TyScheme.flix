mod Talpin.Data {

pub type alias TyScheme = {
    tVars = Set[String],
    rVars = Set[String],
    eVars = Set[String],
    tpe = Type,
    k = Constraint
}

}
