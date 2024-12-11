mod Talpin.Data {

pub type alias TyEnv = Map[Id, TyScheme]

mod TyEnv {
    use Talpin.Data.TyScheme
    use Talpin.Data.Id
    use Talpin.Data.Constraint
    use Talpin.Data.Type
    use Talpin.Data.FreeVarSet

    use Coerce.coerce

    pub def map(f: TyScheme -> TyScheme \ ef, env: TyEnv): TyEnv \ ef =
        Map.map(f, env)

    pub def bindType(x: Id, t: Type, env: TyEnv): TyEnv = {
        Map.insert(x, TyScheme.fromType(t), env)
    }

    pub def bind(x: Id, fv: FreeVarSet, t: Type, k: Constraint, env: TyEnv): TyEnv = {
        let {tVars, rVars, eVars} = coerce(fv);
        Map.insert(x, {tVars = tVars, rVars = rVars, eVars = eVars, tpe = t, k = k}, env)
    }

    pub def unbind(x: Id, env: TyEnv): TyEnv =
        Map.remove(x, env)

    pub def fv(env: TyEnv): FreeVarSet = ???

}

}
