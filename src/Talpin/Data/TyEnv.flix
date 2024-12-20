mod Talpin.Data {

pub type alias TyEnv = Map[Id, TyScheme]

mod TyEnv {
    use Talpin.Data.TyScheme
    use Talpin.Data.Id
    use Talpin.Data.Constraint
    use Talpin.Data.Type
    use Talpin.Data.FreeVarSet

    use Coerce.coerce

    pub def empty(): TyEnv =
        Map.empty()

    pub def map(f: TyScheme -> TyScheme \ ef, env: TyEnv): TyEnv \ ef =
        Map.map(f, env)

    pub def bindType(x: Id, t: Type, env: TyEnv): TyEnv =
        Map.insert(x, TyScheme.fromType(t), env)

    pub def bind(x: Id, s: TyScheme, env: TyEnv): TyEnv =
        Map.insert(x, s, env)

    pub def unbind(x: Id, env: TyEnv): TyEnv =
        Map.remove(x, env)

    pub def fv(env: TyEnv): FreeVarSet =
        (FreeVarSet.empty(), env) ||> Map.foldLeft(acc -> s -> acc + TyScheme.fv(s))

}

}
