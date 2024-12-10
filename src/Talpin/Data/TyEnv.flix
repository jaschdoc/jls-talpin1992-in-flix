mod Talpin.Data {

pub type alias TyEnv = Map[Id, TyScheme]

mod TyEnv {
    use Talpin.Data.TyScheme

    pub def map(f: TyScheme -> TyScheme \ ef, env: TyEnv): TyEnv \ ef =
        Map.map(f, env)

}

}
