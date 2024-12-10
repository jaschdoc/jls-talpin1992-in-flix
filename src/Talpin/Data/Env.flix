mod Talpin.Data {
    use Coerce.coerce

pub enum Env(Map[Id, Value])

instance Indexable[Env] {
    type Idx = Id
    type Elm = Value
    type Aef = KeyNotFound

    pub def get(env: Env, id: Id): Value \ KeyNotFound = Env.lookup(id, env)
}

instance Coerce[Env] {
    type Out = Map[Id, Value]

    pub def coerce(env: Env): Map[Id, Value] = Env.coerce(env)
}


mod Env {
    use Talpin.Data.Id
    use Talpin.Data.Value

    pub def empty(): Env =
        Env(Map.empty())

    pub def lookupOpt(id: Id, env: Env): Option[Value] =
        env |> coerce |> Map.get(id)

    pub def lookup(id: Id, env: Env): Value \ KeyNotFound =
        match lookupOpt(id, env) {
            case None => KeyNotFound.keyNotFound("key not found: ${id}")
            case Some(v) => v
        }

    pub def coerce(env: Env): Map[Id, Value] =
        match env { case Env(m) => m }

    pub def coerceMap(f: Map[Id, Value] -> Map[Id, Value] \ ef, env: Env): Env \ ef =
        env |> coerce |> f |> Env

    pub def unbind(id: Id, env: Env): Env =
        env |> coerceMap(Map.remove(id))

    pub def bind(id: Id, v: Value, env: Env): Env =
        env |> coerceMap(Map.insert(id, v))

}

}
