mod Talpin.BigStep {
    use Talpin.Data.Env
    use Talpin.Data.Env.{bind, unbind}
    use Talpin.Data.Exp
    use Talpin.Data.Id
    use Talpin.Data.Store
    use Talpin.Data.Trace
    use Talpin.Data.Trace.insert
    use Talpin.Data.Value
    use Talpin.Data.Store.{newLoc, remove, store}
    use Talpin.Data.RuntimeEffect.{Init, Read, Write}
    use Talpin.Data.Value.{Closure, Ref, U}

    pub def eval(s: Store, env: Env, exp: Exp): (Value, Trace, Store) \ {Abort, KeyNotFound} = match exp {
        case Exp.Var(x) =>
            (env[x], Trace.empty(), s)
        case Exp.Abs(x, e) =>
            (Closure(x, e, () -> unbind(x, env)), Trace.empty(), s)
        case Exp.Rec(f, x, e) =>
            def c() = Closure(x, e, () -> env |> unbind(f) |> unbind(x) |> bind(f, c()));
            (c(), Trace.empty(), s)
        case Exp.App(e, e1) =>
            evalApp(s, env, e, e1)
        case Exp.Let(x, e, e1) =>
            evalLet(s, env, x, e, e1)
        case Exp.New(e) =>
            evalNew(s, env, e)
        case Exp.Get(e) =>
            evalGet(s, env, e)
        case Exp.Set(e, e1) =>
            evalSet(s, env, e, e1)
    }

    def evalApp(s0: Store, env: Env, e: Exp, e1: Exp): (Value, Trace, Store) \ {Abort, KeyNotFound} = {
        let (vTemp, f, s) = eval(s0, env, e);
        let (x, e11, env1) = vTemp |> Value.getClosure;
        let (v1, f1, s1) = eval(s, env, e1);
        let (v11, f11, s11) = eval(s1, env1 |> bind(x, v1), e11);
        (v11, f + f1 + f11, s11)
    }

    def evalLet(s0: Store, env: Env, x: Id, e: Exp, e1: Exp): (Value, Trace, Store) \ {Abort, KeyNotFound} = {
        let (v, f, s) = eval(s0, env, e);
        let (v1, f1, s1) = eval(s, env |> unbind(x) |> bind(x, v), e1);
        (v1, f + f1, s1)
    }

    def evalNew(s0: Store, env: Env, e: Exp): (Value, Trace, Store) \ {Abort, KeyNotFound} = {
        let (v, f, s) = eval(s0, env, e);
        let l = newLoc(s);
        (Ref(l), f |> insert(Init(l)), s |> store(l, v))
    }

    def evalGet(s0: Store, env: Env, e: Exp): (Value, Trace, Store) \ {Abort, KeyNotFound} = {
        let (vTemp, f, s) = eval(s0, env, e);
        let l = Value.getRef(vTemp);
        (s[l], f |> insert(Read(l)), s)
    }

    def evalSet(s0: Store, env: Env, e: Exp, e1: Exp): (Value, Trace, Store) \ {Abort, KeyNotFound} = {
        let (vTemp, f, s) = eval(s0, env, e);
        let l = Value.getRef(vTemp);
        let (v, f1, s1) = eval(s, env, e1);
        (U, (f + f1) |> insert(Write(l)), s1 |> remove(l) |> store(l, v))
    }

}
