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
    use Talpin.Data.RuntimeEffect.{Init, Read, Write, Eff}
    use Talpin.Data.Value.{Closure, Ref, U, BoolVal, IntVal}

    pub def eval(s: Store, env: Env, exp: Exp): (Value, Trace, Store) \ {Abort, KeyNotFound} = match exp {
        case Exp.Var(x) =>
            (env[x], Trace.empty(), s)
        case Exp.Abs(x, e) =>
            let env1 = unbind(x, env);
            (Closure(x, e, () -> env1), Trace.empty(), s)
        case Exp.Rec(f, x, e) =>
            let env1 = env |> unbind(f) |> unbind(x);
            def c() = Closure(x, e, () -> env1 |> bind(f, c()));
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

        case Exp.BoolCst(b) =>
            (BoolVal(b), Trace.empty(), s)
        case Exp.If(e, e1, e2) =>
            evalIf(s, env, e, e1, e2)
        case Exp.IntCst(b) =>
            (IntVal(b), Trace.empty(), s)
        case Exp.Add(e, e1) =>
            evalIntOp(s, env, e, e1, Add.add)
        case Exp.Sub(e, e1) =>
            evalIntOp(s, env, e, e1, Sub.sub)
        case Exp.Eq(e, e1) =>
            evalEq(s, env, e, e1)

        case Exp.Do(ef) =>
            (U, Trace.singleton(Eff(ef)), s)
        case Exp.Run(e, ef) =>
            let (v, f, s1) = eval(s, env, e);
            (v, f |> Trace.remove(Eff(ef)), s1)
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
        let env1 = if (x == "_") env else env |> unbind(x) |> bind(x, v);
        let (v1, f1, s1) = eval(s, env1, e1);
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

    def evalIf(s0: Store, env: Env, e: Exp, e1: Exp, e2: Exp): (Value, Trace, Store) \ {Abort, KeyNotFound} = {
        let (vTemp, f, s) = eval(s0, env, e);
        match Value.getBool(vTemp) {
            case true =>
                let (v, f1, s1) = eval(s, env, e1);
                (v, f + f1, s1)
            case false =>
                let (v, f1, s1) = eval(s, env, e2);
                (v, f + f1, s1)
        }
    }

    def evalIntOp(s0: Store, env: Env, e: Exp, e1: Exp, op: (Int32, Int32) -> Int32): (Value, Trace, Store) \ {Abort, KeyNotFound} = {
        let (vTemp, f, s) = eval(s0, env, e);
        let v = Value.getInt(vTemp);
        let (v1Temp, f1, s1) = eval(s, env, e1);
        let v1 = Value.getInt(v1Temp);
        (IntVal(op(v, v1)), f + f1, s1)
    }

    def evalEq(s0: Store, env: Env, e: Exp, e1: Exp): (Value, Trace, Store) \ {Abort, KeyNotFound} = {
        let (v, f, s) = eval(s0, env, e);
        let (v1, f1, s1) = eval(s, env, e1);
        let resB = match (v, v1) {
            case (IntVal(i1), IntVal(i2)) => i1 == i2
            case (BoolVal(b1), BoolVal(b2)) => b1 == b2
            case _ => Abort.abort("'${v}' and '${v1}' cannot be compared with eq")
        };
        (BoolVal(resB), f + f1, s1)
    }

}
