mod Talpin.Examples {
    use Talpin.Data.Exp
    use Talpin.Data.Value
    use Talpin.Data.Trace
    use Talpin.Data.Env
    use Talpin.Data.Store
    use Talpin.Data.TyEnv
    use Talpin.Data.Type
    use Talpin.Data.Effect
    use Talpin.Data.Constraint
    use Talpin.FreshName
    use Talpin.EffectSolver
    use Talpin.BigStep.eval
    use Talpin.Parsing.Parser.parse
    use Talpin.Reconstruction.reconstruct
    use Util.Util.runAll
    use Util.Util.handleAll

    pub def exp01(): Exp \ IO = runAll(() -> parse(" ( (  lambda  (  x  )  (  new   x  )  )  (  lambda ( q )  q ) ) "))

    pub def runExp01(): (Value, Trace, Store) \ IO = runAll(() -> {
        eval(Store.empty(), Env.empty(), exp01())
    })

    pub def exp02(): Exp \ IO = runAll(() -> parse(List.join(" ", List#{
        "(let (mul (lambda (x) (lambda (y)",
            "(let (sum (new 0))",
            "(let (inner (rec (loop n)",
                "(if (== n 0) (get sum)",
                    "(let (tmpA (set sum (+ (get sum) y)))",
                    "(loop (- n 1))",
                    ")",
                ")))",
            "(inner x)",
            "))",
        ")))",
        "((mul 6) 8)",
        ")"
    })))

    pub def runExp02(): (Value, Trace, Store) \ IO = runAll(() -> {
        eval(Store.empty(), Env.empty(), exp02())
    })

    pub def exp03(): Exp \ IO = runAll(() -> parse(List.join(" ", List#{
        "(lambda (initial)",
            "(let (counter (new initial))",
                "(lambda (inc)",
                    "(begin (set counter (+ (get counter) inc))",
                            "(get counter)))))"
    })))

    pub def runExp03(): (Value, Trace, Store) \ IO = runAll(() -> {
        eval(Store.empty(), Env.empty(), exp03())
    })

    pub def inferExp03(): (Type, Effect) \ IO = FreshName.runWithState(handleAll(() -> {
        let (_, t, ef) = reconstruct(exp03()) |> EffectSolver.solvFull;
        (t, ef)
    }))

}
