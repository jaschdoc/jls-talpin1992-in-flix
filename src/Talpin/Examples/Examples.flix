mod Talpin.Examples {
    use Talpin.Data.Exp
    use Talpin.Data.Value
    use Talpin.Data.Trace
    use Talpin.Data.Env
    use Talpin.Data.Store
    use Talpin.BigStep.eval
    use Talpin.Parsing.Parser.parse
    use Util.Util.runAll

    pub def exp01(): Exp \ IO = runAll(() -> parse(" ( (  lambda  (  x  )  (  new   x  )  )  (  lambda ( q )  q ) ) "))

    pub def runExp01(): (Value, Trace, Store) \ IO = runAll(() -> {
        eval(Store.empty(), Env.empty(), exp01())
    })

    pub def exp02(): Exp \ IO = runAll(() -> parse(List.join(" ", List#{
        "(let (mul (lambda (x) (lambda (y)",
            "(let (sum (new 0))",
            "(let (inner (rec (loop n)",
                "(if (eq n 0) (get sum)",
                    "(let (tmpA (set sum (add (get sum) y)))",
                    "(loop (sub n 1))",
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

}
