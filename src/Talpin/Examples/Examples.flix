mod Talpin.Examples {
    use Talpin.Data.Exp
    use Talpin.Data.Value
    use Talpin.Data.Trace
    use Talpin.Data.Env
    use Talpin.Data.Store
    use Talpin.BigStep.eval
    use Talpin.Parsing.Parser.parse
    use Util.Util.runAll

    pub def exp01(): Exp \ IO = runAll(() -> parse("  (  lambda  (  x  )  (  new   x  )  )  "))

    pub def runExp01(): (Value, Trace, Store) \ IO = runAll(() -> {
        eval(Store.empty(), Env.empty(), exp01())
    })

}
