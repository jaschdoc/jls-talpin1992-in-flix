mod Talpin.Repl {
    use Talpin.Parsing.Parser
    use Talpin.Reconstruction
    use Talpin.Data.Type
    use Talpin.Data.Constraint
    use Talpin.Data.Subst
    use Talpin.FreshName
    use Talpin.EffectSolver
    use Talpin.Data.Effect
    use Util.Util

    pub def main(): Unit \ IO + Net = App.runAll(() -> {
        println("-- REPL READY (':exit' to exit) --");
        println("Write a single line expression and I will type it");
        loop()
    })

    def loop(): Unit \ IO + Console = {
        let input = Console.readln();
        if (input == ":exit" or input == ":quit" or input == ":q")
            ()
        else {
            typeInput(input);
            loop()
        }
    }

    def typeInput(input: String): Unit \ IO = {
        run {
            let p = Parser.parse(input);
            println("-----------: ----------------------------------");
            println("        Exp: ${p}");
            let ((s, t, ef, k), binders) = Util.runWith(() -> Reconstruction.reconstruct(p), FreshName.handleWithState >> Reconstruction.BinderTypes.handleWithList);
            let s1 = EffectSolver.solv(k);
            let finalSubst = Subst.compose(inner = s, s1);
            println("       Type: ${Subst.appType(s1)(t)}");
            println("     Effect: ${Subst.appEffect(s1)(ef)}");
            println("Constraints: ${k}");
            println("      Subst: ${s |> ToString.toString |> (str -> "\n" + str) |> String.indent(13)}");
            println("  PostSubst: ${finalSubst |> ToString.toString |> (str -> "\n" + str) |> String.indent(13)}");
            println("    binders: ${binders |> Vector.map(match (x, xt) -> "${x}: ${Subst.appType(finalSubst)(xt)}") |> Vector.join(",\n") |> (str -> "\n${str}") |> String.indent(13)}")
        } with Abort {
            def abort(msg, _k) = println("Err: ${msg}")
        } with KeyNotFound {
            def keyNotFound(msg, _k) = println("Err: ${msg}")
        }
    }

}
