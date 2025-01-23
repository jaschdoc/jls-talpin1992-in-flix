mod Talpin.Repl {
    use Talpin.Parsing.Parser
    use Talpin.Reconstruction
    use Talpin.Data.Type
    use Talpin.Data.Constraint
    use Talpin.Data.TyScheme
    use Talpin.Data.Subst
    use Talpin.FreshName
    use Talpin.EffectSolver
    use Talpin.Data.Effect
    use Util.Util

    pub def main(): Unit \ IO = run {
        println("-- REPL READY (':exit' to exit) --");
        println("Write a single line expression and I will type it");
        loop()
    } with Console.runWithIO

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
            let ((s, t, ef, k), binders) = run Reconstruction.reconstruct(p) with FreshName.runWithState with Reconstruction.BinderTypes.runWithList;
            let s1 = EffectSolver.solv(k);
            let finalSubst = Subst.compose(inner = s, s1);
            println("       Type: ${Subst.appType(s1)(t)}");
            println("     Effect: ${Subst.appEffect(s1)(ef)}");
            println("Constraints: ${k}");
            println("      Subst: ${s |> ToString.toString |> (str -> "\n" + str) |> String.indent(13)}");
            println("  PostSubst: ${finalSubst |> ToString.toString |> (str -> "\n" + str) |> String.indent(13)}");
            println("    binders: ${binders |> Vector.map(match (x, xt) -> "${x}: ${Subst.appTyScheme(finalSubst)(xt) |> TyScheme.toString}") |> Vector.join(",\n") |> (str -> "\n${str}") |> String.indent(13)}")
        } with handler Abort {
            def abort(msg, _k) = println("Err: ${msg}")
        } with handler KeyNotFound {
            def keyNotFound(msg, _k) = println("Err: ${msg}")
        }
    }

}
