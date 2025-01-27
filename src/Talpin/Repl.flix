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
        println("(Start with 'flix:' to use flix-ish syntax)");
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
            let info = run Reconstruction.reconstructInfo(p) with FreshName.runWithState;
            info |> Reconstruction.Info.toStrings |> List.forEach(println)
        } with handler Abort {
            def abort(msg, _k) = println("Err: ${msg}")
        } with handler KeyNotFound {
            def keyNotFound(msg, _k) = println("Err: ${msg}")
        }
    }

}
