mod Talpin.Repl {
    use Talpin.Parsing.Parser
    use Talpin.Reconstruction
    use Talpin.Data.Type
    use Talpin.Data.Constraint
    use Talpin.FreshName
    use Talpin.Data.Effect

    pub def main(): Unit \ IO = App.runAll(() -> {
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
            let (s, t, ef, k) = FreshName.runWithState(() -> Reconstruction.reconstruct(p));
            println("       Type: ${t}");
            println("     Effect: ${ef}");
            println("Constraints: ${k}");
            println("      Subst: ${s |> ToString.toString |> (str -> "\n" + str)|> String.indent(13)}")
        } with Abort {
            def abort(msg, _k) = println("Err: ${msg}")
        } with KeyNotFound {
            def keyNotFound(msg, _k) = println("Err: ${msg}")
        }
    }

}
