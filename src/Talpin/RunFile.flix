mod Talpin.RunFile {
    use Talpin.FreshName
    use Talpin.Reconstruction
    use Talpin.EffectSolver
    use Talpin.Data.Subst
    use Util.Util

    pub def checkFileArg(): Result[String, Unit] \ {Sys, IO} = Util.runWith(() -> {
        match Environment.getArgs() {
            case path :: Nil => checkFile(path)
            case _ => println("expects one path argument")
        }
    }, Environment.handle >> Util.fileReadHandleWithAbort >> Abort.handle)

    pub def checkFile(path: String): Unit \ IO =
        run {
            match FileRead.handle(checkFileEff)(path) {
                case Ok() => ()
                case Err(_err) => println("IO Error")
            }
        } with Abort {
            def abort(m, _k) = println(m)
        }

    pub def checkFileEff(path: String): Unit \ IO + Abort + FileRead = {
        if (FileRead.isReadable(path)) {
            let input = FileRead.read(path);
            let p =
                run Talpin.Parsing.Parser.parse(input)
                with Abort {
                    def abort(m, _k) = Abort.abort("Parsing Error: ${m}")
                };
            let ((s, t, ef, k), _binders) =
                run Util.runWith(() -> Reconstruction.reconstruct(p), FreshName.handleWithState >> Reconstruction.BinderTypes.handleWithList)
                with Abort {
                    def abort(m, _k) = Abort.abort("Typer Error: ${m}")
                } with KeyNotFound {
                    def keyNotFound(msg, _k) = Abort.abort("Typer Error: ${msg}")
                };
            let s1 = EffectSolver.solv(k);
            let finalSubst = Subst.compose(inner = s, s1);
            println("       Type: ${Subst.appType(s1)(t)}");
            println("     Effect: ${Subst.appEffect(s1)(ef)}");
            println("Constraints: ${k}");
            println("      Subst: ${s |> ToString.toString |> (str -> "\n" + str)|> String.indent(13)}");
            println("  PostSubst: ${finalSubst |> ToString.toString |> (str -> "\n" + str)|> String.indent(13)}")

        } else {
            println("cannot find ${path}")
        }
    }

}
