mod Talpin.RunFile {
    use Talpin.FreshName
    use Talpin.Reconstruction
    use Talpin.EffectSolver
    use Talpin.Data.Subst
    use Util.Util

    pub def checkFileArg(): Result[String, Unit] \ IO =
        run {
            match Env.getArgs() {
                case path :: Nil => checkFile(path)
                case _ => println("expects one path argument")
            }
        }
        with Env.runWithIO
        with Util.fileReadRunWithAbort
        with Abort.runWithResult

    pub def checkFile(path: String): Unit \ IO =
        run checkFileEff(path)
        with Util.fileReadRunWithAbort
        with handler Abort {
            def abort(m, _k) = println(m)
        }

    pub def checkFileEff(path: String): Unit \ IO + Abort + FileRead = {
        if (FileRead.isReadable(path)) {
            let input = FileRead.read(path);
            let p =
                run Talpin.Parsing.Parser.parse(input)
                with handler Abort {
                    def abort(m, _k) = Abort.abort("Parsing Error: ${m}")
                };
            let info =
                run Reconstruction.reconstructInfo(p)
                with FreshName.runWithState
                with handler Abort {
                    def abort(m, _k) = Abort.abort("Typer Error: ${m}")
                } with handler KeyNotFound {
                    def keyNotFound(msg, _k) = Abort.abort("Typer Error: ${msg}")
                };
            info |> Reconstruction.Info.toStrings |> List.forEach(println)

        } else {
            println("cannot find ${path}")
        }
    }

}
