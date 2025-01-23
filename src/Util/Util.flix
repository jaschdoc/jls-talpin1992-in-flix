mod Util.Util {
    import java.lang.System

    pub def abortRunWithCrash(f: Unit -> b \ ef): b \ ef - Abort + IO =
        run f() with handler Abort {
            def abort(m, _k) = {
                System.err.println(m);
                ?abortCrash
            }
        }

    pub def keyNotFoundRunWithAbort(f: Unit -> b \ ef): b \ ef - KeyNotFound + Abort =
        run f() with handler KeyNotFound {
            def keyNotFound(m, _k) = Abort.abort(m)
        }

    pub def runAll(f: Unit -> t \ ef): t \ ef - Abort - KeyNotFound - Talpin.FreshName - Talpin.Reconstruction.BinderTypes + IO =
        run f()
        with Talpin.FreshName.runWithState
        with Talpin.Reconstruction.BinderTypes.runWithNothing
        with keyNotFoundRunWithAbort
        with abortRunWithCrash

    pub def init(f: Unit -> t \ ef, n: Int32): List[t] \ ef = {
        def loop(count, acc) = {
            if (count <= 0) List.reverse(acc)
            else loop(count - 1, f() :: acc)
        };
        loop(n, Nil)
    }

    pub def fileReadRunWithAbort(f: Unit -> b \ ef): b \ ef - FileRead + {Abort, IO} =
        match FileRead.runWithIO(f) {
            case Ok(v) => v
            case Err(_ioError) => Abort.abort("IoError")
        }
}
