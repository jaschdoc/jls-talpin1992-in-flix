mod Util.Util {
    import java.lang.System

    pub def handleAbort(f: a -> b \ ef): a -> b \ ef - Abort + IO =
        x -> run f(x) with Abort {
            def abort(m, _k) = {
                System.err.println(m);
                ???
            }
        }

    pub def handleKeyNotFound(f: a -> b \ ef): a -> b \ ef - KeyNotFound + Abort =
        x -> run f(x) with KeyNotFound {
            def keyNotFound(m, _k) = Abort.abort(m)
        }

    pub def runAll(f: Unit -> t \ ef): t \ ef - Abort - KeyNotFound + IO =
        (handleKeyNotFound >> handleAbort)(f)()
}
