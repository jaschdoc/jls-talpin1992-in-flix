mod Util.Util {
    import java.lang.System

    pub def handleAbort(f: a -> b \ ef): a -> b \ ef - Abort + IO =
        x -> run f(x) with Abort {
            def abort(m, _k) = {
                System.err.println(m);
                ?abortCrash
            }
        }

    pub def handleKeyNotFound(f: a -> b \ ef): a -> b \ ef - KeyNotFound + Abort =
        x -> run f(x) with KeyNotFound {
            def keyNotFound(m, _k) = Abort.abort(m)
        }

    pub def runAll(f: Unit -> t \ ef): t \ ef - Abort - KeyNotFound + IO =
        handleAll(f)()

    pub def handleAll(f: a -> b \ ef): a -> b \ ef - Abort - KeyNotFound + IO =
        (handleKeyNotFound >> handleAbort)(f)

    pub def init(f: Unit -> t \ ef, n: Int32): List[t] \ ef = {
        def loop(count, acc) = {
            if (count <= 0) List.reverse(acc)
            else loop(count - 1, f() :: acc)
        };
        loop(n, Nil)
    }
}
