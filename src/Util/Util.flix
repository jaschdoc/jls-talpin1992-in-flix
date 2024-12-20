mod Util.Util {
    import java.lang.System

    pub def abortHandleWithCrash(f: a -> b \ ef): a -> b \ ef - Abort + IO =
        x -> run f(x) with Abort {
            def abort(m, _k) = {
                System.err.println(m);
                ?abortCrash
            }
        }

    pub def keyNotFoundHandleWithAbort(f: a -> b \ ef): a -> b \ ef - KeyNotFound + Abort =
        x -> run f(x) with KeyNotFound {
            def keyNotFound(m, _k) = Abort.abort(m)
        }

    pub def runAll(f: Unit -> t \ ef): t \ ef - Abort - KeyNotFound + IO =
        handleAll(f)()

    pub def handleAll(f: a -> b \ ef): a -> b \ ef - Abort - KeyNotFound + IO =
        (keyNotFoundHandleWithAbort >> abortHandleWithCrash)(f)

    pub def init(f: Unit -> t \ ef, n: Int32): List[t] \ ef = {
        def loop(count, acc) = {
            if (count <= 0) List.reverse(acc)
            else loop(count - 1, f() :: acc)
        };
        loop(n, Nil)
    }

    pub def runWith(f: Unit -> a \ ef1, handler: (Unit -> a \ ef1) -> (Unit -> b \ ef2)): b \ ef2 =
        handler(f)()

    pub def fileReadHandleWithAbort(f: a -> b \ ef): a -> b \ ef - FileRead + {Abort, IO} =
        x -> match FileRead.handle(f)(x) {
            case Ok(v) => v
            case Err(_ioError) => Abort.abort("IoError")
        }
}
