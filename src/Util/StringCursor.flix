mod Util {

pub struct StringCursor[r] {
    mut p: Int32,
    s: String,
    eof: Char
}

mod StringCursor {
    use ToString.toString

    pub def mk(rc: Region[r], eof: Char, s: String): StringCursor[r] \ r =
        new StringCursor @ rc {p = 0, s = s, eof = eof}

    pub def peekChar(sc: StringCursor[r]): Char \ r =
        nthChar(0, sc)

    pub def nthChar(n: Int32, sc: StringCursor[r]): Char \ r = {
        if (inBounds(sc->p + n, sc)) String.charAt(sc->p + n, sc->s)
        else sc->eof
    }

    pub def remaining(sc: StringCursor[r]): String \ r = {
        if (inBounds(sc->p, sc)) String.slice(start = sc->p, end = String.length(sc->s), sc->s)
        else ""
    }

    pub def peek(n: Int32, sc: StringCursor[r]): String \ r = {
        let available = inBounds(sc->p + n - 1, sc);
        if (available) String.slice(start = sc->p, end = sc->p + n, sc->s)
        else String.slice(start = sc->p, end = String.length(sc->s), sc->s) + toString(sc->eof)
    }

    pub def take(n: Int32, sc: StringCursor[r]): String \ r = {
        let res = peek(n, sc);
        sc->p = sc->p + n;
        res
    }

    pub def eat(s: String, sc: StringCursor[r]): Bool \ r = {
        let len = String.length(s);
        if (peek(len, sc) == s) {
            take(len, sc);
            true
        } else {
            false
        }
    }

    pub def dropWhile(f: Char -> Bool \ ef, sc: StringCursor[r]): Unit \ ef + r = {
        let c = peekChar(sc);
        if (c != sc->eof and f(c)) {
            discard take(1, sc);
            dropWhile(f, sc)
        } else ()
    }

    pub def hasPrefix(s: String, sc: StringCursor[r]): Bool \ r = {
        peek(String.length(s), sc) == s
    }

    def inBounds(idx: Int32, sc: StringCursor[r]): Bool =
        idx < String.length(sc->s)

}

}
