mod Talpin.Parsing.Parser {
    use Talpin.Data.Exp
    use Util.StringCursor
    use Util.StringCursor.peekChar
    use Util.StringCursor.nthChar
    use Util.StringCursor.remaining
    use Util.StringCursor.eat
    use Util.StringCursor.take
    use Util.StringCursor.dropWhile

    pub def parse(s: String): Exp \ Abort = region local {
        let sc = StringCursor.mk(local, '&', s);
        parseExp(sc)
    }

    def parseExp(sc: StringCursor[r]): Exp \ r + Abort = {
        whiteSpace(sc);
        match peekChar(sc) {
            case '(' => parseParenExp(sc)
            case c if Char.isLetter(c) => parseVar(sc)
            case _ => err("letter or '('", "parseExp", sc)
        }
    }

    /// ALWAYS CONSUMES
    def parseParenExp(sc: StringCursor[r]): Exp \ r + Abort = {
        def ept(s) = expect(s, "parseParenExp", sc);
        ept("(");
        whiteSpace(sc);
        if (sc |> eat("lambda")) {
            whiteSpace(sc);
            ept("(");
            whiteSpace(sc);
            let x = parseName(sc);
            whiteSpace(sc);
            ept(")");
            whiteSpace(sc);
            let e = parseExp(sc);
            whiteSpace(sc);
            ept(")");
            Exp.Abs(x, e)
        } else if (sc |> eat("rec")) {
            whiteSpace(sc);
            ept("(");
            whiteSpace(sc);
            let f = parseName(sc);
            whiteSpace(sc);
            let x = parseName(sc);
            whiteSpace(sc);
            ept(")");
            whiteSpace(sc);
            let e = parseExp(sc);
            whiteSpace(sc);
            ept(")");
            Exp.Rec(f, x, e)
        } else if (sc |> eat("let")) {
            whiteSpace(sc);
            ept("(");
            whiteSpace(sc);
            let x = parseName(sc);
            whiteSpace(sc);
            let e = parseExp(sc);
            whiteSpace(sc);
            ept(")");
            whiteSpace(sc);
            let e1 = parseExp(sc);
            whiteSpace(sc);
            ept(")");
            Exp.Let(x, e, e1)
        } else if (sc |> eat("new")) {
            whiteSpace(sc);
            let e = parseExp(sc);
            whiteSpace(sc);
            ept(")");
            Exp.New(e)
        } else if (sc |> eat("get")) {
            whiteSpace(sc);
            let e = parseExp(sc);
            whiteSpace(sc);
            ept(")");
            Exp.Get(e)
        } else if (sc |> eat("set")) {
            whiteSpace(sc);
            let e = parseExp(sc);
            whiteSpace(sc);
            let e1 = parseExp(sc);
            whiteSpace(sc);
            ept(")");
            Exp.Set(e, e1)
        } else {
            whiteSpace(sc);
            let e = parseExp(sc);
            whiteSpace(sc);
            let e1 = parseExp(sc);
            whiteSpace(sc);
            ept(")");
            Exp.App(e, e1)
        }
    }

    def expect(s: String, rule: String, sc: StringCursor[r]): Unit \ r + Abort = {
        if (eat(s, sc)) ()
        else err("'${s}'", rule, sc)
    }

    /// ALWAYS CONSUMES
    def parseVar(sc: StringCursor[r]): Exp \ r + Abort =
        Exp.Var(parseName(sc))

    /// ALWAYS CONSUMES
    def parseName(sc: StringCursor[r]): String \ r + Abort = {
        def loop(i) = {
            if (nthChar(i, sc) |> Char.isLetter) loop(i+1)
            else i
        };
        let i = loop(0);
        if (i == 0) err("letter", "parseName", sc)
        else take(i, sc)
    }

    def whiteSpace(sc: StringCursor[r]): Unit \ r =
        dropWhile(Char.isWhiteSpace, sc)

    def err(expected: String, rule: String, sc: StringCursor[r]): a \ r + Abort =
        Abort.abort("(${rule}) expected ${expected} in: '${remaining(sc)}'")

}
