mod Talpin.Parsing.FlixParser {
    use Talpin.Data.Exp
    use Util.StringCursor
    use Util.StringCursor.peekChar
    use Util.StringCursor.nthChar
    use Util.StringCursor.remaining
    use Util.StringCursor.eat
    use Util.StringCursor.at
    use Util.StringCursor.take
    use Util.StringCursor.dropWhile
    use Util.StringCursor.dropWhile!

    pub def parse(s: String): Exp \ Abort = region local {
        let sc = StringCursor.mk(local, '&', s);
        let e = parseExp(sc, canSemi = true);
        whiteSpace(sc);
        expect("&", "parse", sc);
        e
    }

    def parseExp(sc: StringCursor[r], canSemi: {canSemi = Bool}): Exp \ r + Abort = {
        let e0 = parseExpSimp(sc);
        def loop(e) = {
            if (eat("(", sc)) {
                let e1 = parseExp(sc, canSemi = true);
                whiteSpace(sc);
                expect(")", "parseExp", sc);
                loop(Exp.App(e, e1))
            } else {
                whiteSpace(sc);
                if (at(";", sc) and canSemi#canSemi) {
                    expect(";", "parseExp", sc);
                    let e1 = parseExp(sc, canSemi);
                    loop(Exp.Let("_", e, e1))
                } else if (eat("+", sc)) {
                    let e1 = parseExp(sc, canSemi);
                    loop(Exp.Add(e, e1))
                } else if (eat("-", sc)) {
                    let e1 = parseExp(sc, canSemi);
                    loop(Exp.Sub(e, e1))
                } else if (eat("==", sc)) {
                    let e1 = parseExp(sc, canSemi);
                    loop(Exp.Eq(e, e1))
                } else e
            }
        };
        loop(e0)
    }

    def parseExpSimp(sc: StringCursor[r]): Exp \ r + Abort = {
        def ept(s, h) = expect(s, "parseExpSimp:${h#inner}", sc);
        def ws!(h) = whiteSpace!("parseExpSimp:${h}", sc);
        whiteSpace(sc);
        if (eat("(", sc)) { // (e)
            let e = parseExp(sc, canSemi = true);
            whiteSpace(sc);
            ept(")", inner = "paren");
            e
        } else if (eat("\\", sc)) { // \x -> e
            let x = parseName(sc);
            whiteSpace(sc);
            ept("->", inner = "lambda");
            let e = parseExp(sc, canSemi = false);
            Exp.Abs(x, e)
        } else if (eat("rec", sc)) { // rec f \x -> e
            ws!("rec");
            let f = parseName(sc);
            whiteSpace(sc);
            ept("\\", inner = "rec");
            let x = parseName(sc);
            whiteSpace(sc);
            ept("->", inner = "rec");
            let e = parseExp(sc, canSemi = false);
            Exp.Rec(f, x, e)
        } else if (eat("let", sc)) { // let x = e; e1
            ws!("let");
            let x = parseName(sc);
            whiteSpace(sc);
            ept("=", inner = "let");
            let e = parseExp(sc, canSemi = false);
            whiteSpace(sc);
            ept(";", inner = "let");
            let e1 = parseExp(sc, canSemi = true);
            Exp.Let(x, e, e1)
        } else if (eat("new", sc)) { // new(e)
            ept("(", inner = "new");
            let e = parseExp(sc, canSemi = true);
            whiteSpace(sc);
            ept(")", inner = "new");
            Exp.New(e)
        } else if (eat("get", sc)) { // get(e)
            ept("(", inner = "get");
            let e = parseExp(sc, canSemi = true);
            whiteSpace(sc);
            ept(")", inner = "get");
            Exp.Get(e)
        } else if (eat("set", sc)) { // set(e, e1)
            ept("(", inner = "set");
            let e = parseExp(sc, canSemi = true);
            whiteSpace(sc);
            ept(",", inner = "set");
            let e1 = parseExp(sc, canSemi = true);
            whiteSpace(sc);
            ept(")", inner = "set");
            Exp.Set(e, e1)
        } else if (eat("if", sc)) { // if (e) e1 else e2
            whiteSpace(sc);
            ept("(", inner = "if");
            let e = parseExp(sc, canSemi = true);
            whiteSpace(sc);
            ept(")", inner = "if");
            let e1 = parseExp(sc, canSemi = true);
            whiteSpace(sc);
            ept("else", inner = "if");
            ws!("if");
            let e2 = parseExp(sc, canSemi = false);
            Exp.If(e, e1, e2)
        } else if (eat("do", sc)) { // do Effect
            ws!("do");
            let ef = parseName(sc);
            Exp.Do(ef)
        } else if (eat("run", sc)) { // run e Effect
            ws!("run");
            let e = parseExp(sc, canSemi = false);
            whiteSpace(sc);
            let ef = parseName(sc);
            Exp.Run(e, ef)
        } else if (eat("true", sc)) Exp.BoolCst(true)
        else if (eat("false", sc)) Exp.BoolCst(false)
        else if (Char.isDigit(peekChar(sc))) parseInt(sc)
        else if (Char.isLetter(peekChar(sc))) parseVar(sc)
        else err("expression", "parseExpSimp", sc)
    }

    def parseInt(sc: StringCursor[r]): Exp \ r + Abort = {
        def loop(i) = {
            if (nthChar(i, sc) |> Char.isDigit) loop(i+1)
            else i
        };
        let chars = loop(0);
        if (chars == 0) err("digit", "parseInt", sc) else ();
        match take(chars, sc) |> Int32.fromString {
            case Some(i) => Exp.IntCst(i)
            case None => ?unreachable // unreachable
        }
    }

    /// ALWAYS CONSUMES
    def parseVar(sc: StringCursor[r]): Exp \ r + Abort =
        Exp.Var(parseName(sc))

    /// ALWAYS CONSUMES
    def parseName(sc: StringCursor[r]): String \ r + Abort = {
        def loop(i) = {
            let c = nthChar(i, sc);
            if (Char.isLetter(c) or c == '_') loop(i+1)
            else i
        };
        let chars = loop(0);
        if (chars == 0) err("letter", "parseName", sc)
        else take(chars, sc)
    }

    def expect(s: String, rule: String, sc: StringCursor[r]): Unit \ r + Abort = {
        if (eat(s, sc)) ()
        else err("'${s}'", rule, sc)
    }

    def whiteSpace(sc: StringCursor[r]): Unit \ r =
        dropWhile(Char.isWhiteSpace, sc)

    def whiteSpace!(rule: String, sc: StringCursor[r]): Unit \ r + Abort =
        if (dropWhile!(Char.isWhiteSpace, sc)) ()
        else {
            err("whitespace", rule, sc)
        }

    def err(expected: String, rule: String, sc: StringCursor[r]): a \ r + Abort =
        Abort.abort("(${rule}) expected ${expected} in: '${remaining(sc)}'")

}
