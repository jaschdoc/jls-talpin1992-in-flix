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
        match String.stripPrefix(substr = "flix:", s) {
            case Some(input) =>
                Talpin.Parsing.FlixParser.parse(input)
            case None =>
                let sc = StringCursor.mk(local, '&', s);
                parseExp(sc)
        }
    }

    def parseExp(sc: StringCursor[r]): Exp \ r + Abort = {
        whiteSpace(sc);
        let c = peekChar(sc);
        if (c == '(') parseParenExp(sc)
        else if (eat("true", sc)) Exp.BoolCst(true)
        else if (eat("false", sc)) Exp.BoolCst(false)
        else if (Char.isDigit(c)) parseInt(sc)
        else if (Char.isLetter(c)) parseVar(sc)
        else err("expression or '('", "parseExp", sc)
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
        } else if (sc |> eat("begin")) {
            whiteSpace(sc);
            let e = parseExp(sc);
            whiteSpace(sc);
            let e1 = parseExp(sc);
            whiteSpace(sc);
            ept(")");
            Exp.Let("_", e, e1)
        } else if (sc |> eat("if")) {
            whiteSpace(sc);
            let e = parseExp(sc);
            whiteSpace(sc);
            let e1 = parseExp(sc);
            whiteSpace(sc);
            let e2 = parseExp(sc);
            whiteSpace(sc);
            ept(")");
            Exp.If(e, e1, e2)
        } else if (sc |> eat("+")) {
            whiteSpace(sc);
            let e = parseExp(sc);
            whiteSpace(sc);
            let e1 = parseExp(sc);
            whiteSpace(sc);
            ept(")");
            Exp.Add(e, e1)
        } else if (sc |> eat("-")) {
            whiteSpace(sc);
            let e = parseExp(sc);
            whiteSpace(sc);
            let e1 = parseExp(sc);
            whiteSpace(sc);
            ept(")");
            Exp.Sub(e, e1)
        } else if (sc |> eat("==")) {
            whiteSpace(sc);
            let e = parseExp(sc);
            whiteSpace(sc);
            let e1 = parseExp(sc);
            whiteSpace(sc);
            ept(")");
            Exp.Eq(e, e1)
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
    def parseName(sc: StringCursor[r]): String \ r + Abort = {
        def loop(i) = {
            if (nthChar(i, sc) |> Char.isLetter) loop(i+1)
            else i
        };
        let chars = loop(0);
        if (chars == 0) err("letter", "parseName", sc)
        else take(chars, sc)
    }

    def whiteSpace(sc: StringCursor[r]): Unit \ r =
        dropWhile(Char.isWhiteSpace, sc)

    def err(expected: String, rule: String, sc: StringCursor[r]): a \ r + Abort =
        Abort.abort("(${rule}) expected ${expected} in: '${remaining(sc)}'")

}
