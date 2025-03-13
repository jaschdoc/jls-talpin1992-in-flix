mod Talpin.Data {

pub enum Effect with Eq, Order {
    case Empty
    case Init(Reg)
    case Read(Reg)
    case Write(Reg)
    case Union(Effect, Effect)
    case Var(EVar)

    case Eff(String)
}

instance ToString[Effect] {
    pub def toString(ef: Effect): String = Effect.toString(ef)
}

instance Add[Effect] {
    pub def add(ef1: Effect, ef2: Effect): Effect = Effect.Union(ef1, ef2)
}

mod Effect {
    use Talpin.Data.FreeVarSet
    use Talpin.Data.Reg

    use Functor.map
    use Option.getWithDefault

    pub def toString(effect: Effect): String = match effect {
        case Empty => "Ø"
        case Init(p) => "init(${p})"
        case Read(p) => "read(${p})"
        case Write(p) => "write(${p})"
        case Union(p, p1) => "${toString(p)} ∪ ${toString(p1)}"
        case Var(z) => "${z}"

        case Eff(ef) => ef
    }

    pub def union(l: i): Effect \ Iterable.Aef[i] with Iterable[i] where Iterable.Elm[i] ~ Effect = region local {
        // TODO: don't materialize
        l |> Iterable.iterator(local) |> Iterator.toList |> List.toNel |> map(Nel.reduceLeft(Union)) |> getWithDefault(Empty)
    }

    pub def fv(effect: Effect): FreeVarSet = match effect {
        case Empty => FreeVarSet.empty()
        case Init(p) => Reg.fv(p)
        case Read(p) => Reg.fv(p)
        case Write(p) => Reg.fv(p)
        case Union(ef, ef1) => fv(ef) + fv(ef1)
        case Var(x) => FreeVarSet.singleEVar(x)

        case Eff(_) => FreeVarSet.empty()
    }

    pub def simplify(effect: Effect): Effect = region local {
        let workList = MutPriorityQueue.empty(local);
        MutPriorityQueue.enqueue(workList, effect);

        let seen = MutSet.empty(local);
        def loop() = match MutPriorityQueue.dequeue(workList) {
            case Some(ef) =>
                match ef {
                    case Empty => ()
                    case Init(_) => seen |> MutSet.add(ef)
                    case Read(_) => seen |> MutSet.add(ef)
                    case Write(_) => seen |> MutSet.add(ef)
                    case Union(e, e1) =>
                        MutPriorityQueue.enqueue(workList, e);
                        MutPriorityQueue.enqueue(workList, e1)
                    case Var(_) => seen |> MutSet.add(ef)

                    case Eff(_) => seen |> MutSet.add(ef)
                };
                loop()
            case None => ()
        };
        loop();
        union(seen)
    }

    pub def remove(toRemove: {toRemove = Effect}, effect: Effect): Effect = match effect {
        case Empty => Empty
        case Init(p) => if (toRemove#toRemove == Init(p)) Empty else Init(p)
        case Read(p) => if (toRemove#toRemove == Read(p)) Empty else Read(p)
        case Write(p) => if (toRemove#toRemove == Write(p)) Empty else Write(p)
        case Union(ef, ef1) => Union(remove(toRemove, ef), remove(toRemove, ef1))
        case Var(x) => if (toRemove#toRemove == Var(x)) Empty else Var(x)

        case Eff(s) => if (toRemove#toRemove == Eff(s)) Empty else Eff(s)
    }

}

}
