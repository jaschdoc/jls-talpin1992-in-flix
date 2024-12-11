mod Talpin.Data {

pub enum FreeVarSet {
    case Sets(Set[String], Set[String], Set[String])
    case Empty
}

instance Add[FreeVarSet] {
    pub def add(fv: FreeVarSet, fv1: FreeVarSet): FreeVarSet = FreeVarSet.union(fv, fv1)
}

instance Sub[FreeVarSet] {
    pub def sub(fv: FreeVarSet, fv1: FreeVarSet): FreeVarSet = FreeVarSet.difference(fv, fv1)
}

mod FreeVarSet {

    pub def empty(): FreeVarSet =
        Empty

    pub def singleTVar(x: String): FreeVarSet =
        Sets(Set#{x}, Set.empty(), Set.empty())

    pub def containsTVar(x: String, fv: FreeVarSet): Bool = match fv {
        case Sets(t, _, _) => Set.memberOf(x, t)
        case Empty => false
    }

    pub def singleRVar(x: String): FreeVarSet =
        Sets(Set.empty(), Set#{x}, Set.empty())

    pub def containsRVar(x: String, fv: FreeVarSet): Bool = match fv {
        case Sets(_, r, _) => Set.memberOf(x, r)
        case Empty => false
    }

    pub def singleEVar(x: String): FreeVarSet =
        Sets(Set.empty(), Set.empty(), Set#{x})

    pub def containsEVar(x: String, fv: FreeVarSet): Bool = match fv {
        case Sets(_, _, e) => Set.memberOf(x, e)
        case Empty => false
    }

    pub def union(fv: FreeVarSet, fv1: FreeVarSet): FreeVarSet = match (fv, fv1) {
        case (Sets(t, r, e), Sets(t1, r1, e1)) =>
            Sets(t `Set.union` t1, r `Set.union` r1, e `Set.union` e1)
        case (Sets(_, _, _), Empty) => fv
        case (Empty, Sets(_, _, _)) => fv1
        case (Empty, Empty) => Empty
    }

    pub def difference(fv: FreeVarSet, fv1: FreeVarSet): FreeVarSet = match (fv, fv1) {
        case (Sets(t, r, e), Sets(t1, r1, e1)) =>
            Sets(t `Set.difference` t1, r `Set.difference` r1, e `Set.difference` e1)
        case (Sets(_, _, _), Empty) => fv
        case (Empty, _) => Empty
    }

}

}
