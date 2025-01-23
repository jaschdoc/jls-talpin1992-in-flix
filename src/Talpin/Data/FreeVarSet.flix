mod Talpin.Data {

pub enum FreeVarSet {
    case Sets(Set[TVar], Set[RVar], Set[EVar])
    case Empty
}

instance Add[FreeVarSet] {
    pub def add(fv: FreeVarSet, fv1: FreeVarSet): FreeVarSet = FreeVarSet.union(fv, fv1)
}

instance Sub[FreeVarSet] {
    pub def sub(fv: FreeVarSet, fv1: FreeVarSet): FreeVarSet = FreeVarSet.difference(fv, fv1)
}

instance Coerce[FreeVarSet] {
    type Out = {tVars = Set[TVar], rVars = Set[RVar], eVars = Set[EVar]}

    pub def coerce(fv: FreeVarSet): {tVars = Set[TVar], rVars = Set[RVar], eVars = Set[EVar]} = FreeVarSet.coerce(fv)
}


mod FreeVarSet {
    use Talpin.Data.TVar
    use Talpin.Data.RVar
    use Talpin.Data.EVar
    pub def empty(): FreeVarSet =
        Empty

    pub def coerce(fv: FreeVarSet): {tVars = Set[TVar], rVars = Set[RVar], eVars = Set[EVar]} = match fv {
        case Sets(t, r, e) => {tVars = t, rVars = r, eVars = e}
        case Empty => {tVars = Set.empty(), rVars = Set.empty(), eVars = Set.empty()}
    }

    pub def singleTVar(x: TVar): FreeVarSet =
        Sets(Set#{x}, Set.empty(), Set.empty())

    pub def containsTVar(x: TVar, fv: FreeVarSet): Bool = match fv {
        case Sets(t, _, _) => Set.memberOf(x, t)
        case Empty => false
    }

    pub def removeTVars(s: Set[TVar], fv: FreeVarSet): FreeVarSet = match fv {
        case Sets(t, r, e) => Sets(Set.difference(t, s), r, e)
        case Empty => Empty
    }

    pub def singleRVar(x: RVar): FreeVarSet =
        Sets(Set.empty(), Set#{x}, Set.empty())

    pub def containsRVar(x: RVar, fv: FreeVarSet): Bool = match fv {
        case Sets(_, r, _) => Set.memberOf(x, r)
        case Empty => false
    }

    pub def removeRVars(s: Set[RVar], fv: FreeVarSet): FreeVarSet = match fv {
        case Sets(t, r, e) => Sets(t, Set.difference(r, s), e)
        case Empty => Empty
    }

    pub def singleEVar(x: EVar): FreeVarSet =
        Sets(Set.empty(), Set.empty(), Set#{x})

    pub def containsEVar(x: EVar, fv: FreeVarSet): Bool = match fv {
        case Sets(_, _, e) => Set.memberOf(x, e)
        case Empty => false
    }

    pub def removeEVars(s: Set[EVar], fv: FreeVarSet): FreeVarSet = match fv {
        case Sets(t, r, e) => Sets(t, r, Set.difference(e, s))
        case Empty => Empty
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
