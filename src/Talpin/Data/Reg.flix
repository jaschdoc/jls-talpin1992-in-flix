mod Talpin.Data {

// Region is already used in Flix
pub enum Reg with Eq, Order {
    case RegConst(Int32)
    case RegVar(String)
}

instance ToString[Reg] {
    pub def toString(p: Reg): String = Reg.toString(p)
}

mod Reg {
    use Talpin.Data.FreeVarSet

    pub def toString(p: Reg): String = match p {
        case RegConst(i) => "cst#${i}"
        case RegVar(x) => "${x}"
    }

    pub def fv(p: Reg): FreeVarSet = match p {
        case RegConst(_) => FreeVarSet.empty()
        case RegVar(x) => FreeVarSet.singleRVar(x)
    }

}

}
