mod Talpin.Data {

pub enum RuntimeEffect with Eq, Order, ToString {
    case Init(RefLoc)
    case Read(RefLoc)
    case Write(RefLoc)

    case Eff(String)
}

}
