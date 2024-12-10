mod Talpin.Data {

pub enum RuntimeEffect with Eq, Order, ToString {
    case Init(RefLoc)
    case Read(RefLoc)
    case Write(RefLoc)
}

mod RuntimeEffect {
    use Talpin.Data.RefLoc

    pub def getLoc(e: RuntimeEffect): RefLoc = match e {
        case Init(loc) => loc
        case Read(loc) => loc
        case Write(loc) => loc
    }

}

}
