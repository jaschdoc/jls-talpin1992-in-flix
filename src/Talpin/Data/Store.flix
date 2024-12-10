mod Talpin.Data {

pub type alias Store = Map[RefLoc, Value]

mod Store {
    use Add.add
    use Functor.map
    use Option.getWithDefault

    use Talpin.Data.RefLoc
    use Talpin.Data.Value

    pub def store(l: RefLoc, v: Value, s: Store): Store =
        Map.insert(l, v, s)

    pub def newLoc(s: Store): RefLoc =
        s |> Map.maximumKey |> map(fst) |> getWithDefault(-1) |> add(1)

    pub def remove(l: RefLoc, s: Store): Store =
        Map.remove(l, s)

    pub def empty(): Store =
        Map.empty()

}

}
