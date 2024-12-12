mod Talpin {

pub eff FreshName {
    def get(): String
}

mod FreshName {
    use Add.add

    pub def runWithState(prefix: String, f: Unit -> b \ ef): b \ ef - FreshName = region local {
        let counter = Ref.fresh(local, -1);
        def next() = {
            counter |> Ref.transform(add(1));
            Ref.get(counter)
        };
        run f() with FreshName {
            def get(k) = k("${prefix}${next()}")
        }
    }

}
}
