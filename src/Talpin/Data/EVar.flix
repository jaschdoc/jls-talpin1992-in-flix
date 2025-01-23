mod Talpin.Data {
    use Coerce.coerce

    pub enum EVar(String) with Eq, Coerce, Order

    instance ToString[EVar] {
        pub def toString(x: EVar): String = EVar.toString(x)
    }

    mod EVar {
        pub def toString(x: EVar): String = coerce(x)
    }

}
