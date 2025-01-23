mod Talpin.Data {
    use Coerce.coerce

    pub enum TVar(String) with Eq, Coerce, Order

    instance ToString[TVar] {
        pub def toString(x: TVar): String = TVar.toString(x)
    }

    mod TVar {
        pub def toString(x: TVar): String = coerce(x)
    }

}
