mod Talpin.Data {
    use Coerce.coerce

    pub enum RVar(String) with Eq, Coerce, Order

    instance ToString[RVar] {
        pub def toString(x: RVar): String = RVar.toString(x)
    }

    mod RVar {
        pub def toString(x: RVar): String = coerce(x)
    }

}
