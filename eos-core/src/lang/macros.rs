macro_rules! bail {
    ($pos:expr, $err:expr $(,)?) => {
        return $crate::lang::private::Err($crate::lang::error::Error {
            pos: $pos,
            kind: $err.into(),
        })
    };
}
