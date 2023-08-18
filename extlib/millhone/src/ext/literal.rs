/// Create a generic collection at compile time:
///
/// ```rust
/// # use millhone::collection;
/// # use std::collections::HashMap;
/// #
/// let got: Vec<_> = collection![1, 2, 3];
/// let expected = vec![1, 2, 3];
/// assert_eq!(got, expected);
///
/// let got: HashMap<_, _> = collection! { 1 => 2, 3 => 4 };
/// let expected = HashMap::from([(1, 2), (3, 4)]);
/// assert_eq!(got, expected);
/// ```
///
/// Primarily intended to support tests in this library.
#[macro_export]
macro_rules! collection {
    ($($k:expr => $v:expr),* $(,)?) => {{
        use std::iter::{Iterator, IntoIterator};
        Iterator::collect(IntoIterator::into_iter([$(($k, $v),)*]))
    }};
    ($($v:expr),* $(,)?) => {{
        use std::iter::{Iterator, IntoIterator};
        Iterator::collect(IntoIterator::into_iter([$($v,)*]))
    }};
}
