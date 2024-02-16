pub use fsyn_proc::{fsyn_anon_lambda as l, fsyn_anon_lambda_fn as lf};

#[cfg(test)]
mod tests {
    use super::*;

    fn add(a: i32, b: i32) -> i32 {
        a + b
    }

    fn map<T, U, F: FnMut(T) -> U>(v: Vec<T>, f: F) -> Vec<U> {
        v.into_iter().map(f).collect()
    }

    fn map2<T, U, X, F: FnMut(T, U) -> X>(v: Vec<(T, U)>, mut f: F) -> Vec<X> {
        v.into_iter().map(|(t, u)| f(t, u)).collect()
    }

    #[lf]
    #[test]
    fn it_works() {
        assert_eq!(map(vec![(0, 1), (1, 2)], _.1), vec![1, 2]);
        assert_eq!(map(vec![1, 2, 3], add(_, 1)), vec![2, 3, 4]);
        assert_eq!(map2(vec![(0, 1), (1, 2)], _ + _), vec![1, 3]);
        assert_eq!(map2(vec![(0, 1), (1, 2)], add(_, _)), vec![1, 3]);
    }
}
