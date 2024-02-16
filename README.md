Scala-like Placeholder Syntax for Anonymous Functions, in rust, via proc macros.

See [Scala docs](https://www.scala-lang.org/files/archive/spec/2.11/06-expressions.html#placeholder-syntax-for-anonymous-functions) on how they work.

# Example usage

```rust
use fsyn::lf;

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
```