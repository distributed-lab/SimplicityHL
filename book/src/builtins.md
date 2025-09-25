# Builtin functions

## Bounded loop

Run a function repeatedly with a bounded counter. The loop stops early when the function returns a successful value.

- Signature: `for_while::<f>(initial_accumulator: A, readonly_context: C) -> Either<B, A>`
- Loop body: `fn f(acc: A, ctx: C, counter: uN) -> Either<B, A>` where `N ∈ {1, 2, 4, 8, 16}`

Example: stop when `counter == 10`.

```rust
fn stop_at_10(acc: (), _: (), i: u8) -> Either<u8, ()> {
    match jet::eq_8(i, 10) {
        true => Left(i),   // success → exit loop
        false => Right(acc), // continue with same accumulator
    }
}

fn main() {
    let out: Either<u8, ()> = for_while::<stop_at_10>((), ());
    assert!(jet::eq_8(10, unwrap_left::<()>(out)));
}
```

## List folding

Fold a list of bounded length by repeatedly applying a function.

- Signature: `fold::<f, N>(list: List<E, N>, initial_accumulator: A) -> A`
- Fold step: `fn f(element: E, acc: A) -> A`
- Note: `N` is a power of two; lists hold fewer than `N` elements.

Example: sum a list of 32-bit integers.

```rust
fn sum(elt: u32, acc: u32) -> u32 {
    let (_, acc): (bool, u32) = jet::add_32(elt, acc);
    acc
}

fn main() {
    let xs: List<u32, 8> = list![1, 2, 3];
    let s: u32 = fold::<sum, 8>(xs, 0);
    assert!(jet::eq_32(s, 6));
}
```

## Array folding

Fold a fixed-size array by repeatedly applying a function.

- Signature: `array_fold::<f, N>(array: [E; N], initial_accumulator: A) -> A`
- Fold step: `fn f(element: E, acc: A) -> A`

Example: sum an array of 7 elements.

```rust
fn sum(elt: u32, acc: u32) -> u32 {
    let (_, acc): (bool, u32) = jet::add_32(elt, acc);
    acc
}

fn main() {
    let arr: [u32; 7] = [1, 2, 3, 4, 5, 6, 7];
    let sum: u32 = array_fold::<sum, 7>(arr, 0);
    assert!(jet::eq_32(sum, 28));
}
```
