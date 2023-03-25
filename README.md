# pysearch

Brute-force search tool for short Python expressions (for code golf).

## Usage

Edit the search parameters at the top of `src/params.rs`, then run `cargo run --release`.

## Example

I wrote this program to find a short expression that computes the next number in [this code golf problem](http://golf.shinh.org/p.rb?reversed+even+or+odd+first).

I had an answer of the form:

```py
n=x=input()
exec"print x;x=expression;"*n
```

and wanted to find a shorter `expression` in terms of `n` and `x`.

- When `n` is 100, it should map `x` like so: 100→98, 98→96 … 2→99, 99→97 ….
- When `n` is 53, it should map `x` like so: 53→51 … 1→52 … 20→18 … 4→2.

(These eight points to "sample" the behavior of the desired equation at are chosen somewhat ad-hoc. Basically I add a data point when the search program outputs invalid fits.)

I express this constraint in the search program as a sort of pointwise vector equation:

```rs
pub const INPUTS: [Input; 2] = [
    Input {
        name: "n",
        vec: [100, 100, 100, 100, 53, 53, 53, 53],
    },
    Input {
        name: "x",
        vec: [100, 98, 2, 99, 53, 1, 20, 4],
    },
];

pub const GOAL: Nums = [98, 96, 99, 97, 51, 52, 18, 2];
```

The program collects all valid expressions over {`n`, `x`, some integer literals} up to the given length, and spits out the ones that match `goal`:

```txt
Finding length 1...
Finding length 2...
Finding length 3...
Finding length 4...
Finding length 5...
Finding length 6...
Finding length 7...
Finding length 8...
Finding length 9...
Finding length 10...
Finding length 11...

--- Length 10 ---
~1%x or~-n

--- Length 11 ---
~1%x or~n%n
(x-2or-1)%n
```

And sure enough, filling in `x=-2%x or~-n` yields a winning 41-byte Python submission.

## TODO

Support `and`, and make the whole "keyword operator" situation a little less ugly.

Support comparison operators. Python chaining syntax makes this a bit complex.
