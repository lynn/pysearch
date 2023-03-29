# pysearch

Brute-force search tool for short Python expressions (for code golf).

## Usage

Edit the search parameters at the top of `src/params.rs`, then run `cargo run --release`.

If your input/goal vectors have length ≤8, you can try `cargo run --release --features simd` (this should be faster).

You can also pass `--features rayon` to enable multithreading, it should be faster but will use more memory.

## Example

Let's golf this Python code!

```py
# 139 bytes
x = y = 0
for c in input():
    if c == 'E': x += 1
    if c == 'W': x -= 1
    if c == 'N': y += 1
    if c == 'S': y -= 1
    print(x, y)
```

It reads a string of compass direction letters and prints coordinates walking across a grid. Let's say the rules guarantee that this string will only consist of `EWNS` and no other letters.

Applying some standard golf tricks, we get:

```py
# 75 bytes
x=y=0
for c in input():x+=(c=='E')-(c=='W');y+=(c=='N')-(c=='S');print(x,y)
```

Let's introduce numbers we can do math on. This makes the code longer for now, but it'll pay off:

```py
# 80 bytes...
x=y=0
for n in map(ord,input()):x+=(n==69)-(n==87);y+=(n==78)-(n==83);print(x,y)
```

It'd be nice to golf down `x+=(n==69)-(n==87)`. We just need _any_ expression that maps the possible inputs (69, 87, 78, 83) to (1, -1, 0, 0). We ask pysearch:

```rs
pub const INPUTS: &[Input] = &[Input {
    name: "n",
    vec: &[69, 87, 78, 83],
}];

pub const GOAL: &[Num] = &[1, -1, 0, 0];
```

And it finds `n%5-3`:

```txt
$ cargo run --release --features simd
Finding length 1...
Found 10 expressions in 1.236ms.
Finding length 2...
Found 43 expressions in 2.073ms.
Finding length 3...
Found 112 expressions in 2.5362ms.
Finding length 4...
Found 496 expressions in 3.1818ms.
Finding length 5...
Found 2526 expressions in 5.7068ms.

--- Length 5 ---
n%5-3
```

So we can just write `x+=n%5-3`.

We do the same for `y`, with `GOAL = [0, 0, 1, -1]`, and it finds `1-n//2%3`. Now our code is a lot shorter.

```py
# 63 bytes 🎉
x=y=0
for n in map(ord,input()):x+=n%5-3;y+=1-n//2%3;print(x,y)
```
