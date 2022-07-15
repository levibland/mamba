# Mamba 🐍
An interpreter for the Mamba language.

## What is Mamba?
Mamba is a simple scripting language with syntax inspired by Rust and JavaScript.

## Instructions
### Building & Testing

```bash
$ cargo build
$ cargo test
```

### Using the Mamba Interpreter

```bash
$ cargo run --release -- --src examples/functions.mb
```

### An Example Program
```rust
fn test(x, y) {
    return x + y;
}

println(test(1, 2));
```

The above code uses the builtin `println` function to print 3.

More examples can be found in [/examples](https://github.com/levibland/mamba/tree/master/examples)