# pest_consume

`pest_consume` extends [pest] to make it easy to consume a pest parse tree.

## Motivation

When using [pest] to write a parser, one has to traverse the resulting untyped parse tree
by hand to extract the data that will be used by the rest of the application.
This usually makes code that is error-prone, difficult to read, and often breaks when the grammar is updated.

`pest_consume` strives to make this phase of parsing easier, cleaner, and more robust.

Features of `pest_consume` include:
- strong types;
- consume parse nodes using an intuitive syntax;
- easy error handling;
- you won't ever need to write `.into_inner().next().unwrap()` again.

## Implementing a parser

Let's start with a pest grammar:

`grammar.pest`:
```
field = { (ASCII_DIGIT | "." | "-")+ }
record = { field ~ ("," ~ field)* }
file = { SOI ~ (record ~ ("\r\n" | "\n"))* ~ EOI }
```

and a pest parser:

```skip
// Construct the first half of the parser using pest as usual.
#[derive(Parser)]
#[grammar = "csv.pest"]
struct CSVParser;
```

TODO
for the other half, we define an impl with attribute
for each rule, a method; note the output type



## Complete example

Here is the [CSV example from the doc](https://pest.rs/book/examples/csv.html),
using `pest_consume`.

`grammar.pest`:
```
field = { (ASCII_DIGIT | "." | "-")+ }
record = { field ~ ("," ~ field)* }
file = { SOI ~ (record ~ ("\r\n" | "\n"))* ~ EOI }
```

`main.rs`:
```rust
use pest_consume::{match_nodes, Error, Parser};

type Result<T> = std::result::Result<T, Error<Rule>>;
type Node<'i> = pest_consume::Node<'i, Rule, ()>;

// Construct the first half of the parser using pest as usual.
#[derive(Parser)]
#[grammar = "../examples/csv/csv.pest"]
struct CSVParser;

// This is the other half of the parser, using pest_consume.
#[pest_consume::parser]
impl CSVParser {
    fn EOI(_input: Node) -> Result<()> {
        Ok(())
    }
    fn field(input: Node) -> Result<f64> {
        input
            .as_str()
            .parse::<f64>()
            // The error will point to the part of the input that caused it
            .map_err(|e| input.error(e.to_string()))
    }
    fn record(input: Node) -> Result<Vec<f64>> {
        Ok(match_nodes!(input.children();
            // Checks that the children all match the rule `field`, and applies
            // the appropriate parsing method. This is strongly typed: for example
            // mixing up `record` and `field` would cause a type error.
            [field(fields)..] => fields.collect(),
        ))
    }
    fn file(input: Node) -> Result<Vec<Vec<f64>>> {
        Ok(match_nodes!(input.children();
            [record(records).., EOI(_)] => records.collect(),
        ))
    }
}

fn parse_csv(input_str: &str) -> Result<Vec<Vec<f64>>> {
    let inputs = CSVParser::parse(Rule::file, input_str)?;
    Ok(match_nodes!(<CSVParser>; inputs;
        [file(e)] => e,
    ))
}

fn main() -> std::result::Result<(), Box<dyn std::error::Error>> {
    let parsed = parse_csv("-20, 12\n42, 0")?;
    let mut sum = 0.;
    for record in parsed {
        for field in record {
            sum += field;
        }
    }
    assert_eq!(sum, 34.0);
    Ok(())
}
```

There are several things to note:
- we use two macros provided by `pest_consume`: `parser` and `match_nodes`;
- there is one `fn` item per (non-silent) rule in the grammar;
- we associate an output type to every rule;
- there is no need to fiddle with `.into_inner()`, `.next()` or `.unwrap()`, as is common when using pest

## How it works

The main types of this crate ([Node], [Nodes] and [Parser]) are mostly wrappers around
corresponding [pest] types.

The `pest_consume::parser` macro does almost nothing when not using advanced features;
most of the magic happens in `match_nodes`.
`match_nodes` desugars rather straightforwardly into calls to the `fn` items corresponding to
the rules matched on.
For example:
```rust
match_nodes!(input.children();
    [field(fields)..] => fields.collect(),
)
```
desugars into:
```rust
let nodes = { input.children() };
if ... { // check that all rules in `nodes` are the `field` rule
    let fields = nodes
        .map(|node| Self::field(node)) // Recursively parse children nodes
        ... // Propagate errors
    { fields.collect() }
} else {
    ... // error because we got unexpected rules
}
```

## Advanced features

TODO

- user data
- rule aliasing
- rule shortcutting

## Compatibility

Works with Rust >= 1.37.

Needs Rust >= 1.37 because it uses
[this feature](https://blog.rust-lang.org/2019/08/15/Rust-1.37.0.html#referring-to-enum-variants-through-type-aliases).
If there is demand for older versions of Rust, we might be able to work around that.

Works with older nightly Rust, with `#![feature(type_alias_enum_variants)]`.

## License

Licensed under either of

 * Apache License, Version 2.0 (http://www.apache.org/licenses/LICENSE-2.0)
 * MIT license (http://opensource.org/licenses/MIT)

at your option.

## Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be
dual licensed as above, without any additional terms or conditions.

[pest]: https://pest.rs

License: MIT OR Apache-2.0
