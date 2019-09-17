# `pest_consume`

`pest_consume` extends [pest] to make it easy to consume a pest parse tree.

Features of `pest_consume` include:
- strong types;
- consume parse nodes using an intuitive syntax;
- easy error handling;
- you won't ever need to use `Pair`s or `.into_inner().next().unwrap()` again.

# Example

Here is the [CSV example from the doc](https://pest.rs/book/examples/csv.html),
using `pest_consume`.

The pest grammar file contains:
```rust
field = { (ASCII_DIGIT | "." | "-")+ }
record = { field ~ ("," ~ field)* }
file = { SOI ~ (record ~ ("\r\n" | "\n"))* ~ EOI }
```

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
            .map_err(|e| input.error(e.to_string()))
    }
    fn record(input: Node) -> Result<Vec<f64>> {
        Ok(match_nodes!(input.children();
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

fn main() {
    let parsed = parse_csv("-273.15, 12\n42, 0").unwrap();
    let mut sum = 0.;
    for record in parsed {
        for field in record {
            sum += field;
        }
    }
    println!("{}", sum);
}
```

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
