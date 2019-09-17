//! `pest_consume` extends [pest] to make it easy to consume a pest parse tree.
//!
//! # Motivation
//!
//! When using [pest] to write a parser, one has to traverse the resulting untyped parse tree
//! by hand to extract the data that will be used by the rest of the application.
//! This usually makes code that is error-prone, difficult to read, and often breaks when the grammar is updated.
//!
//! `pest_consume` strives to make this phase of parsing easier, cleaner, and more robust.
//!
//! Features of `pest_consume` include:
//! - strong types;
//! - consume parse nodes using an intuitive syntax;
//! - easy error handling;
//! - you won't ever need to write `.into_inner().next().unwrap()` again.
//!
//! # Implementing a parser
//!
//! Let's start with a pest grammar:
//!
//! `grammar.pest`:
//! ```text
//! field = { (ASCII_DIGIT | "." | "-")+ }
//! record = { field ~ ("," ~ field)* }
//! file = { SOI ~ (record ~ ("\r\n" | "\n"))* ~ EOI }
//! ```
//!
//! and a pest parser:
//!
//! ```skip
//! // Construct the first half of the parser using pest as usual.
//! #[derive(Parser)]
//! #[grammar = "csv.pest"]
//! struct CSVParser;
//! ```
//!
//! TODO
//! for the other half, we define an impl with attribute
//! for each rule, a method; note the output type
//!
//!
//!
//! # Complete example
//!
//! Here is the [CSV example from the doc](https://pest.rs/book/examples/csv.html),
//! using `pest_consume`.
//!
//! `grammar.pest`:
//! ```text
//! field = { (ASCII_DIGIT | "." | "-")+ }
//! record = { field ~ ("," ~ field)* }
//! file = { SOI ~ (record ~ ("\r\n" | "\n"))* ~ EOI }
//! ```
//!
//! `main.rs`:
//! ```
//! use pest_consume::{match_nodes, Error, Parser};
//!
//! type Result<T> = std::result::Result<T, Error<Rule>>;
//! type Node<'i> = pest_consume::Node<'i, Rule, ()>;
//!
//! // Construct the first half of the parser using pest as usual.
//! #[derive(Parser)]
//! #[grammar = "../examples/csv/csv.pest"]
//! struct CSVParser;
//!
//! // This is the other half of the parser, using pest_consume.
//! #[pest_consume::parser]
//! impl CSVParser {
//!     fn EOI(_input: Node) -> Result<()> {
//!         Ok(())
//!     }
//!     fn field(input: Node) -> Result<f64> {
//!         input
//!             .as_str()
//!             .parse::<f64>()
//!             // The error will point to the part of the input that caused it
//!             .map_err(|e| input.error(e.to_string()))
//!     }
//!     fn record(input: Node) -> Result<Vec<f64>> {
//!         Ok(match_nodes!(input.children();
//!             // Checks that the children all match the rule `field`, and applies
//!             // the appropriate parsing method. This is strongly typed: for example
//!             // mixing up `record` and `field` would cause a type error.
//!             [field(fields)..] => fields.collect(),
//!         ))
//!     }
//!     fn file(input: Node) -> Result<Vec<Vec<f64>>> {
//!         Ok(match_nodes!(input.children();
//!             [record(records).., EOI(_)] => records.collect(),
//!         ))
//!     }
//! }
//!
//! fn parse_csv(input_str: &str) -> Result<Vec<Vec<f64>>> {
//!     let inputs = CSVParser::parse(Rule::file, input_str)?;
//!     Ok(match_nodes!(<CSVParser>; inputs;
//!         [file(e)] => e,
//!     ))
//! }
//!
//! fn main() -> std::result::Result<(), Box<dyn std::error::Error>> {
//!     let parsed = parse_csv("-20, 12\n42, 0")?;
//!     let mut sum = 0.;
//!     for record in parsed {
//!         for field in record {
//!             sum += field;
//!         }
//!     }
//!     assert_eq!(sum, 34.0);
//!     Ok(())
//! }
//! ```
//!
//! There are several things to note:
//! - we use two macros provided by `pest_consume`: `parser` and `match_nodes`;
//! - there is one `fn` item per (non-silent) rule in the grammar;
//! - we associate an output type to every rule;
//! - there is no need to fiddle with `.into_inner()`, `.next()` or `.unwrap()`, as is common when using pest
//!
//! # How it works
//!
//! The main types of this crate ([Node], [Nodes] and [Parser]) are mostly wrappers around
//! corresponding [pest] types.
//!
//! The `pest_consume::parser` macro does almost nothing when not using advanced features;
//! most of the magic happens in `match_nodes`.
//! `match_nodes` desugars rather straightforwardly into calls to the `fn` items corresponding to
//! the rules matched on.
//! For example:
//! ```ignore
//! match_nodes!(input.children();
//!     [field(fields)..] => fields.collect(),
//! )
//! ```
//! desugars into:
//! ```ignore
//! let nodes = { input.children() };
//! if ... { // check that all rules in `nodes` are the `field` rule
//!     let fields = nodes
//!         .map(|node| Self::field(node)) // Recursively parse children nodes
//!         ... // Propagate errors
//!     { fields.collect() }
//! } else {
//!     ... // error because we got unexpected rules
//! }
//! ```
//!
//! # Advanced features
//!
//! TODO
//!
//! - user data
//! - rule aliasing
//! - rule shortcutting
//!
//! # Compatibility
//!
//! Works with Rust >= 1.37.
//!
//! Needs Rust >= 1.37 because it uses
//! [this feature](https://blog.rust-lang.org/2019/08/15/Rust-1.37.0.html#referring-to-enum-variants-through-type-aliases).
//! If there is demand for older versions of Rust, we might be able to work around that.
//!
//! Works with older nightly Rust, with `#![feature(type_alias_enum_variants)]`.
//!
//! # License
//!
//! Licensed under either of
//!
//!  * Apache License, Version 2.0 (http://www.apache.org/licenses/LICENSE-2.0)
//!  * MIT license (http://opensource.org/licenses/MIT)
//!
//! at your option.
//!
//! # Contribution
//!
//! Unless you explicitly state otherwise, any contribution intentionally submitted
//! for inclusion in the work by you, as defined in the Apache-2.0 license, shall be
//! dual licensed as above, without any additional terms or conditions.
//!
//! [pest]: https://pest.rs

pub use pest::error::Error;
use pest::Parser as PestParser;
use pest::RuleType;
pub use pest_derive::Parser;

#[proc_macro_hack::proc_macro_hack]
pub use pest_consume_macros::match_nodes;
pub use pest_consume_macros::parser;

mod node {
    use super::Parser;
    use pest::error::{Error, ErrorVariant};
    use pest::iterators::{Pair, Pairs};
    use pest::Parser as PestParser;
    use pest::{RuleType, Span};

    /// A node of the parse tree.
    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub struct Node<'input, Rule: RuleType, Data> {
        pair: Pair<'input, Rule>,
        user_data: Data,
    }

    /// Iterator over `Node`s. It is created by `Node::children` or `Nodes::new`.
    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub struct Nodes<'input, Rule: RuleType, Data> {
        pairs: Pairs<'input, Rule>,
        span: Span<'input>,
        user_data: Data,
    }

    impl<'i, R: RuleType, D> Node<'i, R, D> {
        pub fn new(pair: Pair<'i, R>, user_data: D) -> Self {
            Node { pair, user_data }
        }
        /// Create an error that points to the span of the node.
        pub fn error(&self, message: String) -> Error<R> {
            Error::new_from_span(
                ErrorVariant::CustomError { message },
                self.as_span(),
            )
        }
        #[doc(hidden)]
        /// Construct a node with the provided pair, passing the user data along.
        pub fn with_pair(&self, new_pair: Pair<'i, R>) -> Self
        where
            D: Clone,
        {
            Node {
                pair: new_pair,
                user_data: self.user_data.clone(),
            }
        }
        #[doc(hidden)]
        /// If the node has exactly one child, return that child; otherwise return None.
        pub fn single_child(&self) -> Option<Self>
        where
            D: Clone,
        {
            let mut children = self.children();
            if let Some(child) = children.next() {
                if children.next().is_none() {
                    return Some(child);
                }
            }
            None
        }
        /// Return an iterator over the children of this node
        // Can't use `-> impl Iterator` because of weird lifetime limitations
        // (see https://github.com/rust-lang/rust/issues/61997).
        pub fn children(&self) -> Nodes<'i, R, D>
        where
            D: Clone,
        {
            Nodes {
                pairs: self.as_pair().clone().into_inner(),
                span: self.as_span(),
                user_data: self.user_data.clone(),
            }
        }

        pub fn user_data(&self) -> &D {
            &self.user_data
        }
        pub fn as_pair(&self) -> &Pair<'i, R> {
            &self.pair
        }
        pub fn into_pair(self) -> Pair<'i, R> {
            self.pair
        }
        pub fn as_span(&self) -> Span<'i> {
            self.pair.as_span()
        }
        pub fn as_str(&self) -> &'i str {
            self.pair.as_str()
        }
        pub fn as_rule(&self) -> R {
            self.pair.as_rule()
        }
        #[doc(hidden)]
        pub fn as_aliased_rule<C>(&self) -> C::AliasedRule
        where
            C: Parser<Rule = R>,
            <C as Parser>::Parser: PestParser<R>,
        {
            C::rule_alias(self.as_rule())
        }
    }

    impl<'i, R: RuleType, D> Nodes<'i, R, D> {
        /// `input` must be the _original_ input that `pairs` is pointing to.
        pub fn new(input: &'i str, pairs: Pairs<'i, R>, user_data: D) -> Self {
            let span = Span::new(input, 0, input.len()).unwrap();
            Nodes {
                pairs,
                span,
                user_data,
            }
        }
        /// Create an error that points to the span of the node.
        pub fn error(&self, message: String) -> Error<R> {
            Error::new_from_span(
                ErrorVariant::CustomError { message },
                self.span.clone(),
            )
        }
        #[doc(hidden)]
        pub fn aliased_rules<C>(&self) -> Vec<C::AliasedRule>
        where
            D: Clone,
            C: Parser<Rule = R>,
            <C as Parser>::Parser: PestParser<R>,
        {
            self.clone().map(|p| p.as_aliased_rule::<C>()).collect()
        }
        /// Construct a node with the provided pair, passing the user data along.
        fn with_pair(&self, pair: Pair<'i, R>) -> Node<'i, R, D>
        where
            D: Clone,
        {
            Node::new(pair, self.user_data.clone())
        }

        pub fn as_pairs(&self) -> &Pairs<'i, R> {
            &self.pairs
        }
        pub fn into_pairs(self) -> Pairs<'i, R> {
            self.pairs
        }
    }

    impl<'i, R, D> Iterator for Nodes<'i, R, D>
    where
        R: RuleType,
        D: Clone,
    {
        type Item = Node<'i, R, D>;

        fn next(&mut self) -> Option<Self::Item> {
            let child_pair = self.pairs.next()?;
            let child = self.with_pair(child_pair);
            Some(child)
        }
    }

    impl<'i, R, D> DoubleEndedIterator for Nodes<'i, R, D>
    where
        R: RuleType,
        D: Clone,
    {
        fn next_back(&mut self) -> Option<Self::Item> {
            let child_pair = self.pairs.next_back()?;
            let child = self.with_pair(child_pair);
            Some(child)
        }
    }

    impl<'i, R: RuleType, D> std::fmt::Display for Node<'i, R, D> {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            self.pair.fmt(f)
        }
    }

    impl<'i, R: RuleType, D> std::fmt::Display for Nodes<'i, R, D> {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            self.pairs.fmt(f)
        }
    }
}

pub use node::{Node, Nodes};

/// Used by the macros.
/// Do not implement manually.
pub trait Parser {
    type Rule: RuleType;
    type AliasedRule: RuleType;
    type Parser: PestParser<Self::Rule>;

    fn rule_alias(rule: Self::Rule) -> Self::AliasedRule;
    fn allows_shortcut(rule: Self::Rule) -> bool;

    /// Parses a `&str` starting from `rule`
    fn parse<'i>(
        rule: Self::Rule,
        input_str: &'i str,
    ) -> Result<Nodes<'i, Self::Rule, ()>, Error<Self::Rule>> {
        Self::parse_with_userdata(rule, input_str, ())
    }

    /// Parses a `&str` starting from `rule`, carrying `user_data` through the parser methods.
    fn parse_with_userdata<'i, D>(
        rule: Self::Rule,
        input_str: &'i str,
        user_data: D,
    ) -> Result<Nodes<'i, Self::Rule, D>, Error<Self::Rule>> {
        let pairs = Self::Parser::parse(rule, input_str)?;
        Ok(Nodes::new(input_str, pairs, user_data))
    }
}
