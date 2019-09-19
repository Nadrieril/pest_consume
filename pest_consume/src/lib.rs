#![doc(html_root_url = "https://docs.rs/pest_consume/1.0.1")]

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
//! Let's start with a pest grammar for parsing CSV files:
//!
//! ```text
//! field = { (ASCII_DIGIT | "." | "-")+ }
//! record = { field ~ ("," ~ field)* }
//! file = { SOI ~ (record ~ ("\r\n" | "\n"))* ~ EOI }
//! ```
//!
//! and the corresponding pest parser:
//!
//! ```no_run
//! use pest_consume::Parser;
//! // Construct the first half of the parser using pest as usual.
//! #[derive(Parser)]
//! #[grammar = "../examples/csv/csv.pest"]
//! struct CSVParser;
//! # fn main() {}
//! ```
//!
//! To complete the parser, define an `impl` block with the `pest_consume::parser` attribute,
//! and for each (non-silent) rule of the grammar a method with the same name.
//! Note how we chose an output type for each rule.
//!
//! ```ignore
//! use pest_consume::Error;
//! type Result<T> = std::result::Result<T, Error<Rule>>;
//! type Node<'i> = pest_consume::Node<'i, Rule, ()>;
//!
//! // This is the other half of the parser, using pest_consume.
//! #[pest_consume::parser]
//! impl CSVParser {
//!     fn EOI(_input: Node) -> Result<()> {
//!         Ok(())
//!     }
//!     fn field(input: Node) -> Result<f64> {
//!         ...
//!     }
//!     fn record(input: Node) -> Result<Vec<f64>> {
//!         ...
//!     }
//!     fn file(input: Node) -> Result<Vec<Vec<f64>>> {
//!         ...
//!     }
//! }
//! ```
//!
//! This will implement [`Parser`] for your type, so that [`Parser::parse`] can be called on it.
//! We can now define a complete parser that returns a structured result:
//! ```ignore
//! fn parse_csv(input_str: &str) -> Result<Vec<Vec<f64>>> {
//!     // Parse the input into `Nodes`
//!     let inputs = CSVParser::parse(Rule::file, input_str)?;
//!     // There should be a single root node in the parsed tree
//!     let input = inputs.single()?;
//!     // Consume the `Node` recursively into the final value
//!     CSVParser::file(input)
//! }
//! ```
//!
//! It only remains to implement parsing for each rule.
//! The simple cases are when the rule has no children.
//! In this case, we usually only care about the captured string, accessible using [`Node::as_str`].
//! ```ignore
//!     fn field(input: Node) -> Result<f64> {
//!         // Get the string captured by this node
//!         input.as_str()
//!             // Convert it into the type we want
//!             .parse::<f64>()
//!             // In case of  an error, we use `Node::error` to link the error
//!             // with the part of the input that caused it
//!             .map_err(|e| input.error(e))
//!     }
//! ```
//!
//! When the rule has children, the [`match_nodes`] macro provides a
//! typed way to parse the children.
//! [`match_nodes`] uses a syntax similar to slice patterns, and allows for several branches like in
//! a `match` expression.
//!
//! We specify for each branch the expected rules of the children, and the macro will recursively consume the
//! children and make the result accessible to the body of the branch.
//! A special `..` syntax indicates a variable-length pattern:
//! it will match zero or more children with the given rule, and provide an iterator with the result.
//!
//! ```ignore
//! use pest_consume::match_nodes;
//! ...
//!     fn record(input: Node) -> Result<Vec<f64>> {
//!         // Checks that the children all match the rule `field`, and captures
//!         // the parsed children in an iterator. `fds` implements
//!         // `Iterator<Item=f64>` here.
//!         Ok(match_nodes!(input.into_children();
//!             [field(fds)..] => fds.collect(),
//!         ))
//!     }
//! ```
//!
//! The case of the `file` rule is similar.
//!
//! # Examples
//!
//! Some toy examples can be found in [the `examples/` directory][examples].
//! A real-world example can be found in [dhall-rust][dhall-rust-parser].
//!
//! # How it works
//!
//! The main types of this crate ([`Node`], [`Nodes`] and [`Parser`]) are mostly wrappers around
//! corresponding [pest] types, respectively `Pair`, `Pairs` and `Parser`.
//! If needed, the wrapped type can be accessed, but that should rarely be necessary.
//!
//! The [`pest_consume::parser`][`parser`] macro implements the [`Parser`] trait for your type, and enables
//! some advanced features, like precedence climbing and rule aliasing.
//! A lot of the magic actually happens in [`match_nodes`]; see there for details.
//!
//! # Advanced features
//!
//! ## Precedence climbing
//!
//! `pest_consume` makes it easy to leverage the precedence climbing facilities provided by `pest`.
//! The special `prec_climb` attribute can be placed on a rule method, and that method will automatically
//! use precedence climbing.
//!
//! The attribute needs two arguments: the first is the name of the rule of the non-operator children of this rule.
//! For example, if the rule definition is `expr = { term ~ (operator ~ term)* }`, the non-operator children of the rule
//! `expr` have the rule `term`.
//! The second argument is an expression that references a `pest` [`PrecClimber`].
//! The usual way is to use [`lazy_static`] to define the [`PrecClimber`], to ensure we only construct it once.
//!
//! Finally, the method itself must take three arguments: a [`Node`] containing the matched operator, and a left and a right value.
//! The type of the values, the return type of the rule, and the return type of the children's rule must all be the same.
//!
//! ```ignore
//! lazy_static::lazy_static! {
//!     static ref PRECCLIMBER: PrecClimber<Rule> = PrecClimber::new(
//!         vec![
//!             pcl::Operator::new(Rule::plus, pcl::Assoc::Left),
//!             pcl::Operator::new(Rule::minus, pcl::Assoc::Left),
//!             pcl::Operator::new(Rule::times, pcl::Assoc::Left),
//!         ]
//!     );
//! }
//! ...
//! #[pest_consume::parser]
//! impl MathParser {
//!     ...
//!     #[prec_climb(term, PRECCLIMBER)]
//!     fn expr(l: i64, op: Node, r: i64) -> Result<i64> {
//!         use Rule::*;
//!         match op.as_rule() {
//!             plus => Ok(l + r),
//!             minus => Ok(l - r),
//!             times => Ok(l * r),
//!             r => Err(op.error(format!("Rule {:?} isn't an operator", r)))?,
//!         }
//!     }
//!     ...
//! }
//! ```
//!
//! For a full example using precedence climbing, see [here][prec_climbing-example].
//!
//! ## Passing user data through the parser
//!
//! Sometimes, you may want to pass some data to the consuming methods.
//! You could want access to configuration data, or to a store for
//! [string interning](https://en.wikipedia.org/wiki/String_interning).
//!
//! This is easy to do: by using [`Parser::parse_with_userdata`] instead of [`Parser::parse`],
//! you can provide some data that will be stored in the returned [`Nodes`] value, and accessible at any point
//! during parsing via [`Node::user_data`].
//!
//! The type of the user data is the second type parameter in the types of `Node<'i, Rule, Data>` and `Nodes<'i, Rule, Data>`.
//! The data needs to be `Clone`, and will be cloned often so it should be cheap to clone.
//! A common usage is to have this data be a reference, which are free to clone.
//!
//! If you need mutable access to some data, use [`Cell`] or [`RefCell`].
//!
//! ```ignore
//! struct AppSettings { ... }
//!
//! // We changed the type alias to include the type of the user data.
//! type Node<'i, 'a> = pest_consume::Node<'i, Rule, &'a AppSettings>;
//!
//! fn parse_with_settings(
//!     input_str: &str,
//!     settings: &AppSettings
//! ) -> Result<Vec<Vec<f64>>> {
//!     // Parse the input into `Nodes`, passing the provided settings along
//!     let inputs = CSVParser::parse_with_userdata(
//!         Rule::file,
//!         input_str,
//!         settings
//!     )?;
//!     let input = inputs.single()?;
//!     CSVParser::file(input)
//! }
//!
//! #[pest_consume::parser]
//! impl CSVParser {
//!     fn field(input: Node) -> Result<f64> {
//!         // The settings can be retrieved from any Node.
//!         let settings = input.user_data();
//!         if settings.do_the_thing {
//!             ...
//!         }
//!         ...
//!     }
//!     ...
//! }
//! ```
//!
//! # Rule aliasing
//!
//! TODO
//!
//! # Rule shortcutting
//!
//! TODO
//!
//! # Compatibility
//!
//! Works with rust >= 1.37.
//!
//! Needs rust >= 1.37 because it uses
//! [this feature](https://blog.rust-lang.org/2019/08/15/Rust-1.37.0.html#referring-to-enum-variants-through-type-aliases).
//! If there is demand for older versions of Rust, we might be able to work around that.
//!
//! Works with older nightly Rust, with `#![feature(type_alias_enum_variants)]`.
//!
//! # License
//!
//! Licensed under either of
//!
//!  * Apache License, Version 2.0 ([http://www.apache.org/licenses/LICENSE-2.0](http://www.apache.org/licenses/LICENSE-2.0))
//!  * MIT license ([http://opensource.org/licenses/MIT](http://opensource.org/licenses/MIT))
//!
//! at your option.
//!
//! # Contribution
//!
//! Unless you explicitly state otherwise, any contribution intentionally submitted
//! for inclusion in the work by you, as defined in the Apache-2.0 license, shall be
//! dual licensed as above, without any additional terms or conditions.
//!
//! [`parser`]: https://docs.rs/pest_consume_macros/1.0.1/pest_consume_macros/attr.parser.html
//! [`match_nodes`]: macro.match_nodes.html
//! [`Nodes`]: struct.Nodes.html
//! [`Node`]: struct.Node.html
//! [`Node::as_str`]: struct.Node.html#method.as_str
//! [`Node::error`]: struct.Node.html#method.as_error
//! [`Node::user_data`]: struct.Node.html#method.user_data
//! [`Parser`]: trait.Parser.html
//! [`Parser::parse`]: trait.Parser.html#method.parse
//! [`Parser::parse_with_userdata`]: trait.Parser.html#method.parse_with_userdata
//! [`Cell`]: https://doc.rust-lang.org/std/cell/struct.Cell.html
//! [`RefCell`]: https://doc.rust-lang.org/std/cell/struct.RefCell.html
//! [pest]: https://pest.rs
//! [`PrecClimber`]: https://docs.rs/pest/2.1.2/pest/prec_climber/struct.PrecClimber.html
//! [`lazy_static`]: https://crates.io/crates/lazy_static
//! [examples]: https://github.com/Nadrieril/pest_consume/tree/master/pest_consume/examples
//! [prec_climbing-example]: https://github.com/Nadrieril/pest_consume/tree/master/pest_consume/examples/prec_climbing
//! [dhall-rust-parser]: https://github.com/Nadrieril/dhall-rust/blob/master/dhall_syntax/src/parser.rs

pub use pest::error::Error;
use pest::Parser as PestParser;
use pest::RuleType;
pub use pest_derive::Parser;

/// Pattern-match on [`Nodes`] with slice-like syntax and strong types..
///
/// See [examples] and [crate-level documentation][`pest_consume`] for usage.
///
/// Example usage:
/// ```ignore
/// let nodes: Nodes<_, _> = ...:
/// match_nodes!(nodes;
///     [string(s), number(n)] => s.len() + n,
///     [ignored(_), field(fs)..] => fs.filter(|f| f > 0).count(),
/// )
/// ```
///
/// # Syntax
///
/// The macro takes an expression followed by `;`, followed by one or more branches separated by `,`.
/// Each branch has the form `[$patterns] => $body`. The body is an arbitrary expression.
/// The patterns are a comma-seperated list of `$rule_name($binder)`, each optionally followed by `..` to indicate
/// a variable-length pattern.
///
/// # How it works
///
/// `match_nodes` desugars rather straightforwardly into calls to the methods corresponding to
/// the rules matched on.
/// For example:
/// ```ignore
/// match_nodes!(input.into_children();
///     [field(fields)..] => fields.count(),
///     [string(s), number(n)] => s.len() + n,
/// )
/// ```
/// desugars roughly into:
/// ```ignore
/// let nodes = { input.into_children() };
/// if ... { // check that all rules in `nodes` are the `field` rule
///     let fields = nodes
///         .map(|node| Self::field(node)) // Recursively parse children nodes
///         ... // Propagate errors
///     { fields.collect() }
/// } else if ... { // check that the nodes has two elements, with rules `string` and `number`
///     let s = Self::string(nodes.next().unwrap())?;
///     let n = Self::number(nodes.next().unwrap())?;
///     { s.len() + n }
/// } else {
///     ... // error because we got unexpected rules
/// }
/// ```
///
/// # Notes
///
/// The macro assumes that it is used within a consumer method, and uses `Self::$method(...)` to
/// parse the input nodes.
/// To use it outside a method, you can pass it the parser struct as follows (the angle brackets are mandatory):
/// ```ignore
/// match_nodes(<CSVParser>; nodes;
///     ...
/// )
/// ```
///
/// It also assumes it can `return Err(...)` in case of errors.
///
/// [`pest_consume`]: index.html
/// [`Nodes`]: struct.Nodes.html
/// [examples]: https://github.com/Nadrieril/pest_consume/tree/master/pest_consume/examples
#[proc_macro_hack::proc_macro_hack]
pub use pest_consume_macros::match_nodes;
pub use pest_consume_macros::parser;

mod node {
    use super::Parser;
    use pest::error::{Error, ErrorVariant};
    use pest::iterators::{Pair, Pairs};
    use pest::prec_climber::PrecClimber;
    use pest::Parser as PestParser;
    use pest::{RuleType, Span};

    /// A node of the parse tree.
    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub struct Node<'input, Rule: RuleType, Data> {
        pair: Pair<'input, Rule>,
        user_data: Data,
    }

    /// Iterator over [`Node`]s. It is created by [`Node::children`] or [`Parser::parse`].
    ///
    /// [`Node`]: struct.Node.html
    /// [`Node::children`]: struct.Node.html#method.children
    /// [`Parser::parse`]: trait.Parser.html#method.parse
    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub struct Nodes<'input, Rule: RuleType, Data> {
        pairs: Pairs<'input, Rule>,
        span: Span<'input>,
        user_data: Data,
    }

    impl<'i, R: RuleType> Node<'i, R, ()> {
        #[doc(hidden)]
        pub fn new(pair: Pair<'i, R>) -> Self {
            Node {
                pair,
                user_data: (),
            }
        }
    }
    impl<'i, R: RuleType, D> Node<'i, R, D> {
        #[doc(hidden)]
        pub fn new_with_user_data(pair: Pair<'i, R>, user_data: D) -> Self {
            Node { pair, user_data }
        }
        pub fn as_str(&self) -> &'i str {
            self.pair.as_str()
        }
        pub fn as_span(&self) -> Span<'i> {
            self.pair.as_span()
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

        /// Returns an iterator over the children of this node
        pub fn into_children(self) -> Nodes<'i, R, D> {
            let span = self.as_span();
            Nodes {
                pairs: self.pair.into_inner(),
                span,
                user_data: self.user_data,
            }
        }
        /// Returns an iterator over the children of this node
        pub fn children(&self) -> Nodes<'i, R, D>
        where
            D: Clone,
        {
            self.clone().into_children()
        }

        /// Create an error that points to the span of the node.
        pub fn error<S: ToString>(&self, message: S) -> Error<R> {
            Error::new_from_span(
                ErrorVariant::CustomError {
                    message: message.to_string(),
                },
                self.as_span(),
            )
        }

        pub fn user_data(&self) -> &D {
            &self.user_data
        }
        pub fn into_user_data(self) -> D {
            self.user_data
        }
        pub fn as_pair(&self) -> &Pair<'i, R> {
            &self.pair
        }
        pub fn into_pair(self) -> Pair<'i, R> {
            self.pair
        }
    }

    impl<'i, R: RuleType, D> Nodes<'i, R, D> {
        /// `input` must be the _original_ input that `pairs` is pointing to.
        #[doc(hidden)]
        pub(crate) fn new(
            input: &'i str,
            pairs: Pairs<'i, R>,
            user_data: D,
        ) -> Self {
            let span = Span::new(input, 0, input.len()).unwrap();
            Nodes {
                pairs,
                span,
                user_data,
            }
        }
        /// Create an error that points to the initial span of the nodes.
        /// Note that this span does not change as the iterator is consumed.
        pub fn error<S: ToString>(&self, message: S) -> Error<R> {
            Error::new_from_span(
                ErrorVariant::CustomError {
                    message: message.to_string(),
                },
                self.span.clone(),
            )
        }
        /// Returns the only element if there is only one element.
        pub fn single(mut self) -> Result<Node<'i, R, D>, Error<R>> {
            match (self.pairs.next(), self.pairs.next()) {
                (Some(pair), None) => {
                    Ok(Node::new_with_user_data(pair, self.user_data))
                }
                (first, second) => {
                    let node_rules: Vec<_> = first
                        .into_iter()
                        .chain(second)
                        .chain(self.pairs)
                        .map(|p| p.as_rule())
                        .collect();

                    Err(Error::new_from_span(
                        ErrorVariant::CustomError {
                            message: format!(
                                "Expected a single node, instead got: {:?}",
                                node_rules
                            ),
                        },
                        self.span,
                    ))
                }
            }
        }
        #[doc(hidden)]
        pub fn aliased_rules<C>(
            &self,
        ) -> std::iter::Map<
            Pairs<'i, R>,
            impl FnMut(Pair<'i, R>) -> <C as Parser>::AliasedRule,
        >
        where
            C: Parser<Rule = R>,
            <C as Parser>::Parser: PestParser<R>,
        {
            self.pairs.clone().map(|p| C::rule_alias(p.as_rule()))
        }
        /// Construct a node with the provided pair, passing the user data along.
        fn with_pair(&self, pair: Pair<'i, R>) -> Node<'i, R, D>
        where
            D: Clone,
        {
            Node::new_with_user_data(pair, self.user_data.clone())
        }
        /// Performs the precedence climbing algorithm on the nodes.
        pub fn prec_climb<T, E, F1, F2>(
            self,
            climber: &PrecClimber<R>,
            mut primary: F1,
            mut infix: F2,
        ) -> Result<T, E>
        where
            D: Clone,
            F1: FnMut(Node<'i, R, D>) -> Result<T, E>,
            F2: FnMut(T, Node<'i, R, D>, T) -> Result<T, E>,
        {
            let user_data = self.user_data;
            let with_pair = |p| Node::new_with_user_data(p, user_data.clone());
            climber.climb(
                self.pairs,
                |p| primary(with_pair(p)),
                |l, p, r| infix(l?, with_pair(p), r?),
            )
        }

        pub fn user_data(&self) -> &D {
            &self.user_data
        }
        pub fn into_user_data(self) -> D {
            self.user_data
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

/// A trait that provides methods to parse strings.
/// Do not implement manually; instead use the [`parser`] macro provided by this crate.
///
/// [`parser`]: https://docs.rs/pest_consume_macros/1.0.1/pest_consume_macros/attr.parser.html
pub trait Parser {
    type Rule: RuleType;
    #[doc(hidden)]
    type AliasedRule: RuleType;
    type Parser: PestParser<Self::Rule>;

    #[doc(hidden)]
    fn rule_alias(rule: Self::Rule) -> Self::AliasedRule;
    #[doc(hidden)]
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
