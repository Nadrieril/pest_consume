/// Pattern-match on [`Nodes`] with slice-like syntax and strong types..
///
/// See [examples] and [crate-level documentation][`pest_consume`] for usage.
///
/// Example usage:
/// ```ignore
/// let nodes: Nodes<_, _> = ...:
/// match_nodes!(nodes;
///     [string(s), number(n)] => s.len() + n,
///     [_, field(fs)..] => fs.filter(|f| f > 0).count(),
/// )
/// ```
///
/// # Syntax
///
/// The macro takes an expression followed by `;`, followed by one or more branches separated by `,`.
/// Each branch has the form `[$patterns] => $body`. The body is an arbitrary expression.
/// The patterns are a comma-seperated list of either `$rule_name($binder)` or just `$binder`, each
/// optionally followed by `..` to indicate a variable-length pattern.
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
///     { fields.count() }
/// } else if ... { // check that the nodes has two elements, with rules `string` and `number`
///     let s = Self::string(nodes.next().unwrap())?;
///     let n = Self::number(nodes.next().unwrap())?;
///     { s.len() + n }
/// } else {
///     ... // error because we got unexpected rules
/// }
/// ```
///
/// # Tags
///
/// `match_nodes` supports matching with tags:
///
/// ```ignore
/// match_nodes!(input.into_children();
///     [tag1 # string(s).., number(n)] => { ... },
/// )
/// ```
///
/// # Matching raw nodes
///
/// Sometimes you may want to manipulate `Node`s directly. For that, just omit a rule name when
/// matching:
/// ```ignore
/// match_nodes!(input.into_children();
///     [string(s), node] => { ... },
/// )
/// ```
/// Here the first node will be parsed using the `string` rule, but the second node will be
/// returned as-is. This also supports the `..` syntax, which returns an iterator of `Node`s.
///
/// This can come useful when `pest_consume` isn't powerful enough for your use-case, for example
/// if you want the ability to choose from multiple parsing functions for the same rule. This can
/// usually be avoided by using some [advanced features] or tweaking the grammar, but if not you
/// can always fall back to manipulating `Node`s by hand.
///
/// # Variable-length patterns
///
/// Variable-length patterns (`rule(binder)..`) are special: they match any number of nodes with
/// the given rule, and the `binder` is bound to an iterator of the parsed results.
///
/// Multiple variable-length patterns match greedily: in `[rule(x).., rule(y)..]`, `y` will always
/// be empty because all the elements were matched by `x`.
///
/// Subtlety with trailing patterns: single trailing patterns are correctly handled, e.g.
/// `[rule(many).., rule(x), rule(y)]` works as expected. This doesn't apply to patterns in the
/// middle: `[rule(many1).., rule(x), other_rule(many2)..]` will always fail because `many1` will
/// have consumed all the `rule` nodes.
///
/// # Or-patterns
///
/// `match_nodes` supports a simple form of or-patterns:
///
/// ```ignore
/// match_nodes!(input.into_children();
///     [number(x), boolean(b)] | [boolean(b), number(x)] => { ... },
/// )
/// ```
///
/// This is implemented by simply duplicating the branch body.
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
/// [advanced features]: advanced_features/index.html
/// [`Nodes`]: struct.Nodes.html
/// [examples]: https://github.com/Nadrieril/pest_consume/tree/master/pest_consume/examples
// We wrap the proc-macro in a macro here because I want to write the doc in this crate.
#[macro_export]
macro_rules! match_nodes {
    ($($x:tt)*) => {
        $crate::match_nodes_!($($x)*)
    };
}
pub use pest_consume_macros::match_nodes as match_nodes_;

// Reexport
pub use itertools::Itertools;

/// The trait that powers the `match_nodes` macro. Exposed to make `match_nodes` testable and
/// usable outside `pest`. It and its siblings are very ad-hoc for macro purposes and will break
/// semver.
pub trait NodeList<M: NodeMatcher> {
    type Node;
    /// The data for the next step.
    type NodeNamer: NodeNamer<M>;
    fn consume(self) -> (Vec<Self::Node>, Self::NodeNamer);
}

/// Sibling trait to `NodeList`. The separate trait is needed so we can guide inference in macros
/// (where we can't write the type name).
pub trait NodeNamer<M: NodeMatcher> {
    type Node;
    /// The type of errors.
    type Error;

    fn node_name(&self, n: &Self::Node) -> M::NodeName;
    fn tag<'a>(&self, n: &'a Self::Node) -> Option<&'a str>;
    fn error(self, message: String) -> Self::Error;
}

/// Sibling trait to `NodeList`.
pub trait NodeMatcher {
    /// An enum such that each `NodeName::$rule` has a corresponding `Self::$rule(n: Node) -> T` function.
    type NodeName: Eq;
}

impl<T: crate::Parser> NodeMatcher for T {
    type NodeName = T::AliasedRule;
}

impl<'i, P, D> NodeList<P> for crate::Nodes<'i, P::Rule, D>
where
    D: Clone,
    P: crate::Parser,
{
    type Node = crate::Node<'i, P::Rule, D>;
    type NodeNamer = Self;

    fn consume(mut self) -> (Vec<Self::Node>, Self::NodeNamer) {
        let vec = self.by_ref().collect();
        (vec, self)
    }
}

impl<'i, P, D> NodeNamer<P> for crate::Nodes<'i, P::Rule, D>
where
    D: Clone,
    P: crate::Parser,
{
    type Node = crate::Node<'i, P::Rule, D>;
    type Error = pest::error::Error<P::Rule>;

    fn node_name(&self, n: &Self::Node) -> P::AliasedRule {
        n.as_aliased_rule::<P>()
    }
    fn tag<'a>(&self, n: &'a Self::Node) -> Option<&'a str> {
        n.as_pair().as_node_tag()
    }
    fn error(self, message: String) -> Self::Error {
        (&self).error(message)
    }
}
