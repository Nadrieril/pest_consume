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
