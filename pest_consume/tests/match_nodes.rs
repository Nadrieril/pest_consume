#![allow(non_camel_case_types, non_snake_case)]
#![allow(dead_code)]

use pest_consume::match_nodes;

// Define a simple matcher based on an enum. Each variant of the enum gives a node name, and the
// corresponding matcher function extracts the contained value. The contructed node type stores the
// enum variant and an optional tag.
macro_rules! simple_matcher {
    (
        #[matcher=$matcher:ident]
        #[node=$node:ident]
        #[names=$name:ident]
        enum $kind:ident { $($variant:ident($ty:ty),)* }
    ) => {
        #[derive(Debug, Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
        enum $name {
            $($variant,)*
        }

        #[derive(Debug, PartialEq, Eq)]
        enum $kind {
            $($variant($ty),)*
        }

        #[derive(Debug, PartialEq, Eq)]
        struct $node {
            kind: $kind,
            tag: Option<String>,
        }

        impl $kind {
            fn no_tag(self) -> $node {
                $node { kind: self, tag: None }
            }
            fn with_tag(self, tag: impl ToString) -> $node {
                $node { kind: self, tag: Some(tag.to_string()) }
            }
            fn name(&self) -> $name {
                match self {
                    $($kind::$variant(_) => $name::$variant,)*
                }
            }
        }

        impl $node {
            fn name(&self) -> $name {
                self.kind.name()
            }
        }

        impl From<$kind> for $node {
            fn from(kind: $kind) -> $node {
                kind.no_tag()
            }
        }

        struct $matcher;

        impl pest_consume::NodeMatcher for $matcher {
            type NodeName = $name;
        }

        impl $matcher {
            $(
                fn $variant(n: $node) -> Result<$ty, ()> {
                    match n.kind {
                        $kind::$variant(x) => Ok(x),
                        _ => Err(()),
                    }
                }

            )*
        }

        impl pest_consume::NodeList<$matcher> for Vec<$node> {
            type Node = $node;
            type NodeNamer = Namer;

            fn consume(self) -> (Vec<Self::Node>, Namer) {
                (self, Namer)
            }
        }

        struct Namer;
        impl pest_consume::NodeNamer<$matcher> for Namer {
            type Node = $node;
            type Error = ();

            fn node_name(&self, n: &Self::Node) -> $name {
                n.name()
            }
            fn tag<'a>(&self, n: &'a Self::Node) -> Option<&'a str>{
                n.tag.as_deref()
            }
            fn error(self, _message: String) -> Self::Error {
                ()
            }
        }
    };
}

use NodeKind::*;
simple_matcher! {
    #[matcher=TestMatcher]
    #[node=Node]
    #[names=NodeName]
    enum NodeKind {
        boolean(bool),
        string(String),
        number(u32),
    }
}

fn notag(input: Vec<NodeKind>) -> Vec<Node> {
    input.into_iter().map(|kind| kind.no_tag()).collect()
}

#[test]
fn single_number() {
    let single_number = |input: Vec<NodeKind>| {
        Ok(match_nodes!(<TestMatcher>; notag(input);
            [number(x)] => x,
        ))
    };
    assert!(single_number(vec![]).is_err());
    assert!(single_number(vec![string(String::new())]).is_err());
    assert_eq!(single_number(vec![number(42)]), Ok(42));
    assert!(single_number(vec![number(42), number(0)]).is_err());
}

#[test]
fn multi_number() {
    let multi_number = |input: Vec<NodeKind>| {
        Ok(match_nodes!(<TestMatcher>; notag(input);
            [number(x)..] => x.collect::<Vec<_>>(),
        ))
    };
    assert_eq!(multi_number(vec![]), Ok(vec![]));
    assert_eq!(multi_number(vec![number(42)]), Ok(vec![42]));
    assert_eq!(multi_number(vec![number(42), number(12)]), Ok(vec![42, 12]));
}

#[test]
fn multi_number_skip() {
    let multi_number_skip = |input: Vec<NodeKind>| {
        Ok(match_nodes!(<TestMatcher>; notag(input);
            [_, number(x).., _] => x.collect::<Vec<_>>(),
        ))
    };
    assert!(multi_number_skip(vec![]).is_err());
    assert_eq!(multi_number_skip(vec![number(0), number(0)]), Ok(vec![]));
    assert_eq!(
        multi_number_skip(vec![number(0), number(41), number(42), number(0)]),
        Ok(vec![41, 42])
    );
    assert_eq!(
        multi_number_skip(vec![
            boolean(true),
            number(41),
            number(42),
            boolean(false)
        ]),
        Ok(vec![41, 42])
    );
}

#[test]
fn multi_skip() {
    let multi_skip = |input: Vec<NodeKind>| {
        Ok(match_nodes!(<TestMatcher>; notag(input);
            [_, x.., _] => x.collect::<Vec<Node>>(),
        ))
    };
    assert!(multi_skip(vec![]).is_err());
    assert_eq!(multi_skip(vec![number(0), number(0)]), Ok(vec![]));
    assert_eq!(
        multi_skip(vec![number(0), number(41), number(42), number(0)]),
        Ok(notag(vec![number(41), number(42)]))
    );
    assert_eq!(
        multi_skip(vec![boolean(true), number(41), number(42), boolean(false)]),
        Ok(notag(vec![number(41), number(42)]))
    );
}

#[test]
fn multi_multi() {
    let multi_multi = |input: Vec<NodeKind>| {
        Ok(match_nodes!(<TestMatcher>; notag(input);
            [number(x).., boolean(y).., _] => (x.sum(), y.clone().all(|b| b)),
        ))
    };
    assert!(multi_multi(vec![]).is_err());
    assert_eq!(multi_multi(vec![number(1), number(2)]), Ok((1, true)));
    assert_eq!(
        multi_multi(vec![number(1), number(2), number(4)]),
        Ok((3, true))
    );
    assert_eq!(
        multi_multi(vec![number(1), boolean(true), number(4)]),
        Ok((1, true))
    );
    assert_eq!(
        multi_multi(vec![boolean(false), boolean(true), number(4)]),
        Ok((0, false))
    );
    assert_eq!(
        multi_multi(vec![boolean(true), boolean(true), boolean(false)]),
        Ok((0, true))
    );
}

#[test]
fn single_tag() {
    let single_tag = |input: Vec<Node>| {
        Ok(match_nodes!(<TestMatcher>; input;
            [tag1 # number(x)] => x,
        ))
    };
    assert!(single_tag(vec![]).is_err());
    assert!(single_tag(vec![number(0).no_tag()]).is_err());
    assert!(single_tag(vec![number(0).with_tag("tag2")]).is_err());
    assert_eq!(single_tag(vec![number(0).with_tag("tag1")]), Ok(0));
}

#[test]
fn multi_tag() {
    let multi_tag = |input: Vec<Node>| {
        Ok(match_nodes!(<TestMatcher>; input;
            [tag1 # x.., tag2 # _] => x.collect::<Vec<_>>(),
        ))
    };
    assert!(multi_tag(vec![number(0).with_tag("tag2")]).is_ok());
    assert_eq!(
        multi_tag(vec![
            number(0).with_tag("tag1"),
            number(0).with_tag("tag1"),
            number(0).with_tag("tag2"),
        ])
        .unwrap()
        .len(),
        2
    );
}

#[test]
fn multi_multi_tag() {
    let multi_multi_tag = |input: Vec<Node>| {
        Ok(match_nodes!(<TestMatcher>; input;
            [tag1 # number(x).., tag2 # number(y)..] => (x.sum(), y.sum()),
        ))
    };
    assert_eq!(
        multi_multi_tag(vec![number(1).with_tag("tag1")]),
        Ok((1, 0))
    );
    assert_eq!(
        multi_multi_tag(vec![number(1).with_tag("tag2")]),
        Ok((0, 1))
    );
    assert_eq!(
        multi_multi_tag(vec![
            number(1).with_tag("tag1"),
            number(1).with_tag("tag2"),
            number(1).with_tag("tag1"),
        ]),
        Err(())
    );
    assert_eq!(
        multi_multi_tag(vec![
            number(1).with_tag("tag1"),
            number(2).with_tag("tag1"),
            number(4).with_tag("tag2"),
        ]),
        Ok((3, 4))
    );
}

#[test]
fn or_pattern() {
    let or_pattern = |input: Vec<NodeKind>| {
        Ok(match_nodes!(<TestMatcher>; notag(input);
            [number(x), boolean(b)] | [boolean(b), number(x)] => (x, b),
        ))
    };
    assert_eq!(or_pattern(vec![number(42), boolean(true)]), Ok((42, true)));
    assert_eq!(or_pattern(vec![boolean(true), number(42)]), Ok((42, true)));
}
