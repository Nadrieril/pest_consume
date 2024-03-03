#![allow(non_camel_case_types, non_snake_case)]
#![allow(dead_code)]

use pest_consume::match_nodes;

// Define a simple matcher based on an enum. Each variant of the enum gives a node name, and the
// corresponding matcher function extracts the contained value.
macro_rules! simple_matcher {
    (
        #[matcher=$matcher:ident]
        #[names=$name:ident]
        enum $node:ident { $($variant:ident($ty:ty),)* }
    ) => {
        #[derive(Debug, Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
        enum $name {
            $($variant,)*
        }

        #[derive(Debug, PartialEq, Eq)]
        enum $node {
            $($variant($ty),)*
        }

        impl $node {
            fn name(&self) -> $name {
                match self {
                    $($node::$variant(_) => $name::$variant,)*
                }
            }
        }

        struct $matcher;

        impl pest_consume::NodeMatcher for $matcher {
            type NodeName = $name;
        }

        impl $matcher {
            $(
                fn $variant(n: $node) -> Result<$ty, ()> {
                    match n {
                        $node::$variant(x) => Ok(x),
                        _ => Err(()),
                    }
                }

            )*
        }

        impl pest_consume::NodeList<$matcher> for Vec<$node> {
            type NodeListIter = <Vec<$node> as IntoIterator>::IntoIter;
            type Error = ();

            fn node_names(&self) -> Vec<$name> {
                self.iter().map($node::name).collect()
            }

            fn iter_nodes(self) -> Self::NodeListIter {
                self.into_iter()
            }

            fn error(&self, _message: String) -> Self::Error {
                ()
            }
        }
    };
}

use Node::*;
simple_matcher! {
    #[matcher=TestMatcher]
    #[names=NodeName]
    enum Node {
        boolean(bool),
        string(String),
        number(u32),
    }
}

#[test]
fn single_number() {
    let single_number = |input: Vec<Node>| {
        Ok(match_nodes!(<TestMatcher>; input;
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
    let multi_number = |input: Vec<Node>| {
        Ok(match_nodes!(<TestMatcher>; input;
            [number(x)..] => x.collect::<Vec<_>>(),
        ))
    };
    assert_eq!(multi_number(vec![]), Ok(vec![]));
    assert_eq!(multi_number(vec![number(42)]), Ok(vec![42]));
    assert_eq!(multi_number(vec![number(42), number(12)]), Ok(vec![42, 12]));
}

#[test]
fn multi_number_skip() {
    let multi_number_skip = |input: Vec<Node>| {
        Ok(match_nodes!(<TestMatcher>; input;
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
    let multi_skip = |input: Vec<Node>| {
        Ok(match_nodes!(<TestMatcher>; input;
            [_, x.., _] => x.collect::<Vec<Node>>(),
        ))
    };
    assert!(multi_skip(vec![]).is_err());
    assert_eq!(multi_skip(vec![number(0), number(0)]), Ok(vec![]));
    assert_eq!(
        multi_skip(vec![number(0), number(41), number(42), number(0)]),
        Ok(vec![number(41), number(42)])
    );
    assert_eq!(
        multi_skip(vec![boolean(true), number(41), number(42), boolean(false)]),
        Ok(vec![number(41), number(42)])
    );
}
