use proc_macro2::{Span, TokenStream, TokenTree};
use quote::quote;
use syn::parse::{Parse, ParseStream, Result};
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::{
    bracketed, parenthesized, parse_quote, token, Error, Expr, Ident, Pat,
    Token, Type,
};

#[derive(Clone)]
struct MatchBranch {
    // Patterns all have the form [a, b, c.., d], with a bunch of simple patterns,
    // optionally a multiple pattern, and then some more simple patterns.
    singles_before_multiple: Vec<(Option<Ident>, Pat)>,
    multiple: Option<(Option<Ident>, Pat)>,
    singles_after_multiple: Vec<(Option<Ident>, Pat)>,

    body: Expr,
}

#[derive(Clone)]
enum MatchBranchPatternItem {
    Single {
        rule_name: Option<Ident>,
        binder: Pat,
    },
    Multiple {
        rule_name: Option<Ident>,
        binder: Pat,
        slice_token: Token![..],
    },
}

#[derive(Clone)]
struct MacroInput {
    parser: Type,
    input_expr: Expr,
    branches: Punctuated<MatchBranch, Token![,]>,
}

impl Parse for MatchBranch {
    fn parse(input: ParseStream) -> Result<Self> {
        let contents;
        let _: token::Bracket = bracketed!(contents in input);

        let pattern: Punctuated<MatchBranchPatternItem, Token![,]> =
            Punctuated::parse_terminated(&contents)?;
        use MatchBranchPatternItem::{Multiple, Single};
        let mut singles_before_multiple = Vec::new();
        let mut multiple = None;
        let mut singles_after_multiple = Vec::new();
        for item in pattern.clone() {
            match item {
                Single { rule_name, binder } => {
                    if multiple.is_none() {
                        singles_before_multiple.push((rule_name, binder))
                    } else {
                        singles_after_multiple.push((rule_name, binder))
                    }
                }
                Multiple {
                    rule_name,
                    binder,
                    slice_token,
                } => {
                    if multiple.is_none() {
                        multiple = Some((rule_name, binder))
                    } else {
                        return Err(Error::new(
                            slice_token.span(),
                            "multiple variable-length patterns are not allowed",
                        ));
                    }
                }
            }
        }

        let _: Token![=>] = input.parse()?;
        let body = input.parse()?;

        Ok(MatchBranch {
            singles_before_multiple,
            multiple,
            singles_after_multiple,
            body,
        })
    }
}

impl Parse for MatchBranchPatternItem {
    fn parse(input: ParseStream) -> Result<Self> {
        let ahead = input.fork();
        let _: TokenTree = ahead.parse()?;
        if ahead.peek(token::Paren) {
            // If `input` starts with `foo(`
            let contents;
            let rule_name = input.parse()?;
            parenthesized!(contents in input);
            if input.peek(Token![..]) {
                let binder = contents.parse()?;
                let slice_token = input.parse()?;
                Ok(MatchBranchPatternItem::Multiple {
                    rule_name: Some(rule_name),
                    binder,
                    slice_token,
                })
            } else if input.is_empty() || input.peek(Token![,]) {
                let binder = contents.parse()?;
                Ok(MatchBranchPatternItem::Single {
                    rule_name: Some(rule_name),
                    binder,
                })
            } else {
                Err(input.error("expected `..` or nothing"))
            }
        } else {
            // A single pattern captures the node itself without parsing anything.
            let binder = input.parse()?;
            if input.peek(Token![..]) {
                let slice_token = input.parse()?;
                Ok(MatchBranchPatternItem::Multiple {
                    rule_name: None,
                    binder,
                    slice_token,
                })
            } else if input.is_empty() || input.peek(Token![,]) {
                Ok(MatchBranchPatternItem::Single {
                    rule_name: None,
                    binder,
                })
            } else {
                Err(input.error("expected `..` or nothing"))
            }
        }
    }
}

impl Parse for MacroInput {
    fn parse(input: ParseStream) -> Result<Self> {
        let parser = if input.peek(token::Lt) {
            let _: token::Lt = input.parse()?;
            let parser = input.parse()?;
            let _: token::Gt = input.parse()?;
            let _: Token![;] = input.parse()?;
            parser
        } else {
            parse_quote!(Self)
        };
        let input_expr = input.parse()?;
        let _: Token![;] = input.parse()?;
        let branches = Punctuated::parse_terminated(input)?;

        Ok(MacroInput {
            parser,
            input_expr,
            branches,
        })
    }
}

fn make_branch(
    branch: &MatchBranch,
    i_nodes: &Ident,
    i_node_namer: &Ident,
    parser: &Type,
) -> Result<TokenStream> {
    let i_nodes_iter = Ident::new("___nodes_iter", Span::call_site());
    let name_enum = quote!(<#parser as ::pest_consume::NodeMatcher>::NodeName);
    let node_namer_ty = quote!(<_ as ::pest_consume::NodeNamer<#parser>>);

    // Find which branch to take
    let mut conditions = Vec::new();
    let start = branch.singles_before_multiple.len();
    let end = branch.singles_after_multiple.len();
    conditions.push(quote!(
        #start + #end <= #i_nodes.len()
    ));
    let matches_rule = |rule: &Option<_>, x| match rule {
        Some(rule_name) => {
            quote!(#node_namer_ty::node_name(&#i_node_namer, &#x) == #name_enum::#rule_name)
        }
        None => quote!(true),
    };
    for (i, (rule, _)) in branch.singles_before_multiple.iter().enumerate() {
        conditions.push(matches_rule(rule, quote!(#i_nodes[#i])))
    }
    for (i, (rule, _)) in branch.singles_after_multiple.iter().enumerate() {
        conditions
            .push(matches_rule(rule, quote!(#i_nodes[#i_nodes.len()-1 - #i])))
    }
    if let Some((rule, _)) = &branch.multiple {
        // Hygiene looks dodgy for the `r`, but it works.
        let matches_r = matches_rule(rule, quote!(*r));
        conditions.push(quote!(
            {
                // We can't use .all() directly in the pattern guard; see
                // https://github.com/rust-lang/rust/issues/59803.
                let all_match = |slice: &[_]| {
                    slice.iter().all(|r| #matches_r)
                };
                all_match(&#i_nodes[#start..#i_nodes.len() - #end])
            }
        ))
    } else {
        // No variable-length pattern, so the size must be exactly the number of patterns
        conditions.push(quote!(
            #start + #end == #i_nodes.len()
        ))
    }

    let parse_rule = |rule: &Option<_>, node| match rule {
        Some(rule_name) => quote!(#parser::#rule_name(#node)),
        None => quote!(Ok(#node)),
    };
    // Once we have found a branch that matches, we need to parse the nodes.
    let mut parses = Vec::new();
    for (rule_name, binder) in branch.singles_before_multiple.iter() {
        let next_node = quote!(#i_nodes_iter.next().unwrap());
        let parse = parse_rule(rule_name, next_node);
        parses.push(quote!(
            let #binder = #parse?;
        ))
    }
    // Note the `rev()`: we are taking nodes from the end of the iterator in reverse order, so that
    // only the unmatched nodes are left in the iterator for the variable-length pattern, if any.
    for (rule, binder) in branch.singles_after_multiple.iter().rev() {
        let next_node = quote!(#i_nodes_iter.next_back().unwrap());
        let parse = parse_rule(rule, next_node);
        parses.push(quote!(
            let #binder = #parse?;
        ))
    }
    // Once we've taken nodes from the front and back, what remains are the nodes matched by the
    // `..` pattern.
    if let Some((rule, binder)) = &branch.multiple {
        // Hygiene looks dodgy for `n`, but it works.
        let parse_n = parse_rule(rule, quote!(n));
        parses.push(quote!(
            let #binder = #i_nodes_iter
                .map(|n| #parse_n)
                .collect::<::std::result::Result<::std::vec::Vec<_>, _>>()?
                .into_iter();
        ))
    }

    let body = &branch.body;
    Ok(quote!(
        _ if #(#conditions &&)* true => {
            #[allow(unused_mut)]
            let mut #i_nodes_iter = #i_nodes.into_iter();
            #(#parses)*
            #body
        }
    ))
}

pub fn match_nodes(
    input: proc_macro::TokenStream,
) -> Result<proc_macro2::TokenStream> {
    let input: MacroInput = syn::parse(input)?;

    let i_nodes = Ident::new("___nodes", input.input_expr.span());
    let i_node_rules = Ident::new("___node_rules", Span::call_site());
    let i_node_namer = Ident::new("___node_namer", Span::call_site());

    let input_expr = &input.input_expr;
    let parser = &input.parser;
    let branches = input
        .branches
        .iter()
        .map(|br| make_branch(br, &i_nodes, &i_node_namer, parser))
        .collect::<Result<Vec<_>>>()?;

    let node_list_ty = quote!(<_ as ::pest_consume::NodeList<#parser>>);
    let node_namer_ty = quote!(<_ as ::pest_consume::NodeNamer<#parser>>);
    Ok(quote!({
        let (#i_nodes, #i_node_namer) = #node_list_ty::consume(#input_expr);

        #[allow(unreachable_code, clippy::int_plus_one)]
        match () {
            #(#branches,)*
            _ => {
                // Collect the rule names to display.
                let #i_node_rules: ::std::vec::Vec<_> =
                        #i_nodes.iter().map(|n| #node_namer_ty::node_name(&#i_node_namer, n)).collect();
                return ::std::result::Result::Err(
                    #node_namer_ty::error(
                        #i_node_namer,
                        format!("Nodes didn't match any pattern: {:?}", #i_node_rules)
                    )
                );
            }
        }
    }))
}
