use proc_macro2::{Span, TokenStream, TokenTree};
use quote::quote;
use syn::parse::{Parse, ParseStream, Result};
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::{
    bracketed, parenthesized, parse_quote, token, Expr, Ident, Pat, Token, Type,
};

#[derive(Clone)]
struct MatchBranchPattern {
    tag: Option<String>,
    rule_name: Option<Ident>,
    binder: Pat,
}

#[derive(Clone)]
struct MatchBranch {
    patterns: Vec<MatchBranchPatternItem>,
    body: Expr,
}

#[derive(Clone)]
enum MatchBranchPatternItem {
    Single { pat: MatchBranchPattern },
    Multiple { pat: MatchBranchPattern },
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

        let patterns: Punctuated<MatchBranchPatternItem, Token![,]> =
            Punctuated::parse_terminated(&contents)?;
        let _: Token![=>] = input.parse()?;
        let body = input.parse()?;

        Ok(MatchBranch {
            patterns: patterns.into_iter().collect(),
            body,
        })
    }
}

impl Parse for MatchBranchPatternItem {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut tag = None;
        let ahead = input.fork();
        let _: TokenTree = ahead.parse()?;
        if ahead.peek(token::Pound) {
            let tag_ident: Ident = input.parse()?;
            tag = Some(tag_ident.to_string());
            let _: token::Pound = input.parse()?;
        }

        let ahead = input.fork();
        let _: TokenTree = ahead.parse()?;
        if ahead.peek(token::Paren) {
            // If `input` starts with `foo(`
            let contents;
            let rule_name = input.parse()?;
            parenthesized!(contents in input);
            if input.peek(Token![..]) {
                let binder = contents.parse()?;
                let _: Token![..] = input.parse()?;
                Ok(MatchBranchPatternItem::Multiple {
                    pat: MatchBranchPattern {
                        tag,
                        rule_name: Some(rule_name),
                        binder,
                    },
                })
            } else if input.is_empty() || input.peek(Token![,]) {
                let binder = contents.parse()?;
                Ok(MatchBranchPatternItem::Single {
                    pat: MatchBranchPattern {
                        tag,
                        rule_name: Some(rule_name),
                        binder,
                    },
                })
            } else {
                Err(input.error("expected `..` or nothing"))
            }
        } else {
            // A single pattern captures the node itself without parsing anything.
            let binder = input.parse()?;
            if input.peek(Token![..]) {
                let _: Token![..] = input.parse()?;
                Ok(MatchBranchPatternItem::Multiple {
                    pat: MatchBranchPattern {
                        tag,
                        rule_name: None,
                        binder,
                    },
                })
            } else if input.is_empty() || input.peek(Token![,]) {
                Ok(MatchBranchPatternItem::Single {
                    pat: MatchBranchPattern {
                        tag,
                        rule_name: None,
                        binder,
                    },
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

/// Takes the ident of a mutable slice. Generates code that matches on the pattern and calls
/// `process_item` for each item. Calls `error` if we can't proceed.
fn traverse_pattern(
    branch: &MatchBranch,
    i_slice: &Ident,
    matches_pat: impl Fn(&MatchBranchPattern, TokenStream) -> TokenStream,
    process_item: impl Fn(&MatchBranchPatternItem, TokenStream) -> TokenStream,
    error: TokenStream,
) -> TokenStream {
    use MatchBranchPatternItem::*;
    let mut steps = Vec::new();
    let mut patterns = branch.patterns.as_slice();

    // We will match variable patterns greedily. In order for trailing single patterns like `[x..,
    // y, z]` to work, we must handle them first.
    while matches!(patterns, [.., Single { .. }]) {
        let [remaining_pats @ .., item] = patterns else {
            unreachable!()
        };
        patterns = remaining_pats;
        let this_node = process_item(item, quote!(node));
        steps.push(quote!(
            let [rest @ .., node] = #i_slice else { #error };
            #this_node;
            #i_slice = rest;
        ));
    }

    for item in patterns {
        match item {
            Single { .. } => {
                let this_node = process_item(item, quote!(node));
                steps.push(quote!(
                    let [node, rest @ ..] = #i_slice else { #error };
                    #this_node;
                    #i_slice = rest;
                ));
            }
            item @ Multiple { pat, .. } => {
                // Match greedily: take nodes as long as they match.
                let matches_node = matches_pat(pat, quote!(node));
                let this_slice = process_item(item, quote!(matched));
                steps.push(quote!(
                    let count_matching = #i_slice.iter().take_while(|node| #matches_node).count();
                    let (matched, rest) = #i_slice.split_at(count_matching);
                    #this_slice;
                    #i_slice = rest;
                ))
            }
        }
    }

    quote!(
        #(#steps)*
    )
}

fn make_branch(
    branch: &MatchBranch,
    i_nodes: &Ident,
    i_node_namer: &Ident,
    parser: &Type,
) -> TokenStream {
    use MatchBranchPatternItem::*;
    let i_nodes_slice = Ident::new("___nodes_slice", Span::call_site());
    let name_enum = quote!(<#parser as ::pest_consume::NodeMatcher>::NodeName);
    let node_namer_ty = quote!(<_ as ::pest_consume::NodeNamer<#parser>>);

    let matches_pat = |pat: &MatchBranchPattern, x| {
        let rule_cond = match &pat.rule_name {
            Some(rule_name) => {
                quote!(#node_namer_ty::node_name(&#i_node_namer, &#x) == #name_enum::#rule_name)
            }
            None => quote!(true),
        };
        let tag_cond = match &pat.tag {
            Some(tag) => {
                quote!(#node_namer_ty::tag(&#i_node_namer, &#x) == Some(#tag))
            }
            None => quote!(true),
        };
        quote!(#rule_cond && #tag_cond)
    };

    // Determine if we can take this branch.
    let process_item = |item: &_, i_matched| match item {
        Single { pat } => {
            let cond = matches_pat(pat, i_matched);
            quote!(
                if !(#cond) { return false; }
            )
        }
        // The matched subslice was already selected to be matching the rules, so nothing to do.
        Multiple { .. } => quote!(),
    };
    let conditions = traverse_pattern(
        branch,
        &i_nodes_slice,
        matches_pat,
        process_item,
        quote!(return false),
    );

    // Once we have found a branch that matches, we need to parse the nodes.
    let parse_rule = |rule: &Option<_>, node| match rule {
        Some(rule_name) => quote!(#parser::#rule_name(#node)),
        None => quote!(Ok(#node)),
    };
    let process_item = |item: &_, i_matched| match item {
        Single { pat } => {
            let parse = parse_rule(&pat.rule_name, quote!(#i_matched.clone()));
            let binder = &pat.binder;
            quote!(
                let #binder = #parse?;
            )
        }
        Multiple { pat, .. } => {
            let parse_node = parse_rule(&pat.rule_name, quote!(node));
            let binder = &pat.binder;
            quote!(
                let #binder = #i_matched
                    .iter()
                    .cloned()
                    .map(|node| #parse_node)
                    .collect::<::std::result::Result<::std::vec::Vec<_>, _>>()?
                    .into_iter();
            )
        }
    };
    let parses = traverse_pattern(
        branch,
        &i_nodes_slice,
        matches_pat,
        process_item,
        quote!(unreachable!()),
    );

    let body = &branch.body;
    quote!(
        _ if {
            #[allow(unused_mut)]
            let check_condition = |mut #i_nodes_slice: &[_]| -> bool {
                #conditions
                #i_nodes_slice.is_empty()
            };
            check_condition(#i_nodes.as_slice())
        } => {
            #[allow(unused_mut)]
            let mut #i_nodes_slice = #i_nodes.as_slice();
            #parses
            #body
        }
    )
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
        .collect::<Vec<_>>();

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
