use proc_macro2::{Span, TokenStream, TokenTree};
use quote::quote;
use syn::parse::{Parse, ParseStream, Result};
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::{
    bracketed, parenthesized, parse_quote, token, Expr, Ident, Pat, Token, Type,
};

struct Pattern {
    tag: Option<String>,
    rule_name: Option<Ident>,
    binder: Pat,
    multiple: bool,
}

struct Alternative {
    patterns: Punctuated<Pattern, Token![,]>,
}

struct MatchBranch {
    alternatives: Punctuated<Alternative, Token![|]>,
    body: Expr,
}

struct MacroInput {
    parser: Type,
    input_expr: Expr,
    branches: Punctuated<MatchBranch, Token![,]>,
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

impl Parse for MatchBranch {
    fn parse(input: ParseStream) -> Result<Self> {
        let alternatives = Punctuated::parse_separated_nonempty(&input)?;
        let _: Token![=>] = input.parse()?;
        let body = input.parse()?;

        Ok(MatchBranch { alternatives, body })
    }
}

impl Parse for Alternative {
    fn parse(input: ParseStream) -> Result<Self> {
        let contents;
        let _: token::Bracket = bracketed!(contents in input);
        let patterns = Punctuated::parse_terminated(&contents)?;
        Ok(Alternative { patterns })
    }
}

impl Parse for Pattern {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut tag = None;
        let binder;
        let multiple;
        let rule_name;

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
            rule_name = Some(input.parse()?);
            let contents;
            parenthesized!(contents in input);
            binder = contents.parse()?;
        } else {
            // A plain pattern captures the node itself without parsing anything.
            rule_name = None;
            binder = input.parse()?;
        }
        if input.peek(Token![..]) {
            let _: Token![..] = input.parse()?;
            multiple = true;
        } else if input.is_empty() || input.peek(Token![,]) {
            multiple = false;
        } else {
            return Err(input.error("expected `..` or nothing"));
        }
        Ok(Pattern {
            tag,
            rule_name,
            binder,
            multiple,
        })
    }
}

/// Takes the ident of a mutable slice. Generates code that matches on the pattern and calls
/// `process_item` for each item. Calls `error` if we can't proceed.
fn traverse_pattern(
    mut patterns: &[Pattern],
    i_iter: &Ident,
    matches_pat: impl Fn(&Pattern, TokenStream) -> TokenStream,
    process_item: impl Fn(&Pattern, TokenStream) -> TokenStream,
    error: TokenStream,
) -> TokenStream {
    let mut steps = Vec::new();

    // We will match variable patterns greedily. In order for trailing single patterns like `[x..,
    // y, z]` to work, we must handle them first.
    while patterns.last().is_some_and(|pat| !pat.multiple) {
        let [remaining_pats @ .., pat] = patterns else {
            unreachable!()
        };
        patterns = remaining_pats;
        let this_node = process_item(pat, quote!(node));
        steps.push(quote!(
            let Some(node) = #i_iter.next_back() else { #error };
            #this_node;
        ));
    }

    for pat in patterns {
        if !pat.multiple {
            let this_node = process_item(pat, quote!(node));
            steps.push(quote!(
                let Some(node) = #i_iter.next() else { #error };
                #this_node;
            ));
        } else {
            // Match greedily: take nodes as long as they match.
            let matches_node = matches_pat(pat, quote!(node));
            let this_slice = process_item(pat, quote!(matched));
            steps.push(quote!(
                let matched = <_ as ::pest_consume::Itertools>::peeking_take_while(&mut #i_iter, |node| #matches_node);
                #this_slice;
            ))
        }
    }

    quote!(
        #[allow(unused_mut)]
        let mut #i_iter = #i_iter.peekable();
        #(#steps)*
    )
}

fn make_alternative(
    alternative: Alternative,
    body: &Expr,
    i_nodes: &Ident,
    i_node_namer: &Ident,
    parser: &Type,
) -> TokenStream {
    let i_nodes_iter = Ident::new("___nodes_iter", Span::call_site());
    let name_enum = quote!(<#parser as ::pest_consume::NodeMatcher>::NodeName);
    let node_namer_ty = quote!(<_ as ::pest_consume::NodeNamer<#parser>>);
    let patterns: Vec<_> = alternative.patterns.into_iter().collect();

    let matches_pat = |pat: &Pattern, x| {
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
    let process_item = |pat: &Pattern, i_matched| {
        if !pat.multiple {
            let cond = matches_pat(pat, i_matched);
            quote!(
                if !(#cond) { return false; }
            )
        } else {
            quote!(
                // Consume the iterator.
                #i_matched.count();
            )
        }
    };
    let conditions = traverse_pattern(
        patterns.as_slice(),
        &i_nodes_iter,
        matches_pat,
        process_item,
        quote!(return false),
    );

    // Once we have found a branch that matches, we need to parse the nodes.
    let parse_rule = |rule: &Option<_>, node| match rule {
        Some(rule_name) => quote!(#parser::#rule_name(#node)),
        None => quote!(Ok(#node)),
    };
    let process_item = |pat: &Pattern, i_matched| {
        if !pat.multiple {
            let parse = parse_rule(&pat.rule_name, quote!(#i_matched));
            let binder = &pat.binder;
            quote!(
                let #binder = #parse?;
            )
        } else {
            let parse_node = parse_rule(&pat.rule_name, quote!(node));
            let binder = &pat.binder;
            quote!(
                let #binder = #i_matched
                    .map(|node| #parse_node)
                    .collect::<::std::result::Result<::std::vec::Vec<_>, _>>()?
                    .into_iter();
            )
        }
    };
    let parses = traverse_pattern(
        patterns.as_slice(),
        &i_nodes_iter,
        matches_pat,
        process_item,
        quote!(unreachable!()),
    );

    quote!(
        _ if {
            let check_condition = |slice: &[_]| -> bool {
                let #i_nodes_iter = slice.iter();
                #conditions
                #i_nodes_iter.next().is_none()
            };
            check_condition(#i_nodes.as_slice())
        } => {
            let #i_nodes_iter = #i_nodes.into_iter();
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
        .into_iter()
        .flat_map(|br| {
            let body = br.body;
            let i_nodes = &i_nodes;
            let i_node_namer = &i_node_namer;
            br.alternatives.into_iter().map(move |alt| {
                make_alternative(alt, &body, i_nodes, i_node_namer, parser)
            })
        })
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
