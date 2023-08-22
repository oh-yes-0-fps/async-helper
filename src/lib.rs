use proc_macro::TokenStream;
use proc_macro2::Group;
use proc_macro2::Ident;
use proc_macro2::TokenStream as TokenStream2;
use quote::{quote, ToTokens};
use syn::Stmt;
use syn::Token;


fn replace_ident(from: &str, to: &str, stream: TokenStream2) -> TokenStream2 {
    stream.into_iter()
        .map(|token| {
            if let proc_macro2::TokenTree::Ident(ident) = &token {
                if ident.to_string() == from {
                    return proc_macro2::TokenTree::Ident(
                        syn::Ident::new(&to, ident.span())
                    );
                }
            } else if let proc_macro2::TokenTree::Group(group) = &token {
                return proc_macro2::TokenTree::Group(
                    Group::new(
                        group.delimiter(),
                        replace_ident(from, to, group.stream())
                    )
                );
            }
            token
        })
        .collect::<TokenStream2>()
}


/// Uses an inner object and outer handle paradigm to allow for async and blocking implementations
/// without writing the same code twice. There will be a Blocking____Handle and Async____Handle that
/// have an Arc<Inner____>. The macro will take an impl block for _____.
/// The macro will then create an impl block for both handles that simply call the
/// inner object's methods.
///
/// The blocking handle will have a field called rt thats a tokio runtime that can be blocked on.
#[proc_macro]
pub fn async_inner(input: TokenStream) -> TokenStream {

    let input: TokenStream2 = input.into();

    let input = replace_ident("inner", "self", input);

    let mut impl_block = match syn::parse2::<syn::ItemImpl>(input.clone()) {
        Ok(impl_block) => impl_block,
        _ => return input.into(),
    };

    let name_str = impl_block.self_ty.to_token_stream().to_string();

    let inner_name: Ident =
        syn::parse_str(&format!("Inner{}", &name_str)).expect("Failed to parse inner name");
    impl_block.self_ty = Box::new(
        syn::parse2::<syn::Type>(inner_name.to_token_stream()).expect("Failed to parse inner name"),
    );



    for item in impl_block.items.iter() {
        if let syn::ImplItem::Fn(method) = item {
            if method.sig.asyncness.is_none() {
                panic!("All methods in impl block must be async")
            }
        }
    }

    let mut async_impl_block = impl_block.clone();

    let async_name: Ident = syn::parse_str(&format!("Async{}Handle", &name_str))
        .expect("Failed to parse async handle name");
    async_impl_block.self_ty = Box::new(
        syn::parse2::<syn::Type>(async_name.to_token_stream())
            .expect("Failed to parse async handle name"),
    );

    for item in async_impl_block.items.iter_mut() {
        if let syn::ImplItem::Fn(method) = item {
            //make the method call self.inner.method then await it
            let method_name = &method.sig.ident;
            let arg_names = method.sig.inputs.iter().filter_map(|arg| {
                if let syn::FnArg::Typed(pat_type) = arg {
                    if let syn::Pat::Ident(ident) = &*pat_type.pat {
                        return Some(&ident.ident);
                    }
                }
                None
            });

            let new_block = quote! {
                return self.inner.#method_name(#(#arg_names),*).await;
            };

            method.block.stmts = vec![syn::parse2::<Stmt>(new_block).unwrap()];

            method.vis = syn::Visibility::Public(Token![pub](proc_macro2::Span::call_site()));
        }
    }

    let mut blocking_impl_block = impl_block.clone();

    let blocking_name: Ident = syn::parse_str(&format!("Blocking{}Handle", &name_str))
        .expect("Failed to parse blocking handle name");
    blocking_impl_block.self_ty = Box::new(
        syn::parse2::<syn::Type>(blocking_name.to_token_stream())
            .expect("Failed to parse blocking handle name"),
    );

    for item in blocking_impl_block.items.iter_mut() {
        if let syn::ImplItem::Fn(method) = item {
            //make the method call self.inner.method then await it
            let method_name = &method.sig.ident;
            let arg_names = method.sig.inputs.iter().filter_map(|arg| {
                if let syn::FnArg::Typed(pat_type) = arg {
                    if let syn::Pat::Ident(ident) = &*pat_type.pat {
                        return Some(&ident.ident);
                    }
                }
                None
            });

            let new_block = quote! {
                return self.rt.block_on(self.inner.#method_name(#(#arg_names),*));
            };

            method.block.stmts = vec![syn::parse2::<Stmt>(new_block).unwrap()];

            method.sig.asyncness = None;

            method.vis = syn::Visibility::Public(Token![pub](proc_macro2::Span::call_site()));
        }
    }

    let tokens = quote! {
        #impl_block

        #async_impl_block

        #blocking_impl_block
    };

    tokens.into()
}
