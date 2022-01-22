use heck::ToSnakeCase;
use proc_macro::TokenStream;
use proc_macro_error::abort;
use quote::{format_ident, quote, ToTokens};
use syn::{FnArg, Ident, ImplItem, ItemImpl, ItemStruct, PatType, ReturnType, Type, Block};

#[proc_macro]
pub fn top_route(_: TokenStream) -> TokenStream {
    quote!(__srpc_inner_ctx).into()
}

#[proc_macro_attribute]
#[proc_macro_error::proc_macro_error]
pub fn rpc(attrs: TokenStream, item: TokenStream) -> TokenStream {
    match syn::parse::<ItemStruct>(item.clone()) {
        Ok(item) => struct_rpc(attrs, item),
        Err(_) => {
            let item = syn::parse::<ItemImpl>(item).unwrap();
            match syn::parse::<Ident>(attrs.clone()) {
                Ok(ident) => {
                    let rpc: Result<RpcProvider, String> =
                        Some(ident.to_string().as_str()).try_into();
                    match rpc {
                        Ok(rpc) => impl_rpc_provider(rpc, item),
                        Err(span) => {
                            return syn::Error::new(ident.span(), span)
                                .to_compile_error()
                                .into()
                        }
                    }
                }
                Err(_) => impl_rpc_provider(RpcProvider::RwLock, item),
            }
        }
    }
}

fn struct_rpc(_: TokenStream, item: ItemStruct) -> TokenStream {
    let ident = &item.ident;
    let vis = &item.vis;
    let peer_name = format_ident!("{}Peer", &ident);
    let (implgen, tygen, whre) = item.generics.split_for_impl();

    let ty_params = item.generics.type_params().map(|s| &s.ident);

    quote!(
        #item
        #vis struct #peer_name #tygen #whre (pub ::srpc::__private::canary::Channel, ::core::marker::PhantomData<( #(#ty_params),* )>);
        impl #implgen From<::srpc::__private::canary::Channel> for #peer_name #tygen #whre {
            fn from(c: ::srpc::__private::canary::Channel) -> Self {
                #peer_name(c, ::core::marker::PhantomData::default())
            }
        }
        impl #implgen From<#peer_name #tygen> for ::srpc::__private::canary::Channel #whre {
            fn from(c: #peer_name #tygen) -> Self {
                c.0
            }
        }
        impl #implgen ::srpc::Peer for #ident #tygen #whre {
            type Struct = #peer_name #tygen;
        }
    )
    .into()
}

#[derive(Clone)]
struct Method<'a> {
    ident: &'a Ident,
    output: Option<&'a Box<Type>>,
    inputs: Option<Vec<&'a PatType>>,
    mutable: bool,
    consume: MethodKind,
    block: &'a Block
}

#[derive(Clone)]
enum MethodKind {
    Normal,  // normal rpc
    Consume, // consume channel
    Manual,  // reintroduce
    Server,  // similar to consume, but does not create a client method
    Client,  // create a client method
}

enum RpcProvider {
    RwLock,
    Mutex,
    Ref,
}

impl TryFrom<Option<&str>> for RpcProvider {
    type Error = String;

    fn try_from(value: Option<&str>) -> Result<Self, Self::Error> {
        match value {
            Some(s) => match s {
                "rw" => Ok(RpcProvider::RwLock),
                "rwlock" => Ok(RpcProvider::RwLock),
                "mutex" => Ok(RpcProvider::Mutex),
                "mtx" => Ok(RpcProvider::Mutex),
                "none" => Ok(RpcProvider::Ref),
                _ => Err(format!("{:?} not expected in this context, possible values are: `rw`, `rwlock`, `mutex`, `mtx` or `none`", s))
            },
            None => Ok(RpcProvider::RwLock)
        }
    }
}

impl RpcProvider {
    fn get_meta(&self, mutable: bool, meta: &syn::Ident) -> quote::__private::TokenStream {
        match self {
            RpcProvider::RwLock => {
                if mutable {
                    quote!(#meta.write().await)
                } else {
                    quote!(#meta.read().await)
                }
            }
            RpcProvider::Mutex => quote! {
                #meta.lock().await
            },
            RpcProvider::Ref => quote! {
                #meta
            },
        }
    }
    fn get_type<T: ToTokens>(&self, top: T) -> quote::__private::TokenStream {
        match self {
            RpcProvider::RwLock => quote! {
                ::srpc::__private::RwLock<#top>
            },
            RpcProvider::Mutex => quote! {
                ::srpc::__private::Mutex<#top>
            },
            RpcProvider::Ref => quote! {
                #top
            },
        }
    }
}

fn impl_rpc_provider(provider: RpcProvider, mut item: ItemImpl) -> TokenStream {
    item.attrs.clear();
    let methods = item
        .items
        .clone()
        .into_iter()
        .filter(|s| {
            if let ImplItem::Method(_) = s {
                true
            } else {
                false
            }
        })
        .map(|s| {
            if let ImplItem::Method(s) = s {
                s
            } else {
                unreachable!()
            }
        })
        .collect::<Vec<_>>();

    let top_type_name = {
        if let Type::Path(s) = *item.self_ty.clone() {
            s.path.segments.first().unwrap().clone().ident
        } else {
            panic!("unexpected type")
        }
    };
    let top_type = &item.self_ty;

    let top_type_meta = provider.get_type(&top_type);
    let mut method_names = methods.iter().map(|s| &s.sig.ident).collect::<Vec<_>>();
    method_names.sort();
    method_names.dedup();
    let repr = {
        match method_names.len() {
            0 => quote! { #[derive(::srpc::__private::Serialize, ::srpc::__private::Deserialize)] },
            num if num < (u8::MAX as usize) => quote! {
                #[derive(::srpc::__private::Serialize_repr, ::srpc::__private::Deserialize_repr)]
                #[repr(u8)]
            },
            num if num < (u16::MAX as usize) => quote! {
                #[derive(::srpc::__private::Serialize_repr, ::srpc::__private::Deserialize_repr)]
                #[repr(u16)]
            },
            num if num < (u32::MAX as usize) => quote! {
                #[derive(::srpc::__private::Serialize_repr, ::srpc::__private::Deserialize_repr)]
                #[repr(u32)]
            },
            _ => quote! {
                #[derive(::srpc::__private::Serialize_repr, ::srpc::__private::Deserialize_repr)]
                #[repr(u64)]
            },
        }
    };

    let enum_repr = quote! (
        #[allow(non_camel_case_types)]
        #repr
        enum __srpc_action {
            #(#method_names),*
        }
    );

    let methods = methods.iter().map(|s| {
        let output = match &s.sig.output {
            ReturnType::Default => None,
            ReturnType::Type(_, ty) => Some(ty),
        };
        let block = &s.block;

        let mut mutable = false;
        let consume = s
            .attrs
            .iter()
            .any(|attr| quote!(#[consume]).to_string() == quote!(#attr).to_string());
        let manual = s
            .attrs
            .iter()
            .any(|attr| quote!(#[manual]).to_string() == quote!(#attr).to_string());
        let server = s
            .attrs
            .iter()
            .any(|attr| quote!(#[server]).to_string() == quote!(#attr).to_string());
        let client = s
            .attrs
            .iter()
            .any(|attr| quote!(#[client]).to_string() == quote!(#attr).to_string());
        let consume = match (consume, manual, client, server) {
            (false, true, false, false) => MethodKind::Manual,
            (false, false, true, false) => MethodKind::Client,
            (false, false, false, true) => MethodKind::Server,
            (true, false, false, false) => MethodKind::Consume,
            (false, false, false, false) => MethodKind::Normal,
            (consume, manual, client, server) if consume as u8 + manual as u8 + client as u8 + server as u8 > 1 => abort!(
                &s.sig.ident.span(),
                "cannot have a method with more than one modifier"
            ),
            (_, _, _, _) => unreachable!(),
        };

        let iter = s
            .sig
            .inputs
            .iter()
            .filter_map(|i| match i {
                FnArg::Receiver(s) => {
                    mutable = s.mutability.is_some();
                    None
                }
                FnArg::Typed(ty) => Some(ty),
            })
            .collect::<Vec<_>>();
        let inputs = if !iter.is_empty() { Some(iter) } else { None };
        Method {
            ident: &s.sig.ident,
            output,
            inputs,
            mutable,
            consume,
            block
        }
    });

    let meta_ident = format_ident!("__srpc_inner_meta");
    let channel_ident = format_ident!("__srpc_inner_channel");
    let context_ident = format_ident!("__srpc_inner_ctx");

    let matches = methods.clone().map(|method| {
        let ident = method.ident;
        let inputs = method.inputs;

        let meta = provider.get_meta(method.mutable, &meta_ident);

        match (inputs, method.output, method.consume) {
            (None, None, MethodKind::Normal) => quote!(
                __srpc_action::#ident => {
                    #meta.#ident(&#context_ident).await;
                }
            ),
            (None, Some(_), MethodKind::Normal) => quote!(
                __srpc_action::#ident => {
                    let res = #meta.#ident(&#context_ident).await;
                    #channel_ident.send(res).await?;
                }
            ),
            (Some(inputs), None, MethodKind::Normal) => {
                let mut args = vec![];
                inputs.iter().map(|s| {
                    let s = &s.pat;
                    let arg = format_ident!("{}", quote!(#s).to_string());
                    args.push(arg);
                }).for_each(drop);
                let inputs = inputs.into_iter().map(|s| &s.ty);

                let inputs = quote!( ( #(#args),*  ): ( #(#inputs),* ) );
                quote!(
                    __srpc_action::#ident => {
                        #[allow(unused_parens)]
                        let #inputs = #channel_ident.receive().await?;
                        #meta.#ident(#(#args),* , &#context_ident).await;
                    }
                )
            },
            (Some(inputs), Some(_), MethodKind::Normal) => {
                let mut args = vec![];
                inputs.iter().map(|s| {
                    args.push(&s.pat);
                }).for_each(drop);
                let inputs = inputs.into_iter().map(|s| &s.ty);

                let inputs = quote!( ( #(#args),*  ): ( #(#inputs),* ) );
                quote!(
                    __srpc_action::#ident => {
                        #[allow(unused_parens)]
                        let #inputs = #channel_ident.receive().await?;
                        let res = #meta.#ident(#(#args),* , &#context_ident).await;
                        #channel_ident.send(res).await?;
                    }
                )
            },
            (Some(inputs), Some(_), MethodKind::Consume) => {
                if inputs.len() != 1 {
                    abort!(method.ident.span(), "methods that consume can only have one argument with type Channel and return a canary::Result<()>")
                }
                quote! {
                    __srpc_action::#ident => {
                        return #meta.#ident(#channel_ident, &#context_ident).await;
                    }
                }
            }
            (Some(inputs), Some(_), MethodKind::Manual) => {
                if inputs.len() != 1 {
                    abort!(method.ident.span(), "manual methods can only have one argument with type Channel and return a canary::Result<Channel>")
                }
                quote! {
                    __srpc_action::#ident => {
                        match #meta.#ident(#channel_ident, &#context_ident).await {
                            Ok(chan) => #channel_ident = chan,
                            Err(e) => return Err(e),
                        }
                    }
                }
            },
            (Some(inputs), Some(_), MethodKind::Server) => {
                if inputs.len() != 1 {
                    abort!(method.ident.span(), "manual methods can only have one argument with type Channel and return a canary::Result<Channel>")
                }
                quote! {
                    __srpc_action::#ident => {
                        match #meta.#ident(&mut #channel_ident, &#context_ident).await {
                            Ok(_) => (),
                            Err(e) => return Err(e),
                        }
                    }
                }
            },
            (None, None, MethodKind::Client) => quote!(),
            (None, Some(_), MethodKind::Client) => quote!(),
            (Some(_), None, MethodKind::Client) => quote!(),
            (Some(_), Some(_), MethodKind::Client) => quote!(),
            (Some(_), None, MethodKind::Server) => abort!(method.ident.span(), "server methods need an argument with type Channel and return a canary::Result<Channel>"),
            (None, Some(_), MethodKind::Server) => abort!(method.ident.span(), "server methods need an argument with type Channel and return a canary::Result<Channel>"),
            (None, None, MethodKind::Server) => abort!(method.ident.span(), "server methods need an argument with type Channel and return a canary::Result<Channel>"),
            (None, None, MethodKind::Consume) => abort!(method.ident.span(), "methods that consume need an argument with type Channel and return a canary::Result<()>"),
            (None, Some(_), MethodKind::Consume) => abort!(method.ident.span(), "methods that consume can only return a canary::Result<()>"),
            (Some(_), None, MethodKind::Consume) => abort!(method.ident.span(), "methods that consume can only return a canary::Result<()>"),
            (None, None, MethodKind::Manual) => abort!(method.ident.span(), "manual methods need an argument with type Channel and return a canary::Result<Channel>"),
            (None, Some(_), MethodKind::Manual) => abort!(method.ident.span(), "manual methods can only return a canary::Result<Channel>"),
            (Some(_), None, MethodKind::Manual) => abort!(method.ident.span(), "manual methods can only return a canary::Result<Channel>"),
        }
    }).collect::<Vec<_>>();

    let (_, ty_generics, whr) = item.generics.split_for_impl();
    let impl_generics = {
        let s = item.generics.type_params().map(|s| {
            let mut s = s.clone();
            s.bounds.push(syn::parse2(quote!(Send)).unwrap());
            s.bounds.push(syn::parse2(quote!(Sync)).unwrap());
            s.bounds.push(syn::parse2(quote!('static)).unwrap());
            s.bounds
                .push(syn::parse2(quote!(::srpc::__private::Serialize)).unwrap());
            s.bounds
                .push(syn::parse2(quote!(::srpc::__private::DeserializeOwned)).unwrap());
            s
        });
        quote!(<#(#s),*>)
    };
    let endpoint = &top_type_name.to_string().to_snake_case();
    let function = {
        let impl_generics = impl_generics.clone();
        quote! {
            impl #impl_generics ::srpc::__private::canary::service::Service for #top_type #whr {
                const ENDPOINT: &'static str = #endpoint;
                type Pipeline = ();
                type Meta = ::std::sync::Arc<#top_type_meta>;
                fn service(
                    #meta_ident: ::std::sync::Arc<#top_type_meta>,
                ) -> ::srpc::__private::canary::service::Svc {
                    ::srpc::__private::canary::service::run_metadata(
                        #meta_ident,
                        |#meta_ident: ::std::sync::Arc<#top_type_meta>, mut #channel_ident: ::srpc::__private::canary::Channel, #context_ident: ::srpc::__private::canary::Ctx| async move {
                        loop {
                            match #channel_ident.receive::<__srpc_action>().await? {
                                #(#matches),*
                            }
                        }
                    })
                }
            }
        }
    };

    let peer_name = format_ident!("{}Peer", top_type_name);

    let peer_methods = methods.map(|method| {
        let has_output = method.output.is_some();
        let has_input = method.inputs.is_some();

        let res = method.output
            .map(|s| *s.clone())
            .unwrap_or(syn::parse2(quote!(())).unwrap());
        let result = quote!( ::srpc::__private::canary::Result<#res> );

        let inputs = method.inputs.unwrap_or_default();
        let mut params = vec![];
        let name = method.ident;

        let args = inputs.clone().into_iter().map(|inp| {
            let mut inp = inp.clone();
            let ty = inp.ty;
            let ty: Type = syn::parse2(quote!(impl std::borrow::Borrow<#ty>)).unwrap();
            inp.ty = Box::new(ty);

            let ret = quote!( #inp );

            let arg = inp.pat;
            params.push(arg);
            ret
        }).collect::<Vec<_>>();

        match (has_output, has_input, &method.consume) {
            (true, true, MethodKind::Normal) => quote! {
                pub async fn #name(&mut self #(,#args)*) -> #result {
                    self.0.send(__srpc_action::#name).await?;
                    #[allow(unused_parens)]
                    self.0.send((#(#params.borrow()),*)).await?;
                    self.0.receive().await
                }
            },
            (true, false, MethodKind::Normal) => quote! {
                pub async fn #name(&mut self) -> #result {
                    self.0.send(__srpc_action::#name).await?;
                    self.0.receive().await
                }
            },
            (false, true, MethodKind::Normal) => quote! {
                pub async fn #name(&mut self #(,#args)*) -> #result {
                    self.0.send(__srpc_action::#name).await?;
                    #[allow(unused_parens)]
                    self.0.send((#(#params.borrow()),*)).await?;
                    Ok(())
                }
            },
            (false, false, MethodKind::Normal) => quote! {
                pub async fn #name(&mut self) -> #result {
                    self.0.send(__srpc_action::#name).await?;
                    Ok(())
                }
            },
            (true, true, MethodKind::Consume) => quote! {
                pub async fn #name(mut self) -> ::srpc::__private::canary::Result<::srpc::__private::canary::Channel> {
                    self.0.send(__srpc_action::#name).await?;
                    Ok(self.0)
                }
            },
            (true, true, MethodKind::Manual) => quote! {
                pub async fn #name(mut self) -> ::srpc::__private::canary::Result<::srpc::__private::canary::Channel> {
                    self.0.send(__srpc_action::#name).await?;
                    Ok(self.0)
                }
            },
            (_, true, MethodKind::Client) => {
                let block = method.block;
                let mut inputs = inputs.clone();
                let chan_ident = &inputs.first().take().unwrap().pat;
                inputs.remove(0);
                quote! { // todo!
                    pub async fn #name(&mut self #(,#inputs)*) -> #res {
                        self.0.send(__srpc_action::#name).await?;

                        let #chan_ident = &mut self.0;

                        let __private_srpc_inner_result = {
                            #block
                        };
                        __private_srpc_inner_result
                    }
                }
            },
            (_, _, MethodKind::Server) => quote!(),
            (_, _, MethodKind::Client) => abort!(method.ident.span(), "client methods can only have an argument with type &mut Channel"),
            (true, false, MethodKind::Consume) => abort!(method.ident.span(), "methods that consume can only have an argument with type Channel and return a canary::Result<()>"),
            (false, true, MethodKind::Consume) => abort!(method.ident.span(), "methods that consume can only have an argument with type Channel and return a canary::Result<()>"),
            (false, false, MethodKind::Consume) => abort!(method.ident.span(), "methods that consume can only have an argument with type Channel and return a canary::Result<()>"),
            (true, false, MethodKind::Manual) => abort!(method.ident.span(), "manual methods can only have an argument with type Channel and return a canary::Result<Channel>"),
            (false, true, MethodKind::Manual) => abort!(method.ident.span(), "manual methods can only have an argument with type Channel and return a canary::Result<Channel>"),
            (false, false, MethodKind::Manual) => abort!(method.ident.span(), "manual methods can only have an argument with type Channel and return a canary::Result<Channel>"),
        }

    }).collect::<Vec<_>>();
    let peer_impl = quote! {
        impl #impl_generics #peer_name #ty_generics #whr {
            #(#peer_methods)*
        }
    };

    let items = item.items
        .into_iter()
        .filter(|s| {
            let mut stay = true;
            if let ImplItem::Method(method) = &s {
                let attrs = &method.attrs;
                for attr in attrs {
                    if quote!(#[client]).to_string() == quote!(#attr).to_string() {
                        stay = false;
                    }
                }
            }
            stay
        })
        .map(|mut s| {
            if let ImplItem::Method(method) = &mut s {
                let attrs = method.attrs.clone();
                let mut new_attrs = vec![];
                for attr in attrs {
                    if quote!(#[consume]).to_string() != quote!(#attr).to_string()
                        && quote!(#[manual]).to_string() != quote!(#attr).to_string()
                        && quote!(#[server]).to_string() != quote!(#attr).to_string()
                        && quote!(#[client]).to_string() != quote!(#attr).to_string()
                    {
                        new_attrs.push(attr)
                    }
                }
                method.attrs = new_attrs;
            }
            s
        })
        .collect::<Vec<_>>();
    item.items = items;
    item.generics
        .type_params_mut()
        .map(|s| {
            s.bounds
                .push(syn::parse2(quote!(::srpc::__private::Serialize)).unwrap());
            s.bounds
                .push(syn::parse2(quote!(::srpc::__private::DeserializeOwned)).unwrap());
        })
        .for_each(drop);

    item.items.iter_mut().map(|f| {
        if let ImplItem::Method(f) = f {
            let typed = syn::parse2(quote!(#context_ident: &::srpc::__private::canary::Ctx)).unwrap();
            f.sig.inputs.push(typed);
        }
    }).for_each(drop);

    let ts = quote!(
        const _: () = {
            #[cfg(not(target_arch = "wasm32"))]
            #item
            #enum_repr
            #[cfg(not(target_arch = "wasm32"))]
            #function
            #peer_impl
        };
    );
    ts.into()
}
