use proc_macro2::{Span, TokenStream};
use quote::{format_ident, quote};
use syn::{parse::{Parse, ParseBuffer}, punctuated::Punctuated, spanned::Spanned, Expr, ExprInfer, Stmt};

#[proc_macro_attribute]
pub fn fsyn_anon_lambda_fn(
    _attr: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let input = syn::parse_macro_input!(input as syn::ItemFn);

    patch_for_anon_lambda(input)
        .map(|expanded| quote! {#expanded})
        .unwrap_or_else(|e| e.to_compile_error())
        .into()
}

fn patch_for_anon_lambda(mut input: syn::ItemFn) -> syn::Result<TokenStream> {
    rec_patch_block(&mut input.block)?;

    let expanded = quote! {
        #input
    };

    Ok(expanded)
}

#[proc_macro]
pub fn fsyn_anon_lambda(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let mut e = syn::parse_macro_input!(input as Expr);
    collect_anon_params(&mut e, true)
        .map(move |anon_params| {
            quote! {
                |#(#anon_params,)*| {
                    #e
                }
            }
        })
        .unwrap_or_else(|e| e.to_compile_error())
        .into()
}

struct BindCtxt {
    anon_params: Vec<syn::Pat>,
    full_recursion: bool,
}

fn patch_expr_with_anon_params(e: &mut Expr) -> syn::Result<()> {
    let anon_params = collect_anon_params(e, false)?;

    if anon_params.is_empty() {
        return Ok(());
    }

    let new_e = syn::parse_quote! {
        |#(#anon_params,)*| {
            #e
        }
    };

    *e = new_e;

    Ok(())
}

fn rec_patch_block(block: &mut syn::Block) -> syn::Result<()> {
    for stmt in &mut block.stmts {
        match stmt {
            Stmt::Local(local) => {
                if let Some(init) = &mut local.init {
                    rec_patch_expr(&mut init.expr)?;
                }
            }
            Stmt::Item(_item) => {}
            Stmt::Expr(expr, _) => {
                rec_patch_expr(expr)?;
            }
            Stmt::Macro(m) => {
                if m.mac.path.is_ident("assert_eq") {
                    let args = m.mac.tokens.clone();
                    let mut args: ExprList = syn::parse2(args)?;

                    struct ExprList(Punctuated<Expr, syn::Token![,]>);

                    impl Parse for ExprList {
                        fn parse(input: &ParseBuffer) -> syn::Result<Self> {
                            Ok(ExprList(Punctuated::parse_terminated(input)?))
                        }
                    }

                    let mut iter = args.0.iter_mut();
                    let left = iter.next().unwrap();
                    let right = iter.next().unwrap();
                    rec_patch_expr(left)?;
                    rec_patch_expr(right)?;
                    let args = args.0;
                    m.mac.tokens = quote! {#args};
                }
            }
        }
    }
    Ok(())
}

fn rec_patch_expr(e: &mut Expr) -> syn::Result<()> {
    patch_expr_with_anon_params(e)?;

    match e {
        Expr::Call(call) => {
            rec_patch_expr(&mut call.func)?;
            for arg in &mut call.args {
                rec_patch_expr(arg)?;
            }
            Ok(())
        }
        Expr::Array(array) => {
            for elem in &mut array.elems {
                rec_patch_expr(elem)?;
            }
            Ok(())
        }
        Expr::Assign(assign) => {
            rec_patch_expr(&mut assign.left)?;
            rec_patch_expr(&mut assign.right)?;
            Ok(())
        }

        Expr::Async(async_expr) => rec_patch_block(&mut async_expr.block),
        Expr::Await(await_expr) => rec_patch_expr(&mut await_expr.base),
        Expr::Binary(binary) => {
            rec_patch_expr(&mut binary.left)?;
            rec_patch_expr(&mut binary.right)?;
            Ok(())
        }
        Expr::Block(block) => rec_patch_block(&mut block.block),
        Expr::Break(break_expr) => {
            if let Some(expr) = &mut break_expr.expr {
                rec_patch_expr(expr)
            } else {
                Ok(())
            }
        }
        Expr::Cast(cast) => rec_patch_expr(&mut cast.expr),
        Expr::Continue(_continue_expr) => Ok(()),
        Expr::Field(field) => rec_patch_expr(&mut field.base),
        Expr::ForLoop(for_loop) => {
            rec_patch_expr(&mut for_loop.expr)?;
            rec_patch_block(&mut for_loop.body)
        }
        Expr::Group(group) => rec_patch_expr(&mut group.expr),
        Expr::If(if_expr) => {
            rec_patch_expr(&mut if_expr.cond)?;
            rec_patch_block(&mut if_expr.then_branch)?;
            if let Some(else_branch) = &mut if_expr.else_branch {
                rec_patch_expr(&mut else_branch.1)?;
            }
            Ok(())
        }
        Expr::Index(index) => {
            rec_patch_expr(&mut index.expr)?;
            rec_patch_expr(&mut index.index)?;
            Ok(())
        }
        Expr::Let(let_expr) => rec_patch_expr(&mut let_expr.expr),
        Expr::Lit(_) => Ok(()),
        Expr::Loop(loop_expr) => rec_patch_block(&mut loop_expr.body),
        Expr::Macro(_) => Ok(()),
        Expr::Match(match_expr) => {
            rec_patch_expr(&mut match_expr.expr)?;
            for arm in &mut match_expr.arms {
                rec_patch_expr(&mut arm.body)?;
            }

            Ok(())
        }
        Expr::MethodCall(method_call) => {
            rec_patch_expr(&mut method_call.receiver)?;
            for arg in &mut method_call.args {
                rec_patch_expr(arg)?;
            }
            Ok(())
        }
        Expr::Paren(paren) => rec_patch_expr(&mut paren.expr),
        Expr::Path(_p) => Ok(()),

        Expr::Range(range) => {
            if let Some(from) = &mut range.start {
                rec_patch_expr(from)?;
            }
            if let Some(to) = &mut range.end {
                rec_patch_expr(to)?;
            }
            Ok(())
        }
        Expr::Reference(reference) => rec_patch_expr(&mut reference.expr),
        Expr::Repeat(repeat) => {
            rec_patch_expr(&mut repeat.expr)?;
            rec_patch_expr(&mut repeat.len)?;
            Ok(())
        }
        Expr::Return(return_expr) => {
            if let Some(expr) = &mut return_expr.expr {
                rec_patch_expr(expr)
            } else {
                Ok(())
            }
        }
        Expr::Struct(struct_expr) => {
            for field in &mut struct_expr.fields {
                rec_patch_expr(&mut field.expr)?;
            }
            Ok(())
        }
        Expr::Try(try_expr) => rec_patch_expr(&mut try_expr.expr),
        Expr::TryBlock(try_block) => rec_patch_block(&mut try_block.block),
        Expr::Tuple(tuple) => {
            for elem in &mut tuple.elems {
                rec_patch_expr(elem)?;
            }
            Ok(())
        }
        Expr::Unary(unary) => rec_patch_expr(&mut unary.expr),
        Expr::Unsafe(unsafe_expr) => rec_patch_block(&mut unsafe_expr.block),
        Expr::Verbatim(_v) => Ok(()),
        Expr::While(while_expr) => {
            rec_patch_expr(&mut while_expr.cond)?;
            rec_patch_block(&mut while_expr.body)
        }
        Expr::Yield(yield_expr) => {
            if let Some(expr) = &mut yield_expr.expr {
                rec_patch_expr(expr)
            } else {
                Ok(())
            }
        }
        Expr::Closure(c) => rec_patch_expr(&mut c.body),
        Expr::Infer(_infer) => Ok(()),
        _ => Err(syn::Error::new_spanned(e, "unexpected expr kind")),
    }
}

fn collect_anon_params(e: &mut Expr, full_recursion: bool) -> syn::Result<Vec<syn::Pat>> {
    let mut bind_ctxt = BindCtxt {
        anon_params: Vec::new(),
        full_recursion,
    };
    collect_anon_params_inner(e, &mut bind_ctxt)?;
    Ok(bind_ctxt.anon_params)
}

fn patch_process_if_infer(e: &mut Expr, bind_ctxt: &mut BindCtxt) -> syn::Result<()> {
    if let Expr::Infer(infer) = e {
        let span = infer.span();
        patch_process_infer(e, span, bind_ctxt)?;
    }
    Ok(())
}

fn patch_process_infer(e: &mut Expr, span: Span, bind_ctxt: &mut BindCtxt) -> syn::Result<()> {
    let name = format_ident!("_{}", bind_ctxt.anon_params.len());
    bind_ctxt
        .anon_params
        .push(syn::parse_quote_spanned!(span=>#name));
    *e = syn::parse_quote_spanned!(span=>#name);
    Ok(())
}

fn collect_anon_params_inner(e: &mut Expr, bind_ctxt: &mut BindCtxt) -> syn::Result<()> {
    match e {
        Expr::Call(call) => {
            collect_anon_params_inner(&mut call.func, bind_ctxt)?;
            for arg in &mut call.args {
                if bind_ctxt.full_recursion {
                    collect_anon_params_inner(arg, bind_ctxt)?;
                } else {
                    patch_process_if_infer(arg, bind_ctxt)?;
                }
            }
            Ok(())
        }
        Expr::Array(array) => {
            for elem in &mut array.elems {
                collect_anon_params_inner(elem, bind_ctxt)?;
            }
            Ok(())
        }
        Expr::Assign(assign) => {
            collect_anon_params_inner(&mut assign.left, bind_ctxt)?;
            collect_anon_params_inner(&mut assign.right, bind_ctxt)?;
            Ok(())
        }

        Expr::Async(async_expr) => {
            collect_anon_params_inner_block(&mut async_expr.block, bind_ctxt)
        }
        Expr::Await(await_expr) => collect_anon_params_inner(&mut await_expr.base, bind_ctxt),
        Expr::Binary(binary) => {
            collect_anon_params_inner(&mut binary.left, bind_ctxt)?;
            collect_anon_params_inner(&mut binary.right, bind_ctxt)?;
            Ok(())
        }
        Expr::Block(block) => collect_anon_params_inner_block(&mut block.block, bind_ctxt),
        Expr::Break(break_expr) => {
            if let Some(expr) = &mut break_expr.expr {
                collect_anon_params_inner(expr, bind_ctxt)
            } else {
                Ok(())
            }
        }
        Expr::Cast(cast) => {
            collect_anon_params_inner(&mut cast.expr, bind_ctxt)?;
            Ok(())
        }
        Expr::Continue(_continue_expr) => Ok(()),
        Expr::Field(field) => collect_anon_params_inner(&mut field.base, bind_ctxt),
        Expr::ForLoop(for_loop) => {
            collect_anon_params_inner(&mut for_loop.expr, bind_ctxt)?;
            collect_anon_params_inner_block(&mut for_loop.body, bind_ctxt)
        }
        Expr::Group(group) => collect_anon_params_inner(&mut group.expr, bind_ctxt),
        Expr::If(if_expr) => {
            collect_anon_params_inner(&mut if_expr.cond, bind_ctxt)?;
            collect_anon_params_inner_block(&mut if_expr.then_branch, bind_ctxt)?;
            if let Some(else_branch) = &mut if_expr.else_branch {
                collect_anon_params_inner(&mut else_branch.1, bind_ctxt)?;
            }
            Ok(())
        }
        Expr::Index(index) => {
            collect_anon_params_inner(&mut index.expr, bind_ctxt)?;
            collect_anon_params_inner(&mut index.index, bind_ctxt)?;
            Ok(())
        }
        Expr::Let(let_expr) => {
            collect_anon_params_inner(&mut let_expr.expr, bind_ctxt)?;
            Ok(())
        }
        Expr::Lit(_) => Ok(()),
        Expr::Loop(loop_expr) => collect_anon_params_inner_block(&mut loop_expr.body, bind_ctxt),
        Expr::Macro(_) => Ok(()),
        Expr::Match(match_expr) => {
            collect_anon_params_inner(&mut match_expr.expr, bind_ctxt)?;
            for arm in &mut match_expr.arms {
                collect_anon_params_inner(&mut arm.body, bind_ctxt)?;
            }
            Ok(())
        }
        Expr::MethodCall(method_call) => {
            collect_anon_params_inner(&mut method_call.receiver, bind_ctxt)?;
            for arg in &mut method_call.args {
                if bind_ctxt.full_recursion {
                    collect_anon_params_inner(arg, bind_ctxt)?;
                } else {
                    patch_process_if_infer(arg, bind_ctxt)?;
                }
            }
            Ok(())
        }
        Expr::Paren(paren) => collect_anon_params_inner(&mut paren.expr, bind_ctxt),
        Expr::Path(p) => Ok(()),
        Expr::Range(range) => {
            if let Some(from) = &mut range.start {
                collect_anon_params_inner(from, bind_ctxt)?;
            }
            if let Some(to) = &mut range.end {
                collect_anon_params_inner(to, bind_ctxt)?;
            }
            Ok(())
        }
        Expr::Reference(reference) => collect_anon_params_inner(&mut reference.expr, bind_ctxt),
        Expr::Repeat(repeat) => {
            collect_anon_params_inner(&mut repeat.expr, bind_ctxt)?;
            collect_anon_params_inner(&mut repeat.len, bind_ctxt)?;
            Ok(())
        }
        Expr::Return(return_expr) => {
            if let Some(expr) = &mut return_expr.expr {
                collect_anon_params_inner(expr, bind_ctxt)
            } else {
                Ok(())
            }
        }
        Expr::Struct(struct_expr) => {
            for field in &mut struct_expr.fields {
                collect_anon_params_inner(&mut field.expr, bind_ctxt)?;
            }
            Ok(())
        }
        Expr::Try(try_expr) => collect_anon_params_inner(&mut try_expr.expr, bind_ctxt),
        Expr::TryBlock(try_block) => {
            collect_anon_params_inner_block(&mut try_block.block, bind_ctxt)
        }
        Expr::Tuple(tuple) => {
            for elem in &mut tuple.elems {
                collect_anon_params_inner(elem, bind_ctxt)?;
            }
            Ok(())
        }
        Expr::Unary(unary) => collect_anon_params_inner(&mut unary.expr, bind_ctxt),
        Expr::Unsafe(unsafe_expr) => {
            collect_anon_params_inner_block(&mut unsafe_expr.block, bind_ctxt)
        }
        Expr::Verbatim(_v) => Ok(()),
        Expr::While(while_expr) => {
            collect_anon_params_inner(&mut while_expr.cond, bind_ctxt)?;
            collect_anon_params_inner_block(&mut while_expr.body, bind_ctxt)
        }
        Expr::Yield(yield_expr) => {
            if let Some(expr) = &mut yield_expr.expr {
                collect_anon_params_inner(expr, bind_ctxt)
            } else {
                Ok(())
            }
        }
        Expr::Closure(_) => Err(syn::Error::new_spanned(e, "unexpected expr kind")),
        Expr::Infer(infer) => {
            let span = infer.span();
            patch_process_infer(e, span, bind_ctxt)
        }

        _ => Err(syn::Error::new_spanned(e, "unexpected expr kind")),
    }
}

fn collect_anon_params_inner_block(
    block: &mut syn::Block,
    bind_ctxt: &mut BindCtxt,
) -> syn::Result<()> {
    for stmt in &mut block.stmts {
        match stmt {
            Stmt::Local(local) => {
                if let Some(init) = &mut local.init {
                    collect_anon_params_inner(&mut init.expr, bind_ctxt)?;
                }
            }
            Stmt::Item(_item) => {}
            Stmt::Expr(expr, _) => {
                collect_anon_params_inner(expr, bind_ctxt)?;
            }
            Stmt::Macro(_) => {}
        }
    }
    Ok(())
}
