//! A library of specifications for versions of JavaScript.
//!
//! For a human-readable documentation of the latest version of the library:
//! - build the examples
//!    `cargo build --examples`
//! - run example ast-doc
//!    `./target/debug/examples/ast-doc`.
//!

use ast::grammar::*;
use ast::annotation::*;
use ast::annotation::ScopeNodeName as ScopeKind;
use util::JSONGetter;

use std;

use json::JsonValue as JSON;
use json::object::Object as Object;

pub static TOPLEVEL_SCOPE_NAME: &'static str = "AssertedTopLevelScope";
pub static BLOCK_SCOPE_NAME: &'static str = "AssertedBlockScope";
pub static SCOPE_FIELD: &'static str = "scope";
pub static BINJS_VAR_NAME: &'static str = "varDeclaredNames";
pub static BINJS_LET_NAME: &'static str = "lexicallyDeclaredNames";
pub static BINJS_CAPTURED_NAME: &'static str = "capturedNames";
pub static BINJS_DIRECT_EVAL: &'static str = "hasDirectEval";

/// The set of features requested for a syntax.
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum Level {
    /// Empty syntax, for testing purposes.
    Minimal,
    /// All the features for ES6.
    ES6,
    /// All the features of the latest version of JavaScript.
    Latest,
    // FIXME: More levels to be implemented.
}
impl std::fmt::Display for Level {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        match *self {
            Level::Minimal => write!(fmt, "Minimal"),
            Level::ES6 | Level::Latest => write!(fmt, "ES6"),
        }
    }
}

fn uses_strict(object: &Object) -> bool {
    for member in object["directives"].members() {
        if let Some("use strict") = member["value"]["value"].as_str() {
            return true;
        }
    }
    false
}

/// Special nodes used by BINJS. Not visible at source level.
fn setup_binjs(_: &mut SyntaxBuilder) -> Box<Annotator> {
    struct BaseAnnotator;
    impl Annotator for BaseAnnotator {
        fn name(&self) -> String {
            "BaseAnnotator".to_string()
        }

        fn process_references(&self, me: &Annotator, ctx: &mut Context<RefContents>, object: &mut Object) -> Result<(), ASTError> {
            // Default case: no free variable here, just process.
            for (name, field) in object.iter_mut() {
                if let Ok(mut ctx) = ctx.enter_field(name) {
                    me.process_references_aux(me, &mut ctx, field)?;
                }
            }
            // Dropping `ctx` will propagate everything to the parent.
            Ok(())
        }
        fn process_declarations(&self, me: &Annotator, ctx: &mut Context<DeclContents>, object: &mut Object) -> Result<(), ASTError> {
            // Default case: no declaration here, just process.
            for (name, field) in object.iter_mut() {
                if let Ok(mut ctx) = ctx.enter_field(name) {
                    me.process_declarations_aux(me, &mut ctx, field)?;
                }
            }
            Ok(())
        }
    }

    Box::new(BaseAnnotator)
}

fn setup_es6(syntax: &mut SyntaxBuilder, parent: Box<Annotator>) -> Box<Annotator> {
    use ast::library_es6_generated;
    let _names = library_es6_generated::Library::new(syntax) ;

    struct ES6Annotator {
        parent: Box<Annotator>,
    }
    impl Annotator for ES6Annotator {
        fn name(&self) -> String {
            "ES6Annotator".to_string()
        }

        fn process_declarations(&self, me: &Annotator, ctx: &mut Context<DeclContents>, object: &mut Object) -> Result<(), ASTError> {
            debug!(target: "library", "Visiting {}", ctx.kind_str());
            match ctx.kind_str() {
                "BindingIdentifier" => {
                    // Collect the name of the identifier.
                    let name = object.get_string("name", "Field `name` of `BindingIdentifier`")?;
                    debug!(target: "library", "While visiting {kind}, found name {name} with parent {parent:?}",
                        parent = ctx.contents().parent().map(|x| x.borrow().kind_str().to_string()),
                        name = name,
                        kind = ctx.kind_str()
                    );
                    let parent = match ctx.contents().parent() {
                        Some(parent) => parent,
                        None => return Ok(()) // If we are at toplevel, we don't really care about all this.
                    };
                    let mut parent = parent.borrow_mut();
                    match parent.kind_str() {
                        // FIXME: This would probably be much nicer if we had an iterator of ancestors.
                        "FunctionDeclaration" | "Method" | "Getter" | "Setter" | "FunctionExpression" => {
                            if let Some("name") = parent.field_str() {
                                // 1. If the declaration is at the toplevel, this is a `var`.
                                // 2. If the declaration is in a function's toplevel block, this is a `var`.
                                // 3. Otherwise, this is a `let`.
                                if let Some(grand) = parent.parent() {
                                    let mut grand = grand.borrow_mut();
                                    if let "FunctionBody" = grand.kind_str() {
                                        // Case 2.
                                        grand.add_var_name(name)
                                    } else {
                                        // Case 3.
                                        grand.add_let_name(name)
                                    }
                                } else {
                                    // Case 1.
                                    ctx.add_var_name(name);
                                }
                            }
                            // Otherwise, skip. We cannot declare variables in params. Well, not
                            // variables that deserve being stored.
                        },
                        "ForStatement" => {
                            // Handle `for (c = 0; ...; ...)` where `c` is declared implicitly.
                            if let Some("init") = parent.field_str() {
                                if !parent.is_lex_bound(&name) {
                                    parent.add_var_name(name);
                                }
                            }
                        }
                        "ForInStatement" => {
                            // Handle `for (c in ...)` where `c` is declared implicitly.
                            if let Some("left") = parent.field_str() {
                                if !parent.is_lex_bound(&name) {
                                    parent.add_var_name(name);
                                }
                            }
                        }
                        "CatchClause" => {
                            // Now, there is an invisible hack here.
                            //
                            // We want to know about any binding that happens here,
                            // but only to avoid false positives in case of a
                            // `catch (eval) { ... }`.
                            //
                            // On the other hand, the syntax for ES6 does not
                            // recognize any field named `scope` for `CatchClause`,
                            // so this information is trimmed away during encoding.
                            if let Some("binding") = parent.field_str() {
                                if !parent.is_lex_bound(&name) {
                                    parent.add_let_name(name);
                                }
                            }
                        }
                        "VariableDeclarator" => {
                            if let Some("binding") = parent.field_str() {
                                debug!(target: "library", "Inserting binding");
                                match parent.scope_kind() {
                                    ScopeKind::VarDecl => parent.add_var_name(name),
                                    ScopeKind::LetDecl => parent.add_let_name(name),
                                    ScopeKind::ConstDecl => parent.add_const_name(name),
                                    _ => return Err(ASTError::InvalidScope)
                                }
                            } else {
                                debug!(target: "library", "Skipping binding {:?}", parent.field_str());
                            }
                            // Otherwise, skip. We cannot declare variables in `init`.
                        }
                        // FIXME: We should do something with these names. Either store them seomwhere
                        // or remove from `unknown_names`.
                        "FormalParameters" => {
                            ctx.clear_arg_names(&[name]);
                        },
                        _ => {
                            // We don't know what this identifier is yet. Could be declared
                            // in some enclosing scope or never, in which case it is implicitly
                            // a global variable.
                            parent.add_unknown_name(name)
                        }
                    }
                }
                "CatchClause" => {
                    ctx.set_scope_kind(ScopeKind::LetDecl);
                    self.parent.process_declarations(me, ctx, object)?;

                    // Store available information.
                    ctx.store(object, false);

                    // Drop LexDecl, keep VarDecl
                    ctx.clear_lex_names();
                }
                "VariableDeclaration" => {
                    // Configure the scope kind.
                    let scope_kind = {
                        let variable_kind = object.get_string("kind", "Field `kind` of `VariableDeclaration`")?;
                        match variable_kind {
                            "let" => ScopeKind::LetDecl,
                            "const" => ScopeKind::ConstDecl,
                            "var" => ScopeKind::VarDecl,
                            _ => return Err(ASTError::invalid_value(
                                object.get("kind").unwrap(), // Checked just above.
                                "let | var | const"
                            ))
                        }
                    };
                    ctx.set_scope_kind(scope_kind);

                    // Then proceed as usual.
                    self.parent.process_declarations(me, ctx, object)?;
                }
                "Block" | "ForStatement" | "ForInStatement"  => {
                    // Adopt usual behavior.
                    self.parent.process_declarations(me, ctx, object)?;

                    // Store available information.
                    ctx.store(object, ctx.kind_str() == "FunctionBody");

                    // Drop LexDecl, keep VarDecl
                    ctx.clear_lex_names();
                }
                "Script" | "Module" => {
                    // Check directives
                    if uses_strict(object) {
                        ctx.set_uses_strict(true);
                    }

                    // Adopt usual behavior.
                    self.parent.process_declarations(me, ctx, object)?;

                    // Store available information.
                    ctx.clear_special_unknown_names(&["eval"]);
                    ctx.promote_unknown_names_to_var();
                    ctx.store(object, true);
                }
                "FunctionDeclaration" | "Getter"  | "Setter" | "Method" | "FunctionExpression"  => {
                    // Check directives.
                    let uses_strict = ctx.uses_strict() ||
                        if let JSON::Object(ref body) = object["body"] {
                            uses_strict(body)
                        } else {
                            false
                        };
                    if uses_strict {
                        ctx.set_uses_strict(true);
                    }

                    // Adopt usual behavior.
                    self.parent.process_declarations(me, ctx, object)?;

                    // Store available information.
                    // FIXME: Are we storing the arguments?
                    ctx.store(object, true);

                    // Drop LexDecl and VarDecl
                    ctx.clear_lex_names();
                    ctx.clear_var_names();

/*
                    let mut arg_names = vec![];
                    for param in object["params"]["items"].members() {
                        if let Some("BindingIdentifier") = param["type"].as_str() {
                            arg_names.push(param["name"].as_str().unwrap())
                        } else {
                            unimplemented!()
                        }
                    }
                    ctx.clear_arg_names(&arg_names);
*/
                }
                _ => {
                    // Default to parent implementation.
                    // Note that we still pass `me` to ensure dynamic dispatch.
                    self.parent.process_declarations(me, ctx, object)?
                }
            }
            Ok(())
        }

        fn process_references(&self, me: &Annotator, ctx: &mut Context<RefContents>, object: &mut Object) -> Result<(), ASTError> {
            match ctx.kind_str() {
                "IdentifierExpression" => {
                    let name = object.get_string("name", "Field `name` of `IdentifierExpression`")?;
                    let parent = match ctx.contents().parent() {
                        Some(parent) => parent,
                        None => return Ok(()) // If we are at toplevel, we don't really care about all this.
                    };
                    let mut parent = parent.borrow_mut();

                    match parent.kind_str() {
                        "CallExpression" if name == "eval" => {
                            debug!(target: "library", "Encountered mention of `eval`");
                            if let Some("callee") = parent.field_str() {
                                debug!(target: "library", "Encountered CALL to `eval`");

                                if !parent.is_bound("eval") {
                                    debug!(target: "library", "Encountered DIRECT call to `eval`");
                                    parent.add_direct_eval()
                                } else {
                                    debug!(target: "library", "Actually NOT a direct call to `eval`");
                                }
                            } else {
                                debug!(target: "library", "Oh, my bad, `eval` is not the callee");
                                parent.add_free_name(name)
                            }
                        }
                        _ => ctx.add_free_name(name)
                    }
                }
                "BindingIdentifier" => {
                    // With the exception of parameters, recursive functions and catch, we have
                    // handled all bidnings, so we're going to skip all other forms of
                    // declarations
                    // FIXME: Why haven't we handled all this already?
                    let name = object.get_string("name", "Field `name` of `BindingIdentifier`")?;
                    let parent = match ctx.contents().parent() {
                        Some(parent) => parent,
                        None => return Ok(()) // If we are at toplevel, we don't really care about all this.
                    };
                    let mut parent = parent.borrow_mut();

                    match parent.kind_str() {
                        "FunctionDeclaration" | "Method" | "Getter" | "Setter" | "FunctionExpression" => {
                            match parent.field_str() {
                                Some("name") | Some("params") => {
                                    let fun_scope = parent.fun_scope() .expect("Expected a fun scope");
                                    fun_scope.borrow_mut().add_binding(name);
                                }
                                _ => return Err(ASTError::InvalidField("<FIXME: specify field>".to_string())),
                            }
                        }
                        "CatchClause" => {
                            if let Some("binding") = parent.field_str() {
                                parent.add_binding(name)
                            } else {
                                return Err(ASTError::InvalidField("<FIXME: specify field>".to_string()))
                            }
                        }
                        _ => { /* Nothing to do */ }
                    }
                }
                "ForStatement" | "ForInStatement" | "Block" | "Script" | "Module" | "CatchClause" =>
                {
                    // Simply load the stored bindings, then handle fields.
                    ctx.load(object);
                    self.parent.process_references(me, ctx, object)?;
                    ctx.store(object, false)
                },
                "FunctionExpression" | "FunctionDeclaration" | "Method" | "Getter" | "Setter" => {
                    ctx.use_as_fun_scope();

                    // Make sure that we handle the function id and the params before the body,
                    // as they generally introduce bindings.
                    for name in &["name", "params", "body"] {
                        if let Some(field) = object.get_mut(*name) {
                            if let Ok(mut ctx) = ctx.enter_field(name) {
                                me.process_references_aux(me, &mut ctx, field)?;
                            }
                        }
                    }

                    // Store information on the function itself.
                    ctx.store(object, false);
                    // Dropping `ctx` will convert free variables into
                    // free-in-nested-function variables.
                }
                "ObjectProperty" => {
                    // Simply ignore field `key`.
                    // It is specified as an `Expression`, which doesn't really make sense, as it
                    // must be either a literal or an identifier that is not a variable.
                    me.process_references_field(me, ctx, object, "value")?
                }
                "DotExpression" => {
                    me.process_references_field(me, ctx, object, "object")?;
                    // Field `property` contains an identifier, skip it.
                }
                _ => {
                    // Default to parent implementation.
                    // Note that we still pass `me` to ensure dynamic dispatch.
                    self.parent.process_references(me, ctx, object)?
                }
            }
            Ok(())
        }
    }

    Box::new(ES6Annotator {
        parent
    })
}


/// Construct a syntax for a specific version of JavaScript.
pub fn syntax(level: Level) -> Syntax {
    let mut builder = SyntaxBuilder::new();

    let root    = builder.node_name("Script");
    let null    = builder.node_name("_Null");

    let base_annotator = setup_binjs(&mut builder);

    let annotator = match level {
        Level::Minimal => {
            builder.add_interface(&root).unwrap();
            base_annotator
        }
        Level::ES6
        | Level::Latest => {
            setup_es6(&mut builder, base_annotator)
        }
    };
    builder.into_syntax(SyntaxOptions {
        root: &root,
        null: &null,
        annotator
    })
}



#[test]
fn test_syntax() {
    syntax(Level::Minimal);
    syntax(Level::ES6);
    syntax(Level::Latest);
}
