use ast::*;

use binjs_shared::{ Offset, VisitMe };

use std;
use std::cell::RefCell;
use std::rc::Rc;

/// Keep track of the number of nested levels of functions/methods/...
/// we have crossed.
pub struct LevelGuard {
    /// A handle on the current level.
    level: Rc<RefCell<u32>>
}
impl LevelGuard {
    /// Increase the level. It will be decreased when we `drop` this guard.
    fn new(owner: &LazifierVisitor) -> Self {
        let result = Self {
            level: owner.level.clone(),
        };
        *result.level.borrow_mut() += 1;
        result
    }
}
impl Drop for LevelGuard {
    fn drop(&mut self) {
        *self.level.borrow_mut() -= 1;
    }
}

/// A visitor in charge of rewriting an AST to introduce laziness.
pub struct LazifierVisitor {
    /// A nesting level at which to stop.
    ///
    /// 0 = lazify nothing
    /// 1 = lazify functions defined at topevel
    /// 2 = lazify functions defined at toplevel and functions defined immediately inside them
    /// ...
    threshold: u32,

    /// Current nesting level.
    ///
    /// Increased by one every time we enter a function/method/...
    level: Rc<RefCell<u32>>,
}

impl LazifierVisitor {
    pub fn new(threshold: u32) -> Self {
        Self {
            threshold,
            level: Rc::new(RefCell::new(0)),
        }
    }
}

impl LazifierVisitor {
    /// Hack to steal a subtree from a `&mut`.
    fn steal<T: Default, F, U>(source: &mut T, decorator: F) -> Result<Option<U>, ()>
        where F: FnOnce(T) -> U
    {
        // FIXME: Instead of the `default()`, we could swap in some unitialized memory
        // and ensure that it is forgotten.
        let mut stolen = T::default();
        std::mem::swap(source, &mut stolen);
        Ok(Some(decorator(stolen)))
    }

    /// Return `DoneHere` if we're beyond the threshold, hence skipping the subtree.
    /// Otherwise, acquire a `LevelGuard` that will be released once we're done
    /// with this subtree.
    fn cut_at_threshold(&mut self) -> Result<VisitMe<Option<LevelGuard>>, ()> {
        {
            if *self.level.borrow() >= self.threshold {
                return Ok(VisitMe::DoneHere);
            }
        }
        Ok(VisitMe::HoldThis(Some(LevelGuard::new(self))))
    }
}

impl Visitor<(), Option<LevelGuard>> for LazifierVisitor {
    /// Skip subtrees that are beyond the threshold.
    fn enter_method_definition(&mut self, _path: &Path, _node: &mut ViewMutMethodDefinition) -> Result<VisitMe<Option<LevelGuard>>, ()> {
        self.cut_at_threshold()
    }

    /// Convert eager getter/setter/method to lazy.
    ///
    /// Only called if we haven't skipped the subtree.
    fn exit_method_definition(&mut self, _path: &Path, node: &mut ViewMutMethodDefinition) -> Result<Option<MethodDefinition>, ()> {
        match *node {
            ViewMutMethodDefinition::EagerGetter(ref mut steal) => {
                Self::steal(*steal, |stolen| {
                    LazyGetter {
                        name: stolen.name,
                        directives: stolen.directives,
                        contents_skip: Offset::default(),
                        contents: stolen.contents
                    }.into()
                })
            }
            ViewMutMethodDefinition::EagerSetter(ref mut steal) => {
                Self::steal(*steal, |stolen| {
                    LazySetter {
                        name: stolen.name,
                        directives: stolen.directives,
                        contents_skip: Offset::default(),
                        contents: stolen.contents
                    }.into()
                })
            }
            ViewMutMethodDefinition::EagerMethod(ref mut steal) => {
                Self::steal(*steal, |stolen| {
                    LazyMethod {
                        is_async: stolen.is_async,
                        is_generator: stolen.is_generator,
                        name: stolen.name,
                        directives: stolen.directives,
                        contents_skip: Offset::default(),
                        contents: stolen.contents
                    }.into()
                })
            }
            _ => Ok(None)
        }
    }

    /// Skip subtrees that are beyond the threshold.
    fn enter_function_declaration(&mut self, _path: &Path, _node: &mut ViewMutFunctionDeclaration) -> Result<VisitMe<Option<LevelGuard>>, ()> {
        self.cut_at_threshold()
    }

    /// Convert eager function declarations to lazy.
    ///
    /// Only called if we haven't skipped the subtree.
    fn exit_function_declaration(&mut self, _path: &Path, node: &mut ViewMutFunctionDeclaration) -> Result<Option<FunctionDeclaration>, ()> {
        match *node {
            ViewMutFunctionDeclaration::EagerFunctionDeclaration(ref mut steal) => {
                Self::steal(*steal, |stolen| {
                    LazyFunctionDeclaration {
                        is_async: stolen.is_async,
                        is_generator: stolen.is_generator,
                        name: stolen.name,
                        directives: stolen.directives,
                        contents_skip: Offset::default(),
                        contents: stolen.contents
                    }.into()
                })
            }
            _ => Ok(None)
        }
    }

    /// Skip subtrees that are beyond the threshold.
    fn enter_function_expression(&mut self, _path: &Path, _node: &mut ViewMutFunctionExpression) -> Result<VisitMe<Option<LevelGuard>>, ()> {
        self.cut_at_threshold()
    }

    /// Convert eager function expressions to lazy, unless they're called immediately.
    ///
    /// Only called if we haven't skipped the subtree.
    fn exit_function_expression(&mut self, path: &Path, node: &mut ViewMutFunctionExpression) -> Result<Option<FunctionExpression>, ()> {
        // Don't lazify code that's going to be used immediately.
        if let Some(PathItem { interface: ASTNode::CallExpression, field: ASTField::Callee }) = path.get(0) {
            return Ok(None)
        }
        match *node {
            ViewMutFunctionExpression::EagerFunctionExpression(ref mut steal) => {
                Self::steal(*steal, |stolen| {
                    LazyFunctionExpression {
                        is_async: stolen.is_async,
                        is_generator: stolen.is_generator,
                        name: stolen.name,
                        directives: stolen.directives,
                        contents_skip: Offset::default(),
                        contents: stolen.contents
                    }.into()
                })
            }
            _ => Ok(None)
        }
    }
}
