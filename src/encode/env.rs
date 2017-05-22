use kind::IdUsage;

use easter::id::*;

use std::cell::RefCell;
use std::collections::HashSet;
use std::rc::Rc;


struct EnvNode {
    let_declarations: HashSet<String>,
    var_declarations: HashSet<String>,
    looks_like_direct_eval: bool,
    link: EnvLink
}
impl EnvNode {
    fn new(link: EnvLink) -> Self {
        EnvNode {
            let_declarations: HashSet::new(),
            var_declarations: HashSet::new(),
            looks_like_direct_eval: false,
            link
        }
    }

    fn looks_like_direct_eval(&self) -> bool {
        self.looks_like_direct_eval
        && !self.let_declarations.contains("eval")
        && !self.var_declarations.contains("eval")
    }
}

enum EnvLink {
    Toplevel,
    Function(Env),
    Block(Env)
}
#[derive(Clone)]
pub struct Env(Rc<RefCell<EnvNode>>);
impl Drop for Env {
    fn drop(&mut self) {
        let mut borrow = self.0.borrow_mut();
        let looks_like_direct_eval = borrow.looks_like_direct_eval();
        if let EnvNode {
            link: EnvLink::Block(ref mut parent),
            ref mut var_declarations,
            ..
        } = *borrow {
            if looks_like_direct_eval {
                // Propagate `eval`, `var` to parent.
                parent.add_eval()
            }
            let ref mut parent_var_declarations = parent.0
                .borrow_mut()
                .var_declarations;
            for x in var_declarations.drain() {
                parent_var_declarations.insert(x);
            }
        }
    }
}
impl Env {
    fn new(link: EnvLink) -> Self {
        Env(Rc::new(RefCell::new(EnvNode::new(link))))
    }

    pub fn toplevel() -> Self {
        Env::new(EnvLink::Toplevel)
    }

    /// Enter a function. Every variable
    /// declaration local to the function
    /// (e.g. pattern, `let`, `var`, `const`,
    /// arguments) is dropped when we leave
    /// the function. Other declarations
    /// are unaffected.
    pub fn enter_function(&mut self) -> Env {
        Env::new(EnvLink::Function(self.clone()))
    }

    /// Enter a block. Every variable
    /// declaration local to the block
    /// (e.g. `let`) is dropped when we leave
    /// the block. Other declarations are
    /// unaffected.
    pub fn enter_block(&mut self) -> Self {
        Env::new(EnvLink::Block(self.clone()))
    }

    pub fn add_binding<'b>(&mut self, binding: &IdUsage<'b, Id>) {
        use kind::IdUsageKind::*;
        match binding.kind() {
            Let => {
                // Add to current block.
                self.0
                    .borrow_mut()
                    .let_declarations
                    .insert(binding.data().name.clone().into_string());
            },
            Var => {
                // Add to current block, then to parent.
                {
                    self.0
                        .borrow_mut()
                        .var_declarations
                        .insert(binding.data().name.clone().into_string());
                }
                use self::EnvLink::*;
                match self.0.borrow_mut().link {
                    Function(ref mut parent)
                    | Block(ref mut parent) =>
                        parent.add_binding(binding),
                    _ => {}
                }
            }
            // FIXME: Double-check, but it looks like other declarations are just ignored.
            _ => {}
        }
    }

    pub fn looks_like_direct_eval(&self) -> bool {
        let borrow = self.0.borrow();
        borrow.looks_like_direct_eval()
    }

    pub fn let_declarations(&self) -> Vec<String> {
        self.0
            .borrow()
            .let_declarations
            .iter()
            .cloned()
            .collect()
    }

    pub fn var_declarations(&self) -> Vec<String> {
        self.0
            .borrow()
            .var_declarations
            .iter()
            .cloned()
            .collect()
    }

    /// Mark that we have been calling `eval(...)`.
    ///
    /// Note that this does NOT mean that we have a direct call to
    /// `eval`. We only find this out when we leave the block/function
    /// and we find out that `eval` has not been bound.
    pub fn add_eval(&mut self) {
        self.0.borrow_mut().looks_like_direct_eval = true
    }
}