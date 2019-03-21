//! A mechanism used to detect language fragments that can be represented more concisely
//! by using a specialized dictionary.
//!
//! For instance, if it is known that a file contains a large fragment composed of JSON,
//! we may switch the dictionary to JSON for this subtree.

use ast::*;
use binjs_shared::{SharedString, VisitMe};

use std::cell::RefCell;
use std::rc::Rc;

/// The name of the dictionary used to represent pure data subtrees (aka "JSON").
///
/// We use the monicker "pojo" (plain old javascript objects).
pub const DICTIONARY_NAME_PURE_DATA: &'static str = "pojo";

/// Properties observed when examining a subtree.
#[derive(Clone, Copy, Debug)]
enum Properties {
    /// The subtree contains only pure data nodes.
    PureDataSoFar {
        /// The number of nodes in the subtree.
        ///
        /// We use this to determine whether it's worth wrapping a subtree
        /// in a scoped dictionary, e.g. we're not going to wrap every single
        /// `LiteralNumber` in a scoped dictionary as this would waste space.
        size: usize,
    },
    /// The subtree contains at least one node that is not pure data.
    Complex,
}

impl Default for Properties {
    fn default() -> Self {
        Properties::Complex
    }
}

/// A visitor dedicated to detecting fragments of a file that are pure data
/// and injecting scoped dictionary changes to DICTIONARY_NAME_PURE_DATA for
/// these fragments.
///
/// As of this writing, the definition of pure data (or POJO) is:
///
/// ```bnf
/// POJO ::= literal number
///       |  literal boolean
///       |  literal string
///       |  literal null
///       |  [ POJO ]
///       |  { (string: POJO)* }
/// ```
///
/// Note that literal infinity is *not* a literal number.
///
/// As of this writing, this visitor does NOT attempt to introduce scoped
/// dictionary changes in the case of an ArrayExpression or an ObjectExpression
/// containing both pure data elements and complex data elements.
///
/// More precisely, consider a JS array `[pure_1, complex, pure_2]`, where `pure_1`
/// and `pure_2` are pure data, while `complex` isn't. As we have walked `pure_1`
/// before `complex`, we do not have the information that `pure_1`'s parent is
/// `complex`, so we do not attempt to rewrite `pure_1` to inject a scoped
/// dictionary change around this child. On the other hand, as we walk `pure_2`
/// after `complex`, we already have the information, so we may decide to inject
/// a scoped dictionary change around `pure_2`.
///
/// Future versions may fix this limitation.
pub struct InjectVisitor {
    /// If a subtree `T` is pure data and has a size >= `pure_data_threshold`, measured
    /// in number of nodes, and if its parent is not pure data, we should rewrite `T`
    /// to wrap it in a scoped dictionary change to DICTIONARY_NAME_PURE_DATA.
    pure_data_threshold: usize,

    /// The stack obtained by examining the tree so far.
    ///
    /// Note that default implementations of `InjectVisitor` methods automatically
    /// call `(PropertiesGuard as WalkGuard)::new()`, thus inserting `Properties::Complex`.
    stack: Rc<RefCell<Vec<Properties>>>,
}

/// A guard used to propagate `Properties` upwards in the tree.
struct PropertiesGuard {
    /// A mutable reference to the stack of the InjectVisitor.
    stack: Rc<RefCell<Vec<Properties>>>,
}
impl PropertiesGuard {
    fn new(stack: Rc<RefCell<Vec<Properties>>>, top: Properties) -> Self {
        {
            stack.borrow_mut().push(top);
        }
        PropertiesGuard { stack }
    }

    /// Create a new `PropertiesGuard` for a complex tree.
    fn complex(visitor: &InjectVisitor, _path: &WalkPath) -> Self {
        let top = Properties::Complex;
        Self::new(visitor.stack.clone(), top)
    }

    /// Create a new `PropertiesGuard` for a pure data tree.
    fn pure_data(visitor: &InjectVisitor, size: usize, _path: &WalkPath) -> Self {
        let top = Properties::PureDataSoFar { size };
        Self::new(visitor.stack.clone(), top)
    }
}
impl Drop for PropertiesGuard {
    /// When we drop the PropertiesGuard:
    ///
    /// - if both child and parent are `PureDataSoFar`, increase the number of nodes
    ///   in the parent subtree;
    /// - otherwise, mark the parent as `Complex`.
    fn drop(&mut self) {
        let mut stack = self.stack.borrow_mut();
        let me = stack.pop().unwrap();

        if stack.len() == 0 {
            // We are the root, nothing to do.
            return;
        }

        let parent = stack.last_mut().unwrap();
        let new_parent = match (&me, &*parent) {
            // If either the parent or the child is complex, the parent is complex.
            (&Properties::Complex, _) | (_, &Properties::Complex) => Properties::Complex,

            // If both are pure so far, add both sizes.
            (
                &Properties::PureDataSoFar { size: my_size },
                &Properties::PureDataSoFar { size: parent_size },
            ) => Properties::PureDataSoFar {
                size: my_size + parent_size,
            },
        };
        *parent = new_parent
    }
}

impl WalkGuard<InjectVisitor> for PropertiesGuard {
    /// Constructor called by default methods of `InjectVisitor`
    fn new(origin: &InjectVisitor, path: &WalkPath) -> Self {
        PropertiesGuard::complex(origin, path)
    }
}

type EnterResult = Result<VisitMe<PropertiesGuard>, ()>;
type ExitResult<T> = Result<Option<T>, ()>;

impl InjectVisitor {
    pub fn new(pure_data_threshold: usize) -> Self {
        Self {
            pure_data_threshold,
            stack: Rc::new(RefCell::new(Vec::new())),
        }
    }
    pub fn rewrite_script(pure_data_threshold: usize, script: &mut Script) {
        let mut visitor = Self::new(pure_data_threshold);
        script
            .walk(&mut WalkPath::new(), &mut visitor)
            .expect("Could not walk script");
    }

    /// The properties of the current node.
    fn properties(&self) -> Properties {
        self.stack.borrow().last().unwrap().clone()
    }

    /// The properties of the parent node, or `None` if there is no parent.
    fn parent_properties(&self) -> Option<Properties> {
        let stack = self.stack.borrow();
        if stack.len() >= 2 {
            Some(stack[stack.len() - 2].clone())
        } else {
            None
        }
    }
}
impl Visitor<(), PropertiesGuard> for InjectVisitor {
    // --- Literals

    fn enter_literal<'a>(&mut self, path: &WalkPath, node: &mut ViewMutLiteral<'a>) -> EnterResult {
        let _guard = if let ViewMutLiteral::LiteralInfinityExpression(_) = *node {
            // In JavaScript, this is not considered pure data as there is no syntax for it.
            PropertiesGuard::complex(self, path)
        } else {
            // Other literals are pure data.
            PropertiesGuard::pure_data(self, 1, path)
        };
        // No need to visit the children.
        Ok(VisitMe::DoneHere)
    }

    // --- Objects

    fn enter_property_name<'a>(
        &mut self,
        path: &WalkPath,
        _node: &mut ViewMutPropertyName<'a>,
    ) -> EnterResult {
        // Pure data so far.
        Ok(VisitMe::HoldThis(PropertiesGuard::pure_data(self, 0, path)))
    }

    fn enter_object_property<'a>(
        &mut self,
        path: &WalkPath,
        _node: &mut ViewMutObjectProperty<'a>,
    ) -> EnterResult {
        // Pure data so far.
        Ok(VisitMe::HoldThis(PropertiesGuard::pure_data(self, 0, path)))
    }

    fn enter_literal_property_name(
        &mut self,
        path: &WalkPath,
        _node: &mut LiteralPropertyName,
    ) -> EnterResult {
        // Pure data so far.
        Ok(VisitMe::HoldThis(PropertiesGuard::pure_data(self, 1, path)))
    }

    fn enter_object_expression(
        &mut self,
        path: &WalkPath,
        _node: &mut ObjectExpression,
    ) -> EnterResult {
        // Pure data so far.
        Ok(VisitMe::HoldThis(PropertiesGuard::pure_data(self, 1, path)))
    }

    fn enter_data_property(&mut self, path: &WalkPath, _node: &mut DataProperty) -> EnterResult {
        // Pure data so far.
        Ok(VisitMe::HoldThis(PropertiesGuard::pure_data(self, 1, path)))
    }

    // --- Arrays

    fn enter_spread_element_or_expression<'a>(
        &mut self,
        path: &WalkPath,
        _node: &mut ViewMutSpreadElementOrExpression<'a>,
    ) -> EnterResult {
        // Pure data so far.
        Ok(VisitMe::HoldThis(PropertiesGuard::pure_data(self, 1, path)))
    }

    fn enter_array_expression(
        &mut self,
        path: &WalkPath,
        _node: &mut ArrayExpression,
    ) -> EnterResult {
        // Pure data so far.
        Ok(VisitMe::HoldThis(PropertiesGuard::pure_data(self, 1, path)))
    }

    // --- Wrapping in a scoped dictionary

    fn enter_expression<'a>(
        &mut self,
        path: &WalkPath,
        node: &mut ViewMutExpression<'a>,
    ) -> EnterResult {
        match *node {
            ViewMutExpression::LiteralNumericExpression(_)
            | ViewMutExpression::ObjectExpression(_)
            | ViewMutExpression::LiteralStringExpression(_)
            | ViewMutExpression::LiteralNullExpression(_)
            | ViewMutExpression::LiteralBooleanExpression(_)
            | ViewMutExpression::ArrayExpression(_) =>
            // For the moment, this is is pure data
            {
                Ok(VisitMe::HoldThis(PropertiesGuard::pure_data(self, 0, path)))
            }
            _ =>
            // We already know that this is complex.
            {
                Ok(VisitMe::HoldThis(PropertiesGuard::complex(self, path)))
            }
        }
    }

    fn exit_expression<'a>(
        &mut self,
        _path: &WalkPath,
        node: &mut ViewMutExpression<'a>,
    ) -> ExitResult<Expression> {
        if let Properties::PureDataSoFar { size } = self.properties() {
            match self.parent_properties() {
                None | Some(Properties::Complex) => {
                    // We're the topmost root of a pure data subtree,
                    // so embed `node` in a `BinASTExpressionWithProbabilityTable`.
                    if size >= self.pure_data_threshold {
                        return Ok(Some(
                            BinASTExpressionWithProbabilityTable {
                                table: SharedString::from_str(DICTIONARY_NAME_PURE_DATA),
                                expression: node.steal(),
                            }
                            .into(),
                        ));
                    }
                }
                _ => {}
            }
        }
        // Otherwise, nothing to change.
        Ok(None)
    }
}

/// A visitor dedicated to removing instances of `BinASTExpressionWithProbabilityTable`.
pub struct CleanupVisitor;
impl CleanupVisitor {
    pub fn rewrite_script(script: &mut Script) {
        let mut visitor = Self;
        script
            .walk(&mut WalkPath::new(), &mut visitor)
            .expect("Could not walk script");
    }
}

impl Visitor<()> for CleanupVisitor {
    fn exit_expression<'a>(
        &mut self,
        _path: &WalkPath,
        node: &mut ViewMutExpression<'a>,
    ) -> ExitResult<Expression> {
        if let ViewMutExpression::BinASTExpressionWithProbabilityTable(_) = *node {
            if let Expression::BinASTExpressionWithProbabilityTable(scoped) = node.steal() {
                Ok(Some(scoped.expression))
            } else {
                // We just stole `ViewMutExpression::BinASTExpressionWithProbabilityTable`.
                // If the result is not a `Expression::BinASTExpressionWithProbabilityTable`,
                // that's a bug in our implementation of `steal()`.
                panic!();
            }
        } else {
            Ok(None)
        }
    }
}

#[cfg(test)]
mod test {
    use ast::*;
    use binjs_shared::*;
    use sublanguages::{CleanupVisitor, InjectVisitor, DICTIONARY_NAME_PURE_DATA};

    fn check(threshold: usize, source: Script, expected: Script) {
        // Rewrite.
        let mut injected = source.clone();
        InjectVisitor::rewrite_script(threshold, &mut injected);
        assert_eq!(injected, expected);

        // Rewrite back.
        let mut cleaned = injected.clone();
        CleanupVisitor::rewrite_script(&mut cleaned);
        assert_eq!(source, cleaned);
    }

    #[test]
    fn test_sublanguage_pure_data_positive() {
        let source = Script {
            scope: Default::default(),
            directives: vec![],
            statements: vec![ExpressionStatement {
                expression: ArrayExpression {
                    elements: vec![
                        Some(LiteralBooleanExpression { value: true }.into()),
                        Some(LiteralNullExpression {}.into()),
                        Some(LiteralNumericExpression { value: 5. }.into()),
                        Some(LiteralBooleanExpression { value: true }.into()),
                        Some(LiteralBooleanExpression { value: true }.into()),
                        Some(LiteralBooleanExpression { value: true }.into()),
                        Some(LiteralBooleanExpression { value: true }.into()),
                        Some(LiteralBooleanExpression { value: true }.into()),
                    ],
                }
                .into(),
            }
            .into()],
        };
        let expected = Script {
            scope: Default::default(),
            directives: vec![],
            statements: vec![ExpressionStatement {
                expression: BinASTExpressionWithProbabilityTable {
                    table: SharedString::from_str(DICTIONARY_NAME_PURE_DATA),
                    expression: ArrayExpression {
                        elements: vec![
                            Some(LiteralBooleanExpression { value: true }.into()),
                            Some(LiteralNullExpression {}.into()),
                            Some(LiteralNumericExpression { value: 5. }.into()),
                            Some(LiteralBooleanExpression { value: true }.into()),
                            Some(LiteralBooleanExpression { value: true }.into()),
                            Some(LiteralBooleanExpression { value: true }.into()),
                            Some(LiteralBooleanExpression { value: true }.into()),
                            Some(LiteralBooleanExpression { value: true }.into()),
                        ],
                    }
                    .into(),
                }
                .into(),
            }
            .into()],
        };

        check(5, source, expected);
    }

    #[test]
    fn test_sublanguage_pure_data_positive_object() {
        let source = Script {
            scope: Default::default(),
            directives: vec![],
            statements: vec![ExpressionStatement {
                expression: ObjectExpression {
                    properties: vec![
                        DataProperty {
                            name: LiteralPropertyName {
                                value: SharedString::from_str("a"),
                            }
                            .into(),
                            expression: LiteralBooleanExpression { value: true }.into(),
                        }
                        .into(),
                        DataProperty {
                            name: LiteralPropertyName {
                                value: SharedString::from_str("b"),
                            }
                            .into(),
                            expression: LiteralBooleanExpression { value: true }.into(),
                        }
                        .into(),
                        DataProperty {
                            name: LiteralPropertyName {
                                value: SharedString::from_str("c"),
                            }
                            .into(),
                            expression: LiteralBooleanExpression { value: true }.into(),
                        }
                        .into(),
                        DataProperty {
                            name: LiteralPropertyName {
                                value: SharedString::from_str("d"),
                            }
                            .into(),
                            expression: LiteralBooleanExpression { value: true }.into(),
                        }
                        .into(),
                    ],
                }
                .into(),
            }
            .into()],
        };
        let expected = Script {
            scope: Default::default(),
            directives: vec![],
            statements: vec![ExpressionStatement {
                expression: BinASTExpressionWithProbabilityTable {
                    table: SharedString::from_str(DICTIONARY_NAME_PURE_DATA),
                    expression: ObjectExpression {
                        properties: vec![
                            DataProperty {
                                name: LiteralPropertyName {
                                    value: SharedString::from_str("a"),
                                }
                                .into(),
                                expression: LiteralBooleanExpression { value: true }.into(),
                            }
                            .into(),
                            DataProperty {
                                name: LiteralPropertyName {
                                    value: SharedString::from_str("b"),
                                }
                                .into(),
                                expression: LiteralBooleanExpression { value: true }.into(),
                            }
                            .into(),
                            DataProperty {
                                name: LiteralPropertyName {
                                    value: SharedString::from_str("c"),
                                }
                                .into(),
                                expression: LiteralBooleanExpression { value: true }.into(),
                            }
                            .into(),
                            DataProperty {
                                name: LiteralPropertyName {
                                    value: SharedString::from_str("d"),
                                }
                                .into(),
                                expression: LiteralBooleanExpression { value: true }.into(),
                            }
                            .into(),
                        ],
                    }
                    .into(),
                }
                .into(),
            }
            .into()],
        };

        check(5, source, expected);
    }
    /// We have manually specified that `LiteralInfinityExpression` is complex.
    /// Check that this is taken into account.
    #[test]
    fn test_sublanguage_pure_data_negative_with_infinity() {
        let source = Script {
            scope: Default::default(),
            directives: vec![],
            statements: vec![ExpressionStatement {
                expression: ArrayExpression {
                    elements: vec![
                        Some(LiteralBooleanExpression { value: true }.into()),
                        Some(LiteralInfinityExpression {}.into()), // This prevents the subtree from being pure data.
                        Some(LiteralNumericExpression { value: 5. }.into()),
                        Some(LiteralBooleanExpression { value: true }.into()),
                        Some(LiteralBooleanExpression { value: true }.into()),
                        Some(LiteralBooleanExpression { value: true }.into()),
                        Some(LiteralBooleanExpression { value: true }.into()),
                        Some(LiteralBooleanExpression { value: true }.into()),
                    ],
                }
                .into(),
            }
            .into()],
        };

        check(5, source.clone(), source);
    }

    /// We have no rule for `ThisExpression`, so it should be complex.
    /// Check that this is taken into account.
    #[test]
    fn test_sublanguage_pure_data_negative_with_this() {
        let source = Script {
            scope: Default::default(),
            directives: vec![],
            statements: vec![ExpressionStatement {
                expression: ArrayExpression {
                    elements: vec![
                        Some(LiteralBooleanExpression { value: true }.into()),
                        Some(ThisExpression {}.into()), // This prevents the subtree from being pure data.
                        Some(LiteralNumericExpression { value: 5. }.into()),
                        Some(LiteralBooleanExpression { value: true }.into()),
                        Some(LiteralBooleanExpression { value: true }.into()),
                        Some(LiteralBooleanExpression { value: true }.into()),
                        Some(LiteralBooleanExpression { value: true }.into()),
                        Some(LiteralBooleanExpression { value: true }.into()),
                    ],
                }
                .into(),
            }
            .into()],
        };

        check(5, source.clone(), source);
    }

    /// We have no rule for `ThisExpression`, so it should be complex.
    /// Check that this is taken into account.
    #[test]
    fn test_sublanguage_pure_data_negative_object_with_this() {
        let source = Script {
            scope: Default::default(),
            directives: vec![],
            statements: vec![ExpressionStatement {
                expression: ObjectExpression {
                    properties: vec![
                        DataProperty {
                            name: LiteralPropertyName {
                                value: SharedString::from_str("a"),
                            }
                            .into(),
                            expression: LiteralBooleanExpression { value: true }.into(),
                        }
                        .into(),
                        DataProperty {
                            name: LiteralPropertyName {
                                value: SharedString::from_str("b"),
                            }
                            .into(),
                            expression: LiteralBooleanExpression { value: true }.into(),
                        }
                        .into(),
                        DataProperty {
                            name: LiteralPropertyName {
                                value: SharedString::from_str("c"),
                            }
                            .into(),
                            expression: LiteralBooleanExpression { value: true }.into(),
                        }
                        .into(),
                        DataProperty {
                            name: LiteralPropertyName {
                                value: SharedString::from_str("d"),
                            }
                            .into(),
                            expression: ThisExpression {}.into(),
                        }
                        .into(),
                    ],
                }
                .into(),
            }
            .into()],
        };

        check(5, source.clone(), source);
    }
}
