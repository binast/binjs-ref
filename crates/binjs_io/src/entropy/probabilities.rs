use std::cell::RefCell;
use std::rc::Rc;

/// Representation of a symbol in a Cumulative Distribution Frequency (CDF).
#[derive(Clone)]
pub struct Symbol {
    /// The index of the symbol in the Cumulative Distribution Frequency (CDF).
    pub index: usize,

    /// The Cumulative Distribution Frequency (CDF), shared between a number of symbols.
    pub distribution: Rc<RefCell<range_encoding::CumulativeDistributionFrequency>>,
}

/// A structure that may be converted into a probability distribution
/// or a set of probability distributions.
pub trait InstancesToProbabilities {
    type AsProbabilities;
    fn instances_to_probabilities(self) -> Self::AsProbabilities;
}
