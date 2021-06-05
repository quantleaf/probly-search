use crate::index::{DocumentPointer, FieldDetails};
use std::{cell::RefCell, rc::Rc};

pub trait ScoreCalculator<T> {
    fn score(
        &self,
        document_pointer: Rc<RefCell<DocumentPointer<T>>>,
        idf: f64,
        field_lengths: &[usize],
        fields_boost: &[f64],
        expansion_boost: f64,
        fields: &[FieldDetails],
    ) -> f64;
}
