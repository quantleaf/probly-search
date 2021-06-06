use crate::index::{DocumentPointer, FieldDetails};
use std::cell::Ref;

/**
Implement this trait for creating a scoring functionality
 * typeparam `T` Document key.
*/
pub trait ScoreCalculator<T> {
    fn score(
        &self,
        document_pointer: Ref<DocumentPointer<T>>,
        idf: f64,
        field_lengths: &[usize],
        fields_boost: &[f64],
        expansion_boost: f64,
        fields: &[FieldDetails],
    ) -> f64;
}
