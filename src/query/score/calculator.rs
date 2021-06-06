use crate::index::{DocumentPointer, FieldDetails};
use std::cell::Ref;

/**
Implement this trait for creating a scoring functionality
 * typeparam `T` Document key.
 * `document_pointer` reference to a DocumentPointer (a place in the inverted index tree)
 * `idf`Invserse document frequency. Read more at https://en.wikipedia.org/wiki/Okapi_BM25
 * `field_lengths` Field lengths is an array that contains number of terms in each indexed text field.
 * `fields_boost` expected boost from query arguments
 * `expansion_boost` A number between -inf and 1 that describes how similiar the query is to a document in terms of length.
 * `field` information about the fields
*/
pub trait ScoreCalculator<T> {
    /**

    */
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
