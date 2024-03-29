use crate::{
    index::{DocumentDetails, DocumentPointer, FieldDetails, InvertedIndexNode},
    QueryResult,
};
use hashbrown::HashMap;
use std::fmt::Debug;
use typed_generational_arena::StandardIndex as ArenaIndex;

pub struct TermData<'a> {
    // Current query term index.
    pub query_term_index: usize,
    /// Current query term.
    pub query_term: &'a str,
    // Current expanded term from the expanded terms generated
    // from the current query term `query_term`
    pub query_term_expanded: &'a str,
    // Total number of query terms present in the query this TermData was created from
    pub query_terms_len: usize,
}

pub struct FieldData<'a> {
    /// `fields_boost` expected boost from query arguments
    pub fields_boost: &'a [f64],
    /// Statistics about each field.
    pub fields: &'a [FieldDetails],
}

/**
Implement this trait for creating a scoring functionality
 *  typeparam `T` Document key.
 *  typeparam `M` memory object that is emitted from the before_each calculations, that is later injected into the score function
*/
pub trait ScoreCalculator<T: Debug, M> {
    /**
    For expansion term generated for each query term, this method is invoked prior to iterating on the inverted index tree.
    This method can be used to do precalculations with the document frequency parameter (essential for BM25 implementation)
    * `term_expansion` Data about the current term expansion to generate score from
    * `document_frequency` The amount of associated documents to `query_term_expanded`
    * `documents` a map of all documents by key
    */

    #[allow(unused_variables)]
    fn before_each(
        &mut self,
        term_expansion: &TermData,
        document_frequency: usize,
        documents: &HashMap<T, DocumentDetails<T>>,
    ) -> Option<M> {
        None
    }

    /**
     * `before_output` output from `before_each(..)` function, if any.
     * `document_pointer` reference to a DocumentPointer (a place in the inverted index tree)
     * `field_data` Data about the fields
     * `term_expansion` Data about the current term expansion to generate score from
     */
    fn score(
        &mut self,
        before_output: Option<&M>,
        document_pointer: &DocumentPointer<T>,
        document_details: &DocumentDetails<T>,
        index_node: &ArenaIndex<InvertedIndexNode<T>>,
        field_data: &FieldData,
        term_expansion: &TermData,
    ) -> Option<f64>;

    #[allow(unused_variables)]
    fn finalize(&mut self, scores: &mut Vec<QueryResult<T>>) {}
}
