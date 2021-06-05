use std::{cell::RefCell, rc::Rc};

use crate::{
    index::{DocumentPointer, FieldDetails},
    query::score::calculator::ScoreCalculator,
};

pub struct BM25 {
    pub bm25k1: f64,
    pub bm25b: f64,
}
pub fn default() -> BM25 {
    BM25 {
        bm25b: 0.75,
        bm25k1: 1.2,
    }
}
impl<T> ScoreCalculator<T> for BM25 {
    fn score(
        &self,
        document_pointer: Rc<RefCell<DocumentPointer<T>>>,
        idf: f64,
        field_lengths: &[usize],
        fields_boost: &[f64],
        expansion_boost: f64,
        fields: &[FieldDetails],
    ) -> f64 {
        let mut score: f64 = 0_f64;
        for x in 0..field_lengths.len() {
            let mut tf = (&document_pointer.borrow().term_frequency[x]).to_owned() as f64;
            if tf > 0_f64 {
                // calculating BM25 tf
                let field_length = &field_lengths[x];
                let field_details = &fields[x];
                let avg_field_length = field_details.avg;
                tf = ((self.bm25k1 + 1_f64) * tf)
                    / (self.bm25k1
                        * ((1_f64 - self.bm25b)
                            + self.bm25b
                                * (field_length.to_owned() as f64 / avg_field_length as f64))
                        + tf);
                score += tf * idf * fields_boost[x] * expansion_boost;
            }
        }
        score
    }
}
