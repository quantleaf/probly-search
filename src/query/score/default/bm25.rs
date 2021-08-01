/***
    https://en.wikipedia.org/wiki/Okapi_BM25
*/

use std::{collections::HashMap, fmt::Debug};

use crate::{
    index::{DocumentDetails, DocumentPointer},
    query::score::calculator::{FieldData, ScoreCalculator, TermData},
};

pub struct BM25 {
    /// `bm25k1` BM25 ranking function constant `k1`, controls non-linear term frequency normalization (saturation).
    pub bm25k1: f64,

    /// `bm25b` BM25 ranking function constant `b`, controls to what degree document length normalizes tf values.
    pub bm25b: f64,
}
pub fn new() -> BM25 {
    BM25 {
        bm25b: 0.75,
        bm25k1: 1.2,
    }
}
pub struct BM25TermCalculations {
    /// Inverse document frequency
    idf: f64,

    /// Boosting based on term length matching. Bounded by (-inf, 1]
    expansion_boost: f64,
}
impl<T: Debug> ScoreCalculator<T, BM25TermCalculations> for BM25 {
    fn before_each(
        &mut self,
        term_expansion: &TermData,
        document_frequency: usize,
        documents: &HashMap<T, DocumentDetails<T>>,
    ) -> Option<BM25TermCalculations> {
        Some(BM25TermCalculations {
            expansion_boost: {
                if term_expansion.query_term_expanded == term_expansion.query_term {
                    1_f64
                } else {
                    f64::ln(
                        1_f64
                            + (1_f64
                                / (1_f64 + (term_expansion.query_term_expanded.len() as f64)
                                    - (term_expansion.query_term.len() as f64))),
                    )
                }
            },
            idf: f64::ln(
                1_f64
                    + ((documents.len() - document_frequency) as f64 + 0.5)
                        / (document_frequency as f64 + 0.5),
            ),
        })
    }

    fn score(
        &mut self,
        before_output: Option<&BM25TermCalculations>,
        document_pointer: &DocumentPointer<T>,
        document_details: &DocumentDetails<T>,
        field_data: &FieldData,
        _: &TermData,
    ) -> Option<f64> {
        let pre_calculations = &before_output.unwrap(); // it will exist as we need BM25 parameters
        let mut score: f64 = 0_f64;
        for x in 0..document_details.field_length.len() {
            let mut tf = (&document_pointer.term_frequency[x]).to_owned() as f64;
            if tf > 0_f64 {
                // calculating BM25 tf
                let field_length = &document_details.field_length[x];
                let field_details = &field_data.fields[x];
                let avg_field_length = field_details.avg;
                tf = ((self.bm25k1 + 1_f64) * tf)
                    / (self.bm25k1
                        * ((1_f64 - self.bm25b)
                            + self.bm25b
                                * (field_length.to_owned() as f64 / avg_field_length as f64))
                        + tf);
                score += tf
                    * pre_calculations.idf
                    * field_data.fields_boost[x]
                    * pre_calculations.expansion_boost;
            }
        }
        if score > 0_f64 {
            return Some(score);
        }
        None
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::{
        query::QueryResult,
        test_util::{build_test_index, test_score},
    };
    #[test]
    fn it_should_return_doc_1() {
        let mut x = build_test_index(&["a b c", "c d e"]);
        test_score(
            &mut x,
            &mut new(),
            &"a".to_string(),
            vec![QueryResult {
                key: 0,
                score: 0.6931471805599453,
            }],
        );
    }

    #[test]
    fn it_should_return_doc_1_and_2() {
        let mut x = build_test_index(&["a b c", "c d e"]);
        test_score(
            &mut x,
            &mut new(),
            &"c".to_string(),
            vec![
                QueryResult {
                    key: 0,
                    score: 0.1823215567939546,
                },
                QueryResult {
                    key: 1,
                    score: 0.1823215567939546,
                },
            ],
        );
    }
}
