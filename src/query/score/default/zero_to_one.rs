/*
    Normalized 0 to 1 bound scoring
    This is a simple algorithm for performing normalized "pre" term scoring
    The goal/"force" acting on the scoring is:
    -   Penalize repeating query terms. If a document is "abc", and we query "abc abc", score should be 0.5.
    -   Do not penalize repeating document terms, If a document is "abc abc", and we query "abc", score should be perfect (1)
    -   We want the query to have the same amount of terms as the document for a perfect score (1)
    -   We want the query term lengths to match the document term lengths
*/
use std::{
    collections::{HashMap, HashSet},
    fmt::Debug,
    hash::Hash,
};

use crate::{
    index::{DocumentDetails, DocumentPointer},
    query::{
        score::calculator::{FieldData, ScoreCalculator, TermData},
        QueryResult,
    },
};

pub struct ZeroToOne<T> {
    visited_terms_by_document: HashMap<T, HashSet<String>>,
}
pub fn new<T: Eq + Hash + Clone + Debug>() -> ZeroToOne<T> {
    ZeroToOne {
        visited_terms_by_document: HashMap::new(),
    }
}
pub struct ZeroToOneBeforeCalculations {}
impl<T: Debug + Eq + Hash + Clone> ScoreCalculator<T, ZeroToOneBeforeCalculations>
    for ZeroToOne<T>
{
    fn score(
        &mut self,
        _: Option<&ZeroToOneBeforeCalculations>,
        document_pointer: &DocumentPointer<T>,
        document_details: &DocumentDetails<T>,
        field_data: &FieldData,
        term_data: &TermData,
    ) -> Option<f64> {
        /*
           To prevent repeating query terms generating higher scores we track and manipulate
           statistics to track whether scoring has happened for a doc with a certain term
        */
        let key = &document_details.key;
        let mut has_key = false;
        let contains_term_on_key = match self.visited_terms_by_document.get(key) {
            Some(terms) => {
                has_key = true;
                terms.contains(term_data.query_term)
            }
            None => false,
        };
        if !has_key {
            self.visited_terms_by_document
                .insert(key.to_owned(), HashSet::new());
        }

        // Add term to visited
        self.visited_terms_by_document
            .get_mut(key)
            .unwrap()
            .insert(term_data.query_term.to_owned());

        if contains_term_on_key {
            return None; // Prevent boosting on repeating query terms
        }

        /*
           We are good to go, calculating the score which we will define
            as a product of the ratio of how close we are in terms of number
            of terms, and the ratio of how close we are in terms of number
            of characters to documents term freq (tf) vs query terms

        */
        let mut score: f64 = 0_f64;
        for x in 0..document_details.field_length.len() {
            let tf = (&document_pointer.term_frequency[x]).to_owned() as f64;
            if tf > 0_f64 {
                let num_of_terms = term_data.all_query_terms.len() as f64;
                let term_exp_len = term_data.query_term_expanded.len() as f64;
                let term_len = term_data.query_term.len() as f64;
                let field_length = document_details.field_length[x];
                let score_contribution = tf / f64::max(field_length as f64, num_of_terms)
                    * (1_f64 - f64::abs(term_exp_len - term_len) / (term_exp_len));
                score += score_contribution * field_data.fields_boost[x];
            }
        }
        if score > 0_f64 {
            return Some(score);
        }
        None
    }

    fn finalize(&mut self, _: &mut Vec<QueryResult<T>>) {
        self.visited_terms_by_document = HashMap::new(); // Clear statistics since we might reuse this struct for another query
    }
}

#[cfg(test)]
mod tests {

    

    use super::*;
    use crate::{
        index::{create_index_arenas},
        test_util::{build_test_index, test_score},
    };

    #[test]
    fn it_should_perform_partial_matching() {
        let index_arenas = create_index_arenas();
        let mut x = build_test_index(&["abc", "abcefg"], &index_arenas);
        test_score(
            &mut x,
            &mut new(),
            &"abc".to_string(),
            vec![
                QueryResult {
                    key: 0,
                    score: 1_f64,
                },
                QueryResult {
                    key: 1,
                    score: 0.5_f64,
                },
            ],
        );
    }

    #[test]
    fn it_should_penalize_repeating_query_terms() {
        let index_arenas = create_index_arenas();
        let mut x = build_test_index(&["abc"], &index_arenas);
        test_score(
            &mut x,
            &mut new(),
            &"abc abc".to_string(),
            vec![QueryResult {
                key: 0_usize,
                score: 0.5_f64,
            }],
        );
    }

    #[test]
    fn it_should_not_penalize_repeating_document_terms() {
        let index_arenas = create_index_arenas();
        let mut x = build_test_index(&["abc abc"], &index_arenas);
        test_score(
            &mut x,
            &mut new(),
            &"abc".to_string(),
            vec![QueryResult {
                key: 0_usize,
                score: 1_f64,
            }],
        );
    }

    #[test]
    fn it_should_retrieve_multiple_results() {
        let index_arenas = create_index_arenas();
        let mut x = build_test_index(
            &[
                "abcdef",
                "abc abcdef",
                "abcdef abcdef",
                "abcdef abcdefghi",
                "def abcdef",
            ],
            &index_arenas,
        );

        test_score(
            &mut x,
            &mut new(),
            &"abc".to_string(),
            vec![
                QueryResult {
                    key: 0_usize,
                    score: 0.5_f64,
                },
                QueryResult {
                    key: 1_usize,
                    score: 0.5_f64,
                },
                QueryResult {
                    key: 2_usize,
                    score: 0.5_f64,
                },
                QueryResult {
                    key: 3_usize,
                    score: 0.25_f64,
                },
                QueryResult {
                    key: 4_usize,
                    score: 0.25_f64,
                },
            ],
        );
    }

    #[test]
    fn it_should_retrieve_multiple_results_and_penalize_repeating_query_terms() {
        let index_arenas = create_index_arenas();
        let mut x = build_test_index(
            &[
                "abcdef",
                "abc abcdef",
                "abcdef abcdef",
                "abcdef abcdefghi",
                "def abcdef",
            ],
            &index_arenas,
        );

        test_score(
            &mut x,
            &mut new(),
            &"abc abc".to_string(),
            vec![
                QueryResult {
                    key: 1_usize,
                    score: 0.5_f64,
                },
                QueryResult {
                    key: 2_usize,
                    score: 0.5_f64,
                },
                QueryResult {
                    key: 0_usize,
                    score: 0.25_f64,
                },
                QueryResult {
                    key: 3_usize,
                    score: 0.25_f64,
                },
                QueryResult {
                    key: 4_usize,
                    score: 0.25_f64,
                },
            ],
        );
    }
}
