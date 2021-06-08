/***
    normalized 0 to 1 bound scoring
    This is a simple algorithm for performing normalized scoring
    The goal/"force" acting on the scoring is:
    -   Penalize repeating query terms. If a document is "abc", and we query "abc abc", score should be 0.5.
    -   Do not penalize repeating document terms, If a document is "abc abc", and we query "abc", score should be perfect (1)
    -   We want the quey to have the same amount of terms as the document for a perfect score (1)
    -   We want the query term lengths to match the document term lengths
*/
use std::{
    cell::Ref,
    collections::{HashMap, HashSet},
    fmt::Debug,
    hash::Hash,
};

use crate::{
    index::DocumentPointer,
    query::score::calculator::{FieldData, ScoreCalculator, TermData},
};

pub struct ZeroToOne<T> {
    visited_terms_by_document: HashMap<T, HashSet<String>>,
}
pub fn new<T>() -> ZeroToOne<T> {
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
        document_pointer: Ref<DocumentPointer<T>>,
        field_data: &FieldData,
        term_data: &TermData,
    ) -> Option<f64> {
        /*
           To prevent repeating query terms generating higher scores we track and manipulate
           statistics to track whether scoring has happened for a doc with a certain term
        */
        let key = &document_pointer.details.borrow().key;
        let mut has_key = false;
        let contains_term_on_key = match self.visited_terms_by_document.get(&key) {
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
        self.visited_terms_by_document
            .get_mut(&key)
            .unwrap()
            .insert(term_data.query_term.to_owned());

        if contains_term_on_key {
            return None; // Prevent boosting on repeating query terms
        }

        /*
           We are good to go, calculating the score which we will define as a joint product
           that is maximized when the number of terms and the number of characters match between
           documents and the search query

        */
        let mut score: f64 = 0_f64;
        for x in 0..field_data.field_lengths.len() {
            let tf = (&document_pointer.term_frequency[x]).to_owned() as f64;
            if tf > 0_f64 {
                // special
                let num_of_terms = term_data.all_query_terms.len() as f64;
                let term_exp_len = term_data.query_term_expanded.len() as f64;
                let term_len = term_data.query_term.len() as f64;
                let field_length = field_data.field_lengths[x];
                let score_contribution = tf / f64::max(field_length as f64, num_of_terms)
                    * (1_f64 - f64::abs(term_exp_len - term_len) / (term_exp_len as f64));
                score += score_contribution * field_data.fields_boost[x];
            }
        }
        if score > 0_f64 {
            return Some(score);
        }
        None
    }

    fn after_all(&mut self) {
        self.visited_terms_by_document = HashMap::new(); // Clear statistics since we might resuse this struct for another query
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::{
        index::{add_document_to_index, create_index, Index},
        query::{query, QueryResult},
    };

    fn approx_equal(a: f64, b: f64, dp: u8) -> bool {
        let p: f64 = 10f64.powf(-(dp as f64));

        if (a - b).abs() < p {
            return true;
        } else {
            return false;
        }
    }

    struct Doc {
        id: usize,
        title: String,
    }
    fn tokenizer(s: &str) -> Vec<String> {
        s.split(' ')
            .map(|slice| slice.to_owned())
            .collect::<Vec<String>>()
    }
    fn title_extract(d: &Doc) -> Option<&str> {
        Some(d.title.as_str())
    }

    fn filter(s: &String) -> String {
        s.to_owned()
    }

    fn test_score(mut idx: &mut Index<usize>, q: &str, expected: Vec<QueryResult<usize>>) {
        let mut results = query(
            &mut idx,
            q,
            &mut new(),
            tokenizer,
            filter,
            &vec![1., 1.],
            None,
        );
        results.sort_by(|a, b| {
            let mut sort = b.score.partial_cmp(&a.score).unwrap();
            sort = sort.then_with(|| a.key.partial_cmp(&b.key).unwrap());
            return sort;
        });

        for (index, result) in results.iter().enumerate() {
            assert_eq!(expected[index], *result);
            assert_eq!(approx_equal(expected[index].score, result.score, 8), true)
        }
    }

    /***
        Create a index with oducments with title fields, with increasing ids starting from 0
    */
    fn build_index(titles: &[&str]) -> Index<usize> {
        let mut idx: Index<usize> = create_index(1);

        for (index, title) in titles.iter().enumerate() {
            let doc = Doc {
                id: index,
                title: title.to_string(),
            };
            add_document_to_index(&mut idx, &[title_extract], tokenizer, filter, doc.id, doc);
        }
        idx
    }

    #[test]
    fn it_should_perform_partial_matching() {
        let mut idx = build_index(&[&"abc", &"abcefg"]);
        test_score(
            &mut idx,
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
        let mut idx = build_index(&[&"abc"]);
        test_score(
            &mut idx,
            &"abc abc".to_string(),
            vec![QueryResult {
                key: 0 as usize,
                score: 0.5_f64,
            }],
        );
    }

    #[test]
    fn it_should_not_penalize_repeating_document_terms() {
        let mut idx = build_index(&[&"abc abc"]);
        test_score(
            &mut idx,
            &"abc".to_string(),
            vec![QueryResult {
                key: 0 as usize,
                score: 1_f64,
            }],
        );
    }

    #[test]
    fn it_should_retrieve_multiple_results() {
        let mut idx = build_index(&[
            &"abcdef",
            &"abc abcdef",
            &"abcdef abcdef",
            &"abcdef abcdefghi",
            &"def abcdef",
        ]);

        test_score(
            &mut idx,
            &"abc".to_string(),
            vec![
                QueryResult {
                    key: 0 as usize,
                    score: 0.5_f64,
                },
                QueryResult {
                    key: 1 as usize,
                    score: 0.5_f64,
                },
                QueryResult {
                    key: 2 as usize,
                    score: 0.5_f64,
                },
                QueryResult {
                    key: 3 as usize,
                    score: 0.25_f64,
                },
                QueryResult {
                    key: 4 as usize,
                    score: 0.25_f64,
                },
            ],
        );
    }

    #[test]
    fn it_should_retrieve_multiple_results_and_penalize_repeating_query_terms() {
        let mut idx = build_index(&[
            &"abcdef",
            &"abc abcdef",
            &"abcdef abcdef",
            &"abcdef abcdefghi",
            &"def abcdef",
        ]);

        test_score(
            &mut idx,
            &"abc abc".to_string(),
            vec![
                QueryResult {
                    key: 0 as usize,
                    score: 0.5_f64,
                },
                QueryResult {
                    key: 1 as usize,
                    score: 0.5_f64,
                },
                QueryResult {
                    key: 2 as usize,
                    score: 0.25_f64,
                },
                QueryResult {
                    key: 3 as usize,
                    score: 0.25_f64,
                },
                QueryResult {
                    key: 4 as usize,
                    score: 0.25_f64,
                },
            ],
        );
    }
}
