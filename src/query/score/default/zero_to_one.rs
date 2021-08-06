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
    index::{DocumentDetails, DocumentPointer, InvertedIndexNode},
    query::{
        score::calculator::{FieldData, ScoreCalculator, TermData},
        QueryResult,
    },
};

use typed_generational_arena::StandardIndex as ArenaIndex;

pub struct ZeroToOne<T> {
    visited_terms_by_document: HashMap<T, HashSet<String>>,
    scores_by_term: HashMap<T, Vec<ScoreByTerm>>,
}
pub struct ScoreByTerm {
    query_term_index: usize,
    query_term: String,
    query_term_expanded: String,
    all_query_terms_len: usize,
    field_lengths: Vec<usize>,
    index_node_id: usize,
    term_frequencies: Vec<usize>,
    score: f64,
}
pub fn new<T: Eq + Hash + Clone + Debug>() -> ZeroToOne<T> {
    ZeroToOne {
        visited_terms_by_document: HashMap::new(),
        scores_by_term: HashMap::new(),
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
        index_node: &ArenaIndex<InvertedIndexNode<T>>,

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
            //return None; // Prevent boosting on repeating query terms
        }

        /*
           We are good to go, calculating the score which we will define
            as a product of the ratio of how close we are in terms of number
            of terms, and the ratio of how close we are in terms of number
            of characters to documents term freq (tf) vs query terms

        */
        let mut score: f64 = 0_f64;
        for x in 0..document_details.field_length.len() {
            let tf = document_pointer.term_frequency[x].to_owned() as f64; //(document_pointer.term_frequency[x].to_owned() as f64).clamp(0., 1.);
            if tf > 0_f64 {
                let num_of_terms = term_data.all_query_terms.len() as f64;
                let term_exp_len = term_data.query_term_expanded.len() as f64;
                let term_len = term_data.query_term.len() as f64;
                let field_length = document_details.field_length[x];
                /* let score_contribution = tf / f64::max(field_length as f64, num_of_terms)
                    * (1_f64 - f64::abs(term_exp_len - term_len) / (term_exp_len));
                score += score_contribution * field_data.fields_boost[x];*/

                let q = (1. - f64::abs(term_exp_len - term_len) / (term_exp_len)); // / tf;
                score += q;
            }

            // CONSUME TERM FREQUENCY
        }
        if !self.scores_by_term.contains_key(key) {
            self.scores_by_term.insert(key.to_owned(), Vec::new());
        }

        self.scores_by_term
            .get_mut(&key.to_owned())
            .unwrap()
            .push(ScoreByTerm {
                score: score,
                all_query_terms_len: term_data.all_query_terms.len(),
                query_term: term_data.query_term.to_owned(),
                query_term_expanded: term_data.query_term_expanded.to_owned(),
                query_term_index: term_data.query_term_index.to_owned(),
                index_node_id: index_node.to_idx(),
                term_frequencies: document_pointer.term_frequency.clone(),
                field_lengths: document_details.field_length.clone(),
            });

        if score > 0_f64 {
            return Some(score);
        }
        None
    }

    fn finalize(&mut self, results: &mut Vec<QueryResult<T>>) {
        // Adjust scores
        for result in results {
            // For each Index id, create a pool for IDF, to consume
            let mut pool: HashMap<usize, f64> = HashMap::new();
            let mut df_by_id: HashMap<usize, f64> = HashMap::new();
            let mut df_pool_by_id: HashMap<usize, f64> = HashMap::new();

            for s in self.scores_by_term.get(&result.key).unwrap() {
                df_by_id.insert(s.index_node_id, s.term_frequencies[0] as f64);
                df_pool_by_id.insert(s.index_node_id, s.term_frequencies[0] as f64);
            }

            let scores_by_doc = self.scores_by_term.get_mut(&result.key).unwrap();

            // we seek unique pairs of query terms and expanded terms
            // let mut consumed_by_term: HashMap<String, String> = HashMap::new();
            let mut consumed_index: HashSet<usize> = HashSet::new();
            let mut consumed_node_index: HashSet<usize> = HashSet::new();
            let mut scalars: HashMap<usize, f64> = HashMap::new();

            scores_by_doc.sort_by(|a, b| b.score.partial_cmp(&a.score).unwrap());
            let mut score = 0.;
            for s in scores_by_doc {
                if consumed_index.contains(&s.query_term_index)
                //   || consumed_node_index.contains(&s.index_node_id)
                {
                    continue;
                }
                let df_pool = df_pool_by_id.get_mut(&s.index_node_id).unwrap();
                if df_pool <= &mut 0. {
                    continue;
                }
                *df_pool -= 1.;

                scalars.insert(
                    s.index_node_id.to_owned(),
                    s.term_frequencies[0] as f64
                        / ( usize::max(s.field_lengths[0], s.all_query_terms_len)) // s.term_frequencies[0] *
                            as f64,
                );
                if let Some(previous_score) = pool.get_mut(&s.index_node_id) {
                    *previous_score += s.score
                } else {
                    pool.insert(s.index_node_id, s.score);
                }

                /*consumed_by_term.insert(
                    s.query_term_expanded.to_owned(),
                    s.query_term_expanded.to_owned(),
                );*/
                consumed_index.insert(s.query_term_index.to_owned());
                consumed_node_index.insert(s.index_node_id.to_owned());

                score += s.score;
            }

            let mut x = Vec::new();

            let mut score_by_pool = 0.;
            for (key, pool_score) in pool {
                let df = df_by_id.get(&key).unwrap();
                score_by_pool +=
                    f64::min(pool_score / df.to_owned(), 1.) * scalars.get(&key).unwrap();
                x.push(pool_score);
            }
            result.score = score_by_pool;
        }

        self.visited_terms_by_document = HashMap::new(); // Clear statistics since we might reuse this struct for another query
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::{
        index::{add_document_to_index, create_index, Index},
        query::tests::{filter, tokenizer},
        test_util::{build_test_index, test_score},
    };

    #[test]
    fn it_should_perform_partial_matching() {
        let mut x = build_test_index(&["abc", "abcefg", "abcefghij"]);
        test_score(
            &mut x,
            &mut new(),
            &"abc".to_string(),
            vec![
                QueryResult {
                    key: 0,
                    score: 1_f64,
                },
                QueryResult { key: 1, score: 0.5 },
                QueryResult {
                    key: 2,
                    score: 0.33333333333333337,
                },
            ],
        );
    }

    #[test]
    fn it_should_perform_partial_matching_repeating() {
        let mut x = build_test_index(&["abcdef abcdefghi"]);
        test_score(
            &mut x,
            &mut new(),
            &"abc abc".to_string(),
            vec![QueryResult {
                key: 0,
                score: 0.4166666666666667,
            }],
        );
    }

    #[test]
    fn it_should_penalize_repeating_query_terms() {
        let mut x = build_test_index(&["abc"]);
        test_score(
            &mut x,
            &mut new(),
            &"abc abc".to_string(),
            vec![QueryResult { key: 0, score: 0.5 }],
        );
    }

    #[test]
    fn it_should_penalize_missing_repeating_query_terms() {
        let mut x = build_test_index(&["abc abc"]);
        test_score(
            &mut x,
            &mut new(),
            &"abc".to_string(),
            vec![QueryResult { key: 0, score: 0.5 }],
        );
    }
    #[test]
    fn it_should_be_bounded_by_one() {
        let mut x = build_test_index(&["abc abc"]);
        test_score(
            &mut x,
            &mut new(),
            &"abc ab".to_string(),
            vec![QueryResult {
                key: 0,
                score: 0.83333333333333337_f64,
            }],
        );
    }

    #[test]
    fn it_should_be_bounded_by_one_2() {
        let mut x = build_test_index(&["abc ab"]);
        test_score(
            &mut x,
            &mut new(),
            &"abc abc".to_string(),
            vec![QueryResult { key: 0, score: 0.5 }],
        );
    }

    #[test]
    fn it_should_be_bounded_be_one() {
        let mut x = build_test_index(&["oy oy oysters"]);
        test_score(
            &mut x,
            &mut new(),
            &"oy oy oysters".to_string(),
            vec![QueryResult {
                key: 0,
                score: 1_f64,
            }],
        );
    }

    #[test]
    fn it_should_retrieve_multiple_results() {
        let mut x = build_test_index(&[
            "abcdef",
            "abc abcdef",
            "abcdef abcdef",
            "abcdef abcdefghi",
            "def abcdef",
        ]);

        test_score(
            &mut x,
            &mut new(),
            &"abc".to_string(),
            vec![
                QueryResult { key: 0, score: 0.5 },
                QueryResult {
                    key: 1_usize,
                    score: 0.5,
                },
                QueryResult {
                    key: 2_usize,
                    score: 0.25,
                },
                QueryResult {
                    key: 3_usize,
                    score: 0.25,
                },
                QueryResult {
                    key: 4_usize,
                    score: 0.25,
                },
            ],
        );
    }

    #[test]
    fn it_should_retrieve_multiple_results_and_penalize_repeating_query_terms() {
        let mut x = build_test_index(&[
            "abcdef",
            "abc abcdef",
            "abcdef abcdef",
            "abcdef abcdefghi",
            "def abcdef",
        ]);

        test_score(
            &mut x,
            &mut new(),
            &"abc abc".to_string(),
            vec![
                QueryResult {
                    key: 1_usize,
                    score: 0.75,
                },
                QueryResult {
                    key: 2_usize,
                    score: 0.5,
                },
                QueryResult {
                    key: 3_usize,
                    score: 0.4166666666666667,
                },
                QueryResult {
                    key: 0,
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
    fn it_combines_multi_field_result() {
        let mut x: Index<usize> = create_index(2);
        let titles = &["abc", "abcefg", "abcefghij"];
        let descriptions = &["abc", "abcefg", "abcefghij"];
        struct DocTitleDescription {
            id: usize,
            title: String,
            description: String,
        }
        fn title_extract(doc: &DocTitleDescription) -> Option<&str> {
            Some(doc.title.as_str())
        }
        fn description_extract(doc: &DocTitleDescription) -> Option<&str> {
            Some(doc.description.as_str())
        }

        for (i, (title, description)) in titles.iter().zip(descriptions.iter()).enumerate() {
            let doc = DocTitleDescription {
                id: i,
                title: title.to_string(),
                description: description.to_string(),
            };
            add_document_to_index(
                &mut x,
                &[title_extract, description_extract],
                tokenizer,
                filter,
                doc.id,
                doc,
            );
        }

        test_score(
            &mut x,
            &mut new(),
            &"abc".to_string(),
            vec![
                QueryResult {
                    key: 0,
                    score: 1_f64,
                },
                QueryResult { key: 1, score: 0.5 },
                QueryResult {
                    key: 2,
                    score: 0.33333333333333337,
                },
            ],
        );
    }
}
