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
    index::{DocumentDetails, DocumentPointer, InvertedIndexNode, QueryResult},
    score::calculator::{FieldData, ScoreCalculator, TermData},
};

use typed_generational_arena::StandardIndex as ArenaIndex;

pub struct ZeroToOne<T> {
    score_by_document_and_field: HashMap<T, Vec<Vec<ScoreByTerm>>>,
}
pub struct ScoreByTerm {
    query_term_index: usize,
    all_query_terms_len: usize,
    field_length: usize,
    index_node_id: usize,
    term_frequency: usize,
    score: f64,
}
pub fn new<T: Eq + Hash + Clone + Debug>() -> ZeroToOne<T> {
    ZeroToOne {
        score_by_document_and_field: HashMap::new(),
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
        _field_data: &FieldData,
        term_data: &TermData,
    ) -> Option<f64> {
        let key = &document_details.key;
        for x in 0..document_details.field_length.len() {
            let tf = document_pointer.term_frequency[x].to_owned(); //(document_pointer.term_frequency[x].to_owned() as f64).clamp(0., 1.);
            if tf > 0 {
                let term_exp_len = term_data.query_term_expanded.len() as f64;
                let term_len = term_data.query_term.len() as f64;
                let field_length = document_details.field_length[x];

                if !self.score_by_document_and_field.contains_key(key) {
                    let mut score_by_field =
                        Vec::with_capacity(document_details.field_length.len());
                    for _i in 0..document_details.field_length.len() {
                        score_by_field.push(Vec::new());
                    }
                    self.score_by_document_and_field
                        .insert(key.to_owned(), score_by_field);
                }

                self.score_by_document_and_field.get_mut(key).unwrap()[x].push(ScoreByTerm {
                    score: 1. - f64::abs(term_exp_len - term_len) / (term_exp_len),
                    all_query_terms_len: term_data.all_query_terms.len(),
                    query_term_index: term_data.query_term_index.to_owned(),
                    index_node_id: index_node.to_idx(),
                    term_frequency: tf,
                    field_length,
                });
            }
        }
        Some(0.) // A dummy value, we do not know the score yet
    }

    fn finalize(&mut self, results: &mut Vec<QueryResult<T>>) {
        // Calculate scores adhoc
        for result in results {
            for field_scores in self
                .score_by_document_and_field
                .get_mut(&result.key)
                .unwrap()
            {
                // For each Index id, create a pool for IDF, to consume
                let mut df_pool_by_id: HashMap<usize, usize> = HashMap::new();
                // we seek unique pairs of query terms and expanded terms
                // let mut consumed_by_term: HashMap<String, String> = HashMap::new();
                let mut consumed_index: HashSet<usize> = HashSet::new();
                let mut consumed_node_index: HashSet<usize> = HashSet::new();
                field_scores.sort_by(|a, b| b.score.partial_cmp(&a.score).unwrap());
                let mut score_by_pool = 0.;
                for s in field_scores {
                    if consumed_index.contains(&s.query_term_index) {
                        continue;
                    }
                    if let Some(df_pool) = df_pool_by_id.get_mut(&s.index_node_id) {
                        if df_pool <= &mut 0 {
                            //  We have "consumed" this index node/pool/term, do not give more points by refering to it more
                            continue;
                        }
                        *df_pool -= 1;
                    } else {
                        let df_pool = s.term_frequency.to_owned() - 1;
                        df_pool_by_id.insert(s.index_node_id, df_pool);
                    }

                    consumed_index.insert(s.query_term_index.to_owned());
                    consumed_node_index.insert(s.index_node_id.to_owned());
                    let df = s.term_frequency as f64;
                    score_by_pool += f64::min(s.score / df.to_owned(), 1.) * s.term_frequency as f64
                        / ( usize::max(s.field_length, s.all_query_terms_len)) // s.term_frequencies[0] *
                        as f64
                }
                result.score = f64::max(score_by_pool, result.score);
            }
        }
        self.score_by_document_and_field = HashMap::new(); // Clear statistics since we might reuse this struct for another query
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::{
        index::Index,
        test_util::{build_test_index, filter, test_score, tokenizer},
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
        let mut x = Index::<usize>::new(2);
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
            x.add_document(
                &[title_extract, description_extract],
                tokenizer,
                filter,
                doc.id,
                &doc,
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

    #[test]
    fn it_combines_multi_field_result_by_ignoring_lowest() {
        let mut x = Index::<usize>::new(2);
        let titles = &["abc", "abcefg", "abcefghij"];
        let descriptions = &["a", "a", "a"];
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
            x.add_document(
                &[title_extract, description_extract],
                tokenizer,
                filter,
                doc.id,
                &doc,
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
