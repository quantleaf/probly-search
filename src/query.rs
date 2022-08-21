use hashbrown::{HashMap, HashSet};
use std::{fmt::Debug, hash::Hash};

use typed_generational_arena::StandardArena;

use crate::{score::*, Index, InvertedIndexNode, Tokenizer};

/// Result type for querying an index.
#[derive(Debug, PartialEq)]
pub struct QueryResult<T> {
    /// Document key.
    pub key: T,
    /// Result score.
    pub score: f64,
}

impl<T: Eq + Hash + Copy + Debug> Index<T> {
    /// Performs a search with a simple free text query.
    ///
    /// All token separators work as a disjunction operator.
    pub fn query<'a, M, S: ScoreCalculator<T, M>>(
        &self,
        query: &'a str,
        score_calculator: &mut S,
        tokenizer: Tokenizer,
        fields_boost: &[f64],
    ) -> Vec<QueryResult<T>> {
        let removed = self.removed_documents();
        let query_terms = tokenizer(query); /* .iter().map(|term| term.to_string()).collect() */

        let mut scores = HashMap::new();
        let query_terms_len = query_terms.len();

        for (query_term_index, query_term) in query_terms.iter().enumerate() {
            if !query_term.is_empty() {
                let expanded_terms = self.expand_term(query_term.as_ref(), &self.arena_index);
                let mut visited_documents_for_term: HashSet<T> = HashSet::new();
                for query_term_expanded in expanded_terms {
                    let term_node_option = Index::<T>::find_inverted_index_node(
                        self.root,
                        &query_term_expanded,
                        &self.arena_index,
                    );
                    if let Some(term_node_index) = term_node_option {
                        let document_frequency = self.count_documents(term_node_index);
                        let term_node = self.arena_index.get(term_node_index).unwrap();
                        if let Some(term_node_option_first_doc) = term_node.first_doc {
                            if document_frequency > 0 {
                                let term_expansion_data = TermData {
                                    query_term_index,
                                    query_terms_len,
                                    query_term,
                                    query_term_expanded: &query_term_expanded,
                                };
                                let pre_calculations = &score_calculator.before_each(
                                    &term_expansion_data,
                                    document_frequency,
                                    &self.docs,
                                );

                                let mut pointer = Some(term_node_option_first_doc);
                                while let Some(p) = pointer {
                                    let pointer_borrowed = self.arena_doc.get(p).unwrap();
                                    let key = &pointer_borrowed.details_key;
                                    if removed.is_none() || !removed.unwrap().contains(key) {
                                        let fields = &self.fields;
                                        let score = &score_calculator.score(
                                            pre_calculations.as_ref(),
                                            pointer_borrowed,
                                            self.docs.get(key).unwrap(),
                                            &term_node_index,
                                            &FieldData {
                                                fields_boost,
                                                fields,
                                            },
                                            &term_expansion_data,
                                        );
                                        if let Some(s) = score {
                                            let new_score = max_score_merger(
                                                s,
                                                scores.get(key),
                                                visited_documents_for_term.contains(key),
                                            );
                                            scores.insert(*key, new_score);
                                        }
                                    }
                                    visited_documents_for_term.insert(*key);
                                    pointer = pointer_borrowed.next;
                                }
                            }
                        }
                    }
                }
            }
        }

        let mut result = Vec::new();
        for (key, score) in scores {
            result.push(QueryResult { key, score });
        }
        score_calculator.finalize(&mut result);

        result.sort_by(|a, b| b.score.partial_cmp(&a.score).unwrap());

        result
    }

    /// Expands term with all possible combinations.
    fn expand_term(
        &self,
        term: &str,
        arena_index: &StandardArena<InvertedIndexNode<T>>,
    ) -> Vec<String> {
        let node = Index::<T>::find_inverted_index_node(self.root, term, &self.arena_index);
        let mut results = Vec::new();
        if let Some(n) = node {
            Index::<T>::expand_term_from_node(
                self.arena_index.get(n).unwrap(),
                &mut results,
                term,
                arena_index,
            );
        }

        results
    }

    /// Recursively goes through inverted index nodes and
    /// expands term with all possible combinations.
    fn expand_term_from_node(
        node: &InvertedIndexNode<T>,
        results: &mut Vec<String>,
        term: &str,
        arena_index: &StandardArena<InvertedIndexNode<T>>,
    ) {
        if node.first_doc.is_some() {
            results.push(term.to_owned());
        }
        let mut child = node.first_child;
        while let Some(child_index) = child {
            let cb = arena_index.get(child_index).unwrap();
            let mut inter = term.to_owned();
            inter.push(cb.char);
            Index::<T>::expand_term_from_node(cb, results, &inter, arena_index);
            child = cb.next;
        }
    }
}

fn max_score_merger(
    score: &f64,
    previous_score: Option<&f64>,
    document_visited_for_term: bool,
) -> f64 {
    if let Some(p) = previous_score {
        if document_visited_for_term {
            f64::max(*p, *score)
        } else {
            p + score
        }
    } else {
        *score
    }
}

#[cfg(test)]
pub(crate) mod tests {

    use crate::test_util::*;
    use crate::Index;

    fn approx_equal(a: f64, b: f64, dp: u8) -> bool {
        let p: f64 = 10f64.powf(-(dp as f64));

        (a - b).abs() < p
    }

    pub mod query {
        use super::*;

        #[test]
        fn it_should_return_doc_1() {
            let mut index = Index::<usize>::new(2);
            let docs = vec![
                Doc {
                    id: 1,
                    title: "a b c".to_string(),
                    text: "hello world".to_string(),
                },
                Doc {
                    id: 2,
                    title: "c d e".to_string(),
                    text: "lorem ipsum".to_string(),
                },
            ];
            for doc in docs {
                index.add_document(&[title_extract, text_extract], tokenizer, doc.id, &doc);
            }
            let result = index.query(
                &"a".to_string(),
                &mut crate::score::bm25::new(),
                tokenizer,
                &[1., 1.],
            );
            assert_eq!(result.len(), 1);
            assert_eq!(
                approx_equal(result.get(0).unwrap().score, 0.6931471805599453, 8),
                true
            );
            assert_eq!(result.get(0).unwrap().key, 1);
        }

        #[test]
        fn it_should_return_doc_1_and_2() {
            let mut index = Index::<usize>::new(2);
            let docs = vec![
                Doc {
                    id: 1,
                    title: "a b c".to_string(),
                    text: "hello world".to_string(),
                },
                Doc {
                    id: 2,
                    title: "c d e".to_string(),
                    text: "lorem ipsum".to_string(),
                },
            ];

            for doc in docs {
                index.add_document(&[title_extract, text_extract], tokenizer, doc.id, &doc);
            }

            let result = index.query(
                &"c".to_string(),
                &mut crate::score::bm25::new(),
                tokenizer,
                &[1., 1.],
            );

            assert_eq!(result.len(), 2);
            assert_eq!(
                approx_equal(result.get(0).unwrap().score, 0.1823215567939546, 8),
                true
            );
            assert_eq!(
                result.get(0).unwrap().key == 1 || result.get(0).unwrap().key == 2,
                true
            );
            assert_eq!(
                approx_equal(result.get(1).unwrap().score, 0.1823215567939546, 8),
                true
            );
            assert_eq!(
                result.get(1).unwrap().key == 1 || result.get(1).unwrap().key == 2,
                true
            );
            assert_ne!(result.get(0).unwrap().key, result.get(1).unwrap().key);
        }

        #[test]
        fn it_should_match_text_by_expanding() {
            let mut index = Index::<usize>::new(2);
            let docs = vec![
                Doc {
                    id: 1,
                    title: "a b c".to_string(),
                    text: "hello world".to_string(),
                },
                Doc {
                    id: 2,
                    title: "c d e".to_string(),
                    text: "lorem ipsum".to_string(),
                },
            ];

            for doc in docs {
                index.add_document(&[title_extract, text_extract], tokenizer, doc.id, &doc);
            }

            let result = index.query(
                &"h".to_string(),
                &mut crate::score::bm25::new(),
                tokenizer,
                &[1., 1.],
            );
            assert_eq!(result.len(), 1);
            assert_eq!(
                approx_equal(result.get(0).unwrap().score, 0.12637567304702957, 8),
                true
            );
            assert_eq!(result.get(0).unwrap().key, 1);
        }

        #[test]
        fn it_should_use_token_separator_as_disjunction_operator() {
            let mut index = Index::<usize>::new(2);
            let docs = vec![
                Doc {
                    id: 1,
                    title: "a b c".to_string(),
                    text: "hello world".to_string(),
                },
                Doc {
                    id: 2,
                    title: "c d e".to_string(),
                    text: "lorem ipsum".to_string(),
                },
            ];

            for doc in docs {
                index.add_document(&[title_extract, text_extract], tokenizer, doc.id, &doc);
            }

            let result = index.query(
                &"a d".to_string(),
                &mut crate::score::bm25::new(),
                tokenizer,
                &[1., 1.],
            );
            assert_eq!(result.len(), 2);
            assert_eq!(
                approx_equal(result.get(0).unwrap().score, 0.6931471805599453, 8),
                true
            );
            assert_eq!(
                result.get(0).unwrap().key == 1 || result.get(0).unwrap().key == 2,
                true
            );
            assert_eq!(
                approx_equal(result.get(1).unwrap().score, 0.6931471805599453, 8),
                true
            );
            assert_eq!(
                result.get(1).unwrap().key == 1 || result.get(1).unwrap().key == 2,
                true
            );
            assert_ne!(result.get(0).unwrap().key, result.get(1).unwrap().key);
        }
    }
    pub mod expand {
        use super::*;

        #[test]
        fn it_should_expand_all() {
            let mut index = Index::<usize>::new(2);
            let docs: Vec<Doc> = vec![
                Doc {
                    id: 1,
                    title: "abc".to_string(),
                    text: "hello world".to_string(),
                },
                Doc {
                    id: 2,
                    title: "adef".to_string(),
                    text: "lorem ipsum".to_string(),
                },
            ];

            for doc in docs {
                index.add_document(&[title_extract, text_extract], tokenizer, doc.id, &doc);
            }
            let exp = index.expand_term(&"a".to_string(), &index.arena_index);
            assert_eq!(exp, vec!["adef".to_string(), "abc".to_string()]);
        }

        #[test]
        fn it_should_not_expand() {
            let mut index = Index::<usize>::new(2);
            let docs = vec![
                Doc {
                    id: 1,
                    title: "abc def".to_string(),
                    text: "hello world".to_string(),
                },
                Doc {
                    id: 2,
                    title: "adef abc".to_string(),
                    text: "lorem ipsum".to_string(),
                },
            ];

            for doc in docs {
                index.add_document(&[title_extract, text_extract], tokenizer, doc.id, &doc);
            }
            let exp = index.expand_term(&"x".to_string(), &index.arena_index);
            assert_eq!(exp, Vec::new() as Vec<String>);
        }
    }
}
