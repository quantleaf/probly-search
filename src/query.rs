pub mod score;

use typed_generational_arena::StandardArena;

use crate::{
    index::*,
    query::score::calculator::ScoreCalculator,
    utils::{Filter, Tokenizer},
};
use std::{
    collections::{HashMap, HashSet},
    fmt::Debug,
    hash::Hash,
};

use self::score::calculator::{FieldData, TermData};
extern crate typed_generational_arena;
use typed_generational_arena::StandardIndex as ArenaIndex;
/**
 * Query Result.

 * `T` Document key.
*/
#[derive(Debug, PartialEq)]
pub struct QueryResult<T> {
    /**
     * Document key.
     */
    pub key: T,
    /**
     * Result score.
     */
    pub score: f64,
}

pub fn max_score_merger(
    score: &f64,
    previous_score: Option<&f64>,
    document_visited_for_term: bool,
) -> f64 {
    {
        if let Some(p) = previous_score {
            if document_visited_for_term {
                f64::max(p.to_owned(), score.to_owned())
            } else {
                p + score
            }
        } else {
            score.to_owned()
        }
    }
}

/**
Performs a search with a simple free text query.
All token separators work as a disjunction operator.
Arguments
 * typeparam `T` Document key.
 * `index`.
 * `query` Query string.
 * `score_calculator` A struct that implements the ScoreCalculator trait to provide score calculations.
 * `tokenizer Tokenizer is a function that breaks a text into words, phrases, symbols, or other meaningful elements called tokens.
 * `filter` Filter is a function that processes tokens and returns terms, terms are used in Inverted Index to index documents.
 * `fields_boost` Fields boost factors.
 * `remove`d Set of removed document keys.

returns Array of QueryResult structs
*/
pub fn query<T: Eq + Hash + Clone + Debug, M, S: ScoreCalculator<T, M>>(
    index: &mut Index<T>,
    query: &str,
    score_calculator: &mut S,
    tokenizer: Tokenizer,
    filter: Filter,
    fields_boost: &[f64],
    removed: Option<&HashSet<T>>,
) -> Vec<QueryResult<T>> {
    let docs = &index.docs;
    let fields = &index.fields;
    let query_terms = tokenizer(query);
    let mut scores: HashMap<T, f64> = HashMap::new();
    for query_term_pre_filter in &query_terms {
        let query_term = filter(query_term_pre_filter);
        print!("{}", query_term);
        if !query_term.is_empty() {
            let expanded_terms = expand_term(index, &query_term, &index.arena_index);
            let mut visited_documents_for_term: HashSet<T> = HashSet::new();
            for query_term_expanded in expanded_terms {
                let term_node_option =
                    find_inverted_index_node(index.root, &query_term_expanded, &index.arena_index);
                if let Some(term_node_index) = term_node_option {
                    let term_node = index.arena_index.get_mut(term_node_index).unwrap();
                    let mut new_first_doc = None;
                    let mut assign_new_first_doc = false;
                    let mut document_frequency = 0;

                    if let Some(term_node_option_first_doc) = term_node.first_doc {
                        let mut prev_pointer: Option<ArenaIndex<DocumentPointer<T>>> = None;
                        let mut pointer_option = Some(term_node_option_first_doc);
                        while let Some(pointer) = pointer_option {
                            let pointer_value = index.arena_doc.get(pointer).unwrap();
                            if removed.is_some() // Cleanup old removed documents while searching. If vaccume after delete, this will have not effect
                                && removed
                                    .unwrap()
                                    .contains(&pointer_value.details_key)
                            {
                                if let Some(pp) = prev_pointer {
                                    index.arena_doc.get_mut(pp).unwrap().next = pointer_value.next;
                                } else {
                                    new_first_doc = pointer_value.next;
                                    assign_new_first_doc = true;
                                    //  term_node_borrowed.first_doc = (&pointer.get().next).clone();
                                }
                            } else {
                                prev_pointer = Some(pointer);
                                document_frequency += 1;
                            }
                            pointer_option = index.arena_doc.get(pointer).unwrap().next;
                        }
                    }

                    if assign_new_first_doc {
                        term_node.first_doc = new_first_doc;
                    }

                    if let Some(term_node_option_first_doc) = term_node.first_doc {
                        if document_frequency > 0 {
                            let term_expansion_data = TermData {
                                all_query_terms: &query_terms,
                                query_term: &query_term,
                                query_term_expanded: &query_term_expanded,
                            };
                            let pre_calculations = &score_calculator.before_each(
                                &term_expansion_data,
                                document_frequency,
                                docs,
                            );

                            let mut pointer = Some(term_node_option_first_doc);
                            while let Some(p) = pointer {
                                let pointer_borrowed = index.arena_doc.get(p).unwrap();
                                let key = &pointer_borrowed.details_key;
                                if removed.is_none() || !removed.unwrap().contains(key) {
                                    let score = &score_calculator.score(
                                        pre_calculations.as_ref(),
                                        pointer_borrowed,
                                        index.docs.get(key).unwrap(),
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
                                        scores.insert(key.to_owned(), new_score);
                                        visited_documents_for_term.insert(key.to_owned());
                                    }
                                }
                                pointer = pointer_borrowed.next;
                            }
                        }
                    }
                }
            }
        }
    }

    let mut result: Vec<QueryResult<T>> = Vec::new();
    for (key, score) in scores {
        result.push(QueryResult { key, score });
    }

    result.sort_by(|a, b| b.score.partial_cmp(&a.score).unwrap());

    score_calculator.finalize(&mut result);

    result
}

/**
Expands term with all possible combinations.
 * `index`
 * `term` Term.
returns All terms that starts with `term` string.
 */
fn expand_term<I: Debug>(
    index: &Index<I>,
    term: &str,
    arena_index: &StandardArena<InvertedIndexNode<I>>,
) -> Vec<String> {
    let node = find_inverted_index_node(index.root, term, &index.arena_index);
    let mut results = Vec::new();
    if let Some(n) = node {
        expand_term_from_node(
            index.arena_index.get(n).unwrap(),
            &mut results,
            term,
            arena_index,
        );
    }

    results
}

/**
Recursively goes through inverted index nodes and expands term with all possible combinations.

 * typeparam `I` Document ID type.
 * `index {@link Index}
 * `results Results.
 * `term Term.
 */
fn expand_term_from_node<I: Debug>(
    node: &InvertedIndexNode<I>,
    results: &mut Vec<String>,
    term: &str,
    arena_index: &StandardArena<InvertedIndexNode<I>>,
) {
    if node.first_doc.is_some() {
        results.push(term.to_owned());
    }
    let mut child = node.first_child;
    while let Some(child_index) = child {
        let cb = arena_index.get(child_index).unwrap();
        let mut inter = term.to_owned();
        inter.push(cb.char);
        expand_term_from_node(cb, results, &inter, arena_index); // String.fromCharCode(child.charCode)
        child = cb.next;
    }
}

#[cfg(test)]
mod tests {

    fn approx_equal(a: f64, b: f64, dp: u8) -> bool {
        let p: f64 = 10f64.powf(-(dp as f64));

        (a - b).abs() < p
    }

    use super::*;
    struct Doc {
        id: usize,
        title: String,
        text: String,
    }
    fn tokenizer(s: &str) -> Vec<String> {
        s.split(' ')
            .map(|slice| slice.to_owned())
            .collect::<Vec<String>>()
    }
    fn title_extract(d: &Doc) -> Option<&str> {
        Some(d.title.as_str())
    }
    fn text_extract(d: &Doc) -> Option<&str> {
        Some(d.text.as_str())
    }

    fn filter(s: &str) -> String {
        s.to_owned()
    }

    pub mod query {
        use super::*;

        #[test]
        fn it_should_return_doc_1() {
            let mut index = create_index::<usize>(2);
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
                add_document_to_index(
                    &mut index,
                    &[title_extract, text_extract],
                    tokenizer,
                    filter,
                    doc.id,
                    doc,
                );
            }
            let result = query(
                &mut index,
                &"a".to_string(),
                &mut crate::query::score::default::bm25::new(),
                tokenizer,
                filter,
                &[1., 1.],
                None,
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
            let mut index = create_index::<usize>(2);
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
                add_document_to_index(
                    &mut index,
                    &[title_extract, text_extract],
                    tokenizer,
                    filter,
                    doc.id,
                    doc,
                );
            }

            let result = query(
                &mut index,
                &"c".to_string(),
                &mut crate::query::score::default::bm25::new(),
                tokenizer,
                filter,
                &[1., 1.],
                None,
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
            let mut index = create_index::<usize>(2);
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
                add_document_to_index(
                    &mut index,
                    &[title_extract, text_extract],
                    tokenizer,
                    filter,
                    doc.id,
                    doc,
                );
            }

            let result = query(
                &mut index,
                &"h".to_string(),
                &mut crate::query::score::default::bm25::new(),
                tokenizer,
                filter,
                &[1., 1.],
                None,
            );
            assert_eq!(result.len(), 1);
            assert_eq!(
                approx_equal(result.get(0).unwrap().score, 0.12637567304702957, 8),
                true
            );
            assert_eq!(result.get(0).unwrap().key, 1);
        }

        #[test]
        fn it_should_use_filter_for_query() {
            let mut index = create_index::<usize>(2);
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
                add_document_to_index(
                    &mut index,
                    &[title_extract, text_extract],
                    tokenizer,
                    filter,
                    doc.id,
                    doc,
                );
            }

            fn custom_filter(s: &str) -> String {
                if s == "a" {
                    return "".to_string();
                }
                filter(s)
            }
            let result = query(
                &mut index,
                &"a".to_string(),
                &mut crate::query::score::default::bm25::new(),
                tokenizer,
                custom_filter,
                &[1., 1.],
                None,
            );
            assert_eq!(result.len(), 0);
        }

        #[test]
        fn it_should_use_token_separator_as_disjunction_operator() {
            let mut index = create_index::<usize>(2);
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
                add_document_to_index(
                    &mut index,
                    &[title_extract, text_extract],
                    tokenizer,
                    filter,
                    doc.id,
                    doc,
                );
            }

            let result = query(
                &mut index,
                &"a d".to_string(),
                &mut crate::query::score::default::bm25::new(),
                tokenizer,
                filter,
                &[1., 1.],
                None,
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
            let mut index = create_index::<usize>(2);
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
                add_document_to_index(
                    &mut index,
                    &[title_extract, text_extract],
                    tokenizer,
                    filter,
                    doc.id,
                    doc,
                );
            }
            let exp = expand_term(&index, &"a".to_string(), &index.arena_index);
            assert_eq!(exp, vec!["adef".to_string(), "abc".to_string()]);
        }

        #[test]
        fn it_should_not_expand() {
            let mut index = create_index::<usize>(2);
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
                add_document_to_index(
                    &mut index,
                    &[title_extract, text_extract],
                    tokenizer,
                    filter,
                    doc.id,
                    doc,
                );
            }
            let exp = expand_term(&index, &"x".to_string(), &index.arena_index);
            assert_eq!(exp, Vec::new() as Vec<String>);
        }
    }
}
