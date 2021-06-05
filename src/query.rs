pub mod score;

use crate::{
    index::*,
    query::score::calculator::ScoreCalculator,
    utils::{Filter, Tokenizer},
};
use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    fmt::Debug,
    hash::Hash,
    rc::Rc,
};

/**
 * Query Result.

 * `T` Document key.
*/
#[derive(Debug, PartialEq)]
pub struct QueryResult<I> {
    /**
     * Document key.
     */
    pub key: I,
    /**
     * Result score.
     */
    pub score: f64,
}

/**
Performs a search with a simple free text query.
All token separators work as a disjunction operator.
Arguments
 * typeparam `T` Document key.
 * `index`.
 * `fields_boost` Fields boost factors.
 * `bm25k1` BM25 ranking function constant `k1`, controls non-linear term frequency normalization (saturation).
 * `bm25b` BM25 ranking function constant `b`, controls to what degree document length normalizes tf values.
 * `tokenizer Tokenizer is a function that breaks a text into words, phrases, symbols, or other meaningful elements called tokens.
 * `filter` Filter is a function that processes tokens and returns terms, terms are used in Inverted Index to index documents.
 * `remove`d Set of removed document keys.
 * `s` Query string.
returns Array of QueryResult structs
 */
pub fn query<T: Eq + Hash + Clone + Debug, S: ScoreCalculator<T>>(
    index: &mut Index<T>,
    fields_boost: &[f64],
    /*  bm25k1: f64,
    bm25b: f64,*/
    score_calculator: &S,
    tokenizer: Tokenizer,
    filter: Filter,
    removed: Option<&HashSet<T>>,
    s: &str,
) -> Vec<QueryResult<T>> {
    let docs = &index.docs;
    let fields = &index.fields;
    let terms = tokenizer(&s);
    let mut scores: HashMap<T, f64> = HashMap::new();

    for term_pre_filter in terms {
        let term = filter(&term_pre_filter);
        if !term.is_empty() {
            let expanded_terms = expand_term(&index, &term);
            let mut visited_documents: HashSet<T> = HashSet::new();
            for e_term in expanded_terms {
                let expansion_boost = {
                    if e_term == term {
                        1_f64
                    } else {
                        f64::ln(
                            1_f64 + (1_f64 / (1_f64 + (e_term.len() as f64) - (term.len() as f64))),
                        )
                    }
                };

                let term_node_option = find_inverted_index_node(Rc::clone(&index.root), &e_term);

                if let Some(term_node) = term_node_option {
                    if let Some(term_node_option_first_doc) = &term_node.borrow().first_doc {
                        let mut document_frequency = 0;
                        let mut prev_pointer: Option<Rc<RefCell<DocumentPointer<T>>>> = None;

                        let mut pointer_option = Some(Rc::clone(term_node_option_first_doc));
                        while let Some(pointer) = pointer_option {
                            if removed.is_some()
                                && removed
                                    .unwrap()
                                    .contains(&pointer.borrow().details.borrow().key)
                            {
                                if let Some(pp) = &prev_pointer {
                                    pp.borrow_mut().next = pointer.borrow().next.clone();
                                } else {
                                    term_node.borrow_mut().first_doc =
                                        (&pointer.borrow().next).clone();
                                }
                            } else {
                                prev_pointer = Some(Rc::clone(&pointer));
                                document_frequency += 1;
                            }
                            pointer_option = pointer.borrow().next.clone();
                        }

                        if document_frequency > 0 {
                            // calculating BM25 idf
                            let idf = f64::ln(
                                1_f64
                                    + ((docs.len() - document_frequency) as f64 + 0.5)
                                        / (document_frequency as f64 + 0.5),
                            );

                            let mut pointer = Some(Rc::clone(term_node_option_first_doc));
                            while let Some(p) = pointer {
                                let pointer_borrowed = p.borrow();
                                let field_lengths = &pointer_borrowed.details.borrow().field_length;
                                if removed.is_none()
                                    || !removed
                                        .unwrap()
                                        .contains(&pointer_borrowed.details.borrow().key)
                                {
                                    let score = score_calculator.score(
                                        p.borrow(),
                                        idf,
                                        field_lengths,
                                        fields_boost,
                                        expansion_boost,
                                        fields,
                                    );
                                    if score > 0_f64 {
                                        let key = &pointer_borrowed.details.borrow().key;
                                        let new_score = {
                                            if let Some(prev_score) = scores.get(&key) {
                                                if visited_documents.contains(&key) {
                                                    f64::max(prev_score.to_owned(), score)
                                                } else {
                                                    prev_score + score
                                                }
                                            } else {
                                                score
                                            }
                                        };
                                        scores.insert(key.to_owned(), new_score);
                                        visited_documents.insert(key.to_owned());
                                    }
                                }
                                pointer = pointer_borrowed.next.clone();
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
    result
}

/**
Expands term with all possible combinations.
 * `index`
 * `term` Term.
returns All terms that starts with `term` string.
 */
fn expand_term<I: Debug>(index: &Index<I>, term: &str) -> Vec<String> {
    let node = find_inverted_index_node(Rc::clone(&index.root), term);
    let mut results = Vec::new();
    if let Some(n) = node {
        expand_term_from_node(n, &mut results, term);
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
    node: Rc<RefCell<InvertedIndexNode<I>>>,
    results: &mut Vec<String>,
    term: &str,
) {
    if node.borrow().first_doc.is_some() {
        results.push(term.to_owned());
    }
    let mut child = node.borrow().first_child.clone();
    while let Some(c) = child {
        let cb = c.borrow();
        let mut inter = term.to_owned();
        inter.push_str(&String::from(char::from_u32(cb.char_code).unwrap()));
        expand_term_from_node(Rc::clone(&c), results, &inter); // String.fromCharCode(child.charCode)
        child = cb.next.clone();
    }
}

#[cfg(test)]
mod tests {

    fn approx_equal(a: f64, b: f64, dp: u8) -> bool {
        let p: f64 = 10f64.powf(-(dp as f64));

        if (a - b).abs() < p {
            return true;
        } else {
            return false;
        }
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

    fn filter(s: &String) -> String {
        s.to_owned()
    }

    pub mod query {
        use crate::query::score;

        use super::*;

        #[test]
        fn it_should_return_doc_1() {
            let mut idx: Index<usize> = create_index(2);
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
                    &mut idx,
                    &[title_extract, text_extract],
                    tokenizer,
                    filter,
                    doc.id,
                    doc,
                );
            }
            let result = query(
                &mut idx,
                &vec![1., 1.],
                &score::default::bm25::default(),
                tokenizer,
                filter,
                None,
                &"a".to_string(),
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
            let mut idx: Index<usize> = create_index(2);
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
                    &mut idx,
                    &[title_extract, text_extract],
                    tokenizer,
                    filter,
                    doc.id,
                    doc,
                );
            }

            let result = query(
                &mut idx,
                &vec![1., 1.],
                &score::default::bm25::default(),
                tokenizer,
                filter,
                None,
                &"c".to_string(),
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
            let mut idx: Index<usize> = create_index(2);
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
                    &mut idx,
                    &[title_extract, text_extract],
                    tokenizer,
                    filter,
                    doc.id,
                    doc,
                );
            }

            let result = query(
                &mut idx,
                &vec![1., 1.],
                &score::default::bm25::default(),
                tokenizer,
                filter,
                None,
                &"h".to_string(),
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
            let mut idx: Index<usize> = create_index(2);
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
                    &mut idx,
                    &[title_extract, text_extract],
                    tokenizer,
                    filter,
                    doc.id,
                    doc,
                );
            }

            fn custom_filter(s: &String) -> String {
                if s.as_str() == "a" {
                    return "".to_string();
                }
                filter(s)
            }
            let result = query(
                &mut idx,
                &vec![1., 1.],
                &score::default::bm25::default(),
                tokenizer,
                custom_filter,
                None,
                &"a".to_string(),
            );
            assert_eq!(result.len(), 0);
        }

        #[test]
        fn it_should_use_token_separator_as_disjunction_operator() {
            let mut idx: Index<usize> = create_index(2);
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
                    &mut idx,
                    &[title_extract, text_extract],
                    tokenizer,
                    filter,
                    doc.id,
                    doc,
                );
            }

            let result = query(
                &mut idx,
                &vec![1., 1.],
                &score::default::bm25::default(),
                tokenizer,
                filter,
                None,
                &"a d".to_string(),
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
            let mut idx: Index<usize> = create_index(1);
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
                    &mut idx,
                    &[title_extract, text_extract],
                    tokenizer,
                    filter,
                    doc.id,
                    doc,
                );
            }
            let exp = expand_term(&idx, &"a".to_string());
            assert_eq!(exp, vec!["adef".to_string(), "abc".to_string()]);
        }

        #[test]
        fn it_should_not_expand() {
            let mut idx: Index<usize> = create_index(2);
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
                    &mut idx,
                    &[title_extract, text_extract],
                    tokenizer,
                    filter,
                    doc.id,
                    doc,
                );
            }
            let exp = expand_term(&idx, &"x".to_string());
            assert_eq!(exp, Vec::new() as Vec<String>);
        }
    }
}
