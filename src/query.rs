#[cfg(test)]
pub(crate) mod tests {
    use crate::{index::expand_term, Index};

    use crate::test_util::*;

    fn approx_equal(a: f64, b: f64, dp: u8) -> bool {
        let p: f64 = 10f64.powf(-(dp as f64));

        (a - b).abs() < p
    }

    //struct Doc {
    //id: usize,
    //title: String,
    //text: String,
    //}

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
                index.add_document(
                    &[title_extract, text_extract],
                    tokenizer,
                    filter,
                    doc.id,
                    &doc,
                );
            }
            let result = index.query(
                &"a".to_string(),
                &mut crate::score::bm25::new(),
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
                index.add_document(
                    &[title_extract, text_extract],
                    tokenizer,
                    filter,
                    doc.id,
                    &doc,
                );
            }

            let result = index.query(
                &"c".to_string(),
                &mut crate::score::bm25::new(),
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
                index.add_document(
                    &[title_extract, text_extract],
                    tokenizer,
                    filter,
                    doc.id,
                    &doc,
                );
            }

            let result = index.query(
                &"h".to_string(),
                &mut crate::score::bm25::new(),
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
                index.add_document(
                    &[title_extract, text_extract],
                    tokenizer,
                    filter,
                    doc.id,
                    &doc,
                );
            }

            fn custom_filter(s: &str) -> &str {
                if s == "a" {
                    return "";
                }
                filter(s)
            }
            let result = index.query(
                &"a".to_string(),
                &mut crate::score::bm25::new(),
                tokenizer,
                custom_filter,
                &[1., 1.],
                None,
            );
            assert_eq!(result.len(), 0);
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
                index.add_document(
                    &[title_extract, text_extract],
                    tokenizer,
                    filter,
                    doc.id,
                    &doc,
                );
            }

            let result = index.query(
                &"a d".to_string(),
                &mut crate::score::bm25::new(),
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
                index.add_document(
                    &[title_extract, text_extract],
                    tokenizer,
                    filter,
                    doc.id,
                    &doc,
                );
            }
            let exp = expand_term(&index, &"a".to_string(), &index.arena_index);
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
                index.add_document(
                    &[title_extract, text_extract],
                    tokenizer,
                    filter,
                    doc.id,
                    &doc,
                );
            }
            let exp = expand_term(&index, &"x".to_string(), &index.arena_index);
            assert_eq!(exp, Vec::new() as Vec<String>);
        }
    }
}
