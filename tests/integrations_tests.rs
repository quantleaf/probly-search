use std::collections::HashSet;

use probly_search::{
    index::{add_document_to_index, create_index, remove_document_from_index, Index},
    query::{
        query,
        score::default::{bm25, zero_to_one},
        QueryResult,
    },
};

#[test]
pub fn test_add_query_delete_bm25() {
    #[derive(Clone)]
    struct Doc {
        id: usize,
        title: String,
        description: String,
    }

    fn tokenizer(s: &str) -> Vec<String> {
        s.split(' ')
            .map(|slice| slice.to_owned())
            .collect::<Vec<String>>()
    }
    fn title_extract(d: &Doc) -> Option<&str> {
        Some(d.title.as_str())
    }

    fn description_extract(d: &Doc) -> Option<&str> {
        Some(d.description.as_str())
    }

    fn filter(s: &String) -> String {
        s.to_owned()
    }

    let mut idx: Index<usize> = create_index(2);

    let doc_1 = Doc {
        id: 0,
        title: "abc".to_string(),
        description: "dfg".to_string(),
    };

    let doc_2 = Doc {
        id: 1,
        title: "dfgh".to_string(),
        description: "abcd".to_string(),
    };

    add_document_to_index(
        &mut idx,
        &[title_extract, description_extract],
        tokenizer,
        filter,
        doc_1.id,
        doc_1.clone(),
    );

    add_document_to_index(
        &mut idx,
        &[title_extract, description_extract],
        tokenizer,
        filter,
        doc_2.id,
        doc_2,
    );

    // Search, expected 2 results
    let mut result = query(
        &mut idx,
        &"abc",
        &mut bm25::new(),
        tokenizer,
        filter,
        &[1., 1.],
        None,
    );
    assert_eq!(result.len(), 2);
    assert_eq!(
        result[0],
        QueryResult {
            key: 0,
            score: 0.6931471805599453
        }
    );
    assert_eq!(
        result[1],
        QueryResult {
            key: 1,
            score: 0.28104699650060755
        }
    );

    let mut removed_docs = HashSet::new();
    remove_document_from_index(&mut idx, &mut removed_docs, doc_1.id);

    // Search, expect 1 result
    result = query(
        &mut idx,
        &"abc",
        &mut bm25::new(),
        tokenizer,
        filter,
        &[1., 1.],
        Some(&removed_docs),
    );
    assert_eq!(result.len(), 1);
    assert_eq!(
        result[0],
        QueryResult {
            key: 1,
            score: 0.1166450426074421
        }
    );
}

#[test]
pub fn test_add_query_delete_zero_to_one() {
    #[derive(Clone)]
    struct Doc {
        id: usize,
        title: String,
        description: String,
    }

    fn tokenizer(s: &str) -> Vec<String> {
        s.split(' ')
            .map(|slice| slice.to_owned())
            .collect::<Vec<String>>()
    }
    fn title_extract(d: &Doc) -> Option<&str> {
        Some(d.title.as_str())
    }

    fn description_extract(d: &Doc) -> Option<&str> {
        Some(d.description.as_str())
    }

    fn filter(s: &String) -> String {
        s.to_owned()
    }

    let mut idx: Index<usize> = create_index(2);

    let doc_1 = Doc {
        id: 0,
        title: "abc".to_string(),
        description: "dfg".to_string(),
    };

    let doc_2 = Doc {
        id: 1,
        title: "dfgh".to_string(),
        description: "abcd".to_string(),
    };

    add_document_to_index(
        &mut idx,
        &[title_extract, description_extract],
        tokenizer,
        filter,
        doc_1.id,
        doc_1.clone(),
    );

    add_document_to_index(
        &mut idx,
        &[title_extract, description_extract],
        tokenizer,
        filter,
        doc_2.id,
        doc_2,
    );

    // Search, expected 2 results
    let mut result = query(
        &mut idx,
        &"abc",
        &mut zero_to_one::new(),
        tokenizer,
        filter,
        &[1., 1.],
        None,
    );
    assert_eq!(result.len(), 2);
    assert_eq!(result[0], QueryResult { key: 0, score: 1. });
    assert_eq!(
        result[1],
        QueryResult {
            key: 1,
            score: 0.75
        }
    );

    let mut removed_docs = HashSet::new();
    remove_document_from_index(&mut idx, &mut removed_docs, doc_1.id);

    // Search, expect 1 result
    result = query(
        &mut idx,
        &"abc",
        &mut zero_to_one::new(),
        tokenizer,
        filter,
        &[1., 1.],
        Some(&removed_docs),
    );
    assert_eq!(result.len(), 1);
    assert_eq!(
        result[0],
        QueryResult {
            key: 1,
            score: 0.75
        }
    );
}