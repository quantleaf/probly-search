use std::{borrow::Cow, sync::Mutex};

use probly_search::{
    score::{bm25, zero_to_one},
    Index, QueryResult,
};

#[derive(Clone)]
struct Doc {
    id: usize,
    title: String,
    description: String,
}

fn tokenizer(s: &str) -> Vec<Cow<'_, str>> {
    s.split(' ').map(Cow::from).collect::<Vec<_>>()
}

fn title_extract(d: &Doc) -> Vec<&str> {
    vec![d.title.as_str()]
}

fn description_extract(d: &Doc) -> Vec<&str> {
    vec![d.description.as_str()]
}

#[test]
pub fn test_add_query_delete_bm25() {
    // Create index with 2 fields
    let mut index = Index::<usize>::new(2);

    // Create docs from a custom Doc struct
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
    // Add documents to index
    index.add_document(
        &[title_extract, description_extract],
        tokenizer,
        doc_1.id,
        &doc_1,
    );

    index.add_document(
        &[title_extract, description_extract],
        tokenizer,
        doc_2.id,
        &doc_2,
    );

    // Search, expected 2 results
    let mut result = index.query("abc", &mut bm25::new(), tokenizer, &[1., 1.]);
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

    // Remove documents from index
    index.remove_document(doc_1.id);

    // Vacuum to remove completely
    index.vacuum();

    // Search, expect 1 result
    result = index.query("abc", &mut bm25::new(), tokenizer, &[1., 1.]);
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
    let mut index = Index::<usize>::new(2);

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

    index.add_document(
        &[title_extract, description_extract],
        tokenizer,
        doc_1.id,
        &doc_1,
    );

    index.add_document(
        &[title_extract, description_extract],
        tokenizer,
        doc_2.id,
        &doc_2,
    );

    // Search, expected 2 results
    let mut result = index.query("abc", &mut zero_to_one::new(), tokenizer, &[1., 1.]);
    assert_eq!(result.len(), 2);
    assert_eq!(result[0], QueryResult { key: 0, score: 1. });
    assert_eq!(
        result[1],
        QueryResult {
            key: 1,
            score: 0.75
        }
    );

    index.remove_document(doc_1.id);

    // Search, expect 1 result
    result = index.query("abc", &mut zero_to_one::new(), tokenizer, &[1., 1.]);
    assert_eq!(result.len(), 1);
    assert_eq!(
        result[0],
        QueryResult {
            key: 1,
            score: 0.75
        }
    );
}

#[test]
pub fn it_is_thread_safe() {
    lazy_static::lazy_static! {
        static ref IDX: Mutex<Index<usize>> = Mutex::new(Index::<usize>::new(2));
    }
    let doc_1 = Doc {
        id: 0,
        title: "abc".to_string(),
        description: "dfg".to_string(),
    };
    let mut idx = IDX.lock().unwrap();
    idx.add_document(
        &[title_extract, description_extract],
        tokenizer,
        doc_1.id,
        &doc_1,
    );
}
