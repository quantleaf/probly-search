/*use std::{
    borrow::BorrowMut,
    collections::HashSet,
    sync::{Arc, Mutex},
};

use probly_search::{
    index::{add_document_to_index, create_index, remove_document_from_index, Index},
    query::{
        query,
        score::default::{bm25, zero_to_one},
        QueryResult,
    },
};
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

#[test]
pub fn test_add_query_delete_bm25() {
    let mut x = create_index::<usize>(2);

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
        &mut x.index,
        &x.arena_index,
        &x.arena_doc,
        &[title_extract, description_extract],
        tokenizer,
        filter,
        doc_1.id,
        doc_1.clone(),
    );

    add_document_to_index(
        &mut x.index,
        &x.arena_index,
        &x.arena_doc,
        &[title_extract, description_extract],
        tokenizer,
        filter,
        doc_2.id,
        doc_2,
    );

    // Search, expected 2 results
    let mut result = query(
        &mut x.index,
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
    remove_document_from_index(&mut x.index, &mut removed_docs, doc_1.id);

    // Search, expect 1 result
    result = query(
        &mut x.index,
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
    let mut x = create_index::<usize>(2);

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
        &mut x.index,
        &x.arena_index,
        &x.arena_doc,
        &[title_extract, description_extract],
        tokenizer,
        filter,
        doc_1.id,
        doc_1.clone(),
    );

    add_document_to_index(
        &mut x.index,
        &x.arena_index,
        &x.arena_doc,
        &[title_extract, description_extract],
        tokenizer,
        filter,
        doc_2.id,
        doc_2,
    );

    // Search, expected 2 results
    let mut result = query(
        &mut x.index,
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
    remove_document_from_index(&mut x.index, &mut removed_docs, doc_1.id);

    // Search, expect 1 result
    result = query(
        &mut x.index,
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
}*/
/*
#[test]
pub fn it_is_thread_safe() {
    struct ThreadSafeIndex {
        index: Arc<Mutex<Index<'arena, usize>>>,
    }

    fn new() -> ThreadSafeIndex {
        ThreadSafeIndex {
            index: Arc::new(Mutex::new(create_index(2))),
        }
    }
    lazy_static::lazy_static! {
      static ref IDX: ThreadSafeIndex = new();
    }
    let doc_1 = Doc {
        id: 0,
        title: "abc".to_string(),
        description: "dfg".to_string(),
    };
    add_document_to_index(
        IDX.index.lock().unwrap().borrow_mut(),
        &[title_extract, description_extract],
        tokenizer,
        filter,
        doc_1.id,
        doc_1.clone(),
    );
}
*/
