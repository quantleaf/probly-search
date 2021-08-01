# [probly-search](https://github.com/quantleaf/probly-search) &middot; [![GitHub license](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/quantleaf/probly-search/blob/master/LICENSE) [![Coverage Status](https://coveralls.io/repos/github/quantleaf/probly-search/badge.svg?branch=master&service=github)](https://coveralls.io/github/quantleaf/probly-search?branch=master) [![Latest Version]][crates.io] [![PRs Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg)](https://github.com/quantleaf/probly-search)

[Latest Version]: https://img.shields.io/crates/v/probly-search.svg
[crates.io]: https://crates.io/crates/probly-search

A full-text search library, optimized for insertion speed, that provides full control over the scoring calculations.

This start initially as a port of the Node library [NDX](https://github.com/ndx-search/ndx).

## Demo
Recipe (title) search with 50k documents.

https://quantleaf.github.io/probly-search-demo/

## Features 
- Three ways to do scoring
    -   [BM25](https://en.wikipedia.org/wiki/Okapi_BM25) ranking function to rank matching documents. The same ranking function that is used by default in [Lucene](http://lucene.apache.org/core/) >= 6.0.0.
    -   *zero-to-one*, a library unique scoring function that provides a normalized score that is bounded by 0 and 1. Perfect for matching titles/labels with queries.
    -   Ability to fully customize your own scoring function by implenting the `ScoreCalculator` trait. 

- [Trie](https://en.wikipedia.org/wiki/Trie) based dynamic
  [Inverted Index](https://en.wikipedia.org/wiki/Inverted_index).
- Multiple fields full-text indexing and searching.
- Per-field score boosting.
- Configurable tokenizer and term filter.
- Free text queries with query expansion.
- Fast allocation, insertion.
- (Con) Document deletion, but you need to rebuild the index to clear up all the used memory.


## Documentation 
Documentation is under development. For now read the source tests.

### Example
*Creating an index with a document that has 2 fields. Query documents, and remove a document.*
```rust
use std::collections::HashSet;
use probly_search::{
    index::{add_document_to_index, create_index, remove_document_from_index, Index},
    query::{
        query,
        score::default::{bm25, zero_to_one},
        QueryResult,
    },
};


// Create index with two fields
let mut idx: Index<usize> = create_index(2);

// Create docs from a custom Doc struct
struct Doc {
    id: usize,
    title: String,
    description: String,
}

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

// Search, expect 2 results
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
```

Go through source tests in for the [BM25 implementation](https://github.com/quantleaf/probly-search/blob/master/src/query/score/default/bm25.rs) and [zero-to-one implementation](https://github.com/quantleaf/probly-search/blob/master/src/query/score/default/zero_to_one.rs) for more query examples.
## License

[MIT](http://opensource.org/licenses/MIT)
