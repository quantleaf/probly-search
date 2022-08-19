# [probly-search](https://github.com/quantleaf/probly-search) &middot; [![GitHub license](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/quantleaf/probly-search/blob/master/LICENSE) [![Coverage Status](https://coveralls.io/repos/github/quantleaf/probly-search/badge.svg?branch=master&service=github)](https://coveralls.io/github/quantleaf/probly-search?branch=master) [![Latest Version]][crates.io] [![PRs Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg)](https://github.com/quantleaf/probly-search)

[Latest Version]: https://img.shields.io/crates/v/probly-search.svg
[crates.io]: https://crates.io/crates/probly-search

A full-text search library, written in Rust, optimized for insertion speed, that provides full control over the scoring calculations.

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
- Fast allocation, but latent deletion.
- WASM compatible


## Documentation 
### Adding, Removing and Searching documents
See [Integration tests](/tests/integrations_tests.rs).

### Use this library with WASM
See [recipe search demo project](https://github.com/quantleaf/probly-search-demo)

### A basic example
*Creating an index with a document that has 2 fields. Query documents, and remove a document.*
```rust
use std::collections::HashSet;
use probly_search::{
    index::Index,
    query::{
        score::default::{bm25, zero_to_one},
        QueryResult,
    },
};

// A white space tokenizer
fn tokenizer(s: &str) -> Vec<&str> {
     s.split(' ').collect::<Vec<_>>()
}

// We have to provide extraction functions for the fields we want to index

// Title
fn title_extract(d: &Doc) -> Option<&str> {
    Some(d.title.as_str())
}

// Description
fn description_extract(d: &Doc) -> Option<&str> {
    Some(d.description.as_str())
}

// A no-op filter
fn filter(s: &str) -> &str {
   s
}

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
    filter,
    doc_1.id,
    &doc_1,
);

index.add_document(
    &[title_extract, description_extract],
    tokenizer,
    filter,
    doc_2.id,
    &doc_2,
);

// Search, expected 2 results
let mut result = index.query(
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

// Remove documents from index
let mut removed_docs = HashSet::new();
index.remove_document(&mut removed_docs, doc_1.id);

// Vacuum to remove completely
index.vacuum(&mut removed_docs);

// Search, expect 1 result
result = index.query(
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
```

Go through source tests in for the [BM25 implementation](https://github.com/quantleaf/probly-search/blob/master/src/query/score/default/bm25.rs) and [zero-to-one implementation](https://github.com/quantleaf/probly-search/blob/master/src/query/score/default/zero_to_one.rs) for more query examples.

## Testing
Run all tests with
```rust
cargo test
```

## Benchmark
Run all benchmarks with
```rust
cargo bench
```

## License

[MIT](http://opensource.org/licenses/MIT)
