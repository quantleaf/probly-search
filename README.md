# [probly-search](https://github.com/quantleaf/probly-search) &middot; [![GitHub license](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/quantleaf/probly-search/blob/master/LICENSE) [![Coverage Status](https://coveralls.io/repos/github/quantleaf/probly-search/badge.svg?branch=master&service=github)](https://coveralls.io/github/quantleaf/probly-search?branch=master) [![Latest Version]][crates.io] [![PRs Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg)](https://github.com/quantleaf/probly-search)

[Latest Version]: https://img.shields.io/crates/v/probly-search.svg
[crates.io]: https://crates.io/crates/probly-search

A lightweight full-text search library that provides full control over the scoring calculations. Intended for creating small and short lifetime indices. 

This library started as port of the Node library [NDX](https://github.com/ndx-search/ndx).

## Documentation

Documentation is under development. For now read the source tests.

## Features 

- Multiple fields full-text indexing and searching.
- Per-field score boosting.
- [BM25](https://en.wikipedia.org/wiki/Okapi_BM25) ranking function to rank matching documents. The same ranking function that is used by default in [Lucene](http://lucene.apache.org/core/) >= 6.0.0.
- Ability to fully customize your own scoring function by implenting the `ScoreCalculator` trait. 
- [Trie](https://en.wikipedia.org/wiki/Trie) based dynamic
  [Inverted Index](https://en.wikipedia.org/wiki/Inverted_index).
- Configurable tokenizer and term filter.
- Free text queries with query expansion.
- Small memory footprint, optimized for mobile devices.

## Documentation 


### Example
*Creating an index with a document that has 2 fields. Then indexing two documents and query for one using the BM25 scoring function*
```rust
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
    &"a",
);
assert_eq!(result.len(), 1);
assert_eq!(
    approx_equal(result.get(0).unwrap().score, 0.6931471805599453, 8),
    true
);
assert_eq!(result.get(0).unwrap().key, 1);
```

Go through source tests for more examples.
## License

[MIT](http://opensource.org/licenses/MIT)
