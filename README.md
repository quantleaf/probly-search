# [probly-search](https://github.com/quantleaf/probly-search) &middot; [![GitHub license](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/quantleaf/probly-search/blob/master/LICENSE) [![Coverage Status](https://coveralls.io/repos/github/quantleaf/probly-search/badge.svg?branch=master)](https://coveralls.io/github/quantleaf/probly-search?branch=master) [![PRs Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg)](https://github.com/quantleaf/probly-search)


A lightweight full-text search library for creating small and short lifetime indices. 

This library started as port of the Node library [NDX](https://github.com/ndx-search/ndx).

## Documentation

Documentation is under development. For now go through the tests for examples.

## Features 

- Multiple fields full-text indexing and searching.
- Per-field score boosting.
- [BM25](https://en.wikipedia.org/wiki/Okapi_BM25) ranking function to rank matching documents. The same ranking
  function that is used by default in [Lucene](http://lucene.apache.org/core/) >= 6.0.0.
- [Trie](https://en.wikipedia.org/wiki/Trie) based dynamic
  [Inverted Index](https://en.wikipedia.org/wiki/Inverted_index).
- Configurable tokenizer and term filter.
- Free text queries with query expansion.
- Small memory footprint, optimized for mobile devices.
- Serializable index.


## License

[MIT](http://opensource.org/licenses/MIT)
