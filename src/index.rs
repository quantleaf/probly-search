use std::{
    collections::{HashMap, HashSet},
    fmt::{Debug, Formatter},
    hash::Hash,
    usize,
};

use crate::{
    query::{score::calculator::*, *},
    utils::{FieldAccessor, Filter, Tokenizer},
};
extern crate typed_generational_arena;
use typed_generational_arena::StandardArena;
use typed_generational_arena::StandardIndex as ArenaIndex;

/**
Index data structure.

This data structure is optimized for memory consumption and performant mutations during indexing, so it contains only
basic information.
 * typeparam `T` Document key.
 */
#[derive(Debug)]

pub struct Index<T> {
    /// Additional information about documents.
    pub docs: HashMap<T, DocumentDetails<T>>,
    /// Inverted index root node.
    pub root: ArenaIndex<InvertedIndexNode<T>>,
    /// Additional information about indexed fields in all documents.
    pub fields: Vec<FieldDetails>,

    pub arena_index: StandardArena<InvertedIndexNode<T>>,
    pub arena_doc: StandardArena<DocumentPointer<T>>,
}

impl<T: Eq + Hash + Copy + Debug> Index<T> {
    /**
    Creates an Index.
     * typeparam `T` Document key.
     * `fieldsNum` Number of fields.
     * returns `Index`
     */
    pub fn new(fields_num: usize) -> Self {
        Self::new_with_capacity(fields_num, 1000, 10000)
    }

    /**
    Creates an Index.
     * typeparam `T` Document key.
     * `fieldsNum` Number of fields.
     * `expected_index_size` Expected node count of index tree.
     * `expected_documents_count` Expected amount of documents added
     * returns `Index`
     */
    pub fn new_with_capacity(
        fields_num: usize,
        expected_index_size: usize,
        expected_documents_count: usize,
    ) -> Self {
        let fields: Vec<FieldDetails> = vec![FieldDetails { sum: 0, avg: 0_f64 }; fields_num];
        let mut arena_index = StandardArena::new();
        arena_index.reserve(expected_index_size);
        let mut arena_doc = StandardArena::new();
        arena_doc.reserve(expected_documents_count);
        Self {
            docs: HashMap::new(),
            root: arena_index.insert(create_inverted_index_node(&char::from_u32(0).unwrap())),
            fields,
            arena_doc,
            arena_index,
        }
    }

    pub fn get_root(&self) -> &InvertedIndexNode<T> {
        self.arena_index.get(self.root).unwrap()
    }

    pub fn get_root_mut(&mut self) -> &mut InvertedIndexNode<T> {
        self.arena_index.get_mut(self.root).unwrap()
    }

    /**
    Adds a document to the index.
     * typeparam `T` Document key.
     * `node` Inverted index node.
     * `doc` Posting.
    */

    pub fn add_document<D>(
        &mut self,
        field_accessors: &[FieldAccessor<D>],
        tokenizer: Tokenizer,
        filter: Filter,
        key: T,
        doc: &D,
    ) {
        let docs = &mut self.docs;
        let fields = &mut self.fields;
        let mut field_length = vec![0; fields.len()];
        let mut term_counts: HashMap<&str, Vec<usize>> = HashMap::new();
        let mut all_terms: Vec<&str> = Vec::new();
        for i in 0..fields.len() {
            if let Some(field_value) = field_accessors[i](doc) {
                let fields_len = fields.len();
                let mut field_details = fields.get_mut(i).unwrap();

                // tokenize text
                let terms = tokenizer(field_value);

                // filter and count terms, ignore empty strings
                let mut filtered_terms_count = 0;
                for mut term in terms {
                    term = filter(term);
                    if !term.is_empty() {
                        all_terms.push(term);
                        filtered_terms_count += 1;
                        let counts = term_counts.get_mut(term);
                        match counts {
                            None => {
                                let mut new_count = vec![0; fields_len];
                                new_count[i] += 1;
                                term_counts.insert(term, new_count);
                            }
                            Some(c) => {
                                c[i] += 1;
                            }
                        }
                    }
                }

                field_details.sum += filtered_terms_count;
                field_details.avg = field_details.sum as f64 / (docs.len() as f64 + 1_f64);
                field_length[i] = filtered_terms_count;
            }
        }

        docs.insert(key, DocumentDetails { key, field_length });
        for term in all_terms {
            let mut node_index = self.root;
            for (i, char) in term.chars().enumerate() {
                let node = self.arena_index.get(node_index).unwrap();
                if node.first_child.is_none() {
                    node_index =
                        create_inverted_index_nodes(&mut self.arena_index, node_index, term, &i);
                    break;
                }
                let next_node =
                    find_inverted_index_node_child_nodes_by_char(node, &char, &self.arena_index);
                match next_node {
                    None => {
                        node_index = create_inverted_index_nodes(
                            &mut self.arena_index,
                            node_index,
                            term,
                            &i,
                        );
                        break;
                    }
                    Some(n) => {
                        node_index = n;
                    }
                }
            }
            add_inverted_index_doc(
                self.arena_index.get_mut(node_index).unwrap(),
                DocumentPointer {
                    next: None,
                    details_key: key.to_owned(),
                    term_frequency: term_counts[term].to_owned(),
                },
                &mut self.arena_doc,
            )
        }
    }

    /**
     * Remove document from the index.

     * typeparam `T` Document key.
     * `index` Index.
     * `removed` Set of removed document ids.
     * `key` Document key.
    */
    pub fn remove_document(&mut self, removed: &mut HashSet<T>, key: T) {
        let fields = &mut self.fields;
        let doc_details_option = self.docs.get(&key);
        let mut remove_key = false;
        if let Some(doc_details) = doc_details_option {
            removed.insert((&key).to_owned());
            let details = doc_details;
            remove_key = true;
            let new_len = (self.docs.len() - 1) as f64;
            for i in 0..fields.len() {
                let field_length = &details.field_length[i];
                if field_length > &0 {
                    let field = fields.get_mut(i);
                    if let Some(f) = field {
                        f.sum -= *field_length;
                        f.avg = f.sum as f64 / new_len;
                    }
                }
            }
        }

        if remove_key {
            self.docs.remove(&key);
        }
    }

    /**
    Cleans up removed documents from the {@link Index}.
    Recursively cleans up removed documents from the index.
     * `T` Document key.
     * `index` Index.
     * `removed` Set of removed document ids.
    */
    pub fn vacuum(&mut self, removed: &mut HashSet<T>) {
        self.vacuum_node(self.root, removed);
        removed.clear();
    }

    /**
    Recursively cleans up removed documents from the index.
     * `T` Document key.
     * `index` Index.
     * `removed` Set of removed document ids.
    */
    fn vacuum_node(
        &mut self,
        node_index: ArenaIndex<InvertedIndexNode<T>>,
        removed: &HashSet<T>,
    ) -> usize {
        self.disconnect_and_count_documents(node_index, Some(removed));
        let mut prev_child: Option<ArenaIndex<InvertedIndexNode<T>>> = None;
        let mut ret = 0;
        let node = self.arena_index.get(node_index).unwrap();
        if node.first_doc.is_some() {
            ret = 1;
        }

        let mut child_option = node.first_child;
        while let Some(child_index) = child_option {
            let r = self.vacuum_node(child_index, removed);
            ret |= r;
            if r == 0 {
                // subtree doesn't have any documents, remove this node
                match prev_child {
                    Some(prev) => {
                        self.arena_index.get_mut(prev).unwrap().next =
                            self.arena_index.get(child_index).unwrap().next;
                    }
                    None => {
                        self.arena_index.get_mut(node_index).unwrap().first_child =
                            self.arena_index.get(child_index).unwrap().next;
                    }
                }
            } else {
                prev_child = Some(child_index);
            }
            child_option = self.arena_index.get(child_index).unwrap().next;

            if r == 0 {
                self.arena_index.remove(child_index);
            }
        }
        ret
    }

    /**
    Cleans up removed documents from a node, and returns the document frequency
     * `T` Document key.
     * `node_index` Index of the node
     * `index` Index.
     * `removed` Set of removed document ids.
    */
    pub(crate) fn disconnect_and_count_documents(
        &mut self,
        node_index: ArenaIndex<InvertedIndexNode<T>>,
        removed: Option<&HashSet<T>>,
    ) -> usize {
        let node = self.arena_index.get_mut(node_index).unwrap();
        let mut prev_pointer: Option<ArenaIndex<DocumentPointer<T>>> = None;
        let mut pointer_option = node.first_doc;
        let mut document_frequency = 0;
        while let Some(pointer) = pointer_option {
            let is_removed = removed.is_some()
                && removed
                    .unwrap()
                    .contains(&self.arena_doc.get(pointer).unwrap().details_key);
            if is_removed {
                match &prev_pointer {
                    None => {
                        node.first_doc = self.arena_doc.get(pointer).unwrap().next;
                    }
                    Some(prev) => {
                        self.arena_doc.get_mut(*prev).unwrap().next =
                            self.arena_doc.get(pointer).unwrap().next;
                    }
                }
            } else {
                document_frequency += 1;
                prev_pointer = Some(pointer);
            }
            pointer_option = self.arena_doc.get(pointer).unwrap().next;
            if is_removed {
                self.arena_doc.remove(pointer);
            }
        }
        document_frequency
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
    pub fn query<M, S: ScoreCalculator<T, M>>(
        &mut self,
        query: &str,
        score_calculator: &mut S,
        tokenizer: Tokenizer,
        filter: Filter,
        fields_boost: &[f64],
        removed: Option<&HashSet<T>>,
    ) -> Vec<QueryResult<T>>
    where
        T: Copy,
    {
        let query_terms = tokenizer(query);
        let mut scores: HashMap<T, f64> = HashMap::new();
        for (query_term_index, query_term_pre_filter) in query_terms.iter().enumerate() {
            let query_term = filter(query_term_pre_filter);
            if !query_term.is_empty() {
                let expanded_terms = expand_term(self, query_term, &self.arena_index);
                let mut visited_documents_for_term: HashSet<T> = HashSet::new();
                for query_term_expanded in expanded_terms {
                    let term_node_option = find_inverted_index_node(
                        self.root,
                        &query_term_expanded,
                        &self.arena_index,
                    );
                    if let Some(term_node_index) = term_node_option {
                        let document_frequency =
                            self.disconnect_and_count_documents(term_node_index, removed);
                        let term_node = self.arena_index.get(term_node_index).unwrap();
                        if let Some(term_node_option_first_doc) = term_node.first_doc {
                            if document_frequency > 0 {
                                let term_expansion_data = TermData {
                                    query_term_index,
                                    all_query_terms: query_terms.clone(),
                                    query_term,
                                    query_term_expanded: &query_term_expanded,
                                };
                                let pre_calculations = &score_calculator.before_each(
                                    &term_expansion_data,
                                    document_frequency,
                                    &self.docs,
                                );

                                let mut pointer = Some(term_node_option_first_doc);
                                while let Some(p) = pointer {
                                    let pointer_borrowed = self.arena_doc.get(p).unwrap();
                                    let key = &pointer_borrowed.details_key;
                                    if removed.is_none() || !removed.unwrap().contains(key) {
                                        let fields = &self.fields;
                                        let score = &score_calculator.score(
                                            pre_calculations.as_ref(),
                                            pointer_borrowed,
                                            self.docs.get(key).unwrap(),
                                            &term_node_index,
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
                                        }
                                    }
                                    visited_documents_for_term.insert(key.to_owned());
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
        score_calculator.finalize(&mut result);

        result.sort_by(|a, b| b.score.partial_cmp(&a.score).unwrap());

        result
    }
}

/**
Document Details object stores additional information about documents.
 * typeparam `T` Document key.
 */

#[derive(Debug, PartialEq, Eq)]
pub struct DocumentDetails<T> {
    /**
    Document key. It can be a simple unique ID or a direct reference to original document.
     */
    pub key: T,
    /**
    Field lengths is an array that contains number of terms in each indexed text field.
     */
    pub field_length: Vec<usize>,
}

/**
Document pointer contains information about term frequency for a document.
* typeparam `T` Document key.
*/
#[derive(Debug)]

pub struct DocumentPointer<T> {
    /**
    Next DocumentPointer in the intrusive linked list.
     */
    pub next: Option<ArenaIndex<DocumentPointer<T>>>,
    /**
    Reference to a DocumentDetailsobject that is used for this document.
     */
    pub details_key: T,
    /**
    Term frequency in each field.
     */
    pub term_frequency: Vec<usize>,
}

/**
Inverted Index Node.
Inverted index is implemented with a [trie](https://en.wikipedia.org/wiki/Trie) data structure.
 * typeparam `T` Document key.
*/
pub struct InvertedIndexNode<T> {
    /**
    Char code is used to store keys in the trie data structure.
     */
    pub char: char,
    /**
    Next InvertedIndexNode in the intrusive linked list.
     */
    pub next: Option<ArenaIndex<InvertedIndexNode<T>>>,
    /**
    Linked list of children {@link InvertedIndexNode}.
     */
    pub first_child: Option<ArenaIndex<InvertedIndexNode<T>>>,
    /**
    Linked list of documents associated with this node.
     */
    pub first_doc: Option<ArenaIndex<DocumentPointer<T>>>,
}

impl<T> PartialEq for InvertedIndexNode<T> {
    fn eq(&self, other: &InvertedIndexNode<T>) -> bool {
        other.char == self.char
    }
}

impl<T> Debug for InvertedIndexNode<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        f.debug_struct("InvertedIndexNode")
            .field("char", &self.char)
            .finish()
    }
}

/**
Field Details contains additional information about fields.
 */
#[derive(Clone, Debug, PartialEq)]
pub struct FieldDetails {
    /**
    Sum of field lengths in all documents.
     */
    pub sum: usize,
    /**
    Average of field lengths in all documents.
     */
    pub avg: f64,
}

/**
Creates inverted index node.
 * typeparam `T` Document key.
 * `charCode` Char code.
 returns InvertedIndexNode instance.
 */
fn create_inverted_index_node<T>(char: &char) -> InvertedIndexNode<T> {
    InvertedIndexNode {
        char: char.to_owned(),
        next: None,
        first_child: None,
        first_doc: None,
    }
}

/**
Finds inverted index node that matches the `term`.
 * typeparam `T` Document key.
 * `node` Root node.
 * `term` Term.
returns Inverted index node that contains `term` or an `undefined` value.
 */
pub(crate) fn find_inverted_index_node<T>(
    node: ArenaIndex<InvertedIndexNode<T>>,
    term: &str,
    index_arena: &StandardArena<InvertedIndexNode<T>>,
) -> Option<ArenaIndex<InvertedIndexNode<T>>> {
    let mut node_iteration = Some(node);
    for char in term.chars() {
        if let Some(node) = node_iteration {
            node_iteration = find_inverted_index_node_child_nodes_by_char(
                index_arena.get(node).unwrap(),
                &char,
                index_arena,
            );
        } else {
            break;
        }
    }
    node_iteration
}

/**
Finds inverted index child node with matching `char`.
 * typeparam `T` Document key.
 * `node` InvertedIndexNode.
 * `charCode` Char code.
returns Matching InvertedIndexNode or `undefined`.
 */
fn find_inverted_index_node_child_nodes_by_char<T>(
    from_node: &InvertedIndexNode<T>,
    char: &char,
    index_arena: &StandardArena<InvertedIndexNode<T>>,
) -> Option<ArenaIndex<InvertedIndexNode<T>>> {
    let child = from_node.first_child;
    let mut iter = child;
    while let Some(node_index) = iter {
        let node = index_arena.get(node_index).unwrap();
        if &node.char == char {
            return Some(node_index);
        }

        iter = node.next;
    }
    None
}

/**
Adds inverted index child node.
 * typeparam `T` Document key.
 * `parent` Parent node.
 * `child` Child node to add.
 */
fn add_inverted_index_child_node<T: Clone>(
    parent_index: ArenaIndex<InvertedIndexNode<T>>,
    child: ArenaIndex<InvertedIndexNode<T>>,
    index_arena: &mut StandardArena<InvertedIndexNode<T>>,
) {
    let parent = index_arena.get_mut(parent_index).unwrap();
    if let Some(first) = parent.first_child {
        index_arena.get_mut(child).unwrap().next = Some(first);
    }
    index_arena.get_mut(parent_index).unwrap().first_child = Some(child);
}

/**
Adds a document to the index.

typeparam `T` Document key.
typeparam `D` Document type.
 * index {@link Index}.
 * fieldAccessors Field accessors.
 * `tokenizer` Tokenizer is a function that breaks a text into words, phrases, symbols, or other meaningful elements called tokens.
 * `filter` Filter is a function that processes tokens and returns terms, terms are used in Inverted Index to index documents.
 * `key` Document key.
 * `doc` Document.
 */
fn add_inverted_index_doc<T: Clone>(
    node: &mut InvertedIndexNode<T>,
    mut doc: DocumentPointer<T>,
    arena_doc: &mut StandardArena<DocumentPointer<T>>,
) {
    let node_value = node;
    if let Some(first) = node_value.first_doc {
        doc.next = Some(first);
    }
    let doc_index = arena_doc.insert(doc);
    node_value.first_doc = Some(doc_index);
}

/**
Creates inverted index nodes for the `term` starting from the `start` character.
 * typeparam `T` Document key.
 * `parent` Parent node.
 * `term` Term.
 * `start` First char code position in the `term`.
 * returns leaf InvertedIndexNode.
 */
fn create_inverted_index_nodes<T: Clone>(
    arena_index: &mut StandardArena<InvertedIndexNode<T>>,
    mut parent: ArenaIndex<InvertedIndexNode<T>>,
    term: &str,
    start: &usize,
) -> ArenaIndex<InvertedIndexNode<T>> {
    for char in term.chars().skip(start.to_owned()) {
        let new_node = arena_index.insert(create_inverted_index_node(&char));
        let new_parent = {
            add_inverted_index_child_node(parent, new_node, arena_index); // unsafe { .get().as_mut().unwrap() }
            arena_index.get(parent).unwrap().first_child
        };
        parent = new_parent.unwrap();
    }
    parent
}

#[cfg(test)]
mod tests {

    use super::*;

    /**
        Count the amount of nodes of the index.
        returns the amount, including root node. Which means count will alway be greater than 0
    */
    fn count_nodes<T>(idx: &Index<T>) -> i32 {
        fn count_nodes_recursively<T>(
            idx: &Index<T>,
            node_index: ArenaIndex<InvertedIndexNode<T>>,
        ) -> i32 {
            let mut count = 1;
            let node = idx.arena_index.get(node_index).unwrap();
            if let Some(first) = node.first_child {
                count += count_nodes_recursively(idx, first);
            }
            if let Some(next) = node.next {
                count += count_nodes_recursively(idx, next);
            }
            count
        }
        count_nodes_recursively(idx, idx.root)
    }

    #[derive(Clone)]
    struct Doc {
        id: usize,
        text: String,
    }

    fn tokenizer(s: &str) -> Vec<&str> {
        s.split(' ').collect::<Vec<_>>()
    }

    fn filter(s: &str) -> &str {
        s
    }
    fn field_accessor(doc: &Doc) -> Option<&str> {
        Some(doc.text.as_str())
    }

    mod add {

        use super::*;

        #[test]
        fn it_should_add_one_document_with_three_terms<'idn>() {
            let field_accessors: Vec<FieldAccessor<Doc>> =
                vec![field_accessor as fn(doc: &Doc) -> Option<&str>];

            let mut index = Index::<usize>::new(1);
            let doc = Doc {
                id: 1,
                text: "a b c".to_string(),
            };

            index.add_document(&field_accessors, tokenizer, filter, doc.id, &doc);

            assert_eq!(index.docs.len(), 1);
            let (_, added_doc) = index.docs.iter().next().unwrap();
            let e = DocumentDetails {
                field_length: vec![3],
                key: 1_usize,
            };
            assert_eq!(&*added_doc, &e);
            assert_eq!(index.fields[0], FieldDetails { avg: 3_f64, sum: 3 });
            let root = index.get_root();
            assert_eq!(&root.char, &char::from_u32(0).unwrap());
            assert_eq!(&root.next.is_none(), &true);
            assert_eq!(&root.first_doc.is_none(), &true);

            let first_child = index.arena_index.get(root.first_child.unwrap()).unwrap();
            assert_eq!(&first_child.char, &(char::from_u32(99).unwrap()));
            assert_eq!(&first_child.first_child.is_none(), &true);

            let first_child_next = index.arena_index.get(first_child.next.unwrap()).unwrap();
            assert_eq!(first_child_next.char, (char::from_u32(98).unwrap()));
            assert_eq!(&first_child.first_child.is_none(), &true);
            let first_child_first_doc =
                index.arena_doc.get(first_child.first_doc.unwrap()).unwrap();
            assert_eq!(&first_child_first_doc.term_frequency, &vec![1_usize]);
            assert_eq!(&first_child_first_doc.details_key, &e.key);
            assert_eq!(&first_child_first_doc.next.is_none(), &true);
            assert_eq!(&first_child_first_doc.next.is_none(), &true);

            assert_eq!(
                &index
                    .arena_index
                    .get(first_child_next.next.unwrap())
                    .unwrap()
                    .char,
                &(char::from_u32(97).unwrap())
            );

            // TODO test more properties
        }

        #[test]
        fn it_should_add_shared_terms() {
            let field_accessors: Vec<FieldAccessor<Doc>> =
                vec![field_accessor as fn(doc: &Doc) -> Option<&str>];

            let mut index = Index::<usize>::new(1);
            let doc_1 = Doc {
                id: 1,
                text: "a b c".to_string(),
            };
            let doc_2 = Doc {
                id: 2,
                text: "b c d".to_string(),
            };

            index.add_document(&field_accessors, tokenizer, filter, doc_1.id, &doc_1);

            index.add_document(&field_accessors, tokenizer, filter, doc_2.id, &doc_2);

            assert_eq!(index.docs.len(), 2);
            assert_eq!(
                index.docs.get(&doc_1.id).unwrap(),
                &DocumentDetails {
                    field_length: vec![3],
                    key: 1_usize,
                }
            );
            assert_eq!(
                index.docs.get(&doc_2.id).unwrap(),
                &DocumentDetails {
                    field_length: vec![3],
                    key: 2_usize
                }
            );
            assert_eq!(index.fields[0], FieldDetails { avg: 3_f64, sum: 6 });

            let root = &index.get_root();

            assert_eq!(&root.char, &char::from_u32(0).unwrap());
            assert_eq!(&root.next.is_none(), &true);
            assert_eq!(&root.first_doc.is_none(), &true);

            let first_child = index.arena_index.get(root.first_child.unwrap()).unwrap();
            assert_eq!(&first_child.char, &char::from_u32(100).unwrap());
            assert_eq!(&first_child.first_child.is_none(), &true);

            let first_child_next = index.arena_index.get(first_child.next.unwrap()).unwrap();
            assert_eq!(&first_child_next.char, &char::from_u32(99).unwrap());
            assert_eq!(&first_child.first_child.is_none(), &true);
            let nested_next = index
                .arena_index
                .get(first_child_next.next.unwrap())
                .unwrap();
            assert_eq!(&nested_next.char, &char::from_u32(98).unwrap());
            let nested_nested_next = index.arena_index.get(nested_next.next.unwrap()).unwrap();
            assert_eq!(&nested_nested_next.char, &char::from_u32(97).unwrap());

            // dont test all the properties
        }

        #[test]
        fn it_should_ignore_empty_tokens() {
            let field_accessors: Vec<FieldAccessor<Doc>> =
                vec![field_accessor as fn(doc: &Doc) -> Option<&str>];

            let mut index = Index::<usize>::new(1);
            let doc_1 = Doc {
                id: 1,
                text: "a  b".to_string(), // double space could introduce empty tokens
            };

            index.add_document(&field_accessors, tokenizer, filter, doc_1.id, &doc_1);
        }
    }

    mod delete {

        use super::*;
        #[test]
        fn it_should_delete_1() {
            let mut index = Index::<usize>::new(1);
            assert_eq!(index.arena_doc.is_empty(), true);

            let mut removed = HashSet::new();
            let docs = vec![Doc {
                id: 1,
                text: "a".to_string(),
            }];

            for doc in docs {
                index.add_document(&[field_accessor], tokenizer, filter, doc.id, &doc)
            }

            index.remove_document(&mut removed, 1);
            index.vacuum(&mut removed);

            assert_eq!(index.docs.len(), 0);
            assert_eq!(index.fields.len(), 1);
            assert_eq!(index.fields.get(0).unwrap().sum, 0);
            assert_eq!(index.fields.get(0).unwrap().avg.is_nan(), true);

            let x = index.get_root();
            let y: &InvertedIndexNode<usize> = &InvertedIndexNode {
                char: char::from_u32(0).unwrap(),
                first_child: None,
                first_doc: None,
                next: None,
            };
            assert_eq!(x, y);

            // Delete root from index and assert is empty
            index.arena_index.remove(index.root);
            assert_eq!(index.arena_doc.is_empty(), true);
            assert_eq!(index.arena_index.is_empty(), true);
        }
    }
    mod find {

        use super::*;

        fn create(
            arena_index: &mut StandardArena<InvertedIndexNode<usize>>,
            char: char,
        ) -> ArenaIndex<InvertedIndexNode<usize>> {
            let node: InvertedIndexNode<usize> = create_inverted_index_node(&char);
            arena_index.insert(node)
        }

        mod char_code {
            use super::*;

            #[test]
            fn it_should_find_undefined_children_if_none() {
                let mut index = Index::<usize>::new(1);
                let node = create(&mut index.arena_index, 'x');
                let c = find_inverted_index_node_child_nodes_by_char(
                    index.arena_index.get(node).unwrap(),
                    &'x',
                    &index.arena_index,
                );
                assert_eq!(c.is_none(), true);
            }

            #[test]
            fn it_should_find_existing() {
                let mut index = Index::<usize>::new(1);
                let p = create(&mut index.arena_index, 'x');
                let c1 = create(&mut index.arena_index, 'y');
                let c2 = create(&mut index.arena_index, 'z');
                add_inverted_index_child_node(p, c1, &mut index.arena_index);
                add_inverted_index_child_node(p, c2, &mut index.arena_index);
                assert_eq!(
                    find_inverted_index_node_child_nodes_by_char(
                        index.arena_index.get(p).unwrap(),
                        &'y',
                        &index.arena_index
                    )
                    .unwrap(),
                    c1
                );
                assert_eq!(
                    find_inverted_index_node_child_nodes_by_char(
                        index.arena_index.get(p).unwrap(),
                        &'z',
                        &index.arena_index
                    )
                    .unwrap(),
                    c2
                )
            }
        }

        mod term {
            use super::*;
            #[test]
            fn it_should_find() {
                let mut index = Index::<usize>::new(1);
                let p = create(&mut index.arena_index, 'x');
                let a = create(&mut index.arena_index, 'a');
                let b = create(&mut index.arena_index, 'b');
                let c = create(&mut index.arena_index, 'c');
                add_inverted_index_child_node(p, a, &mut index.arena_index);
                add_inverted_index_child_node(a, b, &mut index.arena_index);
                add_inverted_index_child_node(b, c, &mut index.arena_index);
                assert_eq!(
                    find_inverted_index_node(p, "abc", &index.arena_index).unwrap(),
                    c
                );
            }
        }

        mod count {
            use super::*;

            #[test]
            fn it_should_count_nodes() {
                let field_accessors: Vec<FieldAccessor<Doc>> =
                    vec![field_accessor as fn(doc: &Doc) -> Option<&str>];

                let mut index = Index::<usize>::new(1);
                let doc = Doc {
                    id: 1,
                    text: "abc".to_string(),
                };
                let doc_2 = Doc {
                    id: 1,
                    text: "abe".to_string(),
                };

                index.add_document(&field_accessors, tokenizer, filter, doc.id, &doc);
                index.add_document(&field_accessors, tokenizer, filter, doc_2.id, &doc_2);
                assert_eq!(count_nodes(&index), 5); //
            }

            #[test]
            fn it_should_count_nodes_2() {
                let field_accessors: Vec<FieldAccessor<Doc>> =
                    vec![field_accessor as fn(doc: &Doc) -> Option<&str>];

                let mut index = Index::<usize>::new(1);

                let doc = Doc {
                    id: 1,
                    text: "ab cd".to_string(),
                };
                let doc_2 = Doc {
                    id: 1,
                    text: "ab ef".to_string(),
                };

                index.add_document(&field_accessors, tokenizer, filter, doc.id, &doc);
                index.add_document(&field_accessors, tokenizer, filter, doc_2.id, &doc_2);
                assert_eq!(count_nodes(&index), 7); //
            }

            #[test]
            fn it_should_count_nodes_empty() {
                let index = Index::<usize>::new(1);
                assert_eq!(count_nodes(&index), 1); // 1 for root
            }
        }
    }
}
