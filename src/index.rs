use std::{
    collections::{HashMap, HashSet},
    fmt::{Debug, Formatter},
    hash::Hash,
    usize,
};

use ghost_cell::{GhostCell, GhostToken};
use typed_arena::Arena;

use crate::utils::{FieldAccessor, Filter, Tokenizer};

pub type InvertedIndexNodeRef<'arena, 'idx, 'idn, T> =
    &'arena GhostCell<'idx, InvertedIndexNode<'arena, 'idx, 'idn, T>>;

pub type DocumentPointerRef<'arena, 'idn, T> =
    &'arena GhostCell<'idn, DocumentPointer<'arena, 'idn, T>>;

/**
Index data structure.

This data structure is optimized for memory consumption and performant mutations during indexing, so it contains only
basic information.
 * typeparam `T` Document key.
 */
pub struct Index<'arena, 'idx, 'idn, T> {
    /// Additional information about documents.
    pub docs: HashMap<T, DocumentDetails<T>>,
    /// Inverted index root node.
    pub root: Option<InvertedIndexNodeRef<'arena, 'idx, 'idn, T>>,
    /// Additional information about indexed fields in all documents.
    pub fields: Vec<FieldDetails>,

    // Arena?
    pub arena_index: Arena<InvertedIndexNode<'arena, 'idx, 'idn, T>>,
    pub arena_doc: Arena<DocumentPointer<'arena, 'idn, T>>,
}
unsafe impl<'arena, 'idx, 'idn, T> Send for Index<'arena, 'idx, 'idn, T> {}

/**
Creates an Index.
 * typeparam `T` Document key.
 * `fieldsNum` Number of fields.
 * returns `Index`
 */
pub fn create_index<'arena, 'idx, 'idn, T>(fields_num: usize) -> Index<'arena, 'idx, 'idn, T> {
    let fields: Vec<FieldDetails> = vec![FieldDetails { sum: 0, avg: 0_f64 }; fields_num];
    Index {
        docs: HashMap::new(),
        root: None,
        fields,
        arena_index: Arena::new(),
        arena_doc: Arena::new(),
    }
}

pub fn initialize<'arena, 'idx, 'idn, T>(index: &'arena mut Index<'arena, 'idx, 'idn, T>) {
    index.root =
        Some(GhostCell::from_mut(index.arena_index.alloc(
            create_inverted_index_node(&char::from_u32(0).unwrap()),
        )));
}

/**
Document Details object stores additional information about documents.
 * typeparam `T` Document key.
 */
#[derive(Debug, PartialEq)]
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
pub struct DocumentPointer<'arena, 'idn, T> {
    /**
    Next DocumentPointer in the intrusive linked list.
     */
    pub next: Option<DocumentPointerRef<'arena, 'idn, T>>,
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
pub struct InvertedIndexNode<'arena, 'idx, 'idn, T> {
    /**
    Char code is used to store keys in the trie data structure.
     */
    pub char: char,
    /**
    Next InvertedIndexNode in the intrusive linked list.
     */
    pub next: Option<InvertedIndexNodeRef<'arena, 'idx, 'idn, T>>,
    /**
    Linked list of children {@link InvertedIndexNode}.
     */
    pub first_child: Option<InvertedIndexNodeRef<'arena, 'idx, 'idn, T>>,
    /**
    Linked list of documents associated with this node.
     */
    pub first_doc: Option<DocumentPointerRef<'arena, 'idn, T>>,
}

impl<'arena, 'idx, 'idn, T> PartialEq for InvertedIndexNode<'arena, 'idx, 'idn, T> {
    fn eq(&self, other: &InvertedIndexNode<T>) -> bool {
        other.char == self.char
    }
}

impl<'arena, 'idx, 'idn, T> Debug for InvertedIndexNode<'arena, 'idx, 'idn, T> {
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
fn create_inverted_index_node<'arena, 'idx, 'idn, T>(
    char: &char,
) -> InvertedIndexNode<'arena, 'idx, 'idn, T> {
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
pub fn find_inverted_index_node<'arena, 'idx, 'idn, T>(
    node: InvertedIndexNodeRef<'arena, 'idx, 'idn, T>,
    term: &str,
    token: &GhostToken<'idx>,
) -> Option<InvertedIndexNodeRef<'arena, 'idx, 'idn, T>> {
    let mut node_iteration = Some(node);
    for char in term.chars() {
        if node_iteration.is_none() {
            break;
        }
        node_iteration =
            find_inverted_index_node_child_nodes_by_char(node_iteration.unwrap(), &char, token);
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
fn find_inverted_index_node_child_nodes_by_char<'arena, 'idx, 'idn, T>(
    from_node: InvertedIndexNodeRef<'arena, 'idx, 'idn, T>,
    char: &char,
    token: &GhostToken<'idx>,
) -> Option<InvertedIndexNodeRef<'arena, 'idx, 'idn, T>> {
    let child = from_node.borrow(token).first_child;
    if child.is_none() {
        return None;
    }
    let mut iter = child;
    while let Some(node) = iter {
        let iter_ref = node.borrow(token);
        if &iter_ref.char == char {
            return Some(node);
        }
        iter = iter_ref.next
    }
    return None;
}

/**
Adds inverted index child node.
 * typeparam `T` Document key.
 * `parent` Parent node.
 * `child` Child node to add.
 */
fn add_inverted_index_child_node<'arena, 'idx, 'idn, T: Clone>(
    parent: &mut InvertedIndexNode<'arena, 'idx, 'idn, T>,
    child: InvertedIndexNodeRef<'arena, 'idx, 'idn, T>,
    token: &mut GhostToken<'idx>,
) {
    //-> Rc<GhostCell<'idx,InvertedIndexNode<T>>>
    if let Some(first) = parent.first_child {
        child.borrow_mut(token).next = Some(first);
    }
    //let c=  parent.clone();
    parent.first_child = Some(child);
    //Rc::clone(&parent.borrow().first_child.unwrap())
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
fn add_inverted_index_doc<'arena, 'idx, 'idn, T: Clone>(
    node: InvertedIndexNodeRef<'arena, 'idx, 'idn, T>,
    doc: DocumentPointerRef<'arena, 'idn, T>,
    mut token_x: &mut GhostToken<'idx>,
    mut token_n: &mut GhostToken<'idn>,
) {
    let mut node_locked = node.borrow_mut(&mut token_x);
    if let Some(first) = &node_locked.first_doc {
        doc.borrow_mut(&mut token_n).next = Some(first);
    }
    node_locked.first_doc = Some(doc);
}

/**
Adds a document to the index.
 * typeparam `T` Document key.
 * `node` Inverted index node.
 * `doc` Posting.
*/

pub fn add_document_to_index<'arena, 'idx, 'idn, T: Eq + Hash + Copy, D>(
    index: &'arena mut Index<'arena, 'idx, 'idn, T>,
    field_accessors: &[FieldAccessor<D>],
    tokenizer: Tokenizer,
    filter: Filter,
    key: T,
    doc: D,
    token_x: &mut GhostToken<'idx>,
    token_n: &mut GhostToken<'idn>,
) {
    let docs = &mut index.docs;
    let fields = &mut index.fields;
    let mut field_length = Vec::new();
    let mut term_counts: HashMap<String, Vec<usize>> = HashMap::new();
    let mut all_terms: Vec<String> = Vec::new();
    for i in 0..fields.len() {
        match field_accessors[i](&doc) {
            None => {
                field_length.push(0);
            }
            Some(field_value) => {
                let fields_len = fields.len();
                let mut field_details = fields.get_mut(i).unwrap();

                // tokenize text
                let terms = tokenizer(&field_value);

                // filter and count terms, ignore empty strings
                let mut filtered_terms_count = 0;
                for mut term in terms {
                    term = filter(&term);
                    if !term.is_empty() {
                        all_terms.push(term.to_owned());
                        filtered_terms_count += 1;
                        let counts = term_counts.get_mut(&term);
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
                field_length.push(filtered_terms_count);
            }
        }
    }

    docs.insert(key, DocumentDetails { key, field_length });
    for term in all_terms {
        let mut node = index.root.unwrap();

        for (i, char) in term.chars().enumerate() {
            if node.borrow(token_x).first_child.is_none() {
                node = create_inverted_index_nodes(index, node, &term, &i, token_x);
                break;
            }
            let next_node = find_inverted_index_node_child_nodes_by_char(&node, &char, token_x);
            match next_node {
                None => {
                    node = create_inverted_index_nodes(index, node, &term, &i, token_x);
                    break;
                }
                Some(n) => {
                    node = n;
                }
            }
        }
        let y = index.arena_doc.alloc(DocumentPointer {
            next: None,
            details_key: key.to_owned(),
            term_frequency: term_counts[&term].to_owned(),
        });
        let z = GhostCell::from_mut(y);
        add_inverted_index_doc(node, &*z, token_x, token_n)
    }
}

/**
Creates inverted index nodes for the `term` starting from the `start` character.
 * typeparam `T` Document key.
 * `parent` Parent node.
 * `term` Term.
 * `start` First char code position in the `term`.
 * returns leaf InvertedIndexNode.

 */
fn create_inverted_index_nodes<'arena, 'idx, 'idn, T: Clone>(
    index: &'arena mut Index<'arena, 'idx, 'idn, T>,
    parent: InvertedIndexNodeRef<'arena, 'idx, 'idn, T>,
    term: &str,
    start: &usize,
    token: &mut GhostToken<'idx>,
) -> InvertedIndexNodeRef<'arena, 'idx, 'idn, T> {
    for (i, char) in term.chars().enumerate() {
        if &i < start {
            continue;
        }
        let new_node =
            GhostCell::from_mut(index.arena_index.alloc(create_inverted_index_node(&char)));
        let new_parent = {
            let mut parent_ref = parent.borrow_mut(token);
            add_inverted_index_child_node(parent_ref, new_node, token);
            parent_ref.first_child.unwrap()
        };
        parent = new_parent;
    }
    parent
}

/**
 * Remove document from the index.

 * typeparam `T` Document key.
 * `index` Index.
 * `removed` Set of removed document ids.
 * `key` Document key.
*/
pub fn remove_document_from_index<'idx, T: Hash + Eq + Copy>(
    index: &mut Index<T>,
    removed: &mut HashSet<T>,
    key: T,
    token: &GhostToken<'idx>,
) {
    //
    let fields = &mut index.fields;
    let doc_details_option = index.docs.get(&key);
    let mut remove_key = false;
    if let Some(doc_details) = doc_details_option {
        removed.insert((&key).to_owned());
        let details = doc_details;
        remove_key = true;
        let new_len = (index.docs.len() - 1) as f64;
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
        index.docs.remove(&key);
    }
}

/**
Cleans up removed documents from the {@link Index}.
*/
pub fn vacuum_index<'arena, 'idx, 'idn, T: Hash + Eq>(
    index: &Index<'arena, 'idx, 'idn, T>,
    removed: &mut HashSet<T>,
    token_x: &mut GhostToken<'idx>,
    token_n: &mut GhostToken<'idn>,
) {
    vacuum_node(index, index.root.unwrap(), removed, token_x, token_n);
    removed.clear();
}

/**
Recursively cleans up removed documents from the index.
 * `T` Document key.
 * `index` Index.
 * `removed` Set of removed document ids.
*/
fn vacuum_node<'arena, 'idx, 'idn, T: Hash + Eq>(
    index: &Index<'arena, 'idx, 'idn, T>,
    node: InvertedIndexNodeRef<'arena, 'idx, 'idn, T>,
    removed: &mut HashSet<T>,
    token_x: &mut GhostToken<'idx>,
    token_n: &mut GhostToken<'idn>,
) -> usize {
    let mut prev_pointer: Option<DocumentPointerRef<'arena, 'idn, T>> = None;
    let mut pointer_option = (&node.borrow(token_x).first_doc).clone();
    while let Some(pointer) = pointer_option {
        let pb = &pointer.borrow(token_n);
        if removed.contains(&pb.details_key) {
            match &prev_pointer {
                None => {
                    node.borrow_mut(token_x).first_doc = pb.next.clone();
                }
                Some(prev) => {
                    prev.borrow_mut(token_n).next = pb.next.clone();
                }
            }
        } else {
            prev_pointer = Some((&pointer).clone());
        }
        pointer_option = (&pb.next).clone();
    }

    let mut prev_child: Option<InvertedIndexNodeRef<'arena, 'idx, 'idn, T>> = None;
    let mut ret = 0;
    if node.borrow(token_x).first_doc.is_some() {
        ret = 1;
    }

    let mut child_option = (&node.borrow(token_x).first_child).clone();
    while let Some(child) = child_option {
        let r = vacuum_node(index, child, removed, token_x, token_n);
        ret |= r;
        let child_unwrapped = child.borrow(token_x);
        if r == 0 {
            // subtree doesn't have any documents, remove this node
            match &prev_child {
                Some(prev) => {
                    prev.borrow_mut(token_x).next = child_unwrapped.next.clone();
                }
                None => {
                    node.borrow_mut(token_x).first_child = child_unwrapped.next.clone();
                }
            }
        } else {
            prev_child = Some(&child);
        }
        child_option = (&child_unwrapped.next).clone();
    }
    ret
}

/**
    Count the amount of nodes of the index.
    returns the amount, including root node. Which means count will alway be greater than 0
*/
pub fn count_nodes<'arena, 'idx, 'idn, T>(
    idx: &Index<'arena, 'idx, 'idn, T>,
    token: &GhostToken<'idx>,
) -> i32 {
    fn count_nodes_recursively<'arena, 'idx, 'idn, T>(
        node: InvertedIndexNodeRef<'arena, 'idx, 'idn, T>,
        token: &GhostToken<'idx>,
    ) -> i32 {
        let mut count = 1;
        let n = node.borrow(token);
        if let Some(first) = &n.first_child {
            count += count_nodes_recursively(first, token);
        }
        if let Some(next) = &n.next {
            count += count_nodes_recursively(next, token);
        }
        count
    }
    count_nodes_recursively(&idx.root.unwrap(), token)
}

#[cfg(test)]
mod tests {

    use super::*;

    #[derive(Clone)]
    struct Doc {
        id: usize,
        text: String,
    }

    fn tokenizer(s: &str) -> Vec<String> {
        s.split(' ')
            .map(|slice| slice.to_owned())
            .collect::<Vec<String>>()
    }

    fn filter(s: &String) -> String {
        s.to_owned()
    }
    fn field_accessor(doc: &Doc) -> Option<&str> {
        Some(doc.text.as_str())
    }

    mod add {

        use super::*;

        #[test]
        fn it_should_add_one_document_with_three_terms<'idx,'idn>() {
            GhostToken::new(|token_x| {
                GhostToken::new(|token_n| {
                    let field_accessors: Vec<FieldAccessor<Doc>> =
                        vec![field_accessor as fn(doc: &Doc) -> Option<&str>];

                    let mut index = create_index::<usize>(1);
                    let doc = Doc {
                        id: 1,
                        text: "a b c".to_string(),
                    };
                    add_document_to_index(
                        &mut index,
                        &field_accessors,
                        tokenizer,
                        filter,
                        doc.id,
                        doc,
                        &mut token_x,
                        &mut token_n,
                    );
                    assert_eq!(index.docs.len(), 1);
                    let (_, added_doc) = index.docs.iter().next().unwrap();
                    let e = DocumentDetails {
                        field_length: vec![3],
                        key: 1 as usize,
                    };
                    assert_eq!(&*added_doc, &e);
                    assert_eq!(index.fields[0], FieldDetails { avg: 3_f64, sum: 3 });
                    let root = &index.root.unwrap().borrow(&token_x);
                    assert_eq!(&root.char, &char::from_u32(0).unwrap());
                    assert_eq!(&root.next.is_none(), &true);
                    assert_eq!(&root.first_doc.is_none(), &true);

                    let first_child = &root.first_child.as_ref().unwrap().borrow(&token_x);
                    assert_eq!(&first_child.char, &(char::from_u32(99).unwrap()));
                    assert_eq!(&first_child.first_child.is_none(), &true);

                    let first_child_next = &first_child.next.as_ref().unwrap().borrow(&token_x);
                    assert_eq!(&first_child_next.char, &(char::from_u32(98).unwrap()));
                    assert_eq!(&first_child.first_child.is_none(), &true);
                    let first_child_first_doc =
                        &first_child.first_doc.as_ref().unwrap().borrow(&token_x);
                    assert_eq!(&first_child_first_doc.term_frequency, &vec![1 as usize]);
                    assert_eq!(&first_child_first_doc.details_key, &e.key);
                    assert_eq!(&first_child_first_doc.next.is_none(), &true);
                    assert_eq!(&first_child_first_doc.next.is_none(), &true);

                    assert_eq!(
                        &first_child_next.next.as_ref().unwrap().borrow(&token_x).char,
                        &(char::from_u32(97).unwrap())
                    );
                })
            })
            // TODO test more properties
        }

        #[test]
        fn it_should_add_shared_terms() {
            let field_accessors: Vec<FieldAccessor<Doc>> =
                vec![field_accessor as fn(doc: &Doc) -> Option<&str>];

            let mut index = create_index::<usize>(1);
            let doc_1 = Doc {
                id: 1,
                text: "a b c".to_string(),
            };
            let doc_2 = Doc {
                id: 2,
                text: "b c d".to_string(),
            };
            GhostToken::new(|token_x| {
                GhostToken::new(|token_n| {
                add_document_to_index(
                    &mut index,
                    &field_accessors,
                    tokenizer,
                    filter,
                    doc_1.id,
                    doc_1.clone(),
                    &mut token_x,
                    &mut token_n
                );

                add_document_to_index(
                    &mut index,
                    &field_accessors,
                    tokenizer,
                    filter,
                    doc_2.id,
                    doc_2.clone(),
                    &mut token_x,
                    &mut token_n
                );

                assert_eq!(index.docs.len(), 2);
                assert_eq!(
                    index.docs.get(&doc_1.id).unwrap(),
                    &DocumentDetails {
                        field_length: vec![3],
                        key: 1 as usize,
                    }
                );
                assert_eq!(
                    index.docs.get(&doc_2.id).unwrap(),
                    &DocumentDetails {
                        field_length: vec![3],
                        key: 2 as usize
                    }
                );
                assert_eq!(index.fields[0], FieldDetails { avg: 3_f64, sum: 6 });
                let root = &index.root.unwrap().borrow(&token_x);
                assert_eq!(&root.char, &char::from_u32(0).unwrap());
                assert_eq!(&root.next.is_none(), &true);
                assert_eq!(&root.first_doc.is_none(), &true);

                let first_child = &root.first_child.as_ref().unwrap().borrow(&token_x);
                assert_eq!(&first_child.char, &char::from_u32(100).unwrap());
                assert_eq!(&first_child.first_child.is_none(), &true);

                let first_child_next = &first_child.next.as_ref().unwrap().borrow(&token_x);
                assert_eq!(&first_child_next.char, &char::from_u32(99).unwrap());
                assert_eq!(&first_child.first_child.is_none(), &true);
                let nested_next = first_child_next.next.as_ref().unwrap().borrow(&token_x);
                assert_eq!(&nested_next.char, &char::from_u32(98).unwrap());
                let nested_nested_next = &nested_next.next.as_ref().unwrap().borrow(&token_x);
                assert_eq!(&nested_nested_next.char, &char::from_u32(97).unwrap());
            });
        });
            // dont test all the properties
        }

        #[test]
        fn it_should_ignore_empty_tokens() {
            let field_accessors: Vec<FieldAccessor<Doc>> =
                vec![field_accessor as fn(doc: &Doc) -> Option<&str>];

            let mut index = create_index::<usize>(1);
            let doc_1 = Doc {
                id: 1,
                text: "a  b".to_string(), // double space could introduce empty tokens
            };
            GhostToken::new(|token_x| {
                GhostToken::new(|token_n| {
                    add_document_to_index(
                        &mut index,
                        &field_accessors,
                        tokenizer,
                        filter,
                        doc_1.id,
                        doc_1.clone(),
                        &mut token_x,
                        &mut token_n
                    );
                });
            });
        }
    }

    mod delete {

        use super::*;
        #[test]
        fn it_should_delete_1() {
            let mut idx = create_index(1);
            let mut removed = HashSet::new();
            let docs = vec![Doc {
                id: 1,
                text: "a".to_string(),
            }];
            GhostToken::new(|token_x| {
                GhostToken::new(|token_n| {
                for doc in docs {
                    add_document_to_index(
                        &mut idx,
                        &[field_accessor],
                        tokenizer,
                        filter,
                        doc.id,
                        doc,
                        &mut token_x,
                        &mut token_n
                    )
                }
                remove_document_from_index(&mut idx, &mut removed, 1, &token_x);
                vacuum_index(&mut idx, &mut removed, &mut token_x, &mut token_n);

                assert_eq!(idx.docs.len(), 0);
                assert_eq!(idx.fields.len(), 1);
                assert_eq!(idx.fields.get(0).unwrap().sum, 0);
                assert_eq!(idx.fields.get(0).unwrap().avg.is_nan(), true);

                let x = idx.root.unwrap().borrow(&token_x);
                let y: &InvertedIndexNode<usize> = &InvertedIndexNode {
                    char: char::from_u32(0).unwrap(),
                    first_child: None,
                    first_doc: None,
                    next: None,
                };
                assert_eq!(x, y);
            });
        });
        }
    }
    mod find {

        use ghost_cell::GhostToken;

        use super::*;

        fn create<'arena, 'idx, 'idn>(
            index: &'arena mut Index<'arena, 'idx, 'idn, usize>,
            char: char,
            token: &GhostToken<'idx>,
        ) -> InvertedIndexNodeRef<'arena, 'idx, 'idn, usize> {
            let node: InvertedIndexNode<'arena, 'idx, 'idn, usize> =
                create_inverted_index_node(&char);
            GhostCell::from_mut(index.arena_index.alloc(node))
        }

        mod char_code {
            use super::*;

            #[test]
            fn it_should_find_undefined_children_if_none() {
                GhostToken::new(|token_x| {
                GhostToken::new(|token_n| {
                    let mut index = create_index::<usize>(1);
                    let node = create(&mut index, 'x', &token);
                    let c = find_inverted_index_node_child_nodes_by_char(&node, &'x', &token);
                    assert_eq!(c.is_none(), true);
                });
            }

            #[test]
            fn it_should_find_existing() {
                GhostToken::new(|token_x| {
                GhostToken::new(|token_n| {
                    let mut index = create_index::<usize>(1);
                    let p = create(&mut index, 'x', &token);
                    let c1 = create(&mut index, 'y', &token);
                    let c2 = create(&mut index, 'z', &token);
                    add_inverted_index_child_node(&mut p.borrow_mut(&mut token), &c1, &mut token);
                    add_inverted_index_child_node(&mut p.borrow_mut(&mut token), &c2, &mut token);
                    assert_eq!(
                        std::ptr::eq(
                            find_inverted_index_node_child_nodes_by_char(&p, &'y', &token).unwrap(),
                            c1
                        ),
                        true
                    );
                    assert_eq!(
                        std::ptr::eq(
                            find_inverted_index_node_child_nodes_by_char(&p, &'z', &token).unwrap(),
                            c2
                        ),
                        true
                    );
                });
            }
        }

        mod term {
            use super::*;
            #[test]
            fn it_should_find() {
                GhostToken::new(|token_x| {
                GhostToken::new(|token_n| {
                    let mut index = create_index::<usize>(1);
                    let p = create(&mut index, 'x', &token);
                    let a = create(&mut index, 'a', &token);
                    let b = create(&mut index, 'b', &token);
                    let c = create(&mut index, 'c', &token);
                    add_inverted_index_child_node(&mut p.borrow_mut(&mut token), &a, &mut token);
                    add_inverted_index_child_node(&mut a.borrow_mut(&mut token), &b, &mut token);
                    add_inverted_index_child_node(&mut b.borrow_mut(&mut token), &c, &mut token);
                    assert_eq!(
                        std::ptr::eq(find_inverted_index_node(p, &"abc", &token).unwrap(), c),
                        true
                    );
                });
            }
        }

        mod count {
            use super::*;

            #[test]
            fn it_should_count_nodes() {
                let field_accessors: Vec<FieldAccessor<Doc>> =
                    vec![field_accessor as fn(doc: &Doc) -> Option<&str>];

                let mut index = create_index::<usize>(1);
                let doc = Doc {
                    id: 1,
                    text: "abc".to_string(),
                };
                let doc_2 = Doc {
                    id: 1,
                    text: "abe".to_string(),
                };
                GhostToken::new(|token_x| {
                GhostToken::new(|token_n| {
                    add_document_to_index(
                        &mut index,
                        &field_accessors,
                        tokenizer,
                        filter,
                        doc.id,
                        doc,
                        &mut token,
                    );
                    add_document_to_index(
                        &mut index,
                        &field_accessors,
                        tokenizer,
                        filter,
                        doc_2.id,
                        doc_2,
                        &mut token,
                    );
                    assert_eq!(count_nodes(&index, &token), 5); //
                });
            }

            #[test]
            fn it_should_count_nodes_2() {
                let field_accessors: Vec<FieldAccessor<Doc>> =
                    vec![field_accessor as fn(doc: &Doc) -> Option<&str>];

                let mut index = create_index::<usize>(1);
                let doc = Doc {
                    id: 1,
                    text: "ab cd".to_string(),
                };
                let doc_2 = Doc {
                    id: 1,
                    text: "ab ef".to_string(),
                };
                GhostToken::new(|token_x| {
                GhostToken::new(|token_n| {
                    add_document_to_index(
                        &mut index,
                        &field_accessors,
                        tokenizer,
                        filter,
                        doc.id,
                        doc,
                        &mut token,
                    );
                    add_document_to_index(
                        &mut index,
                        &field_accessors,
                        tokenizer,
                        filter,
                        doc_2.id,
                        doc_2,
                        &mut token,
                    );
                    assert_eq!(count_nodes(&index, &token), 7); //
                });
            }

            #[test]
            fn it_should_count_nodes_empty() {
                let index = create_index::<usize>(1);
                GhostToken::new(|token_x| {
                GhostToken::new(|token_n| {
                    assert_eq!(count_nodes(&index, &token), 1); // 1 for root
                });
            }
        }
    }
}
