use core::panic;
use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
    sync::{Arc, RwLock},
    usize,
};

use crate::utils::{FieldAccessor, Filter, Tokenizer};

/**s
Index data structure.

This data structure is optimized for memory consumption and performant mutations during indexing, so it contains only
basic information.
 * typeparam `T` Document key.
 */
pub struct Index<T> {
    /// Additional information about documents.
    pub docs: HashMap<T, Arc<RwLock<DocumentDetails<T>>>>,
    /// Inverted index root node.
    pub root: Arc<RwLock<InvertedIndexNode<T>>>,
    /// Additional information about indexed fields in all documents.
    pub fields: Vec<FieldDetails>,
}

/**
Creates an Index.
 * typeparam `T` Document key.
 * `fieldsNum` Number of fields.
 * returns `Index`
 */
pub fn create_index<T>(fields_num: usize) -> Index<T> {
    let fields: Vec<FieldDetails> = vec![FieldDetails { sum: 0, avg: 0_f64 }; fields_num];
    Index {
        docs: HashMap::new(),
        root: Arc::new(RwLock::new(create_inverted_index_node(&0))),
        fields,
    }
}
/**
Document Details object stores additional information about documents.
 * typeparam `T` Document key.
 */
#[derive(Clone, Debug, PartialEq)]
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
#[derive(Clone, Debug)]
pub struct DocumentPointer<T> {
    /**
    Next DocumentPointer in the intrusive linked list.
     */
    pub next: Option<Arc<RwLock<DocumentPointer<T>>>>,
    /**
    Reference to a DocumentDetailsobject that is used for this document.
     */
    pub details: Arc<RwLock<DocumentDetails<T>>>,
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
#[derive(Clone, Debug)]
pub struct InvertedIndexNode<T> {
    /**
    Char code is used to store keys in the trie data structure.
     */
    pub char_code: u32,
    /**
    Next InvertedIndexNode in the intrusive linked list.
     */
    pub next: Option<Arc<RwLock<InvertedIndexNode<T>>>>,
    /**
    Linked list of children {@link InvertedIndexNode}.
     */
    pub first_child: Option<Arc<RwLock<InvertedIndexNode<T>>>>,
    /**
    Linked list of documents associated with this node.
     */
    pub first_doc: Option<Arc<RwLock<DocumentPointer<T>>>>,
}

impl<T> PartialEq for InvertedIndexNode<T> {
    fn eq(&self, other: &InvertedIndexNode<T>) -> bool {
        other.char_code == self.char_code
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
fn create_inverted_index_node<T>(char_code: &u32) -> InvertedIndexNode<T> {
    InvertedIndexNode {
        char_code: char_code.to_owned(),
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
pub fn find_inverted_index_node<T>(
    node: Arc<RwLock<InvertedIndexNode<T>>>,
    term: &str,
) -> Option<Arc<RwLock<InvertedIndexNode<T>>>> {
    let chars = term.chars();
    let mut node_iteration = Some(node);
    for char in chars {
        if node_iteration.is_none() {
            break;
        }
        node_iteration = find_inverted_index_node_child_nodes_by_char_code(
            &node_iteration.unwrap(),
            char as u32,
        );
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
fn find_inverted_index_node_child_nodes_by_char_code<T>(
    node: &Arc<RwLock<InvertedIndexNode<T>>>,
    char_code: u32,
) -> Option<Arc<RwLock<InvertedIndexNode<T>>>> {
    let node_unwrapped = node.read().unwrap();
    let child = &node_unwrapped.first_child;
    if child.is_none() {
        return None;
    }
    let mut iter = Arc::clone(node_unwrapped.first_child.as_ref().unwrap());
    loop {
        if iter.read().unwrap().char_code == char_code {
            return Some(Arc::clone(&iter));
        }
        let new_iter = iter.read().unwrap().next.as_ref().map(|c| Arc::clone(&c));
        match new_iter {
            Some(n) => {
                iter = n;
            }
            None => return None,
        }
    }
}

/**
Adds inverted index child node.
 * typeparam `T` Document key.
 * `parent` Parent node.
 * `child` Child node to add.
 */
fn add_inverted_index_child_node<T: Clone>(
    parent: &Arc<RwLock<InvertedIndexNode<T>>>,
    child: Arc<RwLock<InvertedIndexNode<T>>>,
) {
    //-> Arc<RwLock<InvertedIndexNode<T>>>
    if let Some(first) = parent.read().unwrap().first_child.clone() {
        child.write().unwrap().next = Some(first);
    }
    //let c=  parent.clone();
    parent.write().unwrap().first_child = Some(child);
    //Arc::clone(&parent.borrow().first_child.unwrap())
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
    node: &Arc<RwLock<InvertedIndexNode<T>>>,
    mut doc: DocumentPointer<T>,
) {
    if let Some(first) = &node.write().unwrap().first_doc {
        doc.next = Some(Arc::clone(first));
    }
    node.write().unwrap().first_doc = Some(Arc::new(RwLock::new(doc)));
}

/**
Adds a document to the index.
 * typeparam `T` Document key.
 * `node` Inverted index node.
 * `doc` Posting.
*/

pub fn add_document_to_index<T: Eq + Hash + Copy, D>(
    index: &mut Index<T>,
    field_accessors: &[FieldAccessor<D>],
    tokenizer: Tokenizer,
    filter: Filter,
    key: T,
    doc: D,
) {
    let docs = &mut index.docs;
    let fields = &mut index.fields;
    let mut field_length = Vec::new();
    let mut term_counts: HashMap<String, Vec<usize>> = HashMap::new();
    let mut all_terms: Vec<String> = Vec::new();
    for i in 0..fields.len() {
        match field_accessors.get(i).unwrap()(&doc) {
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
                    if term.as_str() != "" {
                        all_terms.push(term.to_owned());
                        filtered_terms_count += 1;
                        let counts = term_counts.get_mut(&term);
                        match counts {
                            None => {
                                let mut new_count = vec![0; fields_len];
                                new_count[i] += 1;
                                term_counts.insert(term, new_count);
                            }
                            _ => {
                                counts.unwrap()[i] += 1;
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

    let details = Arc::new(RwLock::new(DocumentDetails { key, field_length }));

    docs.insert(key, Arc::clone(&details));
    for term in all_terms {
        let mut node = Arc::clone(&index.root);
        for (i, char) in term.chars().enumerate() {
            if node.read().unwrap().first_child.is_none() {
                node = create_inverted_index_nodes(node, &term, &i);
                break;
            }
            let next_node =
                find_inverted_index_node_child_nodes_by_char_code(&Arc::clone(&node), char as u32);
            match next_node {
                None => {
                    node = create_inverted_index_nodes(node, &term, &i);
                    break;
                }
                Some(n) => {
                    node = n;
                }
            }
        }
        add_inverted_index_doc(
            &node,
            DocumentPointer {
                next: None,
                details: Arc::clone(&details),
                term_frequency: term_counts.get(&term).unwrap().to_owned(),
            },
        )
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
fn create_inverted_index_nodes<T: Clone>(
    mut parent: Arc<RwLock<InvertedIndexNode<T>>>,
    term: &str,
    start: &usize,
) -> Arc<RwLock<InvertedIndexNode<T>>> {
    for (i, char) in term.chars().enumerate() {
        if &i < start {
            continue;
        }
        let new_node: InvertedIndexNode<T> = create_inverted_index_node(&(char as u32));
        add_inverted_index_child_node(&parent, Arc::new(RwLock::new(new_node)));
        let new_parent = match &parent.read().unwrap().first_child {
            None => {
                panic!();
            }
            Some(x) => Arc::clone(x),
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
pub fn remove_document_from_index<T: Hash + Eq + Copy>(
    index: &mut Index<T>,
    removed: &mut HashSet<T>,
    key: T,
) {
    //
    let fields = &mut index.fields;
    let doc_details_option = index.docs.get(&key);
    let mut remove_key = false;
    if let Some(doc_details) = doc_details_option {
        removed.insert((&key).to_owned());
        let details = &doc_details.read().unwrap();
        remove_key = true;
        let new_len = (index.docs.len() - 1) as f64;
        for i in 0..fields.len() {
            let field_length = details.field_length.get(i).unwrap();
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
pub fn vacuum_index<T: Hash + Eq>(index: &Index<T>, removed: &mut HashSet<T>) {
    vacuum_node(Arc::clone(&index.root), removed);
    removed.clear();
}

/**
Recursively cleans up removed documents from the index.
 * `T` Document key.
 * `index` Index.
 * `removed` Set of removed document ids.
*/
fn vacuum_node<T: Hash + Eq>(
    node: Arc<RwLock<InvertedIndexNode<T>>>,
    removed: &mut HashSet<T>,
) -> usize {
    let mut prev_pointer: Option<Arc<RwLock<DocumentPointer<T>>>> = None;
    let mut pointer_option = (&node.write().unwrap().first_doc).clone();
    while let Some(pointer) = pointer_option {
        let pb = &pointer.read().unwrap();
        if removed.contains(&pb.details.read().unwrap().key) {
            match &prev_pointer {
                None => {
                    node.write().unwrap().first_doc = pb.next.clone();
                }
                Some(prev) => {
                    prev.write().unwrap().next = pb.next.clone();
                }
            }
        } else {
            prev_pointer = Some((&pointer).clone());
        }
        pointer_option = (&pb.next).clone();
    }

    let mut prev_child: Option<Arc<RwLock<InvertedIndexNode<T>>>> = None;
    let mut ret = 0;
    if node.read().unwrap().first_doc.is_some() {
        ret = 1;
    }

    let mut child_option = (&node.read().unwrap().first_child).clone();
    while let Some(child) = child_option {
        let r = vacuum_node(Arc::clone(&child), removed);
        ret |= r;
        let child_unwrapped = child.read().unwrap();
        if r == 0 {
            // subtree doesn't have any documents, remove this node
            match &prev_child {
                Some(prev) => {
                    prev.write().unwrap().next = child_unwrapped.next.clone();
                }
                None => {
                    node.write().unwrap().first_child = child_unwrapped.next.clone();
                }
            }
        } else {
            prev_child = Some(Arc::clone(&child));
        }
        child_option = (&child_unwrapped.next).clone();
    }
    ret
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
        fn it_should_add_one_document_with_three_terms() {
            let field_accessors: Vec<FieldAccessor<Doc>> =
                vec![field_accessor as fn(doc: &Doc) -> Option<&str>];

            let mut index = create_index::<usize>(1);
            let doc = Doc {
                id: 1,
                text: "a b c".to_string(),
            };
            add_document_to_index(&mut index, &field_accessors, tokenizer, filter, doc.id, doc);
            assert_eq!(index.docs.len(), 1);
            let (_, added_doc) = index.docs.iter().next().unwrap();
            let e = DocumentDetails {
                field_length: vec![3],
                key: 1 as usize,
            };
            assert_eq!(&*added_doc.read().unwrap(), &e);
            assert_eq!(index.fields[0], FieldDetails { avg: 3_f64, sum: 3 });
            let root = &index.root.read().unwrap();
            assert_eq!(&root.char_code, &0);
            assert_eq!(&root.next.is_none(), &true);
            assert_eq!(&root.first_doc.is_none(), &true);

            let first_child = &root.first_child.as_ref().unwrap().read().unwrap();
            assert_eq!(&first_child.char_code, &(99 as u32));
            assert_eq!(&first_child.first_child.is_none(), &true);

            let first_child_next = &first_child.next.as_ref().unwrap().read().unwrap();
            assert_eq!(&first_child_next.char_code, &(98 as u32));
            assert_eq!(&first_child.first_child.is_none(), &true);
            let first_child_first_doc = &first_child.first_doc.as_ref().unwrap().read().unwrap();
            assert_eq!(&first_child_first_doc.term_frequency, &vec![1 as usize]);
            assert_eq!(&*first_child_first_doc.details.read().unwrap(), &e);
            assert_eq!(&first_child_first_doc.next.is_none(), &true);
            assert_eq!(&first_child_first_doc.next.is_none(), &true);

            assert_eq!(
                &first_child_next
                    .next
                    .as_ref()
                    .unwrap()
                    .read()
                    .unwrap()
                    .char_code,
                &(97 as u32)
            );

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
            add_document_to_index(
                &mut index,
                &field_accessors,
                tokenizer,
                filter,
                doc_1.id,
                doc_1.clone(),
            );

            let doc_2 = Doc {
                id: 2,
                text: "b c d".to_string(),
            };
            add_document_to_index(
                &mut index,
                &field_accessors,
                tokenizer,
                filter,
                doc_2.id,
                doc_2.clone(),
            );
            assert_eq!(index.docs.len(), 2);
            assert_eq!(
                &*index.docs.get(&doc_1.id).as_ref().unwrap().read().unwrap(),
                &DocumentDetails {
                    field_length: vec![3],
                    key: 1 as usize
                }
            );
            assert_eq!(
                &*index.docs.get(&doc_2.id).as_ref().unwrap().read().unwrap(),
                &DocumentDetails {
                    field_length: vec![3],
                    key: 2 as usize
                }
            );
            assert_eq!(index.fields[0], FieldDetails { avg: 3_f64, sum: 6 });
            let root = &index.root.read().unwrap();
            assert_eq!(&root.char_code, &0);
            assert_eq!(&root.next.is_none(), &true);
            assert_eq!(&root.first_doc.is_none(), &true);

            let first_child = &root.first_child.as_ref().unwrap().read().unwrap();
            assert_eq!(&first_child.char_code, &(100 as u32));
            assert_eq!(&first_child.first_child.is_none(), &true);

            let first_child_next = &first_child.next.as_ref().unwrap().read().unwrap();
            assert_eq!(&first_child_next.char_code, &(99 as u32));
            assert_eq!(&first_child.first_child.is_none(), &true);
            let nested_next = first_child_next.next.as_ref().unwrap().read().unwrap();
            assert_eq!(&nested_next.char_code, &(98 as u32));
            let nested_nested_next = &nested_next.next.as_ref().unwrap().read().unwrap();
            assert_eq!(&nested_nested_next.char_code, &(97 as u32));

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
            add_document_to_index(
                &mut index,
                &field_accessors,
                tokenizer,
                filter,
                doc_1.id,
                doc_1.clone(),
            );
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
            for doc in docs {
                add_document_to_index(&mut idx, &[field_accessor], tokenizer, filter, doc.id, doc)
            }
            remove_document_from_index(&mut idx, &mut removed, 1);
            vacuum_index(&mut idx, &mut removed);

            assert_eq!(idx.docs.len(), 0);
            assert_eq!(idx.fields.len(), 1);
            assert_eq!(idx.fields.get(0).unwrap().sum, 0);
            assert_eq!(idx.fields.get(0).unwrap().avg.is_nan(), true);

            assert_eq!(
                *idx.root.read().unwrap(),
                InvertedIndexNode {
                    char_code: 0,
                    first_child: None,
                    first_doc: None,
                    next: None
                }
            );
        }
    }
    mod find {
        use super::*;

        fn create(char: u32) -> Arc<RwLock<InvertedIndexNode<String>>> {
            let node: InvertedIndexNode<String> = create_inverted_index_node(&char);
            Arc::new(RwLock::new(node))
        }

        mod char_code {
            use super::*;

            #[test]
            fn it_should_find_undefined_children_if_none() {
                let node = create(0);
                let c = find_inverted_index_node_child_nodes_by_char_code(&node, 0);
                assert_eq!(c.is_none(), true);
            }

            #[test]
            fn it_should_find_existing() {
                let p = create(0);
                let c1 = create(1);
                let c2 = create(2);
                add_inverted_index_child_node(&p, Arc::clone(&c1));
                add_inverted_index_child_node(&p, Arc::clone(&c2));
                assert_eq!(
                    *find_inverted_index_node_child_nodes_by_char_code(&p, 1)
                        .unwrap()
                        .read()
                        .unwrap(),
                    *c1.read().unwrap()
                );
                assert_eq!(
                    *find_inverted_index_node_child_nodes_by_char_code(&p, 2)
                        .unwrap()
                        .read()
                        .unwrap(),
                    *c2.read().unwrap()
                );
            }
        }

        mod term {
            use super::*;
            #[test]
            fn it_should_find() {
                let p = create(0);
                let a = create(97);
                let b = create(98);
                let c = create(99);
                add_inverted_index_child_node(&p, Arc::clone(&a));
                add_inverted_index_child_node(&a, Arc::clone(&b));
                add_inverted_index_child_node(&b, Arc::clone(&c));
                assert_eq!(
                    *find_inverted_index_node(p, &"abc".to_string())
                        .unwrap()
                        .read()
                        .unwrap(),
                    *c.read().unwrap()
                );
            }
        }
    }
}
