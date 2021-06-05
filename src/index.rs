use core::panic;
use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    hash::Hash,
    rc::Rc,
    usize,
};

use crate::utils::{FieldAccessor, Filter, Tokenizer};


/**
Index data structure.

This data structure is optimized for memory consumption and performant mutations during indexing, so it contains only
basic information.

 * @typeparam T Document key.
 */
pub struct Index<T> {
    /// Additional information about documents.
    pub docs: HashMap<T, Rc<RefCell<DocumentDetails<T>>>>,
    /// Inverted index root node.
    pub root: Rc<RefCell<InvertedIndexNode<T>>>,
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
        root: Rc::new(RefCell::new(create_inverted_index_node(&0))),
        fields: fields,
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
#[derive(Clone, Debug, PartialEq)]
pub struct DocumentPointer<T> {
    /**
    Next DocumentPointer in the intrusive linked list.
     */
    pub next: Option<Rc<RefCell<DocumentPointer<T>>>>,
    /**
    Reference to a DocumentDetailsobject that is used for this document.
     */
    pub details: Rc<RefCell<DocumentDetails<T>>>,
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
#[derive(Clone, Debug, PartialEq)]
pub struct InvertedIndexNode<T> {
    /**
    Char code is used to store keys in the trie data structure.
     */
    pub char_code: u32,
    /**
    Next InvertedIndexNode in the intrusive linked list.
     */
    pub next: Option<Rc<RefCell<InvertedIndexNode<T>>>>,
    /**
    Linked list of children {@link InvertedIndexNode}.
     */
    pub first_child: Option<Rc<RefCell<InvertedIndexNode<T>>>>,
    /**
    Linked list of documents associated with this node.
     */
    pub first_doc: Option<Rc<RefCell<DocumentPointer<T>>>>,
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
    node: Rc<RefCell<InvertedIndexNode<T>>>,
    term: &String,
) -> Option<Rc<RefCell<InvertedIndexNode<T>>>> {
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
    return node_iteration;
}

/**
Finds inverted index child node with matching `char`.

* typeparam `T` Document key.
 * `node` InvertedIndexNode.
 * `charCode` Char code.
returns Matching InvertedIndexNode or `undefined`.
 */
fn find_inverted_index_node_child_nodes_by_char_code<T>(
    node: &Rc<RefCell<InvertedIndexNode<T>>>,
    char_code: u32,
) -> Option<Rc<RefCell<InvertedIndexNode<T>>>> {
    let child = &node.borrow().first_child;
    if child.is_none() {
        return None;
    }
    let mut iter = Rc::clone(node.borrow().first_child.as_ref().unwrap());
    loop {
        if iter.borrow().char_code == char_code {
            return Some(Rc::clone(&iter));
        }
        let new_iter = match iter.borrow().next.as_ref() {
            Some(c) => Some(Rc::clone(&c)),
            None => None,
        };
        if new_iter.is_none() {
            return None;
        }
        iter = new_iter.unwrap();
    }
}

/**
 * Adds inverted index child node.
 *
 * typeparam `T` Document key.
 * `parent` Parent node.
 * `child` Child node to add.
 */
fn add_inverted_index_child_node<T: Clone>(
    parent: &Rc<RefCell<InvertedIndexNode<T>>>,
    child: Rc<RefCell<InvertedIndexNode<T>>>,
) {
    //-> Rc<RefCell<InvertedIndexNode<T>>>
    if let Some(first) = parent.borrow().first_child.clone() {
        child.borrow_mut().next = Some(first);
    }
    //let c=  parent.clone();
    parent.borrow_mut().first_child = Some(child);
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
fn add_inverted_index_doc<T: Clone>(
    node: &Rc<RefCell<InvertedIndexNode<T>>>,
    mut doc: DocumentPointer<T>,
) {
    if let Some(first) = &node.borrow().first_doc {
        doc.next = Some(Rc::clone(first));
    }
    node.borrow_mut().first_doc = Some(Rc::new(RefCell::new(doc)));
}

/**
Adds a document to the index.
* typeparam `T` Document key.
* `node` Inverted index node.
* `doc` Posting.
*/

pub fn add_document_to_index<T: Eq + Hash + Copy, D>(
    index: &mut Index<T>,
    field_accessors: &Vec<FieldAccessor<D>>,
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
                for j in 0..terms.len() {
                    let term = filter(&terms[j]);
                    all_terms.push(term.to_owned());
                    if term.as_str() != "" {
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

    let details = Rc::new(RefCell::new(DocumentDetails {
        key: key,
        field_length: field_length,
    }));

    docs.insert(key, Rc::clone(&details));
    for term in all_terms {
        let mut node = Rc::clone(&index.root);
        for (i, char) in term.chars().enumerate() {
            if node.borrow().first_child.is_none() {
                node = create_inverted_index_nodes(node, &term, &i);
                break;
            }
            let next_node =
                find_inverted_index_node_child_nodes_by_char_code(&Rc::clone(&node), char as u32);
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
                details: Rc::clone(&details),
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
fn create_inverted_index_nodes<'a, T: Clone>(
    mut parent: Rc<RefCell<InvertedIndexNode<T>>>,
    term: &String,
    start: &usize,
) -> Rc<RefCell<InvertedIndexNode<T>>> {
    for (i, char) in term.chars().enumerate() {
        if &i < start {
            continue;
        }
        let new_node: InvertedIndexNode<T> = create_inverted_index_node(&(char as u32));
        add_inverted_index_child_node(&parent, Rc::new(RefCell::new(new_node)));
        let new_parent = match &parent.borrow().first_child {
            None => {
                panic!();
            }
            Some(x) => Rc::clone(x),
        };
        parent = new_parent;
    }
    return parent;
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
        let details = &doc_details.borrow();
        remove_key = true;
        let new_len = (index.docs.len() - 1) as f64;
        for i in 0..fields.len() {
            let field_length = details.field_length.get(i).unwrap();
            if field_length > &&0 {
                let field = fields.get_mut(i);
                match field {
                    Some(f) => {
                        f.sum -= *field_length;
                        f.avg = f.sum as f64 / new_len;
                    }
                    None => {}
                }
            }
        }
    }

    if remove_key {
        index.docs.remove(&key);
    }
}

/**
* Cleans up removed documents from the {@link Index}.

*/
pub fn vacuum_index<T: Hash + Eq>(index: &Index<T>, removed: &mut HashSet<T>) {
    vacuum_node(Rc::clone(&index.root), removed);
    removed.clear();
}

/**
 * Recursively cleans up removed documents from the index.

 * `T` Document key.
 * `index` Index.
 * `removed` Set of removed document ids.
 */
fn vacuum_node<T: Hash + Eq>(
    node: Rc<RefCell<InvertedIndexNode<T>>>,
    removed: &mut HashSet<T>,
) -> usize {
    let mut prev_pointer: Option<Rc<RefCell<DocumentPointer<T>>>> = None;
    let mut pointer_option = (&node.borrow().first_doc).clone();
    while let Some(pointer) = pointer_option {
        let pb = &pointer.borrow();
        if removed.contains(&pb.details.borrow().key) {
            match &prev_pointer {
                None => {
                    node.borrow_mut().first_doc = pb.next.clone();
                }
                Some(prev) => {
                    prev.borrow_mut().next = pb.next.clone();
                }
            }
        } else {
            prev_pointer = Some((&pointer).clone());
        }
        pointer_option = (&pointer.borrow().next).clone();
    }

    let mut prev_child: Option<Rc<RefCell<InvertedIndexNode<T>>>> = None;
    let mut ret = 0;
    if node.borrow().first_doc.is_some() {
        ret = 1;
    }

    let mut child_option = (&node.borrow().first_child).clone();
    while let Some(child) = child_option {
        let r = vacuum_node(Rc::clone(&child), removed);
        ret |= r;
        if r == 0 {
            // subtree doesn't have any documents, remove this node
            match &prev_child {
                Some(prev) => {
                    prev.borrow_mut().next = child.borrow().next.clone();
                }
                None => {
                    node.borrow_mut().first_child = child.borrow().next.clone();
                }
            }
        } else {
            prev_child = Some(Rc::clone(&child));
        }
        child_option = (&child.borrow().next).clone();
    }
    return ret;
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
            assert_eq!(&*added_doc.borrow(), &e);
            assert_eq!(index.fields[0], FieldDetails { avg: 3_f64, sum: 3 });
            let root = &index.root.borrow();
            assert_eq!(&root.char_code, &0);
            assert_eq!(&root.next.is_none(), &true);
            assert_eq!(&root.first_doc.is_none(), &true);

            let first_child = &root.first_child.as_ref().unwrap().borrow();
            assert_eq!(&first_child.char_code, &(99 as u32));
            assert_eq!(&first_child.first_child.is_none(), &true);

            let first_child_next = &first_child.next.as_ref().unwrap().borrow();
            assert_eq!(&first_child_next.char_code, &(98 as u32));
            assert_eq!(&first_child.first_child.is_none(), &true);
            let first_child_first_doc = &first_child.first_doc.as_ref().unwrap().borrow();
            assert_eq!(&first_child_first_doc.term_frequency, &vec![1 as usize]);
            assert_eq!(&*first_child_first_doc.details.borrow(), &e);
            assert_eq!(&first_child_first_doc.next.is_none(), &true);
            assert_eq!(&first_child_first_doc.next.is_none(), &true);

            assert_eq!(
                &first_child_next.next.as_ref().unwrap().borrow().char_code,
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
                &*index.docs.get(&doc_1.id).as_ref().unwrap().borrow(),
                &DocumentDetails {
                    field_length: vec![3],
                    key: 1 as usize
                }
            );
            assert_eq!(
                &*index.docs.get(&doc_2.id).as_ref().unwrap().borrow(),
                &DocumentDetails {
                    field_length: vec![3],
                    key: 2 as usize
                }
            );
            assert_eq!(index.fields[0], FieldDetails { avg: 3_f64, sum: 6 });
            let root = &index.root.borrow();
            assert_eq!(&root.char_code, &0);
            assert_eq!(&root.next.is_none(), &true);
            assert_eq!(&root.first_doc.is_none(), &true);

            let first_child = &root.first_child.as_ref().unwrap().borrow();
            assert_eq!(&first_child.char_code, &(100 as u32));
            assert_eq!(&first_child.first_child.is_none(), &true);

            let first_child_next = &first_child.next.as_ref().unwrap().borrow();
            assert_eq!(&first_child_next.char_code, &(99 as u32));
            assert_eq!(&first_child.first_child.is_none(), &true);
            let nested_next = first_child_next.next.as_ref().unwrap().borrow();
            assert_eq!(&nested_next.char_code, &(98 as u32));
            let nested_nested_next = &nested_next.next.as_ref().unwrap().borrow();
            assert_eq!(&nested_nested_next.char_code, &(97 as u32));

            // dont test all the properties
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
                add_document_to_index(
                    &mut idx,
                    &vec![field_accessor],
                    tokenizer,
                    filter,
                    doc.id,
                    doc,
                )
            }
            remove_document_from_index(&mut idx, &mut removed, 1);
            vacuum_index(&mut idx, &mut removed);
            assert_eq!(idx.docs.len(), 0);
            assert_eq!(idx.fields.len(), 1);
            assert_eq!(idx.fields.get(0).unwrap().sum, 0);
            assert_eq!(idx.fields.get(0).unwrap().avg.is_nan(), true);

            assert_eq!(
                *idx.root.borrow(),
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

        fn create(char: u32) -> Rc<RefCell<InvertedIndexNode<String>>> {
            let node: InvertedIndexNode<String> = create_inverted_index_node(&char);
            Rc::new(RefCell::new(node))
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
                add_inverted_index_child_node(&p, Rc::clone(&c1));
                add_inverted_index_child_node(&p, Rc::clone(&c2));
                assert_eq!(
                    *find_inverted_index_node_child_nodes_by_char_code(&p, 1)
                        .unwrap()
                        .borrow(),
                    *c1.borrow()
                );
                assert_eq!(
                    *find_inverted_index_node_child_nodes_by_char_code(&p, 2)
                        .unwrap()
                        .borrow(),
                    *c2.borrow()
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
                add_inverted_index_child_node(&p, Rc::clone(&a));
                add_inverted_index_child_node(&a, Rc::clone(&b));
                add_inverted_index_child_node(&b, Rc::clone(&c));
                assert_eq!(
                    *find_inverted_index_node(p, &"abc".to_string())
                        .unwrap()
                        .borrow(),
                    *c.borrow()
                );
            }
        }
    }
}
