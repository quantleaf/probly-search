use core::panic;
use std::{
    cell::{Ref, RefMut},
    collections::{HashMap, HashSet},
    hash::Hash,
    rc::Rc,
    usize,
};

use ghost_cell::{GhostCell, GhostToken};
use typed_arena::Arena;

use crate::utils::{FieldAccessor, Filter, Tokenizer};

type InvertedIndexNodeRef<'arena, 'id, T> =
    &'arena GhostCell<'id, InvertedIndexNode<'arena, 'id, T>>;

type DocumentPointerRef<'arena, 'id, T> = &'arena GhostCell<'id, DocumentPointer<'arena, 'id, T>>;
/**s
Index data structure.

This data structure is optimized for memory consumption and performant mutations during indexing, so it contains only
basic information.
 * typeparam `T` Document key.
 */
pub struct Index<'arena, 'id, T> {
    /// Additional information about documents.
    pub docs: HashMap<T, Rc<GhostCell<'id, DocumentDetails<T>>>>,
    /// Inverted index root node.
    pub root: GhostCell<'id, InvertedIndexNode<'arena, 'id, T>>,
    /// Additional information about indexed fields in all documents.
    pub fields: Vec<FieldDetails>,
}
unsafe impl<'arena, 'id, T> Send for Index<'arena, 'id, T> {}

/**
Creates an Index.
 * typeparam `T` Document key.
 * `fieldsNum` Number of fields.
 * returns `Index`
 */
pub fn create_index<'arena, 'id, T>(
    fields_num: usize,
    arena: &'arena Arena<InvertedIndexNode<'arena, 'id, T>>,
) -> Index<'arena, 'id, T> {
    let fields: Vec<FieldDetails> = vec![FieldDetails { sum: 0, avg: 0_f64 }; fields_num];
    Index {
        docs: HashMap::new(),
        root: *GhostCell::from_mut(
            arena.alloc(create_inverted_index_node(&char::from_u32(0).unwrap())),
        ),
        fields,
    }
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
pub struct DocumentPointer<'arena, 'id, T> {
    /**
    Next DocumentPointer in the intrusive linked list.
     */
    pub next: Option<DocumentPointerRef<'arena, 'id, T>>,
    /**
    Reference to a DocumentDetailsobject that is used for this document.
     */
    pub details: GhostCell<'id, DocumentDetails<T>>,
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
pub struct InvertedIndexNode<'arena, 'id, T> {
    /**
    Char code is used to store keys in the trie data structure.
     */
    pub char: char,
    /**
    Next InvertedIndexNode in the intrusive linked list.
     */
    pub next: Option<InvertedIndexNodeRef<'arena, 'id, T>>,
    /**
    Linked list of children {@link InvertedIndexNode}.
     */
    pub first_child: Option<InvertedIndexNodeRef<'arena, 'id, T>>,
    /**
    Linked list of documents associated with this node.
     */
    pub first_doc: Option<DocumentPointerRef<'arena, 'id, T>>,
}

impl<'arena, 'id, T> PartialEq for InvertedIndexNode<'arena, 'id, T> {
    fn eq(&self, other: &InvertedIndexNode<T>) -> bool {
        other.char == self.char
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
fn create_inverted_index_node<'arena, 'id, T>(char: &char) -> InvertedIndexNodeRef<'arena, 'id, T> {
    &GhostCell::new(InvertedIndexNode {
        char: char.to_owned(),
        next: None,
        first_child: None,
        first_doc: None,
    })
}

/**
Finds inverted index node that matches the `term`.
 * typeparam `T` Document key.
 * `node` Root node.
 * `term` Term.
returns Inverted index node that contains `term` or an `undefined` value.
 */
pub fn find_inverted_index_node<'arena, 'id, T>(
    node: InvertedIndexNodeRef<'arena, 'id, T>,
    term: &str,
    token: &GhostToken<'id>,
) -> Option<InvertedIndexNodeRef<'arena, 'id, T>> {
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
fn find_inverted_index_node_child_nodes_by_char<'arena, 'id, T>(
    from_node: InvertedIndexNodeRef<'arena, 'id, T>,
    char: &char,
    token: &GhostToken<'id>,
) -> Option<InvertedIndexNodeRef<'arena, 'id, T>> {
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
fn add_inverted_index_child_node<'arena, 'id, T: Clone>(
    parent: InvertedIndexNodeRef<'arena, 'id, T>,
    child: InvertedIndexNodeRef<'arena, 'id, T>,
    token: &mut GhostToken<'id>,
) {
    //-> Rc<GhostCell<'id,InvertedIndexNode<T>>>
    if let Some(first) = parent.borrow(token).first_child {
        child.borrow_mut(token).next = Some(first);
    }
    //let c=  parent.clone();
    parent.borrow_mut(token).first_child = Some(child);
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
fn add_inverted_index_doc<'arena, 'id, T: Clone>(
    node: InvertedIndexNodeRef<'arena, 'id, T>,
    doc: DocumentPointerRef<'arena, 'id, T>,
    token: &mut GhostToken<'id>,
) {
    let mut node_locked = node.get_mut();
    if let Some(first) = &node_locked.first_doc {
        doc.borrow_mut(token).next = Some(first);
    }
    node_locked.first_doc = Some(doc);
}

/**
Adds a document to the index.
 * typeparam `T` Document key.
 * `node` Inverted index node.
 * `doc` Posting.
*/

pub fn add_document_to_index<'id, T: Eq + Hash + Copy, D>(
    index: &mut Index<T>,
    field_accessors: &[FieldAccessor<D>],
    tokenizer: Tokenizer,
    filter: Filter,
    key: T,
    doc: D,
    token: &mut GhostToken<'id>,
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

    docs.insert(
        key,
        Rc::new(GhostCell::new(DocumentDetails { key, field_length })),
    );
    for term in all_terms {
        let mut node = &index.root;

        for (i, char) in term.chars().enumerate() {
            if node.borrow(token).first_child.is_none() {
                node = create_inverted_index_nodes(node, &term, &i, token);
                break;
            }
            let next_node = find_inverted_index_node_child_nodes_by_char(&node, &char, token);
            match next_node {
                None => {
                    node = create_inverted_index_nodes(node, &term, &i, token);
                    break;
                }
                Some(n) => {
                    node = n;
                }
            }
        }
        add_inverted_index_doc(
            node,
            DocumentPointer {
                next: None,
                details: &GhostCell::new(docs.get(&key)),
                term_frequency: term_counts[&term].to_owned(),
            },
            token,
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
fn create_inverted_index_nodes<'arena, 'id, T: Clone>(
    mut parent: InvertedIndexNodeRef<'arena, 'id, T>,
    term: &str,
    start: &usize,
    token: &mut GhostToken<'id>,
) -> InvertedIndexNodeRef<'arena, 'id, T> {
    for (i, char) in term.chars().enumerate() {
        if &i < start {
            continue;
        }
        let new_node: InvertedIndexNode<T> = create_inverted_index_node(&char);
        let new_parent;
        let new_parent = {
            let mut parent_ref = parent.borrow_mut(token);
            add_inverted_index_child_node(&mut parent_ref, new_node, token);
            new_parent = match &parent_ref.first_child {
                None => {
                    panic!();
                }
                Some(x) => Rc::clone(x),
            };
            new_parent
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
        let details = &doc_details.borrow();
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
pub fn vacuum_index<T: Hash + Eq>(index: &Index<T>, removed: &mut HashSet<T>) {
    vacuum_node(Rc::clone(&index.root), removed);
    removed.clear();
}

/**
Recursively cleans up removed documents from the index.
 * `T` Document key.
 * `index` Index.
 * `removed` Set of removed document ids.
*/
fn vacuum_node<'id, T: Hash + Eq>(
    node: Rc<GhostCell<'id, InvertedIndexNode<T>>>,
    removed: &mut HashSet<T>,
) -> usize {
    let mut prev_pointer: Option<Rc<GhostCell<'id, DocumentPointer<T>>>> = None;
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
        pointer_option = (&pb.next).clone();
    }

    let mut prev_child: Option<Rc<GhostCell<'id, InvertedIndexNode<T>>>> = None;
    let mut ret = 0;
    if node.borrow().first_doc.is_some() {
        ret = 1;
    }

    let mut child_option = (&node.borrow().first_child).clone();
    while let Some(child) = child_option {
        let r = vacuum_node(Rc::clone(&child), removed);
        ret |= r;
        let child_unwrapped = child.borrow();
        if r == 0 {
            // subtree doesn't have any documents, remove this node
            match &prev_child {
                Some(prev) => {
                    prev.borrow_mut().next = child_unwrapped.next.clone();
                }
                None => {
                    node.borrow_mut().first_child = child_unwrapped.next.clone();
                }
            }
        } else {
            prev_child = Some(Rc::clone(&child));
        }
        child_option = (&child_unwrapped.next).clone();
    }
    ret
}

/**
    Count the amount of nodes of the index.
    returns the amount, including root node. Which means count will alway be greater than 0
*/
pub fn count_nodes<'id, T>(idx: &Index<T>) -> i32 {
    fn count_nodes_recursively<'id, T>(node: &Rc<GhostCell<'id, InvertedIndexNode<T>>>) -> i32 {
        let mut count = 1;
        let n = node.borrow();
        if let Some(first) = &n.first_child {
            count += count_nodes_recursively(first);
        }
        if let Some(next) = &n.next {
            count += count_nodes_recursively(next);
        }
        count
    }
    count_nodes_recursively(&idx.root)
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
            assert_eq!(&root.char, &char::from_u32(0).unwrap());
            assert_eq!(&root.next.is_none(), &true);
            assert_eq!(&root.first_doc.is_none(), &true);

            let first_child = &root.first_child.as_ref().unwrap().borrow();
            assert_eq!(&first_child.char, &(char::from_u32(99).unwrap()));
            assert_eq!(&first_child.first_child.is_none(), &true);

            let first_child_next = &first_child.next.as_ref().unwrap().borrow();
            assert_eq!(&first_child_next.char, &(char::from_u32(98).unwrap()));
            assert_eq!(&first_child.first_child.is_none(), &true);
            let first_child_first_doc = &first_child.first_doc.as_ref().unwrap().borrow();
            assert_eq!(&first_child_first_doc.term_frequency, &vec![1 as usize]);
            assert_eq!(&*first_child_first_doc.details.borrow(), &e);
            assert_eq!(&first_child_first_doc.next.is_none(), &true);
            assert_eq!(&first_child_first_doc.next.is_none(), &true);

            assert_eq!(
                &first_child_next.next.as_ref().unwrap().borrow().char,
                &(char::from_u32(97).unwrap())
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
            assert_eq!(&root.char, &char::from_u32(0).unwrap());
            assert_eq!(&root.next.is_none(), &true);
            assert_eq!(&root.first_doc.is_none(), &true);

            let first_child = &root.first_child.as_ref().unwrap().borrow();
            assert_eq!(&first_child.char, &char::from_u32(100).unwrap());
            assert_eq!(&first_child.first_child.is_none(), &true);

            let first_child_next = &first_child.next.as_ref().unwrap().borrow();
            assert_eq!(&first_child_next.char, &char::from_u32(99).unwrap());
            assert_eq!(&first_child.first_child.is_none(), &true);
            let nested_next = first_child_next.next.as_ref().unwrap().borrow();
            assert_eq!(&nested_next.char, &char::from_u32(98).unwrap());
            let nested_nested_next = &nested_next.next.as_ref().unwrap().borrow();
            assert_eq!(&nested_nested_next.char, &char::from_u32(97).unwrap());

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
                *idx.root.borrow(),
                InvertedIndexNode {
                    char: char::from_u32(0).unwrap(),
                    first_child: None,
                    first_doc: None,
                    next: None
                }
            );
        }
    }
    mod find {

        use ghost_cell::GhostToken;

        use super::*;

        fn create(
            char: char,
            token: &GhostToken<'id>,
        ) -> Rc<GhostCell<'id, InvertedIndexNode<String>>> {
            let node: InvertedIndexNode<String> = create_inverted_index_node(&char);
            Rc::new(GhostCell::new(node))
        }

        mod char_code {
            use super::*;

            #[test]
            fn it_should_find_undefined_children_if_none() {
                let node = create('x');
                let c = find_inverted_index_node_child_nodes_by_char(&node.borrow(), &'x');
                assert_eq!(c.is_none(), true);
            }

            #[test]
            fn it_should_find_existing() {
                let p = create('x');
                let c1 = create('y');
                let c2 = create('z');
                add_inverted_index_child_node(&mut p.borrow_mut(), Rc::clone(&c1));
                add_inverted_index_child_node(&mut p.borrow_mut(), Rc::clone(&c2));
                assert_eq!(
                    std::ptr::eq(
                        find_inverted_index_node_child_nodes_by_char(&p.borrow(), &'y')
                            .unwrap()
                            .as_ref(),
                        c1.as_ref()
                    ),
                    true
                );
                assert_eq!(
                    std::ptr::eq(
                        find_inverted_index_node_child_nodes_by_char(&p.borrow(), &'z')
                            .unwrap()
                            .as_ref(),
                        c2.as_ref()
                    ),
                    true
                );
            }
        }

        mod term {
            use super::*;
            #[test]
            fn it_should_find() {
                let p = create('x');
                let a = create('a');
                let b = create('b');
                let c = create('c');
                add_inverted_index_child_node(&mut p.borrow_mut(), Rc::clone(&a));
                add_inverted_index_child_node(&mut a.borrow_mut(), Rc::clone(&b));
                add_inverted_index_child_node(&mut b.borrow_mut(), Rc::clone(&c));
                assert_eq!(
                    std::ptr::eq(
                        find_inverted_index_node(p, &"abc").unwrap().as_ref(),
                        c.as_ref()
                    ),
                    true
                );
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
                add_document_to_index(&mut index, &field_accessors, tokenizer, filter, doc.id, doc);
                add_document_to_index(
                    &mut index,
                    &field_accessors,
                    tokenizer,
                    filter,
                    doc_2.id,
                    doc_2,
                );

                assert_eq!(count_nodes(&index), 5); //
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
                add_document_to_index(&mut index, &field_accessors, tokenizer, filter, doc.id, doc);
                add_document_to_index(
                    &mut index,
                    &field_accessors,
                    tokenizer,
                    filter,
                    doc_2.id,
                    doc_2,
                );

                assert_eq!(count_nodes(&index), 7); //
            }

            #[test]
            fn it_should_count_nodes_empty() {
                let index = create_index::<usize>(1);
                assert_eq!(count_nodes(&index), 1); // 1 for root
            }
        }
    }
}
