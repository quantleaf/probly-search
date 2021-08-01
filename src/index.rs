use std::{
    cell::UnsafeCell,
    collections::{HashMap, HashSet},
    fmt::{Debug, Formatter},
    hash::Hash,
    usize,
};

use typed_arena::Arena;

use crate::utils::{FieldAccessor, Filter, Tokenizer};

/**
Index data structure.

This data structure is optimized for memory consumption and performant mutations during indexing, so it contains only
basic information.
 * typeparam `T` Document key.
 */
#[derive(Debug)]

pub struct Index<'arena, T> {
    /// Additional information about documents.
    pub docs: HashMap<T, DocumentDetails<T>>,
    /// Inverted index root node.
    pub root: UnsafeCell<Option<&'arena InvertedIndexNode<'arena, T>>>,
    /// Additional information about indexed fields in all documents.
    pub fields: Vec<FieldDetails>,
}
unsafe impl<'arena, T> Send for Index<'arena, T> {}

pub struct IndexArenas<'arena, T> {
    pub arena_index: Arena<InvertedIndexNode<'arena, T>>,
    pub arena_doc: Arena<DocumentPointer<'arena, T>>,
}

/**
Creates an Index.
 * typeparam `T` Document key.
 * `fieldsNum` Number of fields.
 * returns `Index`
 */
pub fn create_index<'arena, T>(
    fields_num: usize,
    index_arenas: &'arena IndexArenas<'arena, T>,
) -> Index<'arena, T> {
    let fields: Vec<FieldDetails> = vec![FieldDetails { sum: 0, avg: 0_f64 }; fields_num];
    let index = Index {
        docs: HashMap::new(),
        root: UnsafeCell::new(None),
        fields,
    };
    initialize(&index, &index_arenas);
    return index;
}

pub fn create_index_arenas<'arena, T>() -> IndexArenas<'arena, T> {
    IndexArenas {
        arena_doc: Arena::new(),
        arena_index: Arena::new(),
    }
}

pub fn initialize<'arena, T>(
    index: &Index<'arena, T>,
    index_arenas: &'arena IndexArenas<'arena, T>,
) {
    unsafe {
        index.root.get().replace(Some(
            index_arenas
                .arena_index
                .alloc(create_inverted_index_node(&char::from_u32(0).unwrap())),
        ));
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
#[derive(Debug)]

pub struct DocumentPointer<'arena, T> {
    /**
    Next DocumentPointer in the intrusive linked list.
     */
    pub next: UnsafeCell<Option<&'arena DocumentPointer<'arena, T>>>,
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
pub struct InvertedIndexNode<'arena, T> {
    /**
    Char code is used to store keys in the trie data structure.
     */
    pub char: char,
    /**
    Next InvertedIndexNode in the intrusive linked list.
     */
    pub next: UnsafeCell<Option<&'arena InvertedIndexNode<'arena, T>>>,
    /**
    Linked list of children {@link InvertedIndexNode}.
     */
    pub first_child: UnsafeCell<Option<&'arena InvertedIndexNode<'arena, T>>>,
    /**
    Linked list of documents associated with this node.
     */
    pub first_doc: UnsafeCell<Option<&'arena DocumentPointer<'arena, T>>>,
}

impl<'arena, T> PartialEq for InvertedIndexNode<'arena, T> {
    fn eq(&self, other: &InvertedIndexNode<T>) -> bool {
        other.char == self.char
    }
}

impl<'arena, T> Debug for InvertedIndexNode<'arena, T> {
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
fn create_inverted_index_node<'arena, T>(char: &char) -> InvertedIndexNode<'arena, T> {
    InvertedIndexNode {
        char: char.to_owned(),
        next: UnsafeCell::new(None),
        first_child: UnsafeCell::new(None),
        first_doc: UnsafeCell::new(None),
    }
}

/**
Finds inverted index node that matches the `term`.
 * typeparam `T` Document key.
 * `node` Root node.
 * `term` Term.
returns Inverted index node that contains `term` or an `undefined` value.
 */
pub fn find_inverted_index_node<'arena, T>(
    node: &'arena InvertedIndexNode<'arena, T>,
    term: &str,
) -> Option<&'arena InvertedIndexNode<'arena, T>> {
    let mut node_iteration = Some(node);
    for char in term.chars() {
        if let Some(node) = node_iteration {
            node_iteration = find_inverted_index_node_child_nodes_by_char(node, &char);
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
fn find_inverted_index_node_child_nodes_by_char<'arena, T>(
    from_node: &'arena InvertedIndexNode<'arena, T>,
    char: &char,
) -> Option<&'arena InvertedIndexNode<'arena, T>> {
    let child = unsafe { from_node.first_child.get().read() };
    if child.is_none() {
        return None;
    }
    let mut iter = child;
    while let Some(node) = iter {
        if &node.char == char {
            return Some(node);
        }
        iter = unsafe { node.next.get().read() }
    }
    return None;
}

/**
Adds inverted index child node.
 * typeparam `T` Document key.
 * `parent` Parent node.
 * `child` Child node to add.
 */
fn add_inverted_index_child_node<'arena, T: Clone>(
    parent: &InvertedIndexNode<'arena, T>,
    child: &'arena InvertedIndexNode<'arena, T>,
) {
    //-> Rc<GhostCell<InvertedIndexNode<T>>>
    if let Some(first) = unsafe { parent.first_child.get().read() } {
        unsafe { child.next.get().replace(Some(first)) };
    }

    unsafe { parent.first_child.get().replace(Some(child)) };
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
fn add_inverted_index_doc<'arena, T: Clone>(
    node: &InvertedIndexNode<'arena, T>,
    doc: &'arena DocumentPointer<'arena, T>,
) {
    let node_value = node;
    if let Some(first) = unsafe { node_value.first_doc.get().read() } {
        unsafe { doc.next.get().replace(Some(first)) };
    }
    unsafe { node_value.first_doc.get().replace(Some(doc)) };
}

/**
Adds a document to the index.
 * typeparam `T` Document key.
 * `node` Inverted index node.
 * `doc` Posting.
*/

pub fn add_document_to_index<'arena, T: Eq + Hash + Copy, D>(
    index: &mut Index<'arena, T>,
    arena_index: &'arena Arena<InvertedIndexNode<'arena, T>>,
    arena_doc: &'arena Arena<DocumentPointer<'arena, T>>,
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
        let mut node = index.root.get_mut().unwrap();
        for (i, char) in term.chars().enumerate() {
            if unsafe { node.first_child.get().read() }.is_none() {
                node = create_inverted_index_nodes(arena_index, node, &term, &i);
                break;
            }
            let next_node = find_inverted_index_node_child_nodes_by_char(node, &char);
            match next_node {
                None => {
                    node = create_inverted_index_nodes(arena_index, node, &term, &i);
                    break;
                }
                Some(n) => {
                    node = n;
                }
            }
        }
        let new_alloc = arena_doc.alloc(DocumentPointer {
            next: UnsafeCell::new(None),
            details_key: key.to_owned(),
            term_frequency: term_counts[&term].to_owned(),
        });
        add_inverted_index_doc(node, new_alloc)
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
fn create_inverted_index_nodes<'arena, T: Clone>(
    arena_index: &'arena Arena<InvertedIndexNode<'arena, T>>,
    mut parent: &'arena InvertedIndexNode<'arena, T>,
    term: &str,
    start: &usize,
) -> &'arena InvertedIndexNode<'arena, T> {
    for (i, char) in term.chars().enumerate() {
        if &i < start {
            continue;
        }
        let new_node = arena_index.alloc(create_inverted_index_node(&char));
        let new_parent = {
            add_inverted_index_child_node(parent, new_node); // unsafe { .get().as_mut().unwrap() }
            unsafe { parent.first_child.get().read() }
        };
        parent = new_parent.unwrap();
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
pub fn vacuum_index<'arena, T: Hash + Eq>(index: &Index<'arena, T>, removed: &mut HashSet<T>) {
    vacuum_node(index, unsafe { index.root.get().read() }.unwrap(), removed);
    removed.clear();
}

/**
Recursively cleans up removed documents from the index.
 * `T` Document key.
 * `index` Index.
 * `removed` Set of removed document ids.
*/
fn vacuum_node<'arena, T: Hash + Eq>(
    index: &Index<'arena, T>,
    node: &'arena InvertedIndexNode<'arena, T>,
    removed: &mut HashSet<T>,
) -> usize {
    let mut prev_pointer: Option<&'arena DocumentPointer<'arena, T>> = None;
    let mut pointer_option = unsafe { node.first_doc.get().read() };
    while let Some(pointer) = pointer_option {
        let pb = pointer;
        if removed.contains(&pb.details_key) {
            unsafe {
                match &prev_pointer {
                    None => {
                        node.first_doc.get().replace(pb.next.get().read());
                    }
                    Some(prev) => {
                        prev.next.get().replace(pb.next.get().read());
                    }
                }
            }
        } else {
            prev_pointer = Some(pointer);
        }
        pointer_option = unsafe { pb.next.get().read() };
    }

    let mut prev_child: Option<&'arena InvertedIndexNode<'arena, T>> = None;
    let mut ret = 0;
    if unsafe { node.first_doc.get().read() }.is_some() {
        ret = 1;
    }

    let mut child_option = unsafe { node.first_child.get().read() };
    while let Some(child) = child_option {
        let r = vacuum_node(index, child, removed);
        ret |= r;
        if r == 0 {
            // subtree doesn't have any documents, remove this node
            unsafe {
                match &prev_child {
                    Some(prev) => {
                        prev.next.get().replace(child.next.get().read());
                    }
                    None => {
                        node.first_child.get().replace(child.next.get().read());
                    }
                }
            }
        } else {
            prev_child = Some(child);
        }
        child_option = unsafe { child.next.get().read() };
    }
    ret
}

/**
    Count the amount of nodes of the index.
    returns the amount, including root node. Which means count will alway be greater than 0
*/
pub fn count_nodes<'arena, T>(idx: &Index<'arena, T>) -> i32 {
    fn count_nodes_recursively<'arena, T>(node: &'arena InvertedIndexNode<'arena, T>) -> i32 {
        let mut count = 1;
        if let Some(first) = unsafe { node.first_child.get().read() } {
            count += count_nodes_recursively(first);
        }
        if let Some(next) = unsafe { node.next.get().read() } {
            count += count_nodes_recursively(next);
        }
        count
    }
    count_nodes_recursively(unsafe { idx.root.get().read() }.unwrap())
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
        fn it_should_add_one_document_with_three_terms<'idn>() {
            let field_accessors: Vec<FieldAccessor<Doc>> =
                vec![field_accessor as fn(doc: &Doc) -> Option<&str>];
            let index_arenas = create_index_arenas();
            let mut index = create_index::<usize>(1, &index_arenas);
            index.root = UnsafeCell::new(Some(index_arenas.arena_index.alloc(InvertedIndexNode {
                char: char::from_u32(0).unwrap(),
                first_child: UnsafeCell::new(None),
                first_doc: UnsafeCell::new(None),
                next: UnsafeCell::new(None),
            })));

            let doc = Doc {
                id: 1,
                text: "a b c".to_string(),
            };

            add_document_to_index(
                &mut index,
                &index_arenas.arena_index,
                &index_arenas.arena_doc,
                &field_accessors,
                tokenizer,
                filter,
                doc.id,
                doc,
            );

            unsafe {
                assert_eq!(index.docs.len(), 1);
                let (_, added_doc) = index.docs.iter().next().unwrap();
                let e = DocumentDetails {
                    field_length: vec![3],
                    key: 1 as usize,
                };
                assert_eq!(&*added_doc, &e);
                assert_eq!(index.fields[0], FieldDetails { avg: 3_f64, sum: 3 });
                let root = &index.root.get().read().unwrap();
                assert_eq!(&root.char, &char::from_u32(0).unwrap());
                assert_eq!(&root.next.get().read().is_none(), &true);
                assert_eq!(&root.first_doc.get().read().is_none(), &true);

                let first_child = &root.first_child.get().read().unwrap();
                assert_eq!(&first_child.char, &(char::from_u32(99).unwrap()));
                assert_eq!(&first_child.first_child.get().read().is_none(), &true);

                let first_child_next = &first_child.next.get().read().unwrap();
                assert_eq!(&first_child_next.char, &(char::from_u32(98).unwrap()));
                assert_eq!(&first_child.first_child.get().read().is_none(), &true);
                let first_child_first_doc = first_child.first_doc.get().read().unwrap();
                assert_eq!(&first_child_first_doc.term_frequency, &vec![1 as usize]);
                assert_eq!(&first_child_first_doc.details_key, &e.key);
                assert_eq!(&first_child_first_doc.next.get().read().is_none(), &true);
                assert_eq!(&first_child_first_doc.next.get().read().is_none(), &true);

                assert_eq!(
                    &first_child_next.next.get().read().as_ref().unwrap().char,
                    &(char::from_u32(97).unwrap())
                );
            }

            // TODO test more properties
        }

        #[test]
        fn it_should_add_shared_terms() {
            let field_accessors: Vec<FieldAccessor<Doc>> =
                vec![field_accessor as fn(doc: &Doc) -> Option<&str>];

            let index_arenas = create_index_arenas();
            let mut index = create_index::<usize>(1, &index_arenas);
            let doc_1 = Doc {
                id: 1,
                text: "a b c".to_string(),
            };
            let doc_2 = Doc {
                id: 2,
                text: "b c d".to_string(),
            };

            add_document_to_index(
                &mut index,
                &index_arenas.arena_index,
                &index_arenas.arena_doc,
                &field_accessors,
                tokenizer,
                filter,
                doc_1.id,
                doc_1.clone(),
            );

            add_document_to_index(
                &mut index,
                &index_arenas.arena_index,
                &index_arenas.arena_doc,
                &field_accessors,
                tokenizer,
                filter,
                doc_2.id,
                doc_2.clone(),
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

            unsafe {
                let root = &index.root.get().read().unwrap();

                assert_eq!(&root.char, &char::from_u32(0).unwrap());
                assert_eq!(&root.next.get().read().is_none(), &true);
                assert_eq!(&root.first_doc.get().read().is_none(), &true);

                let first_child = &root.first_child.get().read().unwrap();
                assert_eq!(&first_child.char, &char::from_u32(100).unwrap());
                assert_eq!(&first_child.first_child.get().read().is_none(), &true);

                let first_child_next = &first_child.next.get().read().unwrap();
                assert_eq!(&first_child_next.char, &char::from_u32(99).unwrap());
                assert_eq!(&first_child.first_child.get().read().is_none(), &true);
                let nested_next = first_child_next.next.get().read().unwrap();
                assert_eq!(&nested_next.char, &char::from_u32(98).unwrap());
                let nested_nested_next = &nested_next.next.get().read().unwrap();
                assert_eq!(&nested_nested_next.char, &char::from_u32(97).unwrap());
            }

            // dont test all the properties
        }

        #[test]
        fn it_should_ignore_empty_tokens() {
            let field_accessors: Vec<FieldAccessor<Doc>> =
                vec![field_accessor as fn(doc: &Doc) -> Option<&str>];

            let index_arenas = create_index_arenas();
            let mut index = create_index::<usize>(1, &index_arenas);
            let doc_1 = Doc {
                id: 1,
                text: "a  b".to_string(), // double space could introduce empty tokens
            };

            add_document_to_index(
                &mut index,
                &index_arenas.arena_index,
                &index_arenas.arena_doc,
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
            let index_arenas = create_index_arenas();
            let mut index = create_index::<usize>(1, &index_arenas);
            let mut removed = HashSet::new();
            let docs = vec![Doc {
                id: 1,
                text: "a".to_string(),
            }];

            for doc in docs {
                add_document_to_index(
                    &mut index,
                    &index_arenas.arena_index,
                    &index_arenas.arena_doc,
                    &[field_accessor],
                    tokenizer,
                    filter,
                    doc.id,
                    doc,
                )
            }
            remove_document_from_index(&mut index, &mut removed, 1);
            vacuum_index(&mut index, &mut removed);

            assert_eq!(index.docs.len(), 0);
            assert_eq!(index.fields.len(), 1);
            assert_eq!(index.fields.get(0).unwrap().sum, 0);
            assert_eq!(index.fields.get(0).unwrap().avg.is_nan(), true);

            let mut x = unsafe { index.root.get().read().unwrap() };
            let y: &InvertedIndexNode<usize> = &InvertedIndexNode {
                char: char::from_u32(0).unwrap(),
                first_child: UnsafeCell::new(None),
                first_doc: UnsafeCell::new(None),
                next: UnsafeCell::new(None),
            };
            assert_eq!(x, y);
        }
    }
    mod find {

        use super::*;

        fn create<'arena, 'idn>(
            arena_index: &'arena Arena<InvertedIndexNode<'arena, usize>>,
            char: char,
        ) -> &'arena InvertedIndexNode<'arena, usize> {
            let node: InvertedIndexNode<'arena, usize> = create_inverted_index_node(&char);
            arena_index.alloc(node)
        }

        mod char_code {
            use super::*;

            #[test]
            fn it_should_find_undefined_children_if_none() {
                let index_arenas = create_index_arenas();
                let mut index = create_index::<usize>(1, &index_arenas);
                let node = create(&index_arenas.arena_index, 'x');
                let c = find_inverted_index_node_child_nodes_by_char(node, &'x');
                assert_eq!(c.is_none(), true);
            }

            #[test]
            fn it_should_find_existing() {
                let index_arenas = create_index_arenas();
                let index = create_index::<usize>(1, &index_arenas);
                let p = create(&index_arenas.arena_index, 'x');
                let c1 = create(&index_arenas.arena_index, 'y');
                let c2 = create(&index_arenas.arena_index, 'z');
                add_inverted_index_child_node(p, c1);
                add_inverted_index_child_node(p, c2);
                assert_eq!(
                    std::ptr::eq(
                        find_inverted_index_node_child_nodes_by_char(p, &'y').unwrap(),
                        c1
                    ),
                    true
                );
                assert_eq!(
                    std::ptr::eq(
                        find_inverted_index_node_child_nodes_by_char(p, &'z').unwrap(),
                        c2
                    ),
                    true
                )
            }
        }

        mod term {
            use super::*;
            #[test]
            fn it_should_find() {
                let index_arenas = create_index_arenas();
                let mut index = create_index::<usize>(1, &index_arenas);
                let p = create(&index_arenas.arena_index, 'x');
                let a = create(&index_arenas.arena_index, 'a');
                let b = create(&index_arenas.arena_index, 'b');
                let c = create(&index_arenas.arena_index, 'c');
                add_inverted_index_child_node(p, a);
                add_inverted_index_child_node(a, b);
                add_inverted_index_child_node(b, c);
                assert_eq!(
                    std::ptr::eq(find_inverted_index_node(p, &"abc").unwrap(), c),
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

                let index_arenas = create_index_arenas();
                let mut index = create_index::<usize>(1, &index_arenas);
                let doc = Doc {
                    id: 1,
                    text: "abc".to_string(),
                };
                let doc_2 = Doc {
                    id: 1,
                    text: "abe".to_string(),
                };

                add_document_to_index(
                    &mut index,
                    &index_arenas.arena_index,
                    &index_arenas.arena_doc,
                    &field_accessors,
                    tokenizer,
                    filter,
                    doc.id,
                    doc,
                );
                add_document_to_index(
                    &mut index,
                    &index_arenas.arena_index,
                    &index_arenas.arena_doc,
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

                let index_arenas = create_index_arenas();
                let mut index = create_index::<usize>(1, &index_arenas);

                let doc = Doc {
                    id: 1,
                    text: "ab cd".to_string(),
                };
                let doc_2 = Doc {
                    id: 1,
                    text: "ab ef".to_string(),
                };

                add_document_to_index(
                    &mut index,
                    &index_arenas.arena_index,
                    &index_arenas.arena_doc,
                    &field_accessors,
                    tokenizer,
                    filter,
                    doc.id,
                    doc,
                );
                add_document_to_index(
                    &mut index,
                    &index_arenas.arena_index,
                    &index_arenas.arena_doc,
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
                let index_arenas = create_index_arenas();
                let mut index = create_index::<usize>(1, &index_arenas);
                assert_eq!(count_nodes(&index), 1); // 1 for root
            }
        }
    }
}
