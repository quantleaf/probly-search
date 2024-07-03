use probly_search::{score::bm25, Index};
use std::borrow::Cow;

#[test]
fn should_not_panic_when_document_frequency_gt_documents_len() {
    struct Doc {
        id: usize,
        content: String,
    }

    // A white space tokenizer
    fn tokenizer(s: &str) -> Vec<Cow<str>> {
        s.split(' ').map(Cow::from).collect::<Vec<_>>()
    }

    fn content_extract(d: &Doc) -> Vec<&str> {
        vec![&d.content]
    }

    let mut index = Index::<usize>::new(1);
    let doc = Doc {
        id: 0,
        content: "this is text with lots of the, the, the, the".to_owned(),
    };
    index.add_document(&[content_extract], tokenizer, doc.id, &doc);
    index.query(
        &"What did the author do growing up?",
        &mut bm25::new(),
        tokenizer,
        &[1.],
    );
}
