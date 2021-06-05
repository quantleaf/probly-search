use std::cell::Ref;

use crate::{
    index::{DocumentPointer, FieldDetails},
    query::score::calculator::ScoreCalculator,
};

pub struct BM25 {
    pub bm25k1: f64,
    pub bm25b: f64,
}
pub fn default() -> BM25 {
    BM25 {
        bm25b: 0.75,
        bm25k1: 1.2,
    }
}
impl<T> ScoreCalculator<T> for BM25 {
    fn score(
        &self,
        document_pointer: Ref<DocumentPointer<T>>,
        idf: f64,
        field_lengths: &[usize],
        fields_boost: &[f64],
        expansion_boost: f64,
        fields: &[FieldDetails],
    ) -> f64 {
        let mut score: f64 = 0_f64;
        for x in 0..field_lengths.len() {
            let mut tf = (&document_pointer.term_frequency[x]).to_owned() as f64;
            if tf > 0_f64 {
                // calculating BM25 tf
                let field_length = &field_lengths[x];
                let field_details = &fields[x];
                let avg_field_length = field_details.avg;
                tf = ((self.bm25k1 + 1_f64) * tf)
                    / (self.bm25k1
                        * ((1_f64 - self.bm25b)
                            + self.bm25b
                                * (field_length.to_owned() as f64 / avg_field_length as f64))
                        + tf);
                score += tf * idf * fields_boost[x] * expansion_boost;
            }
        }
        score
    }
}

#[cfg(test)]
mod tests {

    use crate::{
        index::{add_document_to_index, create_index, Index},
        query::{query, score},
    };

    fn approx_equal(a: f64, b: f64, dp: u8) -> bool {
        let p: f64 = 10f64.powf(-(dp as f64));

        if (a - b).abs() < p {
            return true;
        } else {
            return false;
        }
    }

    struct Doc {
        id: usize,
        title: String,
        text: String,
    }
    fn tokenizer(s: &str) -> Vec<String> {
        s.split(' ')
            .map(|slice| slice.to_owned())
            .collect::<Vec<String>>()
    }
    fn title_extract(d: &Doc) -> Option<&str> {
        Some(d.title.as_str())
    }
    fn text_extract(d: &Doc) -> Option<&str> {
        Some(d.text.as_str())
    }

    fn filter(s: &String) -> String {
        s.to_owned()
    }

    #[test]
    fn it_should_return_doc_1() {
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
            &"a".to_string(),
        );
        assert_eq!(result.len(), 1);
        assert_eq!(
            approx_equal(result.get(0).unwrap().score, 0.6931471805599453, 8),
            true
        );
        assert_eq!(result.get(0).unwrap().key, 1);
    }

    #[test]
    fn it_should_return_doc_1_and_2() {
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
            &"c".to_string(),
        );
        assert_eq!(result.len(), 2);
        assert_eq!(
            approx_equal(result.get(0).unwrap().score, 0.1823215567939546, 8),
            true
        );
        assert_eq!(
            result.get(0).unwrap().key == 1 || result.get(0).unwrap().key == 2,
            true
        );
        assert_eq!(
            approx_equal(result.get(1).unwrap().score, 0.1823215567939546, 8),
            true
        );
        assert_eq!(
            result.get(1).unwrap().key == 1 || result.get(1).unwrap().key == 2,
            true
        );
        assert_ne!(result.get(0).unwrap().key, result.get(1).unwrap().key);
    }
}
