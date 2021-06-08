/***
    normalized 0 to 1 bound scoring
*/
use std::{
    cell::{Ref, RefCell},
    collections::HashMap,
    rc::Rc,
};

use crate::{
    index::{DocumentDetails, DocumentPointer, FieldDetails},
    query::score::calculator::{FieldData, ScoreCalculator, TermData},
};

pub struct ZeroToOne {}
pub fn default() -> ZeroToOne {
    ZeroToOne {}
}
pub struct ZeroToOneBeforeCalculations {}
impl<T> ScoreCalculator<T, ZeroToOneBeforeCalculations> for ZeroToOne {
    fn before(
        &self,
        query_term: &str,
        query_term_expanded: &str,
        document_frequency: usize,
        documents: &HashMap<T, Rc<RefCell<DocumentDetails<T>>>>,
    ) -> ZeroToOneBeforeCalculations {
        ZeroToOneBeforeCalculations {}
    }

    fn score(
        &self,
        before_output: &ZeroToOneBeforeCalculations,
        document_pointer: Ref<DocumentPointer<T>>,
        field_data: &FieldData,
        term_data: &TermData,
    ) -> Option<f64> {
        let mut score: f64 = 0_f64;
        for x in 0..field_data.field_lengths.len() {
            let mut tf = (&document_pointer.term_frequency[x]).to_owned() as f64;
            if tf > 0_f64 {
                // special
                let num_of_terms = term_data.all_query_terms.len() as f64;
                let field_length = field_data.field_lengths[x];
                tf = tf / f64::max(field_length as f64, num_of_terms)
                    * (1_f64
                        - f64::abs(term_data.query_term_expanded.len() as f64 - num_of_terms)
                            / (term_data.query_term_expanded.len() as f64));
                score += tf * field_data.fields_boost[x];
            }
        }
        if score > 0_f64 {
            return Some(score);
        }
        return None;
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
            &"a",
            &score::default::bm25::default(),
            tokenizer,
            filter,
            &vec![1., 1.],
            None,
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
            &"c".to_string(),
            &score::default::bm25::default(),
            tokenizer,
            filter,
            &vec![1., 1.],
            None,
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
