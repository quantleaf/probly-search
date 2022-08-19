mod index;
pub mod score;

pub use index::*;

/// Type for functions that extract a field value from a document.
pub type FieldAccessor<D> = fn(&D) -> Option<&str>;

/// Type used to tokenize a field.
pub type Tokenizer = fn(&str) -> Vec<&str>;

/// Type used to filter fields.
pub type Filter = fn(&str) -> &str;

#[cfg(test)]
mod query;

#[cfg(test)]
pub mod test_util {

    use crate::{
        index::{Index, QueryResult},
        score::ScoreCalculator,
    };

    fn approx_equal(a: f64, b: f64, dp: u8) -> bool {
        let p: f64 = 10f64.powf(-(dp as f64));

        (a - b).abs() < p
    }

    pub struct Doc {
        pub id: usize,
        pub title: String,
        pub text: String,
    }

    pub fn title_extract(d: &Doc) -> Option<&str> {
        Some(d.title.as_str())
    }

    pub fn text_extract(d: &Doc) -> Option<&str> {
        Some(d.text.as_str())
    }

    pub fn tokenizer(s: &str) -> Vec<&str> {
        s.split(' ').collect::<Vec<_>>()
    }

    pub fn filter(s: &str) -> &str {
        s
    }

    pub fn test_score<'arena, M, S: ScoreCalculator<usize, M>>(
        idx: &mut Index<usize>,
        score_calculator: &mut S,
        q: &str,
        expected: Vec<QueryResult<usize>>,
    ) {
        let fields_len = idx.fields.len();
        let mut results = idx.query(
            q,
            score_calculator,
            tokenizer,
            filter,
            &vec![1.; fields_len],
            None,
        );
        results.sort_by(|a, b| {
            let mut sort = b.score.partial_cmp(&a.score).unwrap();
            sort = sort.then_with(|| a.key.partial_cmp(&b.key).unwrap());
            sort
        });

        assert_eq!(expected.len(), results.len());

        for (index, result) in results.iter().enumerate() {
            assert_eq!(expected[index], *result);
            assert_eq!(approx_equal(expected[index].score, result.score, 8), true)
        }
    }

    /***
        Create a index with docucments with title fields, with increasing ids starting from 0
    */

    pub fn build_test_index<'arena>(titles: &[&str]) -> Index<usize> {
        let mut index = Index::<usize>::new(1);
        for (i, title) in titles.iter().enumerate() {
            let doc = Doc {
                id: i,
                title: title.to_string(),
                text: String::new(),
            };
            index.add_document(&[title_extract], tokenizer, filter, doc.id, &doc);
        }
        index
    }
}
