pub mod index;
pub mod query;
pub mod utils;

#[cfg(test)]
pub mod test_util {

    use crate::{
        index::{add_document_to_index, initialize, FieldDetails, Index, IndexArenas},
        query::{query, score::calculator::ScoreCalculator, QueryResult},
    };
    use std::{cell::UnsafeCell, collections::HashMap};
    fn approx_equal(a: f64, b: f64, dp: u8) -> bool {
        let p: f64 = 10f64.powf(-(dp as f64));

        (a - b).abs() < p
    }

    struct Doc {
        id: usize,
        title: String,
    }
    fn tokenizer(s: &str) -> Vec<String> {
        s.split(' ')
            .map(|slice| slice.to_owned())
            .collect::<Vec<String>>()
    }
    fn title_extract(d: &Doc) -> Option<&str> {
        Some(d.title.as_str())
    }

    fn filter(s: &str) -> String {
        s.to_owned()
    }

    pub fn test_score<'arena, M, S: ScoreCalculator<usize, M>>(
        mut idx: &mut Index<'arena, usize>,
        score_calculator: &mut S,
        q: &str,
        expected: Vec<QueryResult<usize>>,
    ) {
        let fields_len = idx.fields.len();
        let mut results = query(
            &mut idx,
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

        for (index, result) in results.iter().enumerate() {
            assert_eq!(expected[index], *result);
            assert_eq!(approx_equal(expected[index].score, result.score, 8), true)
        }
    }

    /***
        Create a index with docucments with title fields, with increasing ids starting from 0
    */

    pub fn build_test_index<'arena>(
        titles: &[&str],
        index_arenas: &'arena IndexArenas<'arena, usize>,
    ) -> Index<'arena, usize> {
        let fields: Vec<FieldDetails> = vec![FieldDetails { sum: 0, avg: 0_f64 }; 1];
        let mut index: Index<usize> = Index {
            docs: HashMap::new(),
            root: UnsafeCell::new(None),
            fields,
        };
        initialize(&index, index_arenas);
        for (i, title) in titles.iter().enumerate() {
            let doc = Doc {
                id: i,
                title: title.to_string(),
            };
            add_document_to_index(
                &mut index,
                &index_arenas,
                &[title_extract],
                tokenizer,
                filter,
                doc.id,
                doc,
            );
        }
        index
    }
}
