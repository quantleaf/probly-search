pub mod index;
pub mod query;
pub mod utils;

#[cfg(test)]
pub mod test_util {

    use ghost_cell::GhostToken;

    use crate::{
        index::{add_document_to_index, create_index, Index},
        query::{query, score::calculator::ScoreCalculator, QueryResult},
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
    }
    fn tokenizer(s: &str) -> Vec<String> {
        s.split(' ')
            .map(|slice| slice.to_owned())
            .collect::<Vec<String>>()
    }
    fn title_extract(d: &Doc) -> Option<&str> {
        Some(d.title.as_str())
    }

    fn filter(s: &String) -> String {
        s.to_owned()
    }

    pub fn test_score<'arena, 'idx, 'idn, M, S: ScoreCalculator<usize, M>>(
        mut idx: &mut Index<'arena, 'idx, 'idn, usize>,
        score_calculator: &mut S,
        q: &str,
        expected: Vec<QueryResult<usize>>,
        token: &GhostToken<'idx>,
    ) {
        let mut results = query(
            &mut idx,
            q,
            score_calculator,
            tokenizer,
            filter,
            &vec![1., 1.],
            None,
            &token,
        );
        results.sort_by(|a, b| {
            let mut sort = b.score.partial_cmp(&a.score).unwrap();
            sort = sort.then_with(|| a.key.partial_cmp(&b.key).unwrap());
            return sort;
        });

        for (index, result) in results.iter().enumerate() {
            assert_eq!(expected[index], *result);
            assert_eq!(approx_equal(expected[index].score, result.score, 8), true)
        }
    }

    /***
        Create a index with docucments with title fields, with increasing ids starting from 0
    */
    pub fn build_test_index<'arena, 'idx, 'idn>(
        titles: &[&str],
        token: &GhostToken<'idx>,
    ) -> Index<'arena, 'idx, 'idn, usize> {
        let mut idx = create_index(1);
        for (index, title) in titles.iter().enumerate() {
            let doc = Doc {
                id: index,
                title: title.to_string(),
            };
            add_document_to_index(
                &mut idx,
                &[title_extract],
                tokenizer,
                filter,
                doc.id,
                doc,
                &mut token,
            );
        }
        idx
    }
}
