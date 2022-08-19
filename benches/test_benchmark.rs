use criterion::{criterion_group, criterion_main, Criterion};
use probly_search::index::{create_index_with_capacity, Index};

criterion_group!(benches, test_speed);
criterion_main!(benches);
struct DocX {
    id: usize,
    title: String,
}

fn filter(s: &str) -> &str {
    s
}
fn tokenizer(s: &str) -> Vec<&str> {
    s.split(' ').collect::<Vec<_>>()
}

pub fn test_speed(c: &mut Criterion) {
    use lazy_static::lazy_static;
    use rand::seq::SliceRandom;

    lazy_static! {
        static ref ALLOWED: Vec<char> = "abcdefghilkjapqrstuvwxyz".chars().collect();
    }

    pub fn generate_string(min: i32, max: i32) -> String {
        let mut rng = rand::thread_rng();
        let mut s = String::from("");

        for _ in min..=max {
            s.push(ALLOWED.choose(&mut rng).map(|&c| c as char).unwrap())
        }
        s
    }
    fn title_extract_x(d: &DocX) -> Option<&str> {
        Some(d.title.as_str())
    }

    c.bench_function("add_100k_docs", |b| {
        let mut index = create_index_with_capacity(1, 100000, 100000);
        let mut random_strings: Vec<String> = Vec::new();
        for _ in 1..100000 {
            let mut new_rand = generate_string(0, 4);
            new_rand.push(' ');
            new_rand.push_str(&generate_string(0, 4));
            random_strings.push(new_rand);
        }
        let extractor = [title_extract_x as fn(&_) -> Option<&str>];
        b.iter(|| add_all_documents(&mut index, &extractor, &random_strings));
    });
}

fn add_all_documents(
    index: &mut Index<usize>,
    extractor: &[fn(&DocX) -> Option<&str>],
    random_strings: &[String],
) {
    for (i, s) in random_strings.iter().enumerate() {
        let d = DocX {
            id: i,
            title: s.to_owned(),
        };
        index.add_document(extractor, tokenizer, filter, d.id, &d);
    }
}
