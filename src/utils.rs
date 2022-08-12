pub type FieldAccessor<D> = fn(&D) -> Option<&str>;
pub type Tokenizer = fn(&str) -> Vec<&str>;
pub type Filter = fn(&str) -> &str;
