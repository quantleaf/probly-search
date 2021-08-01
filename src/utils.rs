pub type FieldAccessor<D> = fn(&D) -> Option<&str>;
pub type Tokenizer = fn(&str) -> Vec<String>;
pub type Filter = fn(&str) -> String;
