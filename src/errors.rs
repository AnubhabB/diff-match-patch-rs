#[derive(Debug)]
pub enum Error {
    InvalidInput,
    Utf8Error,
    HtmlWithError(String),
}
