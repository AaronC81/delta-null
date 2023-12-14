/// Describes a location within a source file, used for error reporting.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SourceLocation {
    file: String,
    line: usize,
    col: usize,
}

impl SourceLocation {
    pub fn new(file: String, line: usize, col: usize) -> Self {
        Self { file, line, col }
    }

    /// Creates a useless [SourceLocation], handy for tests.
    pub fn stub() -> Self {
        Self {
            file: "<stub>".to_owned(),
            line: 0,
            col: 0,
        }
    }

    pub fn describe(&self) -> String {
        format!("{}:{}:{}", self.file, self.line, self.col)
    }
}
