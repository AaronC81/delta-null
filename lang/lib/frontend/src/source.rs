use std::{fmt::Display, path::PathBuf};

/// Describes the type of the buffer which a [SourceLocation] refers to.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum SourceInputType {
    /// The input came from a file.
    File(PathBuf),

    /// The input came from an arbitrary input string.
    Buffer {
        /// Optionally, a directory to assume that the buffer was loaded from.
        /// Enables relative imports to be used from buffer inputs.
        fake_directory: Option<PathBuf>,
    },
}

impl Display for SourceInputType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SourceInputType::File(path) => write!(f, "{}", path.to_string_lossy()),
            SourceInputType::Buffer { .. } => write!(f, "<buffer>"),
        }
    }
}

impl SourceInputType {
    /// Convenience method to create a default buffer.
    pub fn buffer() -> Self {
        SourceInputType::Buffer { fake_directory: None }
    }
}

/// Describes a location within a source file, used for error reporting.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SourceLocation {
    pub input: SourceInputType,
    line: usize,
    col: usize,
}

impl SourceLocation {
    pub fn new(input: SourceInputType, line: usize, col: usize) -> Self {
        Self { input, line, col }
    }

    /// Creates a useless [SourceLocation], handy for tests.
    pub fn stub() -> Self {
        Self {
            input: SourceInputType::Buffer { fake_directory: None },
            line: 0,
            col: 0,
        }
    }

    pub fn describe(&self) -> String {
        format!("{}:{}:{}", self.input, self.line, self.col)
    }
}
