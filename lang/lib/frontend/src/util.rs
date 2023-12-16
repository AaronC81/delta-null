#[macro_export]
macro_rules! frontend_error {
    ($name:ident, $friendly_name:literal) => {
        #[derive(Debug, Clone)]
        pub struct $name {
            description: String,
            loc: crate::source::SourceLocation,
        }

        impl $name {
            pub fn new(description: &str, loc: crate::source::SourceLocation) -> Self {
                $name { description: description.to_owned(), loc }
            }
        }

        impl Display for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{} error: {}: {}", $friendly_name, self.loc.describe(), self.description)
            }
        }
        impl Error for $name {}        
    };
}
