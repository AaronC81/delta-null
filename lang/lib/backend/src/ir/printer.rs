use std::collections::HashMap;

use super::VariableId;

/// Contains options about how printed IR should look, or any info it should contain.
pub struct PrintOptions {
    // Additional information alongside variables (such as naming or allocation), which is output
    // alongside its definition.
    pub additional_variable_info: Option<HashMap<VariableId, String>>,
}

impl PrintOptions {
    pub fn new() -> Self {
        PrintOptions {
            additional_variable_info: None,
        }
    }
}

impl Default for PrintOptions {
    fn default() -> Self {
        Self::new()
    }
}

/// Anything which can be converted into a string of IR.
pub trait PrintIR {
    /// Converts this object into an IR string, according to some options.
    fn print_ir(&self, options: &PrintOptions) -> String;
}