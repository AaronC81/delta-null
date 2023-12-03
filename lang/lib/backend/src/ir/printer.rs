use std::collections::HashMap;

use super::VariableId;

/// Contains options about how printed IR should look, or any info it should contain.
pub struct PrintOptions {
    // TODO
    // Additional information alongside variables (such as naming or allocation), which is output
    // alongside its definition.
    // additional_variable_info: HashMap<VariableId, String>,
}

impl PrintOptions {
    pub fn new() -> Self {
        PrintOptions {  }
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