use crate::InstructionOpcode;

impl InstructionOpcode {
    pub fn mnemonic(&self) -> String {
        let raw_name = self.as_ref();
        let mut opcode = String::new();

        // Convert PascalCase to snake_case
        for (i, char) in raw_name.chars().enumerate() {
            if char.is_ascii_uppercase() {
                // Don't add an underscore if we're at the beginning
                // We want "SomeThing" -> "some_thing", not "_some_thing"
                if i != 0 {
                    opcode.push('_');
                }

                opcode.push(char.to_ascii_lowercase());
            } else {
                opcode.push(char);
            }
        }

        opcode
    }
}
