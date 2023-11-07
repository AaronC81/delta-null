pub trait ToAssembly {
    fn to_assembly(&self) -> String;
}

impl ToAssembly for u8 {
    fn to_assembly(&self) -> String {
        self.to_string()
    }
}
