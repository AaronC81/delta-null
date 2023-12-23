use std::collections::HashMap;

use super::{Statement, BasicBlockId, VariableId, Variable, Type, BasicBlock, Instruction, VariableRepository, Function, StatementId, ConstantValue, InstructionKind, util::ShareCell, Local, LocalId, LocalRepository};

struct FunctionBuilderState {
    next_variable_id: usize,
    next_block_id: usize,
    next_local_id: usize,
    variables: HashMap<VariableId, Variable>,
    blocks: HashMap<BasicBlockId, Option<BasicBlock>>,
    locals: HashMap<LocalId, Local>,
}

impl FunctionBuilderState {
    pub fn new() -> Self {
        Self {
            next_variable_id: 0,
            next_block_id: 0,
            next_local_id: 0,

            variables: HashMap::new(),
            blocks: HashMap::new(),
            locals: HashMap::new(),
        }
    }
}

impl FunctionBuilderState {
    pub fn new_variable(&mut self, ty: Type) -> VariableId {
        let id = VariableId(self.next_variable_id);
        self.next_variable_id += 1;
        self.variables.insert(id, Variable { id, ty });
        id
    }
}

impl VariableRepository for FunctionBuilderState {
    fn get_variable(&self, id: VariableId) -> &Variable {
        self.variables.get(&id).unwrap()
    }
}

impl LocalRepository for FunctionBuilderState {
    fn get_local(&self, id: LocalId) -> &Local {
        self.locals.get(&id).unwrap()
    }
}

pub struct FunctionBuilder {
    name: String,
    arguments: Vec<(String, VariableId)>,
    state: ShareCell<FunctionBuilderState>,
}

impl FunctionBuilder {
    pub fn new(name: &str, arguments: &[(String, Type)]) -> Self {
        let mut builder = Self {
            name: name.to_string(),
            arguments: vec![],
            state: ShareCell::new(FunctionBuilderState::new()),
        };

        // Add variables for arguments
        for (name, ty) in arguments {
            let var = builder.state.borrow_mut().new_variable(ty.clone());
            builder.arguments.push((name.to_owned(), var));
        }

        builder
    }

    pub fn new_local(&self, name: &str, ty: Type) -> LocalId {
        let mut state = self.state.borrow_mut();

        let id = LocalId(state.next_local_id);
        state.next_local_id += 1;

        state.locals.insert(id, Local { id, ty, name: name.to_owned() });

        id
    }

    pub fn new_basic_block(&self) -> (BasicBlockId, BasicBlockBuilder) {
        let mut state = self.state.borrow_mut();

        let id = BasicBlockId(state.next_block_id);
        state.next_block_id += 1;

        state.blocks.insert(id, None);

        (id, BasicBlockBuilder {
            id,
            statements: vec![],
            state: self.state.clone(),
        })
    }

    pub fn new_basic_blocks(&self, n: usize) -> (Vec<BasicBlockId>, Vec<BasicBlockBuilder>) {
        let mut indexes = vec![];
        let mut builders = vec![];

        for _ in 0..n {
            let (i, b) = self.new_basic_block();
            indexes.push(i);
            builders.push(b);
        }

        (indexes, builders)
    }

    pub fn finalize(self) -> Function {
        let state = self.state.take();

        Function {
            name: self.name,
            blocks: state.blocks.into_iter()
                .map(|(id, blk)|
                    (id, blk.expect(&format!("block {id:?} should have been finalized")))
                )
                .collect(),
            arguments: self.arguments.iter().map(|(_, ty)| *ty).collect(),
            variables: state.variables,
            locals: state.locals,
        }
    }

    pub fn finalize_blocks(&self, blocks: impl IntoIterator<Item = BasicBlockBuilder>) {
        for block in blocks {
            block.finalize()
        }
    }

    pub fn get_argument(&self, name: &str) -> Option<VariableId> {
        self.arguments.iter()
            .find(|(n, _)| n == name)
            .map(|(_, id)| *id)
    }

    pub fn get_variable_type(&self, id: VariableId) -> Type {
        self.state.borrow().get_variable(id).ty.clone()
    }
}

pub struct BasicBlockBuilder {
    id: BasicBlockId,
    statements: Vec<ShareCell<Statement>>,

    state: ShareCell<FunctionBuilderState>,
}

impl BasicBlockBuilder {
    pub fn id(&self) -> BasicBlockId {
        self.id
    }

    fn add_instruction_internal(&mut self, instruction: Instruction) -> Option<VariableId> {
        let result_ty = instruction.result_type(self.state.borrow(), self.state.borrow()).unwrap();
        let result = match result_ty {
            Some(ty) => Some(self.state.borrow_mut().new_variable(ty)),
            None => None,
        };
        let id = StatementId(self.id, self.statements.len());
        self.statements.push(ShareCell::new(Statement {
            id,
            result,
            instruction,
        }));
        result
    }

    pub fn add_instruction(&mut self, instruction: Instruction) -> VariableId {
        if instruction.is_terminator() {
            panic!("tried to use `add_instruction` with terminator: {:?}", instruction)
        }

        self.add_instruction_internal(instruction).unwrap()
    }

    pub fn add_void_instruction(&mut self, instruction: Instruction) {
        if instruction.is_terminator() {
            panic!("tried to use `add_void_instruction` with terminator: {:?}", instruction)
        }

        self.add_instruction_internal(instruction);
    }

    pub fn add_terminator(&mut self, instruction: Instruction) {
        if !instruction.is_terminator() {
            panic!("tried to use `add_terminator` with non-terminator: {:?}", instruction)
        }
        if self.has_terminator() {
            panic!("block already has terminator, but tried to add another");
        }

        self.add_instruction_internal(instruction);
    }

    pub fn add_terminator_if_none(&mut self, instruction: Instruction) {
        if !self.has_terminator() {
            self.add_terminator(instruction);
        }
    }

    pub fn add_constant(&mut self, constant: ConstantValue) -> VariableId {
        self.add_instruction(Instruction::new(InstructionKind::Constant(constant)))
    }

    pub fn has_terminator(&mut self) -> bool {
        self.statements.iter().any(|s| s.borrow().instruction.is_terminator())
    }

    pub fn statement_count(&self) -> usize {
        self.statements.len()
    }

    pub fn finalize(self) {
        let mut state = self.state.borrow_mut();

        let block = BasicBlock {
            id: self.id,
            statements: self.statements.into_iter()
                .map(|s| s.take())
                .collect(),
        };
        *state.blocks.get_mut(&self.id).unwrap() = Some(block);
    }
}
