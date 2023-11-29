use std::{cell::RefCell, rc::Rc, collections::HashMap};

use super::{Statement, BasicBlockId, VariableId, Variable, Type, BasicBlock, Instruction, VariableRepository, Function, StatementId, ConstantValue, InstructionKind};

struct FunctionBuilderState {
    next_variable_id: usize,
    next_block_id: usize,
    variables: HashMap<VariableId, Variable>,
    blocks: HashMap<BasicBlockId, Option<BasicBlock>>
}

impl FunctionBuilderState {
    pub fn new() -> Self {
        Self {
            next_variable_id: 0,
            next_block_id: 0,

            variables: HashMap::new(),
            blocks: HashMap::new(),
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

pub struct FunctionBuilder {
    name: String,
    state: Rc<RefCell<Option<FunctionBuilderState>>>,
}

impl FunctionBuilder {
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            state: Rc::new(RefCell::new(Some(FunctionBuilderState::new()))),
        }
    }

    pub fn new_basic_block(&mut self) -> (BasicBlockId, BasicBlockBuilder) {
        let mut state_ref = self.state.borrow_mut();
        let mut state = state_ref.as_mut().unwrap();

        let id = BasicBlockId(state.next_block_id);
        state.next_block_id += 1;

        state.blocks.insert(id, None);

        (id, BasicBlockBuilder {
            id,
            statements: vec![],
            state: self.state.clone(),
        })
    }

    pub fn new_basic_blocks(&mut self, n: usize) -> (Vec<BasicBlockId>, Vec<BasicBlockBuilder>) {
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
        let state = self.state.borrow_mut().take().unwrap();

        Function {
            name: self.name,
            blocks: state.blocks.into_iter()
                .map(|(id, blk)|
                    (id, blk.expect(&format!("block {id:?} should have been finalized")))
                )
                .collect(),
            variables: state.variables,
        }
    }

    pub fn finalize_blocks(&self, blocks: impl IntoIterator<Item = BasicBlockBuilder>) {
        for block in blocks {
            block.finalize()
        }
    }
}

pub struct BasicBlockBuilder {
    id: BasicBlockId,
    statements: Vec<Rc<RefCell<Option<Statement>>>>, // never `None`, only used so we can use `Option::take`

    state: Rc<RefCell<Option<FunctionBuilderState>>>,
}

impl BasicBlockBuilder {
    pub fn id(&self) -> BasicBlockId {
        self.id
    }

    fn add_instruction_internal(&mut self, instruction: Instruction) -> Option<VariableId> {
        let result_ty = instruction.result_type(self.state.borrow_mut().as_mut().unwrap()).unwrap();
        let result = match result_ty {
            Some(ty) => Some(self.state.borrow_mut().as_mut().unwrap().new_variable(ty)),
            None => None,
        };
        let id = StatementId(self.id, self.statements.len());
        self.statements.push(Rc::new(RefCell::new(Some(Statement {
            id,
            result,
            instruction,
        }))));
        result
    }

    pub fn add_instruction(&mut self, instruction: Instruction) -> VariableId {
        if instruction.is_terminator() {
            panic!("tried to use `add_instruction` with terminator: {:?}", instruction)
        }

        self.add_instruction_internal(instruction).unwrap()
    }

    pub fn add_terminator(&mut self, instruction: Instruction) {
        if !instruction.is_terminator() {
            panic!("tried to use `add_terminator` with non-terminator: {:?}", instruction)
        }

        self.add_instruction_internal(instruction);
    }

    pub fn add_constant(&mut self, constant: ConstantValue) -> VariableId {
        self.add_instruction(Instruction::new(InstructionKind::Constant(constant)))
    }

    pub fn finalize(self) {
        let mut state_ref = self.state.borrow_mut();
        let state = state_ref.as_mut().unwrap();

        let block = BasicBlock {
            id: self.id,
            statements: self.statements.into_iter()
                .map(|s| s.borrow_mut().take().unwrap())
                .collect(),
        };
        *state.blocks.get_mut(&self.id).unwrap() = Some(block);
    }
}
