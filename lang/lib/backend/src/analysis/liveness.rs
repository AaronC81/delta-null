use std::collections::{HashMap, HashSet};

use crate::ir::{StatementId, VariableId, Function};

/// Performs liveness analysis, determining which variables still need to exist for each statement.
/// 
/// If a variable is "live" for a statement, then it is read in that statement, or one which could
/// execute after it.
/// 
/// Returns a map keyed by every [StatementId] in the function. For a given statement, the value is
/// the set of variables which are "live".
pub fn liveness_analysis(func: &Function) -> HashMap<StatementId, HashSet<VariableId>> {
    todo!()
}
