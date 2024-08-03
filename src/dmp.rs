
/**
 * The data structure representing a diff is an array of tuples:
 * [[DIFF_DELETE, 'Hello'], [DIFF_INSERT, 'Goodbye'], [DIFF_EQUAL, ' world.']]
 * which means: delete 'Hello', add 'Goodbye' and keep ' world.'
 */

/// Enum representing the different ops of diff
#[derive(Debug, PartialEq, Eq)]
pub enum Ops {
    Delete,
    Insert,
    Equal
}

/// A structure representing a diff
/// (Ops::Delete, String::new("Hello")) means delete `Hello`
/// (Ops::Insert, String::new("Goodbye")) means add `Goodbye`
/// (Ops::Equal, String::new("World")) means keep world
#[derive(Debug, PartialEq, Eq)]
pub struct Diff(Ops, String);

impl Diff {
    /// Create a new diff object
    pub fn new(op: Ops, text: &str) -> Self {
        Self(op, text.to_string())
    }
}

pub struct DiffMatchPatch {
    checklines: Option<bool>
}

impl DiffMatchPatch {
    fn checklines(&self) -> bool {
        self.checklines.map_or(true, |c| c)
    }
}

impl DiffMatchPatch {
    pub fn diff_main(&self, old: &str, new: &str) -> Vec<Diff> {
        // First, check if lhs and rhs are equal
        if old == new {
            if old.is_empty() {
                return Vec::new();
            }

            return vec![Diff::new(Ops::Equal, old)];
        }

        if old.is_empty() {
            return vec![Diff::new(Ops::Insert, new)]
        }

        if new.is_empty() {
            return vec![Diff::new(Ops::Delete, old)]
        }


        todo!()
    }
}