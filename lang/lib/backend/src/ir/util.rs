use std::{rc::Rc, cell::{RefCell, Ref, RefMut}, ops::{Deref, DerefMut}};

/// A reference-counted, runtime-borrow-checked cell, like the common pattern of combining [Rc] and
/// [RefCell], but which additionally allows the single value to be moved out of the cell,
/// invaliding all references.
pub struct ShareCell<T> {
    value: Rc<RefCell<Option<T>>>
}

impl<T> ShareCell<T> {
    pub fn new(value: T) -> Self {
        Self { value: Rc::new(RefCell::new(Some(value))) }
    }
    
    /// Returns an immutable reference to the inner value.
    /// 
    /// Panics if the value is already borrowed mutably, or has already been moved out of the cell.
    pub fn borrow(&self) -> impl Deref<Target = T> + '_ {
        Ref::map(
            self.value.borrow(),
            |r| r.as_ref().expect("value already moved out")
        )
    }

    /// Returns a mutable reference to the inner value.
    /// 
    /// Panics if the value is already borrowed, or has already been moved out of the cell. 
    pub fn borrow_mut(&self) -> impl DerefMut<Target = T> + '_ {
        RefMut::map(
            self.value.borrow_mut(),
            |r: &mut Option<T>| r.as_mut().expect("value already moved out")
        )
    }

    /// Moves the inner value out of the cell, invaliding all references.
    /// 
    /// Panics if the value is borrowed, or has already been moved out of the cell.
    pub fn take(self) -> T {
        self.value.borrow_mut().take().expect("value already moved out")
    }
}

impl<T> Clone for ShareCell<T> {
    fn clone(&self) -> Self {
        Self { value: self.value.clone() }
    }
}
