use std::ops::{Try, FromResidual, ControlFlow};

/// Represents the result of an operation which will always yield a value, but possibly with a
/// collection of associated (non-fatal) errors too.
/// 
/// The possibility of a fatal error can be modelled using [MaybeFatal], with
/// `Fallible<MaybeFatal<T>, E>`.
#[derive(Debug, Clone, Hash)]
pub struct Fallible<T, E> {
    item: T,
    errors: Vec<E>,
}

impl<T, E> Fallible<T, E> {
    /// Constructs a new [Fallible] with no errors.
    pub fn new(item: T) -> Fallible<T, E> {
        Fallible {
            item,
            errors: vec![],
        }
    }

    /// Constructs a new [Fallible] with the given [Vec] of errors.
    pub fn new_with_errors(item: T, errors: Vec<E>) -> Fallible<T, E> {
        Fallible { item, errors }
    }

    /// Consumes this [Fallible] and returns its inner item, while moving any errors which occurred
    /// into a different [Fallible]. This is the intended way to access the inner item.
    /// 
    /// This can be used to collect all errors which occur across multiple operations into one.
    pub fn propagate<OT>(self, upper: &mut Fallible<OT, E>) -> T {
        upper.errors.extend(self.errors);
        self.item
    }

    /// Consumes this [Fallible], moving any errors which occurred into a different [Fallible], and
    /// executing a function to merge the result of this [Fallible] into the other one.
    /// 
    /// This can be a convenient shortcut for operations in the shape of `propagate()` followed by
    /// `as_mut().ignore().[...]`.
    pub fn integrate<OT>(self, upper: &mut Fallible<OT, E>, func: impl FnOnce(&mut OT, T)) {
        upper.errors.extend(self.errors);
        func(&mut upper.item, self.item)
    }

    /// Applies a function to the item inside this [Fallible]. Errors are unchanged.
    pub fn map<R>(self, func: impl FnOnce(T) -> R) -> Fallible<R, E> {
        Fallible::new_with_errors(func(self.item), self.errors)
    }

    /// Adds an error to this existing [Fallible].
    pub fn push_error(&mut self, err: E) {
        self.errors.push(err);
    }

    /// Returns a slice over all errors.
    pub fn errors(&self) -> &[E] {
        &self.errors
    }

    /// Assumes there are no errors, and consumes this [Fallible] to fetch the inner value. Panics
    /// if there are in fact errors.
    #[track_caller]
    pub fn unwrap(self) -> T {
        if self.errors.is_empty() {
            self.item
        } else {
            panic!("called `Fallible::unwrap` on a value with errors")
        }
    }

    /// Consumes this [Fallible] to fetch the inner value, ignoring any errors.
    pub fn ignore_errors(self) -> T {
        self.item
    }

    /// Consumes this [Fallible] to fetch the [Vec] of errors, ignoring any inner value.
    pub fn into_errors(self) -> Vec<E> {
        self.errors
    }

    /// Converts a `&Fallible<T, E>` into a `Fallible<&T, &E>`.
    pub fn as_ref(&self) -> Fallible<&T, &E> {
        Fallible {
            item: &self.item,
            errors: self.errors.iter().collect()
        }
    }

    /// Converts a `&mut Fallible<T, E>` into a `Fallible<&mut T, &mut E>`.
    pub fn as_mut(&mut self) -> Fallible<&mut T, &mut E> {
        Fallible {
            item: &mut self.item,
            errors: self.errors.iter_mut().collect()
        }
    }

    /// Returns true if this [Fallible] has any errors.
    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }
}

impl<T, E> From<Fallible<T, E>> for Result<T, Vec<E>> {
    fn from(value: Fallible<T, E>) -> Self {
        if value.has_errors() {
            Err(value.into_errors())
        } else {
            Ok(value.ignore_errors())
        }
    }
}

impl<T, E> From<Result<T, E>> for Fallible<Option<T>, E> {
    fn from(value: Result<T, E>) -> Self {
        match value {
            Ok(v) => Fallible::new(Some(v)),
            Err(e) => Fallible::new_with_errors(None, vec![e]),
        }
    }
}

/// An [Option]-like type, intended to be used as the item type for a [Fallible] where errors may be
/// fatal, leading to the absence of any value.
/// 
/// [Option] would provide the same semantic behaviour, but it might be unclear what exactly is
/// being modelled with a `Fallible<Option<T>, E>`. In addition, `Fallible<MaybeFatal<T>, E>`
/// provides some additional useful APIs.
#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone, Copy)]
pub enum MaybeFatal<T> {
    Ok(T),
    Fatal
}

impl<T> MaybeFatal<T> {
    /// Converts a `&MaybeFatal<T>` into a `MaybeFatal<&T>`.
    pub fn as_ref(&self) -> MaybeFatal<&T> {
        match self {
            MaybeFatal::Ok(v) => MaybeFatal::Ok(v),
            MaybeFatal::Fatal => MaybeFatal::Fatal,
        }
    }

    /// Converts a `&mut Fallible<T, E>` into a `Fallible<&mut T, &mut E>`.
    pub fn as_mut(&mut self) -> MaybeFatal<&T> {
        match self {
            MaybeFatal::Ok(v) => MaybeFatal::Ok(v),
            MaybeFatal::Fatal => MaybeFatal::Fatal,
        }
    }

    /// Assumes this is a [MaybeFatal::Ok], and consumes it to return the inner value. Panics if it
    /// is [MaybeFatal::Fatal] instead.
    #[track_caller]
    pub fn unwrap(self) -> T {
        match self {
            MaybeFatal::Ok(v) => v,
            MaybeFatal::Fatal => panic!("called `MaybeFatal::unwrap` on a `Fatal` value"),
        }
    }
}

impl<T, E> Fallible<MaybeFatal<T>, E> {
    /// Constructs a new [Fallible] with a nested [MaybeFatal::Fatal].
    pub fn new_fatal(errors: Vec<E>) -> Fallible<MaybeFatal<T>, E> {
        Fallible::new_with_errors(MaybeFatal::Fatal, errors)
    }
    
    /// Constructs a new [Fallible] with a nested [MaybeFatal::Ok].
    pub fn new_ok(item: T) -> Fallible<MaybeFatal<T>, E> {
        Fallible::new(MaybeFatal::Ok(item))
    }

    /// Consumes this [Fallible], moving any errors which occurred into a different [Fallible], and
    /// if the item is non-fatal, executing a function to merge the result of this [Fallible] into
    /// the other one.
    pub fn integrate_if_ok<OT>(self, upper: &mut Fallible<OT, E>, func: impl FnOnce(&mut OT, T)) {
        upper.errors.extend(self.errors);
        if let MaybeFatal::Ok(item) = self.item {
            func(&mut upper.item, item)
        }
    }
}

impl<T, E1, E2: From<E1>> FromResidual<Fallible<MaybeFatal<!>, E1>> for Fallible<MaybeFatal<T>, E2> {
    fn from_residual(residual: Fallible<MaybeFatal<!>, E1>) -> Self {
        Fallible::new_with_errors(
            MaybeFatal::Fatal,
            residual.errors.into_iter().map(|e| e.into()).collect(),
        )
    }
}

impl<T, E> Try for Fallible<MaybeFatal<T>, E> {
    type Output = Fallible<T, E>;
    type Residual = Fallible<MaybeFatal<!>, E>;

    fn from_output(output: Self::Output) -> Self {
        Fallible::new_with_errors(MaybeFatal::Ok(output.item), output.errors)
    }

    fn branch(self) -> std::ops::ControlFlow<Self::Residual, Self::Output> {
        match self.item {
            MaybeFatal::Ok(v) => ControlFlow::Continue(Fallible::new_with_errors(v, self.errors)),
            MaybeFatal::Fatal => ControlFlow::Break(Fallible::new_with_errors(MaybeFatal::Fatal, self.errors)),
        }
    }
}

impl<T> From<T> for MaybeFatal<T> {
    fn from(value: T) -> Self {
        MaybeFatal::Ok(value)
    }
}
