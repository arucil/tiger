pub use self::symbol::*;

mod symbol;

#[derive(Debug, Clone)]
pub struct Spanned<T>(pub Span, pub T);

#[derive(Debug, Clone)]
pub struct Span {
  pub start: usize,
  pub end: usize,
}

#[derive(Debug, Clone)]
pub struct Expr {
}

#[derive(Debug, Clone)]
pub enum Var {
  Simple(Spanned<Symbol>),
  Field(Spanned<FieldVar>),
}

#[derive(Debug, Clone)]
pub struct FieldVar {
  pub var: Box<Var>,
  pub field: Spanned<Symbol>,
}

#[derive(Debug, Clone)]
pub struct IndexVar {
  pub var: Box<Var>,
  pub index: Spanned<Box<Expr>>,
}