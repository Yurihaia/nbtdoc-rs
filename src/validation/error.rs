use std::error::Error;
use std::fmt::{
	self,
	Display,
	Formatter
};

use crate::identifier::Identifier;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ValidationError {
	module: Vec<String>,
	err: ValidationErrorType
}

impl ValidationError {
	pub fn new(module: Vec<String>, err: ValidationErrorType) -> Self {
		ValidationError {
			module,
			err
		}
	}

	pub fn get_err(&self) -> &ValidationErrorType {
		&self.err
	}

	pub fn get_module(&self) -> &[String] {
		&self.module
	}
}

impl Error for ValidationError {}

impl Display for ValidationError {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		write!(f, "Validation error: {} in module {}", self.err, self.module.join("::"))
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ValidationErrorType {
	UnresolvedName(String),
	InvalidType {
		name: String, 
		ty: ItemType, 
		ex: Vec<ItemType>
	},
	RootAccess,
	SuperImport,
	DuplicateDescribe {
		reg: Identifier,
		t: Option<Identifier>
	}
}

impl Display for ValidationErrorType {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		match self {
			Self::UnresolvedName(v) => write!(f, "Unresolved name {}", v),
			Self::InvalidType { name, ty, ex } => write!(
				f, "Invalid item type {:?} for {}, expected one of {:?}",
				ty, name, ex.into_iter().map(|x| format!("{:?}", x)).collect::<Vec<String>>().join(", ")
			),
			Self::RootAccess => write!(f, "Attempted module root access"),
			Self::SuperImport => write!(f, "Attempted to import super module"),
			Self::DuplicateDescribe { reg, t } => write!(
				f, "Duplicate describe on {}[{}]",
				reg,
				match t {
					Some(v) => format!("{}", v.clone()),
					None => String::from("default")
				}
			)
		}
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ItemType {
	Compound,
	Enum,
	Module
}