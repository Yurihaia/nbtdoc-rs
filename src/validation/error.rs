use std::error::Error;
use std::fmt::{
	self,
	Display,
	Formatter
};

use crate::identifier::Identifier;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ValidationError {
	pub module: Vec<String>,
	pub err: ValidationErrorType
}

impl ValidationError {
	pub fn new(module: Vec<String>, err: ValidationErrorType) -> Self {
		ValidationError {
			module,
			err
		}
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
	},
	MismatchedEnum {
		ex: EnumType,
		ty: EnumType,
		name: String
	}
}

#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub enum EnumType {
	Byte, Short, Int, Long, Float, Double, String
}

use super::format::EnumType as ETO;

impl From<&ETO> for EnumType {
	fn from(t: &ETO) -> Self {
		match t {
			ETO::Byte(_) => EnumType::Byte,
			ETO::Short(_) => EnumType::Short,
			ETO::Int(_) => EnumType::Int,
			ETO::Long(_) => EnumType::Long,
			ETO::Float(_) => EnumType::Float,
			ETO::Double(_) => EnumType::Double,
			ETO::String(_) => EnumType::String
		}
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
			),
			Self::MismatchedEnum { name, ty, ex } => write!(
				f, "Invalid enum type {:?} for {}, expected {:?}",
				ty, name, ex
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