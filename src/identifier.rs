use std::fmt::{
	Display,
	Formatter
};

#[derive(PartialEq, Eq, Clone, Hash, Debug)]
pub struct Identifier {
	pub namespace: String,
	pub path: String
}

#[cfg(feature = "serde")]
mod serde_impl {
	use serde::{
		Serialize,
		Deserialize,
		Serializer,
		Deserializer,
		de::{self, Visitor}
	};
	use super::*;

	impl Serialize for Identifier {
		fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error> where S: Serializer {
			serializer.serialize_str(&format!("{}:{}", self.namespace, self.path))
		}
	}

	impl <'de> Deserialize<'de> for Identifier {
		fn deserialize<D>(deserializer: D) -> Result<Self, D::Error> where D: Deserializer<'de> {
			deserializer.deserialize_str(IdentifierVisitor)
		}
	}

	struct IdentifierVisitor;

	impl <'de> Visitor<'de> for IdentifierVisitor {
		type Value = Identifier;

		fn expecting(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
			write!(f, "a string in the format of an identifier")
		}

		fn visit_str<E: de::Error>(self, value: &str) -> Result<Self::Value, E> {
			let mut chars = value.chars();
			let mut namespace = String::new();
			loop {
				match chars.next() {
					Some(':') => break,
					Some(v) => match v {
						'a'..='z'|'-'|'_' => namespace.push(v),
						e => return Err(de::Error::custom(format!("invalid character '{}'", e)))
					},
					None => return Err(de::Error::custom(format!("no namespace")))
				}
			};
			let mut path = String::new();
			loop {
				match chars.next() {
					Some(v) => match v {
						'a'..='z'|'-'|'_'|'/' => path.push(v),
						e => return Err(de::Error::custom(format!("invalid character '{}'", e)))
					},
					None => break
				}
			}
			Ok(Identifier::new(namespace, path))
		}
	}
}

impl Identifier {
	pub fn new(namespace: String, path: String) -> Self {
		Identifier { namespace, path }
	}
}

impl Display for Identifier {
	fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
		write!(f, "{}:{}", self.namespace, self.path)
	}
}