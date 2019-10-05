use std::fmt::{
	Display,
	Formatter
};

#[derive(PartialEq, Eq, Clone, Hash, Debug)]
pub struct Identifier {
	pub namespace: String,
	pub path: String
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