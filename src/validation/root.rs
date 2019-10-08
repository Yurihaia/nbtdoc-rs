use super::super::FileProvider;
use super::arena::*;
use super::format::*;
use crate::parse::{ast, root};
use crate::Range;

use std::collections::{
	HashMap
};
use std::io;
use std::path::{
	Path,
	PathBuf
};

use std::error::Error;
use std::fmt::{
	Display,
	Debug,
	Formatter
};

use crate::identifier::Identifier;

use std::convert::From;

use nom::error::convert_error;

#[derive(Debug)]
pub struct Root {
	registries: HashMap<Identifier, (
		HashMap<Identifier, Index<CompoundTag>>,
		Option<Index<CompoundTag>>
	)>,

	root_modules: HashMap<String, Index<Module>>,

	compound_arena: Arena<CompoundTag>,
	enum_arena: Arena<EnumItem>,
	module_arena: Arena<Module>
}

impl Root {
	pub fn new() -> Self {
		Root {
			registries: HashMap::new(),

			root_modules: HashMap::new(),

			compound_arena: Arena::new(),
			enum_arena: Arena::new(),
			module_arena: Arena::new()
		}
	}

	pub fn get_regitry(&self, name: &Identifier, id: &Identifier) -> Option<&CompoundTag> {
		let (r, d) = self.registries.get(name)?;
		Some(&self.compound_arena[*r.get(id).unwrap_or(&(*d)?)])
	}

	pub fn get_compound(&self, name: Index<CompoundTag>) -> &CompoundTag {
		&self.compound_arena[name]
	}

	/// `p` needs to be an absolute path
	pub fn add_root_module<F, P>(
		&mut self,
		p: P,
		fp: &F
	) -> Result<(), NbtDocError> where F: FileProvider, P: AsRef<Path> {
		let module_name = p.as_ref()
			.file_stem()
			.ok_or(
				io::Error::from(io::ErrorKind::NotFound)
			)?.to_str().ok_or_else(
				|| ValidationError::InvalidName(
					p.as_ref().file_name().unwrap().to_os_string()
				)
			)?;
		let module_tree = ModuleTree::read(
			p.as_ref().parent().ok_or(io::Error::from(io::ErrorKind::NotFound))?,
			module_name,
			fp
		)?;
		let root = self.register_module(Module {
			children: HashMap::new(),
			parent: None
		});
		self.root_modules.insert(
			String::from(module_name),
			root
		);
		self.register_module_tree(root, &module_tree)?;
		self.preresolve_module_tree(root, &module_tree)?;
		self.resolve_module_tree(root, module_tree)?;
		Ok(())
	}

	fn register_module_tree(
		&mut self,
		rootind: Index<Module>,
		tree: &ModuleTree
	) -> Result<(), ValidationError> {
		let cast = &tree.val;
		// first register items so lower modules can resolve
		for (n, _) in cast.compounds.iter() {
			let ind = self.register_compound(CompoundTag {
				description: String::new(),
				supers: None,
				fields: HashMap::new()
			});
			self.module_arena[rootind].children.insert(n.clone(), ItemIndex::Compound(ind));
		}
		for (n, e) in cast.enums.iter() {
			let ind = self.register_enum(EnumItem {
				description: String::new(),
				et: match e.values {
					ast::EnumType::Byte(_) => EnumType::Byte(vec![]),
					ast::EnumType::Short(_) => EnumType::Short(vec![]),
					ast::EnumType::Int(_) => EnumType::Int(vec![]),
					ast::EnumType::Long(_) => EnumType::Long(vec![]),
					ast::EnumType::Float(_) => EnumType::Float(vec![]),
					ast::EnumType::Double(_) => EnumType::Double(vec![]),
					ast::EnumType::String(_) => EnumType::String(vec![]),
				}
			});
			self.module_arena[rootind].children.insert(n.clone(), ItemIndex::Enum(ind));
		}
		// register modules, which will register their items
		let mut next = Vec::with_capacity(tree.children.len());
		for (n, m) in tree.children.iter() {
			let root = self.register_module(Module {
				children: HashMap::new(),
				parent: Some(rootind)
			});
			self.module_arena[rootind].children.insert(n.clone(), ItemIndex::Module(root));
			next.push((root, m));
		}
		for (root, m) in next.into_iter() {
			self.register_module_tree(root, &m)?;
		};
		Ok(())
	}

	fn preresolve_module_tree(
		&mut self,
		rootind: Index<Module>,
		tree: &ModuleTree
	) -> Result<(), ValidationError> {
		let cast = &tree.val;
		for (e, n) in cast.uses.iter() {
			if *e {
				let last = match n.last().ok_or(ValidationError::RootAccess)? {
					ast::PathPart::Regular(v) => v,
					_ => return Err(ValidationError::NotAnItem)
				};
				let item = self.get_item_path(
					&n,
					Some(rootind),
					&HashMap::new()
				)?;
				if let ItemIndex::Module(_) = item {
					return Err(ValidationError::NotAnItem);
				}
				self.module_arena[rootind].children.insert(last.clone(), item);
			}
		};
		for (n, v) in &tree.children {
			self.preresolve_module_tree(
				match &self.module_arena[rootind].children[n] {
					ItemIndex::Module(v) => *v,
					_ => panic!()
				},
				v
			)?;
		}
		Ok(())
	}

	fn resolve_module_tree(
		&mut self,
		rootind: Index<Module>,
		tree: ModuleTree
	) -> Result<(), ValidationError> {
		let cast = tree.val;
		let mut imports = HashMap::new();
		for (_, n) in cast.uses {
			imports.insert(
				match n.last().ok_or(ValidationError::RootAccess)? {
					ast::PathPart::Regular(s) => s.clone(),
					ast::PathPart::Root => return Err(ValidationError::RootAccess),
					ast::PathPart::Super => return Err(ValidationError::InvalidPath(ast::PathPart::Super))
				},
				self.get_item_path(&n, Some(rootind), &HashMap::new())?
			);
		}
		for (n, c) in cast.compounds {
			// this better work
			let cpdi = *match self.module_arena[rootind].children.get(&n).unwrap() {
				ItemIndex::Compound(v) => v,
				_ => panic!()
			};
			self.compound_arena[cpdi].description = c.description;
			match c.supers {
				Some(v) => self.compound_arena[cpdi].supers = Some(
					match self.get_item_path(&v, Some(rootind), &imports)? {
						ItemIndex::Compound(v) => v,
						_ => return Err(ValidationError::InvalidExtend)
					}),
				None => self.compound_arena[cpdi].supers = None
			};
			for (n, t) in c.fields {
				let field = Field {
					description: t.description,
					nbttype: self.convert_field_type(t.field_type, rootind, &imports)?
				};
				self.compound_arena[cpdi].fields.insert(n, field);
			}
		}
		for (n, e) in cast.enums {
			let eni = *match self.module_arena[rootind].children.get(&n).unwrap() {
				ItemIndex::Enum(v) => v,
				_ => panic!()
			};
			self.enum_arena[eni].description = e.description;
			self.enum_arena[eni].et = match e.values {
				ast::EnumType::Byte(v) => EnumType::Byte(
					v.into_iter().map(|(n, v)| EnumOption {
						description: v.description,
						name: n,
						value: v.value
					}).collect()
				),
				ast::EnumType::Short(v) => EnumType::Short(
					v.into_iter().map(|(n, v)| EnumOption {
						description: v.description,
						name: n,
						value: v.value
					}).collect()
				),
				ast::EnumType::Int(v) => EnumType::Int(
					v.into_iter().map(|(n, v)| EnumOption {
						description: v.description,
						name: n,
						value: v.value
					}).collect()
				),
				ast::EnumType::Long(v) => EnumType::Long(
					v.into_iter().map(|(n, v)| EnumOption {
						description: v.description,
						name: n,
						value: v.value
					}).collect()
				),
				ast::EnumType::Float(v) => EnumType::Float(
					v.into_iter().map(|(n, v)| EnumOption {
						description: v.description,
						name: n,
						value: v.value
					}).collect()
				),
				ast::EnumType::Double(v) => EnumType::Double(
					v.into_iter().map(|(n, v)| EnumOption {
						description: v.description,
						name: n,
						value: v.value
					}).collect()
				),
				ast::EnumType::String(v) => EnumType::String(
					v.into_iter().map(|(n, v)| EnumOption {
						description: v.description,
						name: n,
						value: v.value
					}).collect()
				),
			}
		}
		for (p, d) in cast.describes {
			let target = match self.get_item_path(&p, Some(rootind), &imports)? {
				ItemIndex::Compound(v) => v,
				_ => return Err(ValidationError::DescribeType)
			};
			let (ref mut dt, ref mut def) = self.registries.entry(d.describe_type)
				.or_default();
			if let Some(targets) = d.targets {
				for n in targets {
					if dt.contains_key(&n) {
						return Err(ValidationError::DuplicateDescribe(format!("{}", n)))
					}
					dt.insert(n, target);
				}
			} else {
				if def.is_some() {
					return Err(ValidationError::DuplicateDescribe(String::from("\"default\"")))
				}
				*def = Some(target);
			}
		};
		for (n, v) in tree.children {
			self.resolve_module_tree(
				match self.module_arena[rootind].children[&n] {
					ItemIndex::Module(v) => v,
					_ => panic!()
				},
				v
			)?;
		}
		Ok(())
	}

	fn get_item_path(
		&self,
		path: &[ast::PathPart],
		rel: Option<Index<Module>>,
		imports: &HashMap<String, ItemIndex>
	) -> Result<ItemIndex, ValidationError> {
		if path.is_empty() {
			return Err(ValidationError::RootAccess)
		}
		let mut start = true;
		let mut current = rel;
		for part in &path[0..path.len() - 1] {
			current = match self.get_child(
				part,
				current,
				if start {
					start = false;
					Some(imports)
				} else {
					None
				}
			)? {
				None => None,
				Some(v) => match v {
					ItemIndex::Module(m) => Some(m),
					_ => return Err(ValidationError::NotAModule)
				}
			}
		};
		self.get_child(path.last().unwrap(), current, if start {
			Some(imports)
		} else {
			None
		})?.ok_or(ValidationError::RootAccess)
	}

	fn get_child(
		&self,
		part: &ast::PathPart,
		path: Option<Index<Module>>,
		imports: Option<&HashMap<String, ItemIndex>>
	) -> Result<Option<ItemIndex>, ValidationError> {
		Ok(match part {
			ast::PathPart::Root => None,
			ast::PathPart::Super => self.module_arena[
				path.ok_or(ValidationError::RootAccess)?
			].parent.map(ItemIndex::Module),
			ast::PathPart::Regular(v) => Some(match path {
				Some(i) => self.module_arena[i].children.get(v.as_str()).cloned().or_else(
						|| imports.and_then(|h| h.get(v.as_str())).cloned()
					),
				None => self.root_modules.get(v.as_str()).map(|v| ItemIndex::Module(*v))
			}.ok_or_else(|| ValidationError::UnresolvedItem(v.clone()))?)
		})
	}

	fn register_module(&mut self, module: Module) -> Index<Module> {
		self.module_arena.push(module)
	}

	fn register_compound(&mut self, item: CompoundTag) -> Index<CompoundTag> {
		self.compound_arena.push(item)
	}

	fn register_enum(&mut self, item: EnumItem) -> Index<EnumItem> {
		self.enum_arena.push(item)
	}

	fn convert_field_type(
		&self,
		ft: ast::FieldType,
		root: Index<Module>,
		imports: &HashMap<String, ItemIndex>
	) -> Result<NbtValue, ValidationError> {
		Ok(match ft {
			ast::FieldType::BooleanType => NbtValue::Boolean,
			ast::FieldType::StringType => NbtValue::String,
			ast::FieldType::NamedType(v) => {
				let item = self.get_item_path(&v, Some(root), imports)?;
				match item {
					ItemIndex::Module(_) => return Err(ValidationError::NotAnItem),
					ItemIndex::Compound(v) => NbtValue::Compound(v),
					ItemIndex::Enum(v) => NbtValue::Enum(v)
				}
			},
			ast::FieldType::ArrayType(v) => match v {
				ast::NumberArrayType::Byte { value_range, len_range } => 
					NbtValue::ByteArray(NumberArrayTag {
						length_range: len_range.map(|x|convert_range(x, 0, i32::max_value())),
						value_range: value_range.map(|x|convert_range(
							x,
							i8::min_value(),
							i8::max_value()
						))
					}),
				ast::NumberArrayType::Int { value_range, len_range } => 
					NbtValue::IntArray(NumberArrayTag {
						length_range: len_range.map(|x|convert_range(x, 0, i32::max_value())),
						value_range: value_range.map(|x|convert_range(
							x,
							i32::min_value(),
							i32::max_value()
						))
					}),
				ast::NumberArrayType::Long { value_range, len_range } => 
					NbtValue::LongArray(NumberArrayTag {
						length_range: len_range.map(|x|convert_range(x, 0, i32::max_value())),
						value_range: value_range.map(|x|convert_range(
							x,
							i64::min_value(),
							i64::max_value()
						))
					})
			},
			ast::FieldType::NumberType(v) => match v {
				ast::NumberPrimitiveType::Byte(range) => NbtValue::Byte(NumberTag {
					range: range.map(|x|convert_range(x, i8::min_value(), i8::max_value()))
				}),
				ast::NumberPrimitiveType::Short(range) => NbtValue::Short(NumberTag {
					range: range.map(|x|convert_range(x, i16::min_value(), i16::max_value()))
				}),
				ast::NumberPrimitiveType::Int(range) => NbtValue::Int(NumberTag {
					range: range.map(|x|convert_range(x, i32::min_value(), i32::max_value()))
				}),
				ast::NumberPrimitiveType::Long(range) => NbtValue::Long(NumberTag {
					range: range.map(|x|convert_range(x, i64::min_value(), i64::max_value()))
				}),
				ast::NumberPrimitiveType::Float(range) => NbtValue::Float(NumberTag {
					range: range.map(|x|convert_range(x, std::f32::NEG_INFINITY,  std::f32::INFINITY))
				}),
				ast::NumberPrimitiveType::Double(range) => NbtValue::Double(NumberTag {
					range: range.map(|x|convert_range(x, std::f64::NEG_INFINITY,  std::f64::INFINITY))
				})
			},
			ast::FieldType::ListType { item_type, len_range } => NbtValue::List {
				length_range: len_range.map(|x|convert_range(x, 0, i32::max_value())),
				value_type: Box::from(
					self.convert_field_type(*item_type, root, imports)?
				)
			},
			ast::FieldType::IndexType { target, path } => NbtValue::Index { path, target },
			ast::FieldType::IdType(v) => NbtValue::Id(v),
			ast::FieldType::OrType(v) => NbtValue::Or(v.into_iter().map(
				|x| self.convert_field_type(x, root, imports)
			).collect::<Result<Vec<NbtValue>, ValidationError>>()?)
		})
	}
}

fn convert_range<T: Copy>(range: ast::Range<T>, min: T, max: T) -> Range<T> {
	match range {
		ast::Range::Single(v) => Range(v, v),
		ast::Range::Both(l, h) => Range(l, h),
		ast::Range::Low(l) => Range(l, max),
		ast::Range::High(h) => Range(min, h)
	}
}

struct ModuleTree {
	val: ast::NbtDocFile,
	children: HashMap<String, ModuleTree>
}

impl ModuleTree {
	pub fn read<'a, P, F>(
		dir: P,
		name: &str,
		fp: &F
	) -> Result<Self, NbtDocError> where P: AsRef<Path>, F: FileProvider {
		let filename = format!("{}.nbtdoc", name);
		let mut newdir = PathBuf::from(dir.as_ref());
		let file = if fp.exists(dir.as_ref().join(&filename)) {
			fp.read_file(dir.as_ref().join(&filename))?
		} else {
			newdir.push(name);
			fp.read_file(dir.as_ref().join(name).join("mod.nbtdoc"))?
		};
		let mut out = ModuleTree {
			val: match root::<nom::error::VerboseError<&str>>(&file) {
				Ok(v) => v,
				Err(e) => return Err(match e {
					nom::Err::Error(e) | nom::Err::Failure(e) =>
						NbtDocError::Parse(convert_error(&file, e)),
					nom::Err::Incomplete(e) => NbtDocError::Parse(match e {
						nom::Needed::Size(e) => format!("Needs {} more bytes of data", e),
						nom::Needed::Unknown => format!("Needs unknown number of bytes")
					})
				})
			}.1,
			children: HashMap::new()
		};
		for x in out.val.mods.iter() {
			out.children.insert(x.clone(), ModuleTree::read(&newdir, x, fp)?);
		}
		Ok(out)
	}
}

#[derive(Debug)]
pub enum NbtDocError {
	Io(io::Error),
	Parse(String),
	Validation(ValidationError)
}

impl Display for NbtDocError {
	fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
		match self {
			Self::Io(e) => write!(f, "{}", e),
			Self::Parse(e) => write!(f, "{}", e),
			Self::Validation(e) => write!(f, "{}", e)
		}
	}
}

impl From<io::Error> for NbtDocError {
	fn from(e: io::Error) -> Self {
		Self::Io(e)
	}
}

impl From<ValidationError> for NbtDocError {
	fn from(e: ValidationError) -> Self {
		Self::Validation(e)
	}
}

impl Error for NbtDocError {}

#[derive(Debug,)]
pub enum ValidationError {
	UnresolvedModule(String),
	InvalidName(std::ffi::OsString),
	RootAccess,
	UnresolvedItem(String),
	InvalidPath(ast::PathPart),
	InvalidExtend,
	NotAnItem,
	NotAModule,
	DescribeType,
	DuplicateDescribe(String)
}

impl Display for ValidationError {
	fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
		match self {
			ValidationError::UnresolvedModule(v) => write!(f, "unresolved module {}", v),
			ValidationError::UnresolvedItem(v) => write!(f, "unresolved item {}", v),
			ValidationError::InvalidName(v) => write!(f, "invalid file name {}", v.to_string_lossy()),
			ValidationError::RootAccess => write!(f, "cannot index root"),
			ValidationError::InvalidPath(v) => write!(f, "invalid path {:?}", v),
			ValidationError::InvalidExtend => write!(f, "extends clause was not targeting a compound"),
			ValidationError::NotAModule => write!(f, "non-module item in path"),
			ValidationError::NotAnItem => write!(f, "field is not an item"),
			ValidationError::DescribeType => write!(f, "describe target is not a compound"),
			ValidationError::DuplicateDescribe(v) => write!(f, "duplicate describe {}", v)
		}
	}
}

impl Error for ValidationError {}

#[cfg(test)]
mod tests {

	use super::*;

	struct MockFileProvider {
		map: HashMap<PathBuf, &'static str>
	}

	impl FileProvider for MockFileProvider {
		fn read_file<F: AsRef<Path>>(&self, f: F) -> io::Result<String> {
			Ok(String::from(*self.map.get(f.as_ref()).unwrap()))
		}

		fn exists<F: AsRef<Path>>(&self, f: F) -> bool {
			self.map.contains_key(&PathBuf::from(f.as_ref()))
		}
	}

	#[test]
	fn small_files() -> Result<(), NbtDocError> {
		let mut fp = MockFileProvider {
			map: HashMap::new()
		};
		fp.map.insert(
			PathBuf::from("/small_file_root.nbtdoc"),
			include_str!("../../tests/small_file_root.nbtdoc")
		);
		fp.map.insert(
			PathBuf::from("/small_file_sibling.nbtdoc"),
			include_str!("../../tests/small_file_sibling.nbtdoc")
		);
		let mut root = Root::new();
		root.add_root_module("/small_file_root.nbtdoc", &fp)?;
		Ok(())
	}
}