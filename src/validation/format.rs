use std::collections::HashMap;

use super::arena::Index;
use crate::identifier::Identifier;

#[derive(Debug)]
pub struct EnumItem {
	pub et: EnumType,
	pub description: String
}

#[derive(Debug)]
pub enum EnumType {
	Byte(Vec<EnumOption<i8>>),
	Short(Vec<EnumOption<i16>>),
	Int(Vec<EnumOption<i32>>),
	Long(Vec<EnumOption<i64>>),
	Float(Vec<EnumOption<f32>>),
	Double(Vec<EnumOption<f64>>),
	String(Vec<EnumOption<String>>)
}

#[derive(Copy, Clone, Debug)]
pub enum ItemIndex {
	Enum(Index<EnumItem>),
	Compound(Index<CompoundTag>),
	Module(Index<Module>)
}

#[derive(Debug)]
pub struct Module {
	pub children: HashMap<String, ItemIndex>,
	pub parent: Option<Index<Module>>
}

#[derive(Debug)]
pub struct CompoundTag {
	pub description: String,
	pub fields: HashMap<String, Field>,
	pub supers: Option<CompoundExtend>
}

#[derive(Debug)]
pub enum CompoundExtend {
	Comound(Index<CompoundTag>),
	Registry {
		target: Identifier,
		path: Vec<FieldPath>
	}
}

#[derive(Debug)]
pub struct Field {
	pub description: String,
	pub nbttype: NbtValue
}

#[derive(Debug)]
pub struct NumberTag<T> {
	pub range: Option<Range<T>>
}

#[derive(Debug)]
pub struct NumberArrayTag<T> {
	pub length_range: Option<Range<i32>>,
	pub value_range: Option<Range<T>>
}

#[derive(Debug)]
pub struct Range<T>(pub T, pub T);

#[derive(Debug)]
pub struct EnumOption<T> {
	pub name: String,
	pub value: T,
	pub description: String
}

#[derive(Debug)]
pub enum NbtValue {
	Boolean,
	Byte(NumberTag<i8>),
	Short(NumberTag<i16>),
	Int(NumberTag<i32>),
	Long(NumberTag<i64>),
	Float(NumberTag<f32>),
	Double(NumberTag<f64>),
	String,
	ByteArray(NumberArrayTag<i8>),
	IntArray(NumberArrayTag<i32>),
	LongArray(NumberArrayTag<i64>),
	Compound(Index<CompoundTag>),
	Enum(Index<EnumItem>),
	List {
		length_range: Option<Range<i32>>,
		value_type: Box<NbtValue>
	},
	Index {
		target: Identifier,
		path: Vec<FieldPath>
	},
	Id(Identifier),
	Or(Vec<NbtValue>)
}

#[derive(Debug, PartialEq, Eq)]
pub enum FieldPath {
	Super,
	Child(String)
}