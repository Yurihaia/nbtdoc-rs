#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

use std::collections::HashMap;

use super::arena::Index;
use crate::identifier::Identifier;

#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct EnumItem {
    pub et: EnumType,
    pub description: String,
}

#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub enum EnumType {
    Byte(HashMap<String, EnumOption<i8>>),
    Short(HashMap<String, EnumOption<i16>>),
    Int(HashMap<String, EnumOption<i32>>),
    Long(HashMap<String, EnumOption<i64>>),
    Float(HashMap<String, EnumOption<f32>>),
    Double(HashMap<String, EnumOption<f64>>),
    String(HashMap<String, EnumOption<String>>),
}

#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ItemIndex {
    Enum(Index<EnumItem>),
    Compound(Index<CompoundTag>),
    Module(Index<Module>),
}

#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    pub children: HashMap<String, ItemIndex>,
    pub parent: Option<Index<Module>>,
}

#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct CompoundTag {
    pub description: String,
    pub fields: HashMap<String, Field>,
    pub supers: Option<CompoundExtend>,
}

#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub enum CompoundExtend {
    Compound(Index<CompoundTag>),
    Registry {
        target: Identifier,
        path: Vec<FieldPath>,
    },
}

#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct Field {
    pub description: String,
    #[cfg_attr(feature = "serde", serde(default))]
    pub optional: bool,
    pub nbttype: NbtValue,
}

#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct NumberTag<T> {
    pub range: Option<Range<T>>,
}

#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct NumberArrayTag<T> {
    pub length_range: Option<Range<i32>>,
    pub value_range: Option<Range<T>>,
}

#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct Range<T>(pub T, pub T);

#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub struct EnumOption<T> {
    pub value: T,
    pub description: String,
}

#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Debug, Clone, PartialEq)]
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
        value_type: Box<NbtValue>,
    },
    Index {
        target: Identifier,
        path: Vec<FieldPath>,
    },
    Id(Identifier),
    Or(Vec<NbtValue>),
}

#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Debug, Clone, PartialEq)]
pub enum FieldPath {
    Super,
    Child(String),
}
