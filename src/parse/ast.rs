
#[derive(Debug, PartialEq)]
pub struct NbtDocFile {
	pub uses: Vec<IdentPath>,
	pub compounds: Vec<(String, CompoundDef)>,
	pub enums: Vec<(String, EnumType)>,
	pub describes: Vec<(IdentPath, DescribeDef)>,
	pub mods: Vec<String>
}

#[derive(Debug, PartialEq)]
pub struct CompoundDef {
	pub description: String,
	pub fields: Vec<(String, Field)>,
	pub supers: Option<IdentPath>
}

#[derive(Debug, PartialEq)]
pub struct Field {
	pub description: String,
	pub field_type: FieldType
}

#[derive(Debug, PartialEq)]
pub enum Range<T> {
	Single(T),
	Low(T),
	High(T),
	Both(T, T)
}

#[derive(Debug, PartialEq)]
pub enum FieldType {
	BooleanType,
	NumberType(NumberPrimitiveType),
	StringType,
	ArrayType(NumberArrayType),
	ListType {
		item_type: Box<FieldType>,
		len_range: Option<Range<i32>>
	},
	NamedType(IdentPath)
}

#[derive(Debug, PartialEq)]
pub enum NumberPrimitiveType {
	Byte(Option<Range<i8>>),
	Short(Option<Range<i16>>),
	Int(Option<Range<i32>>),
	Long(Option<Range<i64>>),
	Float(Option<Range<f32>>),
	Double(Option<Range<f64>>)
}

#[derive(Debug, PartialEq)]
pub enum NumberArrayType {
	Byte {
		value_range: Option<Range<i8>>,
		len_range: Option<Range<i32>>
	},
	Int {
		value_range: Option<Range<i32>>,
		len_range: Option<Range<i32>>
	},
	Long {
		value_range: Option<Range<i64>>,
		len_range: Option<Range<i32>>
	}
}

#[derive(Debug, PartialEq)]
pub enum EnumType {
	Byte(EnumDef<i8>),
	Short(EnumDef<i16>),
	Int(EnumDef<i32>),
	Long(EnumDef<i64>),
	Float(EnumDef<f32>),
	Double(EnumDef<f64>),
	String(EnumDef<String>)
}

#[derive(Debug, PartialEq)]
pub struct EnumDef<T> {
	pub description: String,
	pub values: Vec<(String, EnumValue<T>)>
}

#[derive(Debug, PartialEq)]
pub struct EnumValue<T> {
	pub description: String,
	pub value: T
}

#[derive(Debug, PartialEq)]
pub enum DescribeType {
	Entities,
	Blocks,
	Items,
	Storage
}

#[derive(Debug, PartialEq)]
pub struct DescribeDef {
	pub describe_type: DescribeType,
	pub targets: Option<Vec<String>>
}

type IdentPath = Vec<PathPart>;

#[derive(Debug, PartialEq)]
pub enum PathPart {
	Root,
	Super,
	Regular(String)
}