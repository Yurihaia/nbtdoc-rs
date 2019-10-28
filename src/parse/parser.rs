use nom::{
	branch::alt,
	bytes::complete::{
		escaped,
		tag,
		take_while,
		take_while1
	},
	character::complete::{
		one_of
	},
	combinator::{
		map,
		map_res,
		opt,
		not,
		peek,
		cut
	},
	error::{
		ErrorKind,
		ParseError,
		make_error
	},
	sequence::{
		delimited,
		preceded,
		separated_pair,
		terminated,
		tuple,
		pair
	},
	multi::{
		separated_list,
		many0,
		many1,
		separated_nonempty_list
	},
	number::complete::{
		double,
		float
	},
	IResult
};

use crate::validation::{
	FieldPath
};
use crate::identifier::Identifier;

use super::ast::*;

pub fn root<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, NbtDocFile, E> {
	map(
		many0(
			alt((
				map(
					preceded(sp, terminated(compound_def, sp)),
					RootItem::Compound
				),
				map(
					preceded(sp, terminated(enum_def, sp)),
					RootItem::Enum
				),
				map(
					preceded(sp, terminated(use_def, sp)),
					RootItem::Use
				),
				map(
					preceded(sp, terminated(describe_def, sp)),
					RootItem::Describe
				),
				map(
					preceded(sp, terminated(mod_def, sp)),
					|v| RootItem::Mod(String::from(v))
				)
			))
		),
		|v| {
			let mut out = NbtDocFile {
				uses: vec![],
				compounds: vec![],
				enums: vec![],
				describes: vec![],
				mods: vec![]
			};
			for e in v {
				match e {
					RootItem::Compound(v) => out.compounds.push(v),
					RootItem::Enum(v) => out.enums.push(v),
					RootItem::Describe(v) => out.describes.push(v),
					RootItem::Use(v) => out.uses.push(v),
					RootItem::Mod(v) => out.mods.push(v)
				}
			}
			out
		}
	)(i)
}

enum RootItem {
	Compound((String, CompoundDef)),
	Describe((Vec<PathPart>, DescribeDef)),
	Enum((String, EnumDef)),
	Use((bool, Vec<PathPart>)),
	Mod(String)
}

fn matches<'a, F, E: ParseError<&'a str>>(f: F) -> impl Fn(&'a str) -> IResult<&'a str, (), E>
	where F: Fn(char) -> bool {
	move |i: &'a str| {
		if f(i.chars().next().ok_or(nom::Err::Error(make_error(i, ErrorKind::Eof)))?) {
			Ok((&i[1..], ()))
		} else {
			Err(nom::Err::Error(make_error(i, ErrorKind::Tag)))
		}
	}
}

fn sp<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, (), E> {
	map(many0(
		alt((
			map(take_while1(|c: char| c.is_ascii_whitespace()), |_| ()),
			map(tuple((
				peek(not(tag("///"))),
				tag("//"),
				take_while(|c: char| c != '\n'),
				opt(tag("\n"))
			)), |_| ())
		))
	), |_| ())(i)
}

fn sp1<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, (), E> {
	map(many1(
		alt((
			map(take_while1(|c: char| c.is_ascii_whitespace()), |_| ()),
			map(tuple((
				peek(not(tag("///"))),
				tag("//"),
				take_while(|c: char| c != '\n'),
				opt(tag("\n"))
			)), |_| ())
		))
	), |_| ())(i)
}

fn integer<'a, T: std::str::FromStr, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, T, E> {
	alt((
		map_res(tag("0"), str::parse),
		map_res(tuple((
			opt(tag("-")),
			one_of("123456789"),
			take_while(|c: char| c.is_ascii_digit())
		)), |(neg, f, t)| format!("{}{}{}", neg.unwrap_or(""), f, t).parse())
	))(i)
}

fn natual<'a, T: std::str::FromStr, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, T, E> {
	alt((
		map_res(tag("0"), str::parse),
		map_res(tuple((
			one_of("123456789"),
			take_while(|c: char| c.is_ascii_digit())
		)), |(f, t)| format!("{}{}", f, t).parse())
	))(i)
}

fn range<'a, F, T, E: ParseError<&'a str>>(f: F) -> impl Fn(&'a str) -> IResult<&'a str, Range<T>, E>
	where F: Fn(&'a str) -> IResult<&'a str, T, E> + Copy {
	alt((
		map(separated_pair(
			terminated(f, sp),
			tag(".."),
			preceded(sp, f)
		), |(l, u)| Range::Both(l, u)),
		map(preceded(
			tag(".."),
			preceded(sp, f)
		), Range::High),
		map(terminated(
			terminated(f, sp),
			tag("..")
		), Range::Low),
		map(f, Range::Single)
	))
}

fn int_range<'a, T: std::str::FromStr, E: ParseError<&'a str>>(
	i: &'a str
) -> IResult<&'a str, Range<T>, E> {
	range(integer::<T, E>)(i)
}

fn natural_range<'a, T: std::str::FromStr, E: ParseError<&'a str>>(
	i: &'a str
) -> IResult<&'a str, Range<T>, E> {
	range(natual::<T, E>)(i)
}

fn float_range<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, Range<f32>, E> {
	range(float)(i)
}

fn double_range<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, Range<f64>, E> {
	range(double)(i)
}

fn number_type<'a, E: ParseError<&'a str>>(
	ntype: &'static str
) -> impl Fn(&'a str) -> IResult<&'a str, NumberPrimitiveType, E> {
	let atbind = |v| tuple((sp, tag("@"), sp))(v);
	preceded(
		tag(ntype),
		move |i| match ntype {
			"byte" => map(
				opt(preceded(atbind, cut(int_range::<i8, E>))),
				NumberPrimitiveType::Byte
			)(i),
			"short" => map(
				opt(preceded(atbind, cut(int_range::<i16, E>))),
				NumberPrimitiveType::Short
			)(i),
			"int" => map(
				opt(preceded(atbind, cut(int_range::<i32, E>))),
				NumberPrimitiveType::Int
			)(i),
			"long" => map(
				opt(preceded(atbind, cut(int_range::<i64, E>))),
				NumberPrimitiveType::Long
			)(i),
			"float" => map(
				opt(preceded(atbind, cut(float_range))),
				NumberPrimitiveType::Float
			)(i),
			"double" => map(
				opt(preceded(atbind, cut(double_range))),
				NumberPrimitiveType::Double
			)(i),
			_ => panic!()
		}
	)
}

macro_rules! array_map {
	($i:expr, $id:ident, $t:ty, $atbind:expr) => {
		map(
			pair(
				opt(preceded($atbind, cut(int_range::<$t, E>))),
				preceded(
					tuple((sp, tag("["), sp, tag("]"))),
					opt(preceded(pair(sp, $atbind), cut(natural_range::<i32, E>)))
				)
			),
			|(v, l)| NumberArrayType::$id {
				value_range: v,
				len_range: l
			}
		)($i)
	};
}

fn array_type<'a, E: ParseError<&'a str>>(
	ntype: &'static str
) -> impl Fn(&'a str) -> IResult<&'a str, NumberArrayType, E> {
	let atbind = |v| tuple((sp, tag("@"), sp))(v);
	preceded(
		tag(ntype),
		move |i| match ntype {
			"byte" => array_map!(i, Byte, i8, atbind),
			"int" => array_map!(i, Int, i32, atbind),
			"long" => array_map!(i, Long, i64, atbind),
			_ => panic!()
		}
	)
}

fn ident<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, &'a str, E> {
	preceded(
		peek(matches(|c| c.is_ascii_alphabetic() || c == '_')),
		take_while(|c: char| c.is_ascii_alphanumeric() || c == '_')
	)(i)
}

fn ident_path<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, Vec<PathPart>, E> {
	map(
		pair(
			// to handle a root path
			opt(tag("::")),
			separated_nonempty_list(tag("::"), ident)
		),
		|(r, l): (Option<&'a str>, Vec<&'a str>)| {
			let mut out = vec![];
			if r.is_some() {
				out.push(PathPart::Root)
			}
			out.extend(l.into_iter().map(|v: &'a str| match v {
				"super" => PathPart::Super,
				_ => PathPart::Regular(String::from(v))
			}).collect::<Vec<_>>());
			out
		}
	)(i)
}

fn field_type<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, FieldType, E> {
	alt((
		map(tag("boolean"), |_| FieldType::BooleanType),
		map(tag("string"), |_| FieldType::StringType),
		// arrays need to be done first because they contain extra characters
		map(array_type("byte"), FieldType::ArrayType),
		map(array_type("int"), FieldType::ArrayType),
		map(array_type("long"), FieldType::ArrayType),
		// and number types are a subset of array types in the syntax
		map(number_type("byte"), FieldType::NumberType),
		map(number_type("short"), FieldType::NumberType),
		map(number_type("int"), FieldType::NumberType),
		map(number_type("long"), FieldType::NumberType),
		map(number_type("float"), FieldType::NumberType),
		map(number_type("double"), FieldType::NumberType),
		map(pair(
			delimited(
				pair(tag("["), sp),
				cut(field_type),
				pair(sp, tag("]"))
			),
			opt(preceded(tuple((sp, tag("@"), sp)), cut(natural_range::<i32, E>)))
		), |(f, r)| FieldType::ListType {
			item_type: Box::new(f),
			len_range: r
		}),
		map(
			pair(
				terminated(mc_ident, sp),
				delimited(
					pair(tag("["), sp),
					field_path,
					pair(sp, tag("]"))
				)
			),
			|(d, p)| FieldType::IndexType {
				target: d,
				path: p
			}
		),
		map(
			preceded(
				pair(tag("id"), sp), 
				delimited(
					pair(tag("("), sp),
					mc_ident,
					pair(sp, tag(")"))
				)
			),
			FieldType::IdType
		),
		map(ident_path, FieldType::NamedType),
		map(
			delimited(
				pair(tag("("), sp),
				separated_list(
					tuple((sp, tag("|"), sp)),
					field_type
				),
				pair(sp, tag(")"))
			),
			FieldType::OrType
		)
	))(i)
}

fn field_path<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, Vec<FieldPath>, E> {
	map(
		separated_nonempty_list(
			tag("."),
			ident
		),
		|v| v.into_iter().map(|x| match x {
			"super" => FieldPath::Super,
			x => FieldPath::Child(String::from(x))
		}).collect()
	)(i)
}

fn key<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, &'a str, E> {
	alt((
		quoted_str,
		ident
	))(i)
}

fn quoted_str<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, &'a str, E> {
	delimited(
		tag("\""),
		cut(escaped(
			matches(|c: char| !c.is_control() && c != '"' && c != '\\'),
			'\\',
			one_of("\\\"bfnrt")
		)),
		cut(tag("\""))
	)(i)
}

fn doc_comment<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, String, E> {
	map(
		many0(preceded(
			tag("///"),
			terminated(
				take_while(|c| c != '\n'),
				opt(pair(tag("\n"), sp))
			)
		)),
		|v: Vec<&'a str>| v.join("\n")
	)(i)
}

fn compound_def<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, (String, CompoundDef), E> {
	map(
		tuple((
			terminated(doc_comment, sp),
			preceded(pair(tag("compound"), sp1), cut(tuple((
				ident,
				opt(preceded(tuple((sp1, tag("extends"), sp1)), cut(ident_path))),
				preceded(sp, delimited(
					pair(tag("{"), sp),
					separated_list(
						tuple((sp, tag(","), sp)),
						separated_pair(
							pair(
								terminated(doc_comment, sp),
								map(key, String::from)
							),
							tuple((sp, tag(":"), sp)),
							field_type
						)
					),
					pair(sp, tag("}"))
				))
			)))),
		)),
		|(desc, (id, e, v))| (String::from(id), CompoundDef {
			description: desc,
			supers: e,
			fields: v.into_iter().map(|((dc, k), ft)| (k, Field {
				description: dc,
				field_type: ft
			})).collect()
		})
	)(i)
}

fn enum_def<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, (String, EnumDef), E> {
	alt((
		enum_p("byte"),
		enum_p("short"),
		enum_p("int"),
		enum_p("long"),
		enum_p("float"),
		enum_p("double"),
		enum_p("string")
	))(i)
}

macro_rules! enum_map {
	($desc:expr, $fields:expr, $i:expr, $id:ident) => {
		($i, EnumDef {
			description: $desc,
			values: EnumType::$id($fields.into_iter().map(|((desc, id), v)| (id, EnumValue {
				description: desc,
				value: match v {
					EnumVal::$id(v) => v,
					_ => panic!("Something is very wrong")
				}
			})).collect())
		})
	};
}

fn enum_p<'a, E: ParseError<&'a str>>(
	etype: &'static str
) -> impl Fn(&'a str) -> IResult<&'a str, (String, EnumDef), E> {
	map(
		pair(
			terminated(doc_comment, sp),
			preceded(
				pair(tag("enum"), sp),
				preceded(
					tuple((
						sp,
						delimited(
							pair(tag("("), sp),
							tag(etype),
							pair(sp, tag(")"))
						),
						sp
					)),
					cut(pair(
						map(ident, String::from),
						preceded(sp, delimited(
							pair(tag("{"), sp),
							separated_list(
								tuple((sp, tag(","), sp)),
								separated_pair(
									pair(terminated(doc_comment, sp), map(ident, String::from)),
									tuple((sp, tag("="), sp)),
									move |i| match etype {
										"byte" => map(integer::<i8, E>, EnumVal::Byte)(i),
										"short" => map(integer::<i16, E>, EnumVal::Short)(i),
										"int" => map(integer::<i32, E>, EnumVal::Int)(i),
										"long" => map(integer::<i64, E>, EnumVal::Long)(i),
										"float" => map(float, EnumVal::Float)(i),
										"double" => map(double, EnumVal::Double)(i),
										"string" => map(quoted_str, |v| EnumVal::String(String::from(v)))(i),
										_ => panic!("Something is very wrong")
									}
								)
							),
							pair(sp, tag("}"))
						))
					)),
				)
			)
		),
		move |(desc, (id, fields))| match etype {
			"byte" => enum_map!(desc, fields, id, Byte),
			"short" => enum_map!(desc, fields, id, Short),
			"int" => enum_map!(desc, fields, id, Int),
			"long" => enum_map!(desc, fields, id, Long),
			"float" => enum_map!(desc, fields, id, Float),
			"double" => enum_map!(desc, fields, id, Double),
			"string" => enum_map!(desc, fields, id, String),
			_ => panic!("Something is very wrong")
		}
	) 
}

enum EnumVal {
	Byte(i8),
	Short(i16),
	Int(i32),
	Long(i64),
	Float(f32),
	Double(f64),
	String(String)
}

fn mc_ident<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, Identifier, E> {
	map(
		tuple((
			take_while1(|c: char| c.is_ascii_lowercase() | "-_".contains(c)),
			tag(":"),
			separated_nonempty_list(
				tag("/"),
				take_while1(|c: char| c.is_ascii_lowercase() | "-_".contains(c))
			)
		)),
		|(v, _, p)| Identifier::new(
			String::from(v),
			p.join("/")
		)
	)(i)
}

fn describe_def<'a, E: ParseError<&'a str>>(
	i: &'a str
) -> IResult<&'a str, (Vec<PathPart>, DescribeDef), E> {
	map(
		tuple((
			terminated(ident_path, pair(sp1, tag("describes"))),
			cut(pair(
				preceded(sp1, mc_ident),
				preceded(sp, terminated(
					opt(delimited(
						pair(tag("["), sp),
						cut(separated_list(
							tuple((sp, tag(","), sp)),
							mc_ident
						)),
						pair(sp, tag("]"))
					)),
					pair(sp, tag(";"))
				))
			)),
		)),
		|(id, (t, v))| (id, DescribeDef {
			describe_type: t,
			targets: v
		})
	)(i)
}

fn use_def<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, (bool, Vec<PathPart>), E> {
	map(
		pair(
			opt(pair(tag("export"), sp1)),
			preceded(pair(tag("use"), sp1), cut(terminated(ident_path, pair(sp, tag(";")))))
		),
		|(x, v)| (x.is_some(), v)
	)(i)
}

fn mod_def<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, &'a str, E> {
	preceded(pair(tag("mod"), sp1), cut(terminated(ident, pair(sp, tag(";")))))(i)
}

#[cfg(test)]
mod tests {

	macro_rules! fs {
		($e:expr) => {
			String::from($e)
		};
	}

	macro_rules! id {
		($n:expr, $e:expr) => {
			Identifier {
				namespace: String::from($n),
				path: String::from($e)
			}
		}
	}

	use super::*;

	type NVE<'a> = (&'a str, ErrorKind);

	#[test]
	fn quoted_string() {
		assert_eq!(quoted_str::<NVE>(r#""Hello, World!""#), Ok(("", "Hello, World!")))
	}

	#[test]
	fn ident_num() {
		assert_eq!(ident::<NVE>("Hello027"), Ok(("", "Hello027")))
	}

	mod sp_tests {
		use super::*;
		#[test]
		fn ws() {
			assert_eq!(sp::<NVE>("  \t \n  \t"), Ok(("", ())))
		}

		#[test]
		fn comment_eof() {
			assert_eq!(sp::<NVE>("// hello world"), Ok(("", ())))
		}

		#[test]
		fn comment_nl() {
			assert_eq!(sp::<NVE>("// hello world\n"), Ok(("", ())))
		}

		#[test]
		fn both() {
			assert_eq!(sp::<NVE>(" \t\n// whats up\n   \r"), Ok(("", ())))
		}
	}

	mod field_type_tests {
		use super::*;

		#[test]
		fn string() {
			assert_eq!(field_type::<NVE>("string"), Ok(("", FieldType::StringType)));
		}
		
		#[test]
		fn number() {
			assert_eq!(field_type::<NVE>("double"), Ok(("", FieldType::NumberType(
				NumberPrimitiveType::Double(None)
			))));
		}

		#[test]
		fn number_range_both() {
			assert_eq!(field_type::<NVE>("int @ 0..100"), Ok(("", FieldType::NumberType(
				NumberPrimitiveType::Int(Some(
					Range::Both(0, 100)
				))
			))));
		}

		#[test]
		fn number_range_upper() {
			assert_eq!(field_type::<NVE>("float @ ..0.15"), Ok(("", FieldType::NumberType(
				NumberPrimitiveType::Float(Some(
					Range::High(0.15)
				))
			))));
		}

		#[test]
		fn number_range_lower() {
			assert_eq!(field_type::<NVE>("long @ -7.."), Ok(("", FieldType::NumberType(
				NumberPrimitiveType::Long(Some(
					Range::Low(-7)
				))
			))));
		}

		#[test]
		fn number_range_single() {
			assert_eq!(field_type::<NVE>("short @ 0"), Ok(("", FieldType::NumberType(
				NumberPrimitiveType::Short(Some(
					Range::Single(0)
				))
			))));
		}

		#[test]
		fn array_simple() {
			assert_eq!(field_type::<NVE>("int[]"), Ok(("", FieldType::ArrayType(
				NumberArrayType::Int {
					value_range: None,
					len_range: None
				}
			))));
		}

		#[test]
		fn array_len_range() {
			assert_eq!(field_type::<NVE>("int[] @ 0..4"), Ok(("", FieldType::ArrayType(
				NumberArrayType::Int {
					value_range: None,
					len_range: Some(Range::Both(0, 4))
				}
			))));
		}

		#[test]
		fn array_value_range() {
			assert_eq!(field_type::<NVE>("long @ ..20[]"), Ok(("", FieldType::ArrayType(
				NumberArrayType::Long {
					value_range: Some(Range::High(20)),
					len_range: None
				}
			))));
		}

		#[test]
		fn array_both_range() {
			assert_eq!(field_type::<NVE>("byte @ -20..30[] @ 4.."), Ok(("", FieldType::ArrayType(
				NumberArrayType::Byte {
					value_range: Some(Range::Both(-20, 30)),
					len_range: Some(Range::Low(4))
				}
			))));
		}

		#[test]
		fn list_simple() {
			assert_eq!(field_type::<NVE>("[boolean]"), Ok(("", FieldType::ListType {
				item_type: Box::from(FieldType::BooleanType),
				len_range: None
			})));
		}

		#[test]
		fn list_nested() {
			assert_eq!(field_type::<NVE>("[[[string]]]"), Ok(("", FieldType::ListType {
				item_type: Box::from(FieldType::ListType {
					item_type: Box::from(FieldType::ListType {
						item_type: Box::from(FieldType::StringType),
						len_range: None
					}),
					len_range: None
				}),
				len_range: None
			})));
		}

		#[test]
		fn list_range() {
			assert_eq!(field_type::<NVE>("[int] @ 0..5"), Ok(("", FieldType::ListType {
				item_type: Box::from(FieldType::NumberType(NumberPrimitiveType::Int(None))),
				len_range: Some(Range::Both(0, 5))
			})));
		}

		#[test]
		fn named_simple() {
			assert_eq!(field_type::<NVE>("FooBar"), Ok(("", FieldType::NamedType(vec![
				PathPart::Regular(fs!("FooBar"))
			]))))
		}

		#[test]
		fn named_path() {
			assert_eq!(field_type::<NVE>("super::module::FooBar"), Ok(("", FieldType::NamedType(vec![
				PathPart::Super,
				PathPart::Regular(fs!("module")),
				PathPart::Regular(fs!("FooBar"))
			]))))
		}

		#[test]
		fn id_type() {
			assert_eq!(field_type::<NVE>("id(minecraft:item)"), Ok(("", 
				FieldType::IdType(id!("minecraft", "item"))))
			)
		}

		#[test]
		fn path_index() {
			assert_eq!(field_type::<NVE>("minecraft:item[id.super.field]"), Ok(("", FieldType::IndexType {
				path: vec![
					FieldPath::Child(fs!("id")),
					FieldPath::Super,
					FieldPath::Child(fs!("field"))
				],
				target: id!("minecraft", "item")
			})))
		}

		#[test]
		fn or_type() {
			assert_eq!(field_type::<NVE>("(int | boolean | byte @ 0..1)"), Ok(("", FieldType::OrType(vec![
				FieldType::NumberType(NumberPrimitiveType::Int(None)),
				FieldType::BooleanType,
				FieldType::NumberType(NumberPrimitiveType::Byte(Some(
					Range::Both(0, 1)
				)))
			]))))
		}
	}

	mod describe_def_tests {
		use super::*;

		#[test]
		fn simple() {
			assert_eq!(
				describe_def::<NVE>("MyCompound describes minecraft:item[minecraft:stick, minecraft:tnt];"),
				Ok(("", (
					vec![PathPart::Regular(fs!("MyCompound"))],
					DescribeDef {
						describe_type: id!("minecraft", "item"),
						targets: Some(vec![
							id!("minecraft", "stick"),
							id!("minecraft", "tnt"),
						])
					}
				)))
			);
		}

		#[test]
		fn all() {
			assert_eq!(
				describe_def::<NVE>("MyCompound describes minecraft:block;"),
				Ok(("", (
					vec![PathPart::Regular(fs!("MyCompound"))],
					DescribeDef {
						describe_type: id!("minecraft", "block"),
						targets: None
					}
				)))
			);
		}
	}

	mod enum_def_tests {
		use super::*;

		#[test]
		fn simple() {
			assert_eq!(
				enum_def::<NVE>(r#"enum (int) MyEnum {
					VarOne = 1,
					VarTwo = 2,
					VarThree = 5
				}"#),
				Ok(("", (
					fs!("MyEnum"),
					EnumDef {
						description: fs!(""),
						values: EnumType::Int(vec![
							(fs!("VarOne"), EnumValue {
								description: fs!(""),
								value: 1
							}),
							(fs!("VarTwo"), EnumValue {
								description: fs!(""),
								value: 2
							}),
							(fs!("VarThree"), EnumValue {
								description: fs!(""),
								value: 5
							})
						])
					}
				)))
			);
		}

		#[test]
		fn doc_comments() {
			assert_eq!(
				enum_def::<NVE>(r#"/// My Enum
				enum (int) MyEnum {
					/// var one
					VarOne = 1,
					/// var two
					VarTwo = 2,
					VarThree = 5
				}"#),
				Ok(("", (
					fs!("MyEnum"),
					EnumDef {
						description: fs!(" My Enum"),
						values: EnumType::Int(vec![
							(fs!("VarOne"), EnumValue {
								description: fs!(" var one"),
								value: 1
							}),
							(fs!("VarTwo"), EnumValue {
								description: fs!(" var two"),
								value: 2
							}),
							(fs!("VarThree"), EnumValue {
								description: fs!(""),
								value: 5
							})
						])
					}
				)))
			);
		}
	}

	mod compound_def_tests {
		use super::*;

		#[test]
		fn simple() {
			assert_eq!(
				compound_def::<NVE>(r#"compound Foo {
					field_one: int,
					field_two: string
				}"#), 
				Ok(("",(
					fs!("Foo"),
					CompoundDef {
						description: String::new(),
						fields: vec![
							(fs!("field_one"), Field {
								description: fs!(""),
								field_type:  FieldType::NumberType(
									NumberPrimitiveType::Int(
										None
									)
								)
							}),
							(fs!("field_two"), Field {
								description: fs!(""),
								field_type:  FieldType::StringType
							})
						],
						supers: None
					}
				)))
			);
		}

		#[test]
		fn doc_comments() {
			assert_eq!(
				compound_def::<NVE>(r#"/// This is my compound
				compound Foo {
					/// Hello World
					field: byte
				}"#),
				Ok(("", (
					fs!("Foo"),
					CompoundDef {
						description: fs!(" This is my compound"),
						fields: vec![
							(
								fs!("field"),
								Field {
									description: fs!(" Hello World"),
									field_type:  FieldType::NumberType(
										NumberPrimitiveType::Byte(
											None
										)
									)
								}
							)
						],
						supers: None
					}
				)))
			);
		}

		#[test]
		fn extends() {
			assert_eq!(
				compound_def::<NVE>(r#"compound Foo extends some::module::Bar {
					field: boolean
				}"#),
				Ok(("", (
					fs!("Foo"),
					CompoundDef {
						description: fs!(""),
						supers: Some(vec![
							PathPart::Regular(fs!("some")),
							PathPart::Regular(fs!("module")),
							PathPart::Regular(fs!("Bar"))
						]),
						fields: vec![
							(
								fs!("field"),
								Field {
									description: fs!(""),
									field_type:  FieldType::BooleanType
								}
							)
						]
					}
				)))
			);
		}
	}

	#[test]
	fn parse_file() {
		assert_eq!(
			root::<NVE>(include_str!("../../tests/full_file.nbtdoc")),
			Ok(("", NbtDocFile {
				mods: vec![],
				uses: vec![
					(false, vec![
						PathPart::Regular(fs!("minecraft")),
						PathPart::Regular(fs!("entity"))
					]),
					(true, vec![
						PathPart::Regular(fs!("minecraft")),
						PathPart::Regular(fs!("entity")),
						PathPart::Regular(fs!("Villager"))
					])
				],
				compounds: vec![
					(fs!("Breedable"), CompoundDef {
						description: fs!(" A mob which can be bred. It has no other unique NBT"),
						fields: vec![
							(fs!("InLove"), Field {
								description: fs!(" If the animal has been fed"),
								field_type: FieldType::NumberType(NumberPrimitiveType::Int(None))
							}),
							(fs!("Age"), Field {
								description: fs!(" The age of the animal"),
								field_type: FieldType::NumberType(NumberPrimitiveType::Int(None))
							}),
							(fs!("ForcedAge"), Field {
								description: fs!(" The age of the animal. Will not increment"),
								field_type: FieldType::NumberType(NumberPrimitiveType::Int(None))
							}),
							(fs!("LoveCauseLeast"), Field {
								description: fs!(" The UUIDLeast of the player who fed the animal"),
								field_type: FieldType::NumberType(NumberPrimitiveType::Long(None))
							}),
							(fs!("LoveCauseMost"), Field {
								description: fs!(" The UUIDMost of the player who fed the animal"),
								field_type: FieldType::NumberType(NumberPrimitiveType::Long(None))
							})
						],
						supers: Some(vec![
							PathPart::Regular(fs!("entity")),
							PathPart::Regular(fs!("MobBase"))
						])
					}),
					(fs!("Sheep"), CompoundDef {
						description: fs!(""),
						fields: vec![
							(fs!("Sheared"), Field {
								description: fs!(""),
								field_type: FieldType::BooleanType
							}),
							(fs!("Color"), Field {
								description: fs!(""),
								field_type: FieldType::NamedType(vec![
									PathPart::Regular(fs!("Color"))
								])
							}),
						],
						supers: Some(vec![
							PathPart::Regular(fs!("Breedable"))
						])
					}),
					(fs!("Panda"), CompoundDef {
						description: fs!(""),
						fields: vec![
							(fs!("MainGene"), Field {
								description: fs!(" The displayed gene\n \
													If this gene is recessive \
													'(r)' and 'HiddenGene' is not \
													the same, the panda will display \
													the 'normal' gene"),
								field_type: FieldType::NamedType(vec![
									PathPart::Regular(fs!("Gene"))
								])
							}),
							(fs!("HiddenGene"), Field {
								description: fs!(" The hidden gene"),
								field_type: FieldType::NamedType(vec![
									PathPart::Regular(fs!("Gene"))
								])
							})
						],
						supers: Some(vec![
							PathPart::Regular(fs!("Breedable"))
						])
					})
				],
				enums: vec![
					(fs!("Color"), EnumDef {
						description: fs!(""),
						values: EnumType::Byte(vec![
							(fs!("White"), EnumValue {
								description: fs!(" etc."),
								value: 0
							})
						])
					}),
					(fs!("Gene"), EnumDef {
						description: fs!(""),
						values: EnumType::String(vec![
							(fs!("Normal"), EnumValue {
								description: fs!(" The normal gene (d)"),
								value: fs!("normal")
							}),
							(fs!("Lazy"), EnumValue {
								description: fs!(" The lazy gene (d)"),
								value: fs!("lazy")
							}),
							(fs!("Worried"), EnumValue {
								description: fs!(" The worried gene (d)"),
								value: fs!("worried")
							}),
							(fs!("Playful"), EnumValue {
								description: fs!(" The playful gene (d)"),
								value: fs!("playful")
							}),
							(fs!("Brown"), EnumValue {
								description: fs!(" The brown gene (r)"),
								value: fs!("brown")
							}),
							(fs!("Weak"), EnumValue {
								description: fs!(" The weak gene (r)"),
								value: fs!("weak")
							}),
							(fs!("Aggressive"), EnumValue {
								description: fs!(" The aggressive gene (d)"),
								value: fs!("aggressive")
							})
						])
					})
				],
				describes: vec![
					(vec![PathPart::Regular(fs!("Breedable"))], DescribeDef {
						describe_type: id!("minecraft", "entity"),
						targets: Some(vec![
							id!("minecraft", "cow"),
							id!("minecraft", "pig")
						])
					}),
					(vec![PathPart::Regular(fs!("Sheep"))], DescribeDef {
						describe_type: id!("minecraft", "entity"),
						targets: Some(vec![
							id!("minecraft", "sheep"),
						])
					}),
					(vec![PathPart::Regular(fs!("Panda"))], DescribeDef {
						describe_type: id!("minecraft", "entity"),
						targets: Some(vec![
							id!("minecraft", "panda"),
						])
					})
				]
			}))
		)
	}
}