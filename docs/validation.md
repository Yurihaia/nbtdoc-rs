# Validation

Validation of an NBT tag comes in two steps. The first step is to traverse through the compounds until you reach the correct
tag, then for the second step verify that the data is valid based on the compound.

### Traversal

Traversal of the resultant data structure is designed to be much easier than that of `mc-nbt-paths`.
The way the library references other items is through the `get_compound`, `get_enum`, and `get_module` methods on `Root`.
To use those functions, just pass in the `Index<CompoundTag>`, `Index<EnumItem>`, or `Index<Module>` respectively.
If you are using the serialized data structure, just index the array `compound_arena`, `enum_arena`, or `module_arena` 
respectively.
The fields of a compound tag are stored in an enum called `NbtValue`. The only types that can be traversed further is the 
`Compound` type, the `List` type, the `Index` type, and the `Or` type.
* For the `Compound` type, the field is just another `Index<CompoundTag>`, which points to another compound. If the
field is not present in the compound, then check the compound in the `supers` field, and repeat until there are no `supers`.
* For the `List` type, the item type is just in the field `value_type`
* For the `Index` type, to get the next item, access the registry specified by `target` with the value of the NBT at `path`.
	* To traverse the NBT *value* in accordance to `path`, for each element in `path`:
		* if the variant is `Super`, go to the parent of the current tag.
		* if the variant is `Child(v)`, go to the child of the current tag named `v`.
	* Note that you cannot go into lists through `Child`, as there is no way to get the index. When going up with `Super`, 
	go up to the next compound.
	* When you find the value that contains the id, use `Root::get_regitry_item` to get the next index. If that NBT value is 
	not present, you must use the registry default if it exists.
* For the `Or` type, check each `NbtValue` in order. If one succeeds, use that entry.

Repeat this for each part of the path, and at the end you should be at the correct tag. The validator should create an error 
if it needs to traverse an untraversable tag. Once it reaches the end, it may proceed onto the value validation.

### Value Validation

Given an `NbtValue` which is the type you are validating the tag against:
1. Check that the type of the tag is correct. The list of `NbtValue` variants to NBT types is as follows  

	| `NbtValue` Variant | NBT Type |  
	| ------------------ | -------- |
	| `Boolean` | `TagByte` |
	| `Byte` | `TagByte` |
	| `Short` | `TagShort` |
	| `Int` | `TagInt` |
	| `Long` | `TagLong` |
	| `Float` | `TagFloat` |
	| `Double` | `TagDouble` |
	| `String` | `TagString` |
	| `ByteArray` | `TagByteArray` |
	| `IntArray` | `TagIntArray` |
	| `LongArray` | `TagLongArray` |
	| `Compound` | `TagCompound` |
	| `Enum` | Based on type of enum |
	| `List` | `TagList` |
	| `Index` | `TagCompound` |
	| `Id` | `TagString` |
	| `Or` | Based on entries |
	* For `Enum`, get the `EnumItem.et`. The variant directly corresponds to the nbt value type
	* For `Or`, test each type. If none match, then the type doesn't match. If multiple types match, you will have to validate for each of those types.
2. Based on the type. All ranges are *top inclusive*
	* `Boolean`: Test that the value is either `1b`, `0b`, `true` or `false`.
	* `Byte`: Test that the value is within the range and optionally that it is not `true` or `false`.
	* `Short`, `Int`, `Long`, `Float`, `Double`, `String`: Test that the value is within the range.
	* `ByteArray`, `IntArray`, `LongArray`: Test that the all of the values is within the value range, and that the length 
	is within the length range.
	* `Compound`: Test that the fields of the value match the specified compound's fields. After all of the fields in the
	compound have been checked, check all of the fields in the `supers` compound that are **not** in the original compound.
	Repeat this as many times as nescessary until there are no more parent compounds.
	* `Enum`: Test that the value is one of the values listed in the enum.
	* `List`: Test that the all of the entries of the value match the specified value type and that the length of the list 
	is within the range.
	* `Index`: Test that the compound pointed to by the `Index` type matches the value.
	* `Id`: Test that the value is a valid ID of the specified target registry.
	* `Or`: Test that at least one of the entries matches the value.

## JSON Serialized Data

When traversing through the serialized form of the data, refer to the [TypeScript definition file](json_format.d.ts) for 
the details of the actual format. Most of the data is the same, except that using the `Index` struct needs to be done 
manually. Also note that the generic type of `Index<T>` is erased when serializing data, although it should not be ambigious
to know which arena to use, based on the context of the structure. Another caveat of using the JSON serialized data in a 
language that does not support 64 bit integers is the loss of precision for `i64` ranges and enums.