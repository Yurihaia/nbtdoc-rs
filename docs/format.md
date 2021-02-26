# NBTDoc Format

The NBTDoc format is a format for documenting NBT structures. It has many similarities to Rust's type definitions.  

## Syntax Syntax

| Symbol | Meaning |
|--------|---------|
| `str` | literal |
| A<sup>\*</sup> | A zero or more times |
| A<sup>+</sup> | A one or more times |
| A<sup>?</sup> | A zero or one times |
| A \| B | Either A or B |
| [ `A` `B` `C` ] | One of the literals `A`, `B`, or `C` |
| [ `A`-`Z` ] | Any literal from `A` to `Z` |
| ( A ) | General grouping |
| [NAME](#Syntax-Syntax) | A referenced token rule |
| [_Name_](#Syntax-Syntax) | A referenced parser rule |
| **\n** | A line ending |


## File Structure

An NBTDoc file is made up of a list of compound definitions, enum definitions, module declarations,
`use` clauses, and `describes` clauses.  
  
The Following Section describes syntax in two forms. the syntax preceded with **SYNTAX (TOKEN)** cannot have any whitespace or tokens in between the indiviual "rules". The syntax preceded with **SYNTAX** can have whitespace and standard comments (starting with `//`, but not `///`, and going until the end of the line). The full file has this syntax:

> **SYNTAX**\
> _File_:\
> &nbsp; &nbsp; (\
> &nbsp; &nbsp; &nbsp; [_CompoundDef_](#Compound-Definition)\
> &nbsp; &nbsp; | [_EnumDef_](#Enum-Definition)\
> &nbsp; &nbsp; | [_ModDecl_](#Module-Declaration)\
> &nbsp; &nbsp; | [_UseClause_](#Use-Clause)\
> &nbsp; &nbsp; | [_DescribeClause_](#Describe-Clause)\
> &nbsp; &nbsp; | [_InjectClause_](#Inject-Clause)\
> &nbsp; &nbsp; )<sup>\*</sup>

### Common Syntax

#### Identifier
> **SYNTAX (TOKEN)**\
> IDENT: \
> &nbsp; &nbsp; [ `A`-`Z` `a`-`z` `_` ] [ `A`-`Z` `a`-`z` `0`-`9` `_` ]<sup>\*</sup>

An identifier is the main form of names in the NBTDoc format.

#### Identifier Path
> **SYNTAX (TOKEN)**\
> IDENT_PATH: \
> &nbsp; &nbsp; ( `::` )<sup>?</sup>
>               ( [IDENT](#Identifier) | `super` )
>               ( `::` ( [IDENT](#Identifier) | `super` ) )<sup>\*</sup>

An identifier path is the way to reference items from another module. If it starts with `::`, the resultant path will start at the global root, not the "crate" root. When one of the parts of the path is `super`, it will reference the parent module.

#### Minecraft Identifier
> **SYNTAX (TOKEN)**\
> MINECRAFT_IDENT:\
> &nbsp; &nbsp; [ `a` - `z` `_` `-` ]<sup>\*</sup>
>               `:` [ `a`-`z` `_` `-` ]<sup>\*</sup>
>               ( `/` [ `a`-`z` `_` `-` ]<sup>\*</sup> )<sup>\*</sup>

Minecraft identifiers are just namespaced paths, just like the namespaced resource path in minecraft

#### Integer
> **SYNTAX (TOKEN)**\
> INTEGER:\
> &nbsp; &nbsp; `0` | `-`<sup>?</sup> [ `1`-`9` ] [ `0`-`9` ]<sup>\*</sup>

A signed integer

#### Float
> **SYNTAX (TOKEN)**\
> FLOAT:\
> &nbsp; &nbsp; `-`<sup>?</sup> [ `0`-`9` ]<sup>+</sup> FLOAT_EXPONENT<sup>?</sup>\
> &nbsp;&nbsp;| `-`<sup>?</sup> [ `0`-`9` ]<sup>\*</sup> `.` [ `0`-`9` ]<sup>+</sup> FLOAT_EXPONENT<sup>?</sup>
> 
> FLOAT_EXPONENT:\
> &nbsp; &nbsp; [ `e` `E` ] [ `-` `+` ]<sup>?</sup> [ `0`-`9` ]<sup>+</sup> 

A signed floating point number

#### String
> **SYNTAX (TOKEN)**\
> STRING:\
> &nbsp; &nbsp; `"` ( ~[ `"` `\` *UNICODE_CC* ]
>               | `\` [ `b` `n` `f` `r` `t` `\` `"` ] )<sup>\*</sup> `"`

A string literal. Cannot contain any control characters (*UNICODE_CC*). Can be escaped with a `\`, and the valid escape characters are `\b`, `\n`, `\f`, `\r`, `t`, `\\`, `\"`.

#### Doc Comment
> **SYNTAX**\
> _DocComments_:\
> &nbsp; &nbsp; DOC_COMMENT<sup>\*</sup>
> 
> **SYNTAX (TOKEN)**\
> DOC_COMMENT:\
> &nbsp; &nbsp; `///` ~[ **\n** ]<sup>\*</sup> ( **\n** | **EOF** )

Doc comments are the way to attatch descriptions to parts of the format. Each doc comment takes up the whole line, and multiple will get concatenated together to make the description. Doc comments should use Markdown for syntax, but don't have to

### Compound Definition
> **SYNTAX**\
> _CompoundDef_:\
> &nbsp; &nbsp; [_DocComments_](#Doc-Comment)
>               `compound` [IDENT](#Identifier) _ExtendClause_<sup>?</sup> `{` CompoundFields<sup>?</sup> `}`
> 
> _ExtendClause_:\
> &nbsp; &nbsp; `extends` ( [IDENT_PATH](#Identifier-Path) | [REGISTRY_INDEX](#Registry-Index) )
> 
> _CompoundField_:\
> &nbsp; &nbsp; [_DocComments_](#Doc-Comment) KEY `:` [_FieldType_](#Field-Type)
> 
> _CompoundFields_:\
> &nbsp; &nbsp; _CompoundField_ ( `,` _CompoundField_ )<sup>\*</sup>
> 
> **SYNTAX (TOKEN)**\
> KEY:\
> &nbsp; &nbsp; [IDENT](#Identifier) | [STRING](#String)

Compound Definitions are the way to describe patterns of compound tags.
The compound itself can have doc comments, and each field can have doc comments.
The `extends` clause is used to create inheritance, and it can be another named compound or a registry index.
Each key can be an identifer, or a string in the case that the NBT key cannot be expressed as an identifier.

#### Field Type

> **SYNTAX**\
> _FieldType_:\
> &nbsp;&nbsp;  `byte` _IntRange_ `[` `]` _UnsignedRange_ &nbsp; &nbsp; // Byte Array \
> &nbsp;&nbsp;| `int` _IntRange_ `[` `]` _UnsignedRange_ &nbsp; &nbsp; // Int Array \
> &nbsp;&nbsp;| `long` _IntRange_ `[` `]` _UnsignedRange_ &nbsp; &nbsp; // Long Array \
> &nbsp;&nbsp;| `boolean` &nbsp; &nbsp; // Byte (0b or 1b) \
> &nbsp;&nbsp;| `byte` _IntRange_ &nbsp; &nbsp; // Byte \
> &nbsp;&nbsp;| `short` _IntRange_ &nbsp; &nbsp; // Short \
> &nbsp;&nbsp;| `int` _IntRange_ &nbsp; &nbsp; // Int \
> &nbsp;&nbsp;| `long` _IntRange_ &nbsp; &nbsp; // Long \
> &nbsp;&nbsp;| `float` _FloatRange_ &nbsp; &nbsp; // Float \
> &nbsp;&nbsp;| `double` _FloatRange_ &nbsp; &nbsp; // Double \
> &nbsp;&nbsp;| `string` &nbsp; &nbsp; // String \
> &nbsp;&nbsp;| `[` _FieldType_ `]` _UnsignedRange_ &nbsp; &nbsp; // List \
> &nbsp;&nbsp;| [_RegistryIndex_](#Registry-Index) &nbsp; &nbsp; // Compound (dynamically indexed) \
> &nbsp;&nbsp;| `id` `(` [MINECRAFT_IDENT](#Minecraft-Identifier) `)` &nbsp; &nbsp; // String (with id validation) \
> &nbsp;&nbsp;| [IDENT_PATH](#Identifier-Path) &nbsp; &nbsp; // Compound or Enum \
> &nbsp;&nbsp;| `(` ( _FieldType_ ( `|`  _FieldType_ )<sup>\*</sup> )<sup>?</sup> `)` &nbsp; &nbsp; // Union type
> 
> _IntRange_:\
> &nbsp; &nbsp; `@` ( [INTEGER](#Integer) `..` [INTEGER](#Integer)<sup>?</sup>\
> &nbsp;&nbsp;| `..`<sup>?</sup> [INTEGER](#Integer) )
> 
> _UnsignedRange_:\
> &nbsp; &nbsp; `@` ( ( `0` | [ `1`-`9` ] [ `0`-`9` ] <sup>\*</sup> ) `..` ( `0` | ( [ `1`-`9` ] [ `0`-`9` ] <sup>\*</sup> ) )<sup>?</sup>\
> &nbsp;&nbsp;| `..`<sup>?</sup> ( `0` | [ `1`-`9` ][ `0`-`9` ] )<sup>\*</sup> )
> 
> _FloatRange_:\
> &nbsp; &nbsp; `@` ( [FLOAT](#Float) `..` [FLOAT](#Float)?\
> &nbsp;&nbsp;| `..`<sup>?</sup> [Float](#Float) )

**Field Types**

The field types describes all of the available NBT types.  
All of the array types have an integer range and a natural range. The integer is range is the range which all of the items 
have to be within, and the natural range is the range of the length of the list.  
The byte, short, int, long, float, and double all have their respective ranges that the value must be within.  
The list type contains another field type, which is the lists item, and a natural range which the length of the list must be 
within.  
The dynamically indexed compound accesses the registry specified by the Minecraft identifier with the ID at the path 
described in the square brackets.  
The string with id validation must be a valid minecraft identifier and must be in the registry described by the Minecraft 
identifier in the parenthesis.  
The compound or enum is just the name of a compound or enum.
It can be defined in the current file or in another file, with it being accessed by the indentifier path.
If the item that the name points to is a compound, the value should be a compound, otherwise if the item is an enum,
the type of the value should be the type of the enum.
The union type allows for a field that can have different types. These types must be distinguishable.
If the union type has no field types inside of it, it signifies that the field does not exist.  

**Ranges**

The ranges are almost identical to minecraft's range syntax.
They are preceded with an `@`, and they can be in one of the four forms:
* *VALUE_1* `..` *VALUE_2* - *VALUE_1* <= x <= *VALUE_2*
* *VALUE* `..` - *VALUE* <= x
* `..` *VALUE* - x <= *Value*
* *VALUE* - x = *Value*

Note that these ranges are *always* inclusive.  
There are 3 types of ranges
* Integer ranges, where the values may be signed integers.  
* Unsigned integer ranges, where the values may be unsigned integers.  
* Float ranges, where the values may be signed floating point numbers.

##### Registry Index

> **SYNTAX**\
> _RegistryIndex_:\
> &nbsp; &nbsp; [MINECRAFT_IDENT](#Minecraft-Identifier) `[` FIELD_PATH `]`
> 
> **SYNTAX (TOKEN)**\
> FIELD_PATH:\
> &nbsp; &nbsp; FIELD_PATH_KEY ( `.` FIELD_PATH_KEY )<sup>\*</sup>
> 
> FIELD_PATH_KEY:\
> &nbsp; &nbsp; `super` | [IDENT](#Identifier) | [STRING](#String)

Registry indexes are the way to get dynamic data. They can be used as a field value or in a compound's `extends` clause.
When the field path key is `super` the current tag's parent will be accessed.

### Enum Definition

> **SYNTAX**\
> _EnumDef_:\
> &nbsp; &nbsp; [_DocComments_](#Doc-Comment)
>               `enum` `(` ENUM_TYPE `)` [IDENT](#Identifier) `{` _EnumField_ ( `,` _EnumField_ )<sup>\*</sup> `}`
> 
> _EnumField_:\
> &nbsp; &nbsp; [_DocComments_](#Doc-Comment) [IDENT](#Identifier) `=`
>               [INTEGER](#Integer) | [FLOAT](#Float) | [STRING](#String)
> 
> **SYNTAX (TOKEN)**\
> ENUM_TYPE:\
> &nbsp; &nbsp; `byte` | `short` | `int` | `long` | `string` | `float` | `double`

Enums are the primary way to associate limited data with understandable names.
Each enum has a number of fields that the value must be one of. The data of the enum must match the type of the enum.
* byte - integer
* short - integer
* int - integer
* long - integer
* string - string
* float - float
* double - float

If a value has an invalid type it will not parse

### Module Declaration

> **SYNTAX**\
> _ModDecl_:\
> &nbsp; &nbsp; `mod` [IDENT](#Identifier) `;`

Module declarations describe what children modules this module has.
If this current file is a `mod.nbtdoc` file, then the children will be accessed as siblings (in `./`).
If this file has another name (`ex.nbtdoc`), then all of the children modules will be checked for in `./ex/`.  
The name of the file is the identifier in the delcaration.
For a `mod IDENT;`, if `IDENT.nbtdoc` cannot be found, then the file `IDENT/mod.nbtdoc` will be read from.

### Use Clause

> **SYNTAX**\
> _UseClause_:\
> &nbsp; &nbsp; `export`<sup>?</sup> `use` [IDENT_PATH](#Identifier-Path) `;`

The use clause is the recommended way to import external names. The target must be a compound or an enum.
If the `export` keyword is in front, then the imported name will be re-exported along with the rest of the names in this file. This is primarily useful for utility modules.

### Describe Clause

> **SYNTAX**\
> _DecribeClause_:\
> &nbsp; &nbsp; [IDENT_PATH](#Identifier-Path) `describes` [MINECRAFT_IDENT](#Minecraft-Identifier)
>               (
>                 `[` [MINECRAFT_IDENT](#Minecraft-Identifier)
>                     ( `,` [MINECRAFT_IDENT](#Minecraft-Identifier) )<sup>\*</sup>
>                 `]`
>               )<sup>?</sup> `;`

The describe clause is the way to interface between the compound definitions and registrys.
The ident path is the name of the target item,which will be the one associated with the objects inside the registry 
specified by the Minecraft identifier outside the sqaure brackets.
The Minecraft identifiers inside the square brackets will be mapped to the specified compound.
If the square brackets and idenitifers inside of them are not present,
the specified compound will be used as the default for the registry.

### Inject Clause

> **SYNTAX**\
> _InjectClause_:\
> &nbsp; &nbsp; `inject` ( _CompoundInject_ | _EnumInject_ )
> 
> _CompoundInject_:\
> &nbsp; &nbsp; `compound` [IDENT_PATH](#Identifier-Path) `{` [_CompoundFields_](#Compound-Definition)<sup>?</sup> `}`
> 
> _EnumInject_:\
> &nbsp; &nbsp; `enum` `(` [ENUM_TYPE](#Enum-Definition) `)`
>               [IDENT_PATH](#Identifier-Path) `{`
>               [_EnumField_](#Identifier-Path) ( `,` [_EnumField_](#Identifier-Path) )<sup>\*</sup> `}`

The inject clause gives support for injecting data into other items.
The specified fields will be inserted to the targeted item. The item targeted **does not** have to be loaded in yet,
and injects will be loaded retroactively. If two injects target the same item, the order they will be applied is not defined,
and modules should avoid injecting to the same place twice. The injected field will *always* overwrite the source field
however.
