use core::fmt::{Debug, Formatter, Result, Write};

#[derive(Debug)]
pub struct TypeDef {
	pub name: String,
	pub kind: TypeKind
}

#[derive(Copy, Clone)]
pub struct TypeDefIndex {
	pub index: usize
}

impl Debug for TypeDefIndex {
	fn fmt(&self, f: &mut Formatter <'_>) -> Result {
		match Type::type_list() {
			TypeList::Raw(raw) => raw[self.index].as_backed().fmt(f)
		}
	}
}

#[derive(Debug)]
pub enum TypeKind {
	Enum {
		variants: Vec <EnumVariant>
	},
	Struct {
		fields: Vec <StructField>
	}
}

#[derive(Debug)]
pub struct EnumVariant {
	pub name: String,
	pub data: Vec <Type>
}

#[derive(Debug)]
pub struct StructField {
	pub name: String,
	pub ty: Type
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct TypePointers {
	/// How many pointers are on a type
	///
	/// **T -> 2
	/// *^^**^T -> 6
	/// T -> 0
	pub len: u8,

	/// How much of those pointers are mutable
	/// **T -> 0b00
	/// ^T -> 0b1
	/// *^^**^T -> 0b011001
	pub muts: u8
}

impl TypePointers {
	pub fn from(str: &str) -> Self {
		if str.len() > 8 {
			panic!("no more than 8 pointers at a time are allowed on a type")
		}

		let len = str.len() as u8;
		let mut muts = 0b0000_0000;
		for (idx, char) in str.chars().enumerate() {
			if char == '^' {
				muts |= 1 << idx
			}
		}

		Self {
			len,
			muts
		}
	}
}

#[derive(Eq, PartialEq, Clone)]
pub enum Type {
	Scalar {
		index: usize,
		ptrs: TypePointers
	},
	Tuple { types: Vec <Type> }
}

impl Debug for Type {
	fn fmt(&self, f: &mut Formatter <'_>) -> Result {
		match Self::type_list() {
			TypeList::Raw(raw) => match self {
				Self::Scalar {
					index,
					ptrs
				} => {
					for i in 0..ptrs.len {
						let is_mutable = ((ptrs.muts >> i) & 1) != 0;
						f.write_char(if is_mutable {
							'^'
						} else {
							'*'
						})?
					}
					f.write_str(raw[*index].name())
				},
				Self::Tuple { types } => {
					f.write_char('(')?;
					for ty in types.iter().rev().skip(1).rev() {
						ty.fmt(f)?;
						f.write_str(", ")?
					}
					if let Some(last) = types.last() {
						last.fmt(f)?;
						if types.len() == 1 {
							f.write_char(',')?
						}
					}
					f.write_char(')')
				}
			}
		}
	}
}

impl Type {
	pub const UNIT_TUPLE: Type = Type::Tuple { types: vec![] };

	pub fn type_list() -> &'static mut TypeList {
		static mut TYPE_LIST: TypeList = TypeList::Raw(vec![]);
		unsafe { &mut TYPE_LIST }
	}

	pub fn raw() -> &'static mut Vec <RawType> {
		match Self::type_list() {
			TypeList::Raw(raw) => raw
		}
	}

	pub fn as_scalar_index(&self) -> usize {
		match self {
			Self::Scalar { index, .. } => *index,
			_ => unimplemented!()
		}
	}

	pub fn meet_new_raw_scalar(ptrs: &str, name: String, typedef: Option <TypeDef>) -> Self {
		Self::Scalar {
			index: match Self::raw().iter_mut().enumerate().find(|(_, ty)| ty.name() == name) {
				Some((idx, ty)) => {
					if let Some(typedef) = typedef {
						if ty.is_backed() {
							panic!("type `{name}` already exists")
						} else {
							*ty = RawType::Backed(typedef)
						}
					}
					idx
				},
				None => {
					Self::raw().push(match typedef {
						Some(x) => RawType::Backed(x),
						None => RawType::Stub(name)
					});
					Self::raw().len() - 1
				}
			},
			ptrs: TypePointers::from(ptrs)
		}
	}
}

pub enum TypeList {
	Raw(Vec <RawType>)
}

#[derive(Debug)]
pub enum RawType {
	Stub(String),
	Backed(TypeDef)
}

impl RawType {
	pub fn name(&self) -> &str {
		match self {
			Self::Stub(name) => name,
			Self::Backed(typedef) => &typedef.name
		}
	}

	pub fn is_backed(&self) -> bool {
		matches!(self, Self::Backed(_))
	}

	pub fn as_backed(&self) -> &TypeDef {
		match self {
			Self::Backed(x) => x,
			_ => unimplemented!()
		}
	}
}
