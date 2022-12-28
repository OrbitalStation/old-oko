use core::fmt::{Debug, Formatter, Result, Write};
use std::ffi::CString;
use llvm::core::*;
use llvm::prelude::*;
use crate::*;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
#[repr(u8)]
pub enum AssociatedMethodKind {
	/// Rust's `&self`
	ByRef,

	/// Rust's `&mut self`
	ByMutRef,

	/// Rust's `self`
	ByValue,

	/// Rust's `mut self`
	ByMutValue,

	/// No `self`
	Static
}

impl AssociatedMethodKind {
	pub fn modify_llvm_type(self, ty: LLVMTypeRef) -> LLVMTypeRef {
		unsafe {
			// Always a pointer, because types are expected to be one
			LLVMPointerType(ty, 0)
		}
	}

	pub fn modify_type(self, ty: Type) -> Type {
		match self {
			Self::ByRef | Self::ByMutRef => Type {
				kind: TypeKind::Reference {
					ty: Box::new(ty),
					mutable: self == Self::ByMutRef
				}
			},
			Self::ByValue | Self::ByMutValue => ty,
			Self::Static => unreachable!()
		}
	}
}

#[derive(Debug, Clone)]
pub struct AssociatedMethod {
	pub def: FunDef,
	pub kind: AssociatedMethodKind,
	pub state_of_i: VariableState
}

#[derive(Debug, Clone)]
pub struct TypeDef {
	pub name: String,
	pub kind: TypeDefKind,
	pub methods: Vec <AssociatedMethod>,
	pub subtypes: TypeList
}

#[derive(Debug, Clone)]
pub enum TypeDefKind {
	Enum {
		variants: Vec <EnumVariant>
	},
	Struct {
		fields: Vec <StructField>
	},
	/// Effectively a struct or enum with no fields; an empty type
	Opaque
}

#[derive(Debug, Clone)]
pub struct EnumVariant {
	pub name: String,
	pub data: Vec <Type>
}

#[derive(Debug, Clone)]
pub struct StructField {
	pub name: String,
	pub ty: Type
}

#[derive(Copy, Clone, Eq, PartialEq)]
pub struct TypePointers {
	/// How many pointers are on a type
	///
	/// **T -> 2
	/// *^^**^T -> 6
	/// T -> 0
	pub len: u8,

	/// Which of those pointers are mutable
	/// **T -> 0b00
	/// ^T -> 0b1
	/// *^^**^T -> 0b011001
	pub muts: u8
}

impl Debug for TypePointers {
	fn fmt(&self, f: &mut Formatter <'_>) -> Result {
		for i in 0..self.len {
			let is_mutable = ((self.muts >> i) & 1) != 0;
			f.write_char(if is_mutable {
				'^'
			} else {
				'*'
			})?
		}
		Ok(())
	}
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

#[derive(Eq, Clone)]
pub struct Type {
	pub kind: TypeKind
}

impl PartialEq for Type {
	fn eq(&self, other: &Self) -> bool {
		self.kind.eq(&other.kind)
	}
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum TypeKindScalarLocation {
	Global { index: usize },
	AssociatedItem {
		index: usize,
		mother: Box <TypeKindScalarLocation>
	}
}

impl TypeKindScalarLocation {
	pub fn get_list_and_index(&self) -> (&'static mut TypeList, usize) {
		match self {
			Self::Global { index } => (Type::type_list(), *index),
			Self::AssociatedItem { index, mother } => (&mut mother.type_def().unwrap().subtypes, *index)
		}
	}

	pub fn type_def(&self) -> Option <&'static mut TypeDef> {
		let (list, index) = self.get_list_and_index();

		match list {
			TypeList::Baked(baked) => match &mut baked[index].kind {
				BakedTypeKind::Ordinary(def) => Some(def),
				_ => None
			},
			TypeList::Raw(raw) => match &mut raw[index] {
				RawType::Backed(backed) => Some(backed),
				_ => None
			}
		}
	}

	pub fn baked(&self) -> Option <&'static mut BakedType> {
		let (list, index) = self.get_list_and_index();

		match list {
			TypeList::Baked(baked) => Some(&mut baked[index]),
			_ => None
		}
	}

	pub fn builtin(&self) -> Option <&'static BuiltinType> {
		let (list, index) = self.get_list_and_index();

		match list {
			TypeList::Baked(baked) => match &baked[index].kind {
				BakedTypeKind::Builtin(idx) => Some(&BUILTIN_TYPES[*idx]),
				_ => None
			},
			TypeList::Raw(_) => unimplemented!()
		}
	}

	pub fn name(&self) -> String {
		match self {
			Self::Global { index } => match Type::type_list() {
				TypeList::Raw(raw) => raw[*index].name().to_string(),
				TypeList::Baked(baked) => baked[*index].name().to_string()
			},
			Self::AssociatedItem { index, mother } => format!("{}.{}", mother.name(), match &mother.type_def().unwrap().subtypes {
				TypeList::Baked(baked) => baked[*index].name(),
				TypeList::Raw(raw) => raw[*index].name()
			})
		}
	}
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum TypeKind {
	Scalar { loc: TypeKindScalarLocation },
	Pointer {
		ty: Box <Type>,
		ptrs: TypePointers
	},
	Reference {
		ty: Box <Type>,
		mutable: bool
	},
	Array {
		ty: Box <Type>,
		size: usize
	},
	Tuple { types: Vec <Type> },
	Integer
}

impl Debug for Type {
	fn fmt(&self, f: &mut Formatter <'_>) -> Result {
		match &self.kind {
			TypeKind::Scalar { loc } => f.write_str(&loc.name()),
			TypeKind::Pointer { ty, ptrs } => {
				ptrs.fmt(f)?;
				ty.fmt(f)
			},
			TypeKind::Reference { ty, mutable } => {
				f.write_char(if *mutable {
					'$'
				} else {
					'&'
				})?;
				ty.fmt(f)
			},
			TypeKind::Array { ty, size } => {
				f.write_fmt(format_args!("[{ty:?} x {size}]"))
			},
			TypeKind::Tuple { types } => {
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
			},
			TypeKind::Integer => f.write_str("{{integer}}")
		}
	}
}

impl Type {
	pub const UNIT_TUPLE: Type = Type::from_kind(TypeKind::Tuple { types: vec![] });

	pub fn name_for_llvm(&self) -> String {
		match &self.kind {
			TypeKind::Scalar { loc } => loc.name(),
			TypeKind::Reference { ty, .. } => ty.name_for_llvm(),
			_ => unreachable!()
		}
	}

	/// Will make difference from `is_simplistic` when copy structs would appear
	pub fn is_copy(&self) -> bool {
		self.is_simplistic()
	}

	pub fn get_by_name(name: &str) -> Self {
		match Self::type_list() {
			TypeList::Baked(baked) => {
				Self::from_kind(TypeKind::Scalar {
					loc: TypeKindScalarLocation::Global {
						index: baked.iter().enumerate().find(|(_, x)| x.name() == name).unwrap().0
					}
				})
			},
			_ => unimplemented!()
		}
	}

	pub fn get_fields_of_struct(&self) -> Option <&'static Vec <StructField>> {
		if let TypeKind::Scalar { loc } = &self.kind {
			match loc.type_def() {
				Some(x) => if let TypeDefKind::Struct { fields } = &x.kind {
					Some(fields)
				} else {
					None
				},
				None => None
			}
		} else {
			None
		}
	}

	#[inline]
	pub fn is_struct(&self) -> bool {
		self.get_fields_of_struct().is_some()
	}

	/// A simplistic type is a type that can be used directly in LLVM-IR
	pub fn is_simplistic(&self) -> bool {
		match &self.kind {
			TypeKind::Scalar { loc } => loc.builtin().is_some(),
			TypeKind::Pointer { .. } | TypeKind::Reference { .. } | TypeKind::Integer => true,
			TypeKind::Tuple { types } => types.len() == 0,
			TypeKind::Array { size, .. } => *size == 0
		}
	}

	pub fn eq_implicit(&mut self, other: &mut Self, expr1: Option <&mut ExprKind>, expr2: Option <&mut ExprKind>) -> bool {
		if self == other {
			return true
		}

		if !self.try_implicitly_convert(other, expr1) {
			other.try_implicitly_convert(self, expr2);
		}

		self == other
	}

	pub fn try_implicitly_convert(&mut self, other: &Self, expr: Option <&mut ExprKind>) -> bool {
		match &self.kind {
			TypeKind::Integer => match &other.kind {
				TypeKind::Integer => false,
				_ => {
					*self = other.clone();
					if let Some(expr) = expr {
						match expr {
							ExprKind::BinOp { left, right, .. } => {
								left.ty.try_implicitly_convert(other, Some(&mut left.kind));
								right.ty.try_implicitly_convert(other, Some(&mut right.kind));
							},
							_ => ()
						}
					}
					true
				}
			},
			_ => false
		}
	}

	fn __is_builtin_smth(&self, f: fn(BuiltinTypeKind) -> bool) -> bool {
		match &self.kind {
			TypeKind::Scalar { loc } => loc.builtin().map(|x| f(x.kind)).unwrap_or(false),
			_ => false
		}
	}

	pub fn is_signed(&self) -> bool {
		self.__is_builtin_smth(|x| matches!(x, BuiltinTypeKind::Signed))
	}

	pub fn is_unsigned(&self) -> bool {
		self.__is_builtin_smth(|x| matches!(x, BuiltinTypeKind::Unsigned))
	}

	pub fn is_arithmetic(&self) -> bool {
		self.is_signed() || self.is_unsigned() || matches!(self.kind, TypeKind::Integer)
	}

	pub fn llvm_type(&self) -> LLVMTypeRef {
		match &self.kind {
			TypeKind::Scalar { loc } => {
				let (list, index) = loc.get_list_and_index();
				match list {
					TypeList::Baked(baked) => baked[index].llvm_type,
					TypeList::Raw(_) => unimplemented!()
				}
			},
			TypeKind::Pointer { ty, ptrs } => {
				let mut ty = ty.llvm_type();
				for _ in 0..ptrs.len {
					// No distinction between mutable and const pointers
					ty = unsafe { LLVMPointerType(ty, 0) }
				}
				ty
			},
			TypeKind::Reference { ty, .. } => {
				// No distinction between either refs and ptrs or mutable and immutable
				unsafe { LLVMPointerType(ty.llvm_type(), 0) }
			},
			TypeKind::Array { ty, size } => {
				unsafe { LLVMArrayType(ty.llvm_type(), *size as _) }
			},
			TypeKind::Tuple { types } => {
				let mut types = types.iter().map(|x| x.llvm_type()).collect::<Vec <_>>();
				unsafe { LLVMStructType(types.as_mut_ptr(), types.len() as _, 0) }
			},
			TypeKind::Integer => Self::get_by_name("i32").llvm_type()
		}
	}

	pub fn type_list() -> &'static mut TypeList {
		static mut TYPE_LIST: TypeList = TypeList::Raw(vec![]);
		unsafe { &mut TYPE_LIST }
	}

	pub fn raw() -> &'static mut Vec <RawType> {
		match Self::type_list() {
			TypeList::Raw(raw) => raw,
			_ => unimplemented!()
		}
	}

	pub fn baked() -> &'static mut Vec <BakedType> {
		match Self::type_list() {
			TypeList::Baked(baked) => baked,
			_ => unimplemented!()
		}
	}

	pub fn meet_new_raw_scalar(mother: Option <&TypeKindScalarLocation>, name: String, typedef: Option <TypeDef>) -> Self {
		Self::from_kind(TypeKind::Scalar {
			loc: {
				let list = match mother.map(|mother| &mut mother.type_def().unwrap().subtypes).unwrap_or(Self::type_list()) {
					TypeList::Raw(raw) => raw,
					_ => unimplemented!()
				};
				let index = match list.iter_mut().enumerate().find(|(_, ty)| ty.name() == name) {
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
						list.push(match typedef {
							Some(x) => RawType::Backed(x),
							None => RawType::Stub(name)
						});
						list.len() - 1
					}
				};
				if let Some(mother) = mother {
					TypeKindScalarLocation::AssociatedItem {
						mother: Box::new(mother.clone()),
						index
					}
				} else {
					TypeKindScalarLocation::Global {
						index
					}
				}
			}
		})
	}

	pub fn as_scalar_loc(&self) -> &TypeKindScalarLocation {
		match &self.kind {
			TypeKind::Scalar { loc } => loc,
			_ => unimplemented!()
		}
	}

	pub fn get_existing_scalar(name: String) -> Option <Self> {
		Self::baked().iter().enumerate().find(|(_, ty)| ty.name() == name).map(|(index, _)| Self::from_kind(TypeKind::Scalar {
			loc: TypeKindScalarLocation::Global { index }
		}))
	}

	pub fn pointer(ty: Self, ptrs: &str) -> Self {
		Self::from_kind(TypeKind::Pointer {
			ty: Box::new(ty),
			ptrs: TypePointers::from(ptrs),
		})
	}

	pub fn array(ty: Self, size: &str) -> Self {
		Self::from_kind(TypeKind::Array {
			ty: Box::new(ty),
			size: size.parse().unwrap(),
		})
	}

	pub fn reference(ty: Self, mutable: bool) -> Self {
		Self::from_kind(TypeKind::Reference {
			ty: Box::new(ty),
			mutable
		})
	}

	pub const fn from_kind(kind: TypeKind) -> Self {
		Self {
			kind
		}
	}
}

#[derive(Debug, Clone)]
pub enum TypeList {
	Raw(Vec <RawType>),
	Baked(Vec <BakedType>)
}

#[derive(Debug, Clone)]
pub struct BakedType {
	pub kind: BakedTypeKind,
	pub llvm_type: LLVMTypeRef
}

impl BakedType {
	pub const STUB: BakedType = BakedType {
		kind: BakedTypeKind::Builtin(0),
		llvm_type: core::ptr::null_mut()
	};

	pub fn builtin(idx: usize) -> Self {
		let llvm_type = unsafe { (BUILTIN_TYPES[idx].llvm_create)(llvm_context()) };

		Self {
			kind: BakedTypeKind::Builtin(idx),
			llvm_type
		}
	}

	pub fn ordinary(td: TypeDef, name: String) -> Self {
		let name = CString::new(name.as_str()).unwrap();
		let llvm_type = unsafe { LLVMStructCreateNamed(llvm_context(), name.as_ptr()) };

		Self {
			kind: BakedTypeKind::Ordinary(td),
			llvm_type
		}
	}

	pub fn name(&self) -> &str {
		match &self.kind {
			BakedTypeKind::Builtin(builtin) => BUILTIN_TYPES[*builtin].name,
			BakedTypeKind::Ordinary(ord) => &ord.name
		}
	}
}

#[derive(Debug, Clone)]
pub enum BakedTypeKind {
	Builtin(usize),
	Ordinary(TypeDef)
}

#[derive(Debug, Clone)]
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
