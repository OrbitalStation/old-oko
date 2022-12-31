use core::fmt::{Debug, Formatter, Result, Write};
use std::ffi::{c_ulonglong, CString};
use llvm::core::*;
use llvm::prelude::*;
use llvm::target::*;
use crate::*;

#[derive(Debug, Clone)]
pub struct Tuple {
	pub fields: Vec <Type>,
	pub llvm_type: LLVMTypeRef
}

impl Tuple {
	pub fn from_fields(fields: Vec <Type>) -> Self {
		let name = format!(".tuple.{}", fields.iter().map(|ty| if matches!(ty.kind, TypeKind::Integer) {
			String::from("i32")
		} else {
			format!("{ty:?}")
		}).collect::<Vec <_>>().join("-"));

		let llvm_type = unsafe { LLVMStructCreateNamed(llvm_context(), name.as_ptr() as _) };

		Self {
			fields,
			llvm_type
		}
	}

	pub fn get(index: usize) -> &'static mut Vec <Type> {
		&mut Type::tuple_list()[index].fields
	}
}

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
	pub data: Option <Type>
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
		let self_ = self.unaliasize();
		let other_ = other.unaliasize();
		match &self_.kind {
			TypeKind::Scalar { loc } => match &other_.kind {
				TypeKind::Scalar { loc: loc2 } => return loc.name() == loc2.name(),
				_ => ()
			},
			_ => ()
		}
		self_.kind.eq(&other_.kind)
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
	pub fn simple_scalar(&self) -> Type {
		Type::from_kind(TypeKind::Scalar {
			loc: self.clone()
		})
	}

	pub fn get_list_of_raw_subtypes_for_mother_ty(mother: Option <&Self>) -> &'static mut Vec <RawType> {
		match mother.map(|mother| &mut mother.type_def().unwrap().subtypes).unwrap_or(Type::type_list()) {
			TypeList::Raw(raw) => raw,
			_ => unimplemented!()
		}
	}

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
				BakedTypeKind::SeqAlias(alias) => return alias.type_def(),
				BakedTypeKind::FullAlias { to, ..} => return to.as_scalar_loc()?.type_def(),
				_ => None
			},
			TypeList::Raw(raw) => match &mut raw[index] {
				RawType::Backed(backed) => Some(backed),
				RawType::Alias { to, .. } => return to.as_scalar_loc()?.type_def(),
				_ => None
			}
		}
	}

	pub fn raw(&self) -> Option <&'static mut RawType> {
		let (list, index) = self.get_list_and_index();

		match list {
			TypeList::Raw(raw) => Some(&mut raw[index]),
			_ => None
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
				TypeList::Baked(baked) => baked[*index].name().to_string(),
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
	Tuple { index: usize },
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
			TypeKind::Tuple { index } => {
				let types = Tuple::get(*index);
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
	pub const UNIT_TUPLE: Type = Type::from_kind(TypeKind::Tuple { index: 0 });

	pub fn tuple_list() -> &'static mut Vec <Tuple> {
		static mut L: Vec <Tuple> = vec![];
		unsafe { &mut L }
	}

	pub fn initialize_statics() {
		Type::tuple_list().push(Tuple::from_fields(vec![]));
	}

	pub fn unaliasize(&self) -> &Type {
		fn handle_loc(loc: &TypeKindScalarLocation) -> Option <&Type> {
			let (list, idx) = loc.get_list_and_index();
			match list {
				TypeList::Raw(raw) => match &raw[idx] {
					RawType::Alias { to, .. } => return Some(to),
					_ => ()
				},
				TypeList::Baked(baked) => {
					match &baked[idx].kind {
						BakedTypeKind::FullAlias { to, .. } => return Some(to.unaliasize()),
						BakedTypeKind::SeqAlias(loc) => return handle_loc(loc),
						_ => ()
					}
				},
			}
			None
		}

		match &self.kind {
			TypeKind::Scalar { loc } => if let Some(x) = handle_loc(loc) {
				return x
			},
			_ => ()
		}

		self
	}

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

	pub fn get_baked_seq_type(start_idx: Self, cont: Vec <String>) -> Option <Self> {
		match start_idx.kind {
			TypeKind::Scalar { mut loc } => {
				for part in cont {
					match &loc.type_def()?.subtypes {
						TypeList::Baked(baked) => loc = TypeKindScalarLocation::AssociatedItem {
							index: baked.iter().enumerate().find(|(_, x) | match &x.kind {
								BakedTypeKind::Ordinary(ord) => *ord.name == part,
								_ => unreachable!()
							})?.0,
							mother: Box::new(loc),
						},
						_ => return None
					}
				}
				Some(Self::from_kind(TypeKind::Scalar { loc }))
			},
			_ => None
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

	pub fn is_tuple(&self) -> bool {
		matches!(self.kind, TypeKind::Tuple { .. })
	}

	/// A simplistic type is a type that can be used directly in LLVM-IR
	pub fn is_simplistic(&self) -> bool {
		match &self.unaliasize().kind {
			TypeKind::Scalar { loc } => loc.builtin().is_some(),
			TypeKind::Pointer { .. } | TypeKind::Reference { .. } | TypeKind::Integer => true,
			// Know for sure the only empty tuple is that with `0` index
			TypeKind::Tuple { index } => *index == 0,
			TypeKind::Array { size, .. } => *size == 0
		}
	}

	pub fn eq_implicit(&mut self, other: &mut Self, expr1: Option <&mut ExprKind>, expr2: Option <&mut ExprKind>) -> bool {
		if !self.try_implicitly_convert(other, expr1) {
			other.try_implicitly_convert(self, expr2);
		}

		self == other
	}

	pub fn try_implicitly_convert(&mut self, other: &Self, expr: Option <&mut ExprKind>) -> bool {
		if self == other {
			return true
		}

		match &mut self.kind {
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
			TypeKind::Array { ty, size } => match &other.kind {
				TypeKind::Array { ty: ty2, size: size2 } => {
					if *size == *size2 {
						ty.try_implicitly_convert(ty2, expr)
					} else {
						false
					}
				},
				_ => false
			},
			TypeKind::Tuple { index } => match &other.kind {
				TypeKind::Tuple { index: index2 } => if !Tuple::get(*index).is_empty() {
					let mut all = true;
					for (ty, ty2) in Tuple::get(*index).iter_mut().zip(Tuple::get(*index2).iter()) {
						if !ty.try_implicitly_convert(ty2, unsafe { core::ptr::read(&expr) }) {
							all = false
						}
					}
					all
				} else {
					false
				},
				_ => false
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

	pub fn size(&self) -> c_ulonglong {
		unsafe { LLVMABISizeOfType(LLVMGetModuleDataLayout(llvm_module()), self.llvm_type(false)) }
	}

	pub fn llvm_type(&self, is_needed_to_do_reassign: bool) -> LLVMTypeRef {
		match &self.kind {
			TypeKind::Scalar { loc } => {
				let (list, index) = loc.get_list_and_index();
				match list {
					TypeList::Baked(baked) => {
						if is_needed_to_do_reassign {
							assign_correct_llvm_type_to_a_baked_one(&mut baked[index])
						}
						baked[index].llvm_type
					},
					TypeList::Raw(_) => unimplemented!()
				}
			},
			TypeKind::Pointer { ty, ptrs } => {
				let mut ty = ty.llvm_type(is_needed_to_do_reassign);
				for _ in 0..ptrs.len {
					// No distinction between mutable and const pointers
					ty = unsafe { LLVMPointerType(ty, 0) }
				}
				ty
			},
			TypeKind::Reference { ty, .. } => {
				// No distinction between either refs and ptrs or mutable and immutable
				unsafe { LLVMPointerType(ty.llvm_type(is_needed_to_do_reassign), 0) }
			},
			TypeKind::Array { ty, size } => {
				unsafe { LLVMArrayType(ty.llvm_type(is_needed_to_do_reassign), *size as _) }
			},
			TypeKind::Tuple { index } => {
				Self::tuple_list()[*index].llvm_type
			},
			TypeKind::Integer => Self::get_by_name("i32").llvm_type(is_needed_to_do_reassign)
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
				let list = TypeKindScalarLocation::get_list_of_raw_subtypes_for_mother_ty(mother);
				let index = match list.iter_mut().enumerate().find(|(_, ty)| ty.name() == name) {
					Some((idx, ty)) => {
						if let Some(typedef) = typedef {
							if ty.is_backed() {
								panic!("type `{name}` already exists")
							} else {
								*ty = RawType::Backed(typedef)
							}
						}
						if let RawType::Alias { to, .. } = ty {
							return to.clone()
						} else {
							idx
						}
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

	pub fn as_scalar_loc(&self) -> Option <&TypeKindScalarLocation> {
		match &self.kind {
			TypeKind::Scalar { loc } => Some(loc),
			_ => None
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

	pub fn seq(start: Type, continuation: Vec <String>) -> Self {
		let start_idx = match start.as_scalar_loc().unwrap() {
			TypeKindScalarLocation::Global { index } => *index,
			_ => unimplemented!()
		};
		let ty = RawType::Seq {
			start_idx,
			continuation
		};
		let index = if let Some((idx, _)) = Self::raw().iter().enumerate().find(|(_, ty2)| ty2.is_same_seq(&ty)) {
			idx
		} else {
			Self::raw().push(ty);
			Self::raw().len() - 1
		};
		Self::from_kind(TypeKind::Scalar { loc: TypeKindScalarLocation::Global { index } })
	}

	pub fn add_new_raw_alias(mother: Option <&TypeKindScalarLocation>, name: String) -> TypeKindScalarLocation {
		let list = TypeKindScalarLocation::get_list_of_raw_subtypes_for_mother_ty(mother);
		let index = list.len();
		list.push(RawType::Alias {
			name,
			// Will be replaced soon
			to: Type::UNIT_TUPLE
		});

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

	pub fn meet_new_tuple(fields: Vec <Type>) -> Self {
		if let Some((index, _)) = Self::tuple_list().iter().enumerate().find(|(_, x)| x.fields == fields) {
			Self::from_kind(TypeKind::Tuple { index })
		} else {
			let index = Self::tuple_list().len();
			Self::tuple_list().push(Tuple::from_fields(fields));
			Self::from_kind(TypeKind::Tuple { index })
		}
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

	pub fn seq_alias(loc: TypeKindScalarLocation) -> Self {
		Self {
			kind: BakedTypeKind::SeqAlias(loc),
			// Will be replaced soon afterwards
			llvm_type: unsafe { LLVMVoidTypeInContext(llvm_context()) }
		}
	}

	pub fn name(&self) -> String {
		match &self.kind {
			BakedTypeKind::Builtin(builtin) => BUILTIN_TYPES[*builtin].name.to_string(),
			BakedTypeKind::Ordinary(ord) => ord.name.to_string(),
			BakedTypeKind::SeqAlias(alias) => alias.name(),
			BakedTypeKind::FullAlias { name, .. } => name.clone()
		}
	}
}

#[derive(Debug, Clone)]
pub enum BakedTypeKind {
	Builtin(usize),
	Ordinary(TypeDef),
	SeqAlias(TypeKindScalarLocation),
	FullAlias {
		name: String,
		to: Type
	}
}

#[derive(Debug, Clone)]
pub enum RawType {
	Stub(String),
	Backed(TypeDef),
	Seq {
		start_idx: usize,
		continuation: Vec <String>
	},
	Alias {
		name: String,
		to: Type
	}
}

impl RawType {
	pub fn is_same_seq(&self, other: &Self) -> bool {
		match self {
			Self::Seq { start_idx, continuation} => match other {
				RawType::Seq { start_idx: start_index_b, continuation: continuation_b }
					=> start_idx == start_index_b && continuation == continuation_b,
				_ => false
			},
			_ => false
		}
	}

	pub fn unaliasize_to_self <'a> (&'a self, global_list: &'a Vec <Self>) -> Option <&'a Self> {
		match self {
			Self::Alias { to, .. } => match to.as_scalar_loc()? {
				TypeKindScalarLocation::Global { index } => &global_list[*index],
				TypeKindScalarLocation::AssociatedItem { mother, index } => match &mother.type_def().unwrap().subtypes {
					TypeList::Raw(raw) => &raw[*index],
					_ => unreachable!()
				}
			}.unaliasize_to_self(global_list),
			_ => Some(self)
		}
	}

	pub fn name(&self) -> String {
		match self {
			Self::Stub(name) => name.to_string(),
			Self::Backed(typedef) => typedef.name.to_string(),
			Self::Seq { start_idx, continuation } => format!("{}.{}", Type::raw()[*start_idx].name(), continuation.join(".")),
			Self::Alias { name, .. } => name.clone()
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
