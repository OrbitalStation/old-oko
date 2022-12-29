use crate::*;

#[derive(Debug)]
pub struct ParseFunBodyInputStruct {
	pub fun_loc: FunLocation,
	pub(in crate) mother_ty: Option <Type>,
	line: *mut usize,
	stmts: *mut [Stmt]
}

impl Clone for ParseFunBodyInputStruct {
	fn clone(&self) -> Self {
		Self {
			fun_loc: self.fun_loc.clone(),
			mother_ty: self.mother_ty.clone(),
			line: self.line,
			stmts: self.stmts
		}
	}
}

impl ParseFunBodyInputStruct {
	pub fn new(stmts: &mut [Stmt], line: &mut usize) -> Self {
		Self {
			fun_loc: FunLocation::Global { stmt_index: 0 },
			mother_ty: None,
			line: line as *mut usize,
			stmts: stmts as *mut [Stmt]
		}
	}

	pub fn mother_ty(&self) -> Option <&Type> {
		self.mother_ty.as_ref()
	}

	pub fn cur(&self) -> &Stmt {
		match &self.fun_loc {
			FunLocation::Global { stmt_index } => &self.stmts()[*stmt_index],
			_ => unreachable!()
		}
	}

	pub fn cur_mut(&self) -> &mut Stmt {
		match &self.fun_loc {
			FunLocation::Global { stmt_index } => &mut self.stmts_mut()[*stmt_index],
			_ => unreachable!()
		}
	}

	pub fn cur_fun(&self) -> &FunDef {
		self.fun_loc.fun(self.stmts())
	}

	pub fn cur_fun_mut(&self) -> &mut FunDef {
		self.fun_loc.fun_mut(self.stmts_mut())
	}

	pub fn fun_by_name(&self, name: &str) -> Option <(usize, &FunDef)> {
		self.stmts().iter().enumerate().find_map(|(idx, fun)| match fun {
			Stmt::FunDef(fun) if fun.name == name => Some((idx, fun)),
			_ => None
		})
	}

	pub fn fun_by_name_mut(&self, name: &str) -> Option <(usize, &mut FunDef)> {
		self.stmts_mut().iter_mut().enumerate().find_map(|(idx, fun)| match fun {
			Stmt::FunDef(fun) if fun.name == name => Some((idx, fun)),
			_ => None
		})
	}

	pub fn extern_fun_by_name(&self, name: &str) -> Option <(usize, &ExternFun)> {
		self.stmts().iter().enumerate().find_map(|(idx, fun)| match fun {
			Stmt::ExternFun(fun) if fun.name == name => Some((idx, fun)),
			_ => None
		})
	}

	pub fn line(&self) -> usize {
		unsafe { *self.line }
	}

	pub fn next_line(&self) {
		unsafe { *self.line += 1 }
	}

	pub fn stmts_mut(&self) -> &mut [Stmt] {
		unsafe { &mut *self.stmts }
	}

	pub fn stmts(&self) -> &[Stmt] {
		unsafe { &*self.stmts }
	}
}

impl Iterator for ParseFunBodyInputStruct {
	type Item = Self;

	fn next(&mut self) -> Option <Self::Item> {
		let len = self.stmts().len();
		if let FunLocation::Global { stmt_index } = &mut self.fun_loc {
			if *stmt_index >= len {
				None
			} else {
				*stmt_index += 1;
				Some(Self {
					fun_loc: FunLocation::Global { stmt_index: *stmt_index - 1 },
					mother_ty: self.mother_ty.clone(),
					line: self.line,
					stmts: unsafe { core::mem::transmute(&mut *self.stmts) }
				})
			}
		} else {
			None
		}
	}
}

pub type ParseFunBodyInput <'a> = &'a ParseFunBodyInputStruct;
