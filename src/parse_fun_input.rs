use crate::*;

pub struct ParseFunBodyInputStruct {
	pub cur_stmt: usize,
	pub fun_overload: usize,
	line: *mut usize,
	stmts: *mut [Stmt]
}

impl ParseFunBodyInputStruct {
	pub fn new(stmts: &mut [Stmt], line: &mut usize) -> Self {
		Self {
			cur_stmt: 0,
			fun_overload: 0,
			line: line as *mut usize,
			stmts: stmts as *mut [Stmt]
		}
	}

	pub fn with(&self, o: usize) -> Self {
		Self {
			cur_stmt: self.cur_stmt,
			fun_overload: o,
			line: self.line,
			stmts: self.stmts
		}
	}

	pub fn cur(&self) -> &Stmt {
		&self.stmts()[self.cur_stmt]
	}

	pub fn cur_mut(&self) -> &mut Stmt {
		&mut self.stmts_mut()[self.cur_stmt]
	}

	pub fn cur_fun(&self) -> &FunDef {
		match self.cur() {
			Stmt::FunDef(fun) => fun,
			_ => unimplemented!()
		}
	}

	pub fn cur_fun_mut(&self) -> &mut FunDef {
		match self.cur_mut() {
			Stmt::FunDef(fun) => fun,
			_ => unimplemented!()
		}
	}

	pub fn fun_by_name(&self, name: &str) -> Option <(usize, &FunDef)> {
		self.stmts().iter().enumerate().find_map(|(idx, fun)| match fun {
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

	pub fn fun_stmt_index_by_name(&self, name: &str) -> Option <usize> {
		self.fun_by_name(name).map(|(x, _)| x)
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

	pub fn fun_at_idx(&self, idx: usize) -> &mut FunDef {
		match &mut self.stmts_mut()[idx] {
			Stmt::FunDef(fundef) => fundef,
			_ => unimplemented!()
		}
	}
}

impl Iterator for ParseFunBodyInputStruct {
	type Item = Self;

	fn next(&mut self) -> Option <Self::Item> {
		if self.cur_stmt >= unsafe { (*self.stmts).len() } {
			None
		} else {
			self.cur_stmt += 1;
			Some(Self {
				cur_stmt: self.cur_stmt - 1,
				fun_overload: 0,
				line: self.line,
				stmts: unsafe { core::mem::transmute(&mut *self.stmts) }
			})
		}
	}
}

pub type ParseFunBodyInput <'a> = &'a ParseFunBodyInputStruct;
