use module::Module;
use opcode::{BlockTag, Opcode};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use std::result::Result;
use value::{CompiledCode, List, Value};

#[derive(Debug, Clone)]
pub struct Caller {
    pub code: Rc<CompiledCode>,
    pc: usize,
}

#[derive(Debug, Clone)]
pub struct Context {
    pub caller: Option<Caller>,
    pub code: Rc<CompiledCode>,
    pc: usize, // program counter
    locals: Vec<Value>,
    stack: Vec<Value>,
    sp: usize, // stack pointer
}

#[derive(Debug, Clone)]
pub struct Interp {
    pub ctx_id: RefCell<usize>,
    pub stack: RefCell<Vec<Value>>,
    pub modules: RefCell<HashMap<String, Rc<Module>>>,
}

impl Context {
    #[inline]
    pub fn new(caller_ctx: Option<&Context>, code: Rc<CompiledCode>, args: Vec<Value>) -> Context {
        let mut locals: Vec<Value> = Vec::with_capacity(code.locals);
        let sp = args.len();
        let mut stack = args;
        stack.resize(code.frame * 2, Value::Nil);

        let caller = match caller_ctx {
            None => None,
            Some(ctx) => {
                locals.extend(ctx.locals.clone());
                Some(Caller {
                    code: ctx.code.clone(),
                    pc: (*ctx).pc,
                })
            }
        };
        locals.resize(code.locals, Value::Nil);
        Context {
            caller,
            code,
            pc: 0,
            locals,
            stack,
            sp,
        }
    }

    #[inline]
    fn get(&self, sp: usize) -> &Value {
        self.stack
            .get(sp)
            .unwrap_or_else(|| panic!("value in stack at {} is not found", sp))
    }

    #[inline]
    fn top(&self) -> &Value {
        self.get(self.sp - 1)
    }

    #[inline]
    fn get_local(&self, i: usize) -> &Value {
        self.locals
            .get(i)
            .unwrap_or_else(|| panic!("local {} is not found", i))
    }

    #[inline]
    fn get_const(&self, i: usize) -> &Value {
        self.code
            .consts
            .get(i)
            .unwrap_or_else(|| panic!("constant value at {} is not found", i))
    }

    #[inline]
    fn load(&mut self, value: Value) {
        //println!("# load {} {:?}", self.sp, value);
        self.stack[self.sp] = value;
        self.sp += 1
    }

    #[inline]
    fn load_const(&mut self, i: usize) {
        let val = self.get_const(i).clone();
        //println!("# load const {:?}", val);
        self.load(val)
    }

    #[inline]
    fn load_local(&mut self, i: usize) {
        let val = self.get_local(i).clone();
        self.load(val)
    }

    #[inline]
    fn store(&mut self, i: usize, val: Value) {
        self.locals[i] = val
    }

    #[inline]
    fn pop(&mut self) -> Value {
        self.sp -= 1;
        //println!("# pop => {:?}", self.stack[self.sp].clone());
        self.stack[self.sp].clone()
    }

    #[inline]
    fn pop_string(&mut self) -> String {
        let val = self.pop();
        match val.get_string() {
            Some(s) => s.clone(),
            None => panic!("# popped value {:?} is not string", val),
        }
    }

    #[inline]
    fn pop_bool(&mut self) -> bool {
        match self.pop() {
            Value::Bool(flag) => flag,
            val => panic!("# popped value {:?} is not bool", val),
        }
    }

    #[inline]
    fn pop_only(&mut self) {
        self.sp -= 1
    }

    #[inline]
    fn popn(&mut self, size: usize) -> Vec<Value> {
        let mut vals = Vec::with_capacity(size);
        for _ in 0..size {
            self.sp -= 1;
            let val = &self.stack[self.sp];
            vals.push(val.clone())
        }
        vals.reverse();
        //println!("# popn {:?}", vals);
        vals
    }

    #[inline]
    fn jump(&mut self, n: isize) {
        //println!("# jump {} + {}", self.pc, n);
        self.pc = (self.pc as isize + n) as usize
    }
}

impl Interp {
    pub fn new() -> Interp {
        let len = 65536;
        let mut stack = Vec::with_capacity(len);
        for _ in 0..len {
            stack.push(Value::Nil)
        }
        Interp {
            ctx_id: RefCell::new(0),
            stack: RefCell::new(stack),
            modules: RefCell::new(HashMap::new()),
        }
    }

    #[inline]
    pub fn get_module(&self, name: &str) -> Option<Rc<Module>> {
        let list = self.modules.borrow();
        list.get(name).cloned()
    }

    #[inline]
    pub fn add_module(&self, m: Rc<Module>) {
        let mut list = self.modules.borrow_mut();
        let _ = list.insert((*m).get_name().unwrap().clone(), m);
    }

    #[inline]
    pub fn add_modules(&self, ms: Vec<Module>) {
        for m in ms.into_iter() {
            self.add_module(Rc::new(m))
        }
    }

    pub fn eval(
        &self,
        caller: Option<&Context>,
        code: Rc<CompiledCode>,
        args: Vec<Value>,
    ) -> Result<Value, String> {
        let mut ctx = Context::new(caller, code.clone(), args);
        let ops = &(*code.clone()).ops;
        let op_size = ops.len();
        'eval: loop {
            if ctx.pc >= op_size {
                panic!("# code must have return op")
            }
            let op = &ops[ctx.pc];
            //debug!("eval {} {:?}", ctx.pc + 1, &ops[ctx.pc]);
            match op {
                Opcode::LoadConst(i) => ctx.load_const(*i as usize),
                Opcode::LoadInt(val) => ctx.load(Value::Int(*val as i64)),
                Opcode::LoadLocal(i) => ctx.load_local(*i as usize),
                Opcode::LoadTrue => ctx.load(Value::Bool(true)),
                Opcode::LoadFalse => ctx.load(Value::Bool(false)),
                Opcode::LoadUndef => ctx.load(Value::Atom(Rc::new("undef".to_string()))),
                Opcode::LoadOk => ctx.load(Value::Atom(Rc::new("ok".to_string()))),
                Opcode::LoadError => ctx.load(Value::Atom(Rc::new("error".to_string()))),
                Opcode::LoadBitstr(size, val) => ctx.load(Value::Bitstr32(*size, *val)),
                Opcode::StorePopLocal(i) => {
                    let val = ctx.pop();
                    ctx.store(*i as usize, val);
                }
                Opcode::GetProp => {
                    let pname = ctx.pop_string();
                    let val = ctx.pop();
                    match val {
                        Value::ModuleName(name) => match self.get_module(&name) {
                            Some(m) => match m.fields.get(&pname) {
                                None => panic!("# GetProp: key {:?} is not found", name),
                                Some(val) => ctx.load(val.clone()),
                            },
                            None => panic!("# module {} not found", name),
                        },
                        _ => panic!("# GetProp non-module not impl"),
                    };
                }
                Opcode::GetGlobal => {
                    let name = ctx.pop_string();
                    match self.get_module(&name) {
                        Some(_) => ctx.load(Value::ModuleName(Rc::new(name.clone()))),
                        None => panic!("# GetGlobal not impl non module"),
                    }
                }
                Opcode::SetGlobal => {
                    let val = ctx.pop();
                    let name = ctx.pop_string();
                    match val {
                        Value::Module(m) => {
                            (*m).set_name(&name);
                            self.add_module(m.clone());
                        }
                        _ => panic!("not supported"),
                    }
                }
                Opcode::Pop => ctx.pop_only(),
                Opcode::Loophead => (),
                Opcode::Jump(n) => {
                    ctx.jump(*n as isize);
                    continue 'eval;
                }
                Opcode::BranchFalse(n) => {
                    let flag = ctx.pop_bool();
                    if !flag {
                        ctx.jump(*n as isize);
                        continue 'eval;
                    }
                }
                Opcode::Return => return Ok(ctx.top().clone()),
                Opcode::ReturnUndef => return Ok(Value::Atom(Rc::new("undefined".to_string()))),
                Opcode::MakeBlock(BlockTag::NonConst, size) => {
                    let vals = ctx.popn(*size as usize);
                    ctx.load(Value::Array(Rc::new(vals)))
                }
                Opcode::MakeBlock(BlockTag::Module, size) => {
                    // TODO: attributes
                    let size = *size as usize;
                    let vals = ctx.popn(size);
                    let mut m = Module::new();
                    for i in 0..size {
                        if i % 2 == 0 {
                            let key = vals[i].get_string().unwrap().clone();
                            let val = vals[i + 1].clone();
                            m.fields.insert(key, val);
                        }
                    }
                    ctx.load(Value::Module(Rc::new(m)));
                }
                Opcode::MakeBlock(BlockTag::List, 0) => ctx.load(List::nil_value()),
                Opcode::MakeBlock(BlockTag::List, _) => {
                    let tail = ctx.pop();
                    let head = ctx.pop();
                    ctx.load(Value::List(Box::new(List::new(head, tail))));
                }
                Opcode::Apply(nargs) => {
                    let args = ctx.popn(*nargs as usize);
                    let fval = ctx.pop();
                    match fval {
                        Value::CompiledCode(code) => match self.eval(Some(&ctx), code, args) {
                            Ok(val) => ctx.load(val),
                            Err(err) => panic!("# call error {:?}", err),
                        },
                        Value::Nif(nif) => match ((*nif).fun)(&args) {
                            Ok(ret) => ctx.load(ret),
                            Err(err) => return Err(err),
                        },
                        _ => panic!("# apply: not function {:?}", fval),
                    }
                }
                Opcode::BlockSize => {
                    let val = ctx.pop();
                    match val {
                        Value::List(list) => match list.length() {
                            Some(i) => ctx.load(Value::Int(i as i64)),
                            None => panic!("# block size: bad list"),
                        },
                        _ => panic!("# block size: not block {:?}", val),
                    }
                }
                Opcode::ListLen => {
                    let val = ctx.pop();
                    match val {
                        Value::List(list) => match (*list).length() {
                            None => panic!("# list length: invalid list {:?}", list),
                            Some(len) => ctx.load(Value::Int(len as i64)),
                        },
                        _ => panic!("# list length: not list {:?}", val),
                    }
                }
                Opcode::ListCons => {
                    let tail = ctx.pop();
                    let head = ctx.pop();
                    ctx.load(List::new(head, tail).to_value());
                }
                Opcode::ListComprGen(_) => {
                    // TODO
                    ctx.load(Value::Atom(Rc::new("undefined".to_string())));
                }
                Opcode::ListRev => {
                    // TODO
                }
                Opcode::Ne => {
                    let b = ctx.pop();
                    let a = ctx.pop();
                    match (&a, &b) {
                        (Value::Int(i1), Value::Int(i2)) => ctx.load(Value::Bool(i1 != i2)),
                        _ => panic!("# ne: invalid args {:?}, {:?}", a, b),
                    }
                }
                Opcode::Lt => {
                    let b = ctx.pop();
                    let a = ctx.pop();
                    match (&a, &b) {
                        (Value::Int(i1), Value::Int(i2)) => ctx.load(Value::Bool(i1 < i2)),
                        _ => panic!("# lt: invalid args {:?}, {:?}", a, b),
                    }
                }
                Opcode::Add => {
                    let b = ctx.pop();
                    let a = ctx.pop();
                    match (&a, &b) {
                        (Value::Int(i1), Value::Int(i2)) => ctx.load(Value::Int(i1 + i2)),
                        _ => panic!("# add: invalid args {:?}, {:?}", a, b),
                    }
                }
                Opcode::Add1 => {
                    let a = ctx.pop();
                    match &a {
                        Value::Int(i) => ctx.load(Value::Int(i + 1)),
                        _ => panic!("# add1: invalid args {:?}", a),
                    }
                }
                Opcode::Sub => {
                    let b = ctx.pop();
                    let a = ctx.pop();
                    match (&a, &b) {
                        (Value::Int(i1), Value::Int(i2)) => ctx.load(Value::Int(i1 - i2)),
                        _ => panic!("# sub: invalid args {:?}, {:?}", a, b),
                    }
                }
                op => panic!("# eval: not impl {:?}", op),
            }
            ctx.pc += 1;
        }
    }
}
