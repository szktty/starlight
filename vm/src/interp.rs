use arglist::ArgList;
use dispatch::{Queue, QueuePriority};
use heap::{Content, Heap, Object, ObjectId};
use interp_init;
use list::{BrList, List, ListGenerator};
use module::{Module, ModuleGroup};
use opcode::{BlockTag, Opcode};
use result::Result;
use std::cell::RefCell;
use std::collections::HashMap;
use std::sync::{Arc, Mutex};
use thread_pool::ThreadPool;
use value::{CompiledCode, Value};

#[derive(Debug, Clone)]
pub struct Caller {
    pub code: Arc<CompiledCode>,
    pc: usize,
}

#[derive(Debug, Clone)]
pub struct Context {
    pub caller: Option<Caller>,
    pub code: Arc<CompiledCode>,
    pc: usize, // program counter
    locals: Vec<Value>,
    stack: Vec<Value>,
    sp: usize, // stack pointer
}

pub struct Interp {
    pub pool: Arc<ThreadPool>,
    pub heap: Arc<Heap>,
    pub mgroup_id: ObjectId,
}

impl Context {
    #[inline]
    pub fn new(caller_ctx: Option<&Context>, code: Arc<CompiledCode>, args: Vec<Value>) -> Context {
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
        let pool = Arc::new(ThreadPool::new());
        let heap = Arc::new(Heap::new(pool.clone()));
        let mgroup = Content::ModuleGroup(Arc::new(ModuleGroup::new()));
        let mgroup_id = heap.create(|_| mgroup);
        Interp {
            pool,
            heap,
            mgroup_id: mgroup_id,
        }
    }

    pub fn get_module_group<F, U>(&self, f: F) -> U
    where
        F: FnOnce(&ModuleGroup) -> U,
    {
        match self.heap.get_content(self.mgroup_id).unwrap() {
            Content::ModuleGroup(group) => f(&*group),
            _ => panic!("not found module group"),
        }
    }

    pub fn get_module(&self, name: &str) -> Option<(ObjectId, Arc<Module>)> {
        self.get_module_group(|group| match group.get(name) {
            Some(id) => match self.heap.get_content(id) {
                Some(Content::Module(m)) => Some((id, m.clone())),
                _ => None,
            },
            None => None,
        })
    }

    pub fn get_module_for_id(&self, id: ObjectId) -> Option<Arc<Module>> {
        match self.heap.get_content(id) {
            Some(Content::Module(m)) => Some(m.clone()),
            _ => None,
        }
    }

    pub fn add_module(&self, m: Arc<Module>) {
        self.get_module_group(|group| {
            let name = m.get_name().unwrap();
            group.add(&name, self.heap.create(|_| Content::Module(m)));
        });
    }

    pub fn install_modules(&self, ms: Vec<Module>) {
        for m in ms.into_iter() {
            self.add_module(Arc::new(m))
        }
    }

    pub fn eval(
        interp: Arc<Interp>,
        caller: Option<&Context>,
        code: Arc<CompiledCode>,
        args: Vec<Value>,
    ) -> Result<Value> {
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
                Opcode::Nop => (),
                Opcode::LoadConst(i) => ctx.load_const(*i as usize),
                Opcode::LoadInt(val) => ctx.load(Value::Int(*val as i64)),
                Opcode::LoadLocal(i) => ctx.load_local(*i as usize),
                Opcode::LoadTrue => ctx.load(Value::Bool(true)),
                Opcode::LoadFalse => ctx.load(Value::Bool(false)),
                Opcode::LoadUndef => ctx.load(Value::Atom(Arc::new("undef".to_string()))),
                Opcode::LoadOk => ctx.load(Value::Atom(Arc::new("ok".to_string()))),
                Opcode::LoadError => ctx.load(Value::Atom(Arc::new("error".to_string()))),
                Opcode::LoadBitstr(size, val) => ctx.load(Value::Bitstr32(*size, *val)),
                Opcode::LoadEmpty(BlockTag::List) => ctx.load(interp.heap.get_list_nil().1),
                Opcode::StorePopLocal(i) => {
                    let val = ctx.pop();
                    ctx.store(*i as usize, val);
                }
                Opcode::GetField(i) => {
                    let val = ctx.pop();
                    match val {
                        Value::Tuple(elts) => ctx.load(elts[*i as usize].clone()),
                        _ => panic!("# GetField not supported {:?}", val),
                    }
                }
                Opcode::GetProp => {
                    let pname = ctx.pop_string();
                    let val = ctx.pop();
                    match val {
                        Value::Module(id) => match interp.heap.get(id) {
                            Some(Object {
                                content: Content::Module(ref m),
                                ..
                            }) => match m.fields.get(&pname) {
                                None => panic!("# GetProp: key {:?} is not found", pname),
                                Some(val) => ctx.load(val.clone()),
                            },
                            _ => panic!("# module {:?} not found", id),
                        },
                        _ => panic!("# GetProp non-module not impl"),
                    };
                }
                Opcode::GetGlobal => {
                    let name = ctx.pop_string();
                    match interp.get_module(&name) {
                        Some((id, _)) => ctx.load(Value::Module(id)),
                        None => panic!("# GetGlobal not impl non module"),
                    }
                }
                Opcode::SetGlobal => {
                    let val = ctx.pop();
                    let name = ctx.pop_string();
                    match val {
                        Value::Module(id) => match interp.get_module_for_id(id) {
                            Some(m) => {
                                m.set_name(&name);
                                interp.add_module(m.clone())
                            }
                            None => panic!("# SetGlobal not found module {}", name),
                        },
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
                Opcode::ReturnUndef => return Ok(Value::Atom(Arc::new("undefined".to_string()))),
                Opcode::MakeBlock(BlockTag::NonConst, size) => {
                    let vals = ctx.popn(*size as usize);
                    ctx.load(Value::Array(Arc::new(vals)))
                }
                Opcode::MakeBlock(BlockTag::Module, size) => {
                    // TODO: name, attributes
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
                    // TODO
                    // m.set_name(name);
                    let id = interp.heap.create(|_| Content::Module(Arc::new(m)));
                    ctx.load(Value::Module(id));
                }
                Opcode::MakeBlock(BlockTag::List, 0) => ctx.load(interp.heap.get_list_nil().1),
                Opcode::MakeBlock(BlockTag::List, _) => {
                    let tail = ctx.pop();
                    let head = ctx.pop();
                    ctx.load(List::new_value(&interp.heap, head, tail))
                }
                Opcode::MakeBlock(BlockTag::Tuple, size) => {
                    let vals = ctx.popn(*size as usize);
                    ctx.load(Value::Tuple(Arc::new(vals)))
                }
                Opcode::Apply(nargs) => {
                    let args = ctx.popn(*nargs as usize);
                    let fval = ctx.pop();
                    let ret = try!(Interp::eval_fun(interp.clone(), &ctx, fval, args));
                    ctx.load(ret)
                }
                Opcode::Spawn => {
                    // TODO
                    let args = ctx.pop();
                    let fval = ctx.pop();
                    println!("# spawn f {:?}, {:?}", fval, args);
                    let args = List::get_content(&*interp.heap, &args)
                        .unwrap()
                        .to_vec(&interp.heap)
                        .unwrap();
                    let interp2 = interp.clone();
                    let ctx2 = ctx.clone();
                    interp.pool.group.async(&interp.pool.user, move || {
                        let _ = Arc::new(Interp::eval_fun(interp2, &ctx2, fval, args));
                        println!("# queue exec");
                    });
                    // TODO
                    ctx.load(Value::Nil);
                }
                Opcode::BlockSize => {
                    let val = ctx.pop();
                    let list = BrList::from_value(interp.heap.clone(), &val).unwrap();
                    match BrList::len(&list) {
                        Some(i) => ctx.load(Value::Int(i as i64)),
                        None => panic!("# block size: bad list"),
                    }
                }
                Opcode::ListLen => {
                    let val = ctx.pop();
                    let list = BrList::from_value(interp.heap.clone(), &val).unwrap();
                    ctx.load(Value::Int(BrList::len(&list).unwrap() as i64))
                }
                Opcode::ListCons => {
                    let tail = ctx.pop();
                    let head = ctx.pop();
                    ctx.load(List::new_value(&interp.heap, head, tail))
                }
                Opcode::ListComprGen(_) => {
                    // TODO
                    ctx.load(Value::Atom(Arc::new("undefined".to_string())));
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
                Opcode::TestNonNil => {
                    let val = ctx.pop();
                    match val {
                        Value::Nil => ctx.load(Value::Bool(false)),
                        _ => ctx.load(Value::Bool(true)),
                    }
                }
                op => panic!("# eval: not impl {:?}", op),
            }
            ctx.pc += 1;
        }
    }

    fn eval_fun(
        interp: Arc<Interp>,
        ctx: &Context,
        fval: Value,
        args: Vec<Value>,
    ) -> Result<Value> {
        match fval {
            Value::CompiledCode(code) => Interp::eval(interp.clone(), Some(ctx), code, args),
            Value::Nif(nif) => {
                let arglist = try!(ArgList::new(interp.heap.clone(), args.len(), args));
                match ((*nif).fun)(interp, &arglist) {
                    Ok(val) => Ok(val),
                    Err(err) => return Err(err),
                }
            }
            _ => panic!("# apply: not function {:?}", fval),
        }
    }
}
