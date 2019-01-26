use arglist::ArgList;
use heap::{Content, Heap, ObjectId};
use list::{BrList, List};
use module::{Module, ModuleDesc, ModuleGroup};
use opcode::{BlockTag, Opcode};
use process::{Process, ProcessGroup};
use result::Result;
use std::cell::RefCell;
use std::collections::hash_map::DefaultHasher;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::sync::Arc;
use sync::{RecLock, ThreadPool};
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
    pub procs: ProcessGroup,
    pub mods: RecLock<ModuleGroup>,
    pub atoms: RecLock<RefCell<HashMap<usize, Arc<String>>>>,
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
        let heap = Arc::new(Heap::new(None));
        Interp {
            pool: Arc::new(ThreadPool::new()),
            heap: heap.clone(),
            procs: ProcessGroup::new(heap),
            mods: RecLock::new(ModuleGroup::new()),
            atoms: RecLock::new(RefCell::new(HashMap::new())),
        }
    }

    pub fn get_module(&self, name: &str) -> Option<Arc<Module>> {
        match self.mods.lock() {
            Ok(guard) => (*guard).get(name),
            _ => panic!("cannot lock"),
        }
    }

    pub fn get_module_for_id(&self, id: ObjectId) -> Option<Arc<Module>> {
        match self.heap.get_content(id) {
            Some(Content::Module(m)) => Some(m.clone()),
            _ => None,
        }
    }

    pub fn add_module(&self, name: &str, m: Arc<Module>) {
        match self.mods.lock() {
            Ok(guard) => {
                println!("# add module id {:?}", m);
                (*guard).add(name, m);
            }
            _ => panic!("cannot lock"),
        }
    }

    pub fn install_modules(&self, descs: Vec<ModuleDesc>) {
        for desc in descs.into_iter() {
            self.heap.create(|id| {
                let name = desc.name.clone();
                let m = Arc::new(Module::new(id, desc));
                self.add_module(&name, m.clone());
                Content::Module(m)
            });
        }
    }

    pub fn get_atom(&self, id: usize) -> Option<Arc<String>> {
        match self.atoms.lock() {
            Ok(guard) => guard.borrow().get(&id).cloned(),
            _ => panic!("cannot lock"),
        }
    }

    pub fn add_atom(&self, name: &str) -> usize {
        match self.atoms.lock() {
            Ok(guard) => {
                let mut hasher = DefaultHasher::new();
                name.hash(&mut hasher);
                let id = hasher.finish() as usize;
                let mut atoms = guard.borrow_mut();
                match atoms.get(&id) {
                    Some(_) => id,
                    None => {
                        atoms.insert(id, Arc::new(name.to_string()));
                        id
                    }
                }
            }
            _ => panic!("cannot lock"),
        }
    }

    pub fn eval(
        interp: &Arc<Interp>,
        proc: &Arc<Process>,
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
                Opcode::LoadOk(0) => ctx.load(Value::Atom(Arc::new("ok".to_string()))),
                Opcode::LoadError(0) => ctx.load(Value::Atom(Arc::new("error".to_string()))),
                Opcode::LoadBitstr(size, val) => ctx.load(Value::Bitstr32(*size, *val)),
                Opcode::StorePopLocal(i) => {
                    let val = ctx.pop();
                    ctx.store(*i as usize, val);
                }
                Opcode::GetBlockField(i) => {
                    let val = ctx.pop();
                    match val {
                        Value::Tuple(elts) => ctx.load(elts[*i as usize].clone()),
                        _ => panic!("# GetField not supported {:?}", val),
                    }
                }
                Opcode::XGetBlockField => {
                    let name = ctx.pop_string();
                    let val = ctx.pop();
                    match val {
                        Value::Module(id) => match interp.get_module_for_id(id) {
                            Some(m) => match m.get_prop(&name) {
                                None => panic!("# GetProp: key {:?} is not found {:?}", name, m),
                                Some(val) => ctx.load(val.clone()),
                            },
                            None => panic!("# GetProp: module id {:?} not found", id),
                        },
                        _ => panic!("# GetProp non-module not impl"),
                    };
                }
                Opcode::GetGlobal => {
                    let name = ctx.pop_string();
                    match interp.get_module(&name) {
                        // TODO: check module whether exists or not
                        Some(m) => ctx.load(Value::Module(m.id)),
                        None => panic!("# GetGlobal not impl non module"),
                    }
                }
                Opcode::SetGlobal => {
                    let val = ctx.pop();
                    let name = ctx.pop_string();
                    match val {
                        Value::Module(id) => match proc.heap.get_content(id) {
                            Some(Content::Module(m)) => {
                                let gid = interp.heap.create(|id| {
                                    let m = Arc::new(Module::new(id, m.desc.clone()));
                                    interp.add_module(&name, m.clone());
                                    Content::Module(m)
                                });
                                proc.heap.create(|_| Content::Global(gid));
                            }
                            _ => match interp.heap.get_content(id) {
                                Some(_) => (),
                                _ => panic!("# SetGlobal: module {:?} not found", id),
                            },
                        },
                        _ => panic!("# SetGlobal: not module {:?}", val),
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
                Opcode::CreateBlock(BlockTag::NonConst, size) => {
                    let vals = ctx.popn(*size as usize);
                    ctx.load(Value::Array(Arc::new(vals)))
                }
                /*
                Opcode::CreateBlock(BlockTag::Module, size) => {
                    // TODO: name, attributes
                    let size = *size as usize;
                    let vals = ctx.popn(size);
                    let mut  = Module::new();
                    for i in 0..size {
                        if i % 2 == 0 {
                            let key = vals[i].get_string().unwrap().clone();
                            let val = vals[i + 1].clone();
                            m.fields.insert(key, val);
                        }
                    }
                    // TODO
                    // m.set_name(name);
                    let id = proc.heap.create(|_| Content::Module(Arc::new(m)));
                    ctx.load(Value::Module(id));
                }
                */
                Opcode::CreateBlock(BlockTag::List, 0) => ctx.load(proc.heap.get_list_nil().1),
                Opcode::CreateBlock(BlockTag::List, _) => {
                    let tail = ctx.pop();
                    let head = ctx.pop();
                    ctx.load(List::new_value(&proc.heap, head, tail))
                }
                Opcode::CreateBlock(BlockTag::Tuple, size) => {
                    let vals = ctx.popn(*size as usize);
                    ctx.load(Value::Tuple(Arc::new(vals)))
                }
                Opcode::Apply(nargs) => {
                    let args = ctx.popn(*nargs as usize);
                    let fval = ctx.pop();
                    let ret = try!(Interp::eval_fun(interp, proc, &ctx, fval, args));
                    ctx.load(ret)
                }
                Opcode::Spawn => {
                    // TODO
                    let args = ctx.pop();
                    let fval = ctx.pop();
                    //println!("# spawn f {:?}, {:?}", fval, args);
                    let args = List::get_content(&*proc.heap, &args)
                        .unwrap()
                        .to_vec(&proc.heap)
                        .unwrap();
                    let interp2 = interp.clone();
                    let ctx2 = ctx.clone();
                    let proc2 = interp.procs.create();
                    interp.pool.process_async(move || {
                        let _ = Arc::new(Interp::eval_fun(&interp2, &proc2, &ctx2, fval, args));
                        //println!("# queue exec");
                        interp2.procs.finish(proc2.id);
                        //println!("# end queue exec");
                    });
                    // TODO
                    ctx.load(Value::Nil);
                }
                Opcode::GetBlockSize => {
                    let val = ctx.pop();
                    let list = BrList::from_value(proc.heap.clone(), &val).unwrap();
                    match BrList::len(&list) {
                        Some(i) => ctx.load(Value::Int(i as i64)),
                        None => panic!("# block size: bad list"),
                    }
                }
                Opcode::ListLen => {
                    let val = ctx.pop();
                    let list = BrList::from_value(proc.heap.clone(), &val).unwrap();
                    ctx.load(Value::Int(BrList::len(&list).unwrap() as i64))
                }
                Opcode::ListCons => {
                    let tail = ctx.pop();
                    let head = ctx.pop();
                    ctx.load(List::new_value(&proc.heap, head, tail))
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
        interp: &Arc<Interp>,
        proc: &Arc<Process>,
        ctx: &Context,
        fval: Value,
        args: Vec<Value>,
    ) -> Result<Value> {
        match fval {
            Value::CompiledCode(code) => Interp::eval(interp, proc, Some(ctx), code, args),
            Value::Nif(nif) => {
                let arglist = try!(ArgList::new(proc.heap.clone(), args.len(), args));
                match ((*nif).fun)(interp, proc, &arglist) {
                    Ok(val) => Ok(val),
                    Err(err) => return Err(err),
                }
            }
            _ => panic!("# apply: not function {:?}", fval),
        }
    }
}
