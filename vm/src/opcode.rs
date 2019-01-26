#[derive(Serialize, Deserialize, Debug, Clone)]
pub enum Opcode {
    Nop,
    LoadLocal(u16),
    LoadConst(u16),
    LoadTrue,
    LoadFalse,
    LoadInt(i32),
    LoadUndef,
    LoadOk(u8),
    LoadError(u8),
    LoadSelfFun,
    LoadBitstr(u8, u32),
    StorePopLocal(u16),
    GetGlobal,
    SetGlobal,
    GetBlockField(u16),
    XGetBlockField,
    SetBlockField(u16),
    XSetBlockField,
    GetBlockSize,
    Return,
    Pop,
    NoMatch,
    Loophead,
    Jump(i16),
    BranchTrue(i16),
    BranchFalse(i16),
    CreateBlock(BlockTag, u16),
    CreateBitstr(BitstrType, BitstrSign, BitstrEndian, Option<u8>),
    Apply(u8),
    Spawn,
    ListLen,
    ListCons,
    ListRev,
    ListComprGen(u16),
    Eq,
    Ne,
    Lt,
    Add,
    Add1,
    Sub,
    TestNonNil,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub enum BlockTag {
    // non-constant constructor
    NonConst,

    // structure
    Record,
    Map,
    CompiledCode,
    Closure,

    // primitive
    NoScan,
    String,
    Binary,
    Bitstr,
    List,
    Tuple,
    Custom,
}

/*
#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct BitstrSpec {
    ty: BitstrType,
    size: u32,
    sign: BitstrSign,
    endian: BitstrEndian,
    unit: Option<u8>,
}
*/

#[derive(Serialize, Deserialize, Debug, Clone)]
pub enum BitstrType {
    Int,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub enum BitstrSign {
    Signed,
    Unsigned,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub enum BitstrEndian {
    Little,
    Big,
    Native,
}
