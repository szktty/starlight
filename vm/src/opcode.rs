#[derive(Serialize, Deserialize, Debug, Clone)]
pub enum Opcode {
    Nop,
    LoadLocal(u16),
    LoadConst(u16),
    LoadTrue,
    LoadFalse,
    LoadInt(i32),
    LoadUndef,
    LoadOk,
    LoadError,
    LoadEmpty(BlockTag),
    LoadSelfFun,
    LoadBitstr(u8, u32),
    StorePopLocal(u16),
    GetField(u16),
    GetProp,
    GetGlobal,
    SetField(u16),
    SetGlobal,
    Return,
    ReturnTrue,
    ReturnFalse,
    ReturnUndef,
    ReturnOk,
    ReturnError,
    Pop,
    NoMatch,
    Loophead,
    Jump(i16),
    BranchTrue(i16),
    BranchFalse(i16),
    MakeBlock(BlockTag, u16),
    MakeBitstr(BitstrType, BitstrSign, BitstrEndian, Option<u8>),
    MakeOk(u8),
    MakeError(u8),
    Apply(u8),
    Spawn,
    BlockSize,
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
    Process,
    Module,
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
