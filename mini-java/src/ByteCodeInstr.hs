module Jvm.Data.ByteCodeInstr where


-- each bytecide instruction with arguments
data ByteCode_Instr_wArgs = 
    IConst
    | BIPush
    | Ldc
    | Ldc_W
    | Ldc2_W
    | ALoad
    | AStore
    | ILoad
    | IStore
    | If
    | If_ICmpEq
    | If_ICmpNeq
    | If_ICmpLeq
    | If_ICmpLt
    | If_ICmpGeq
    | If_ICmpGt
    | If_ACmpEq
    | If_ACmpNeq 
    | NoInstr
    deriving (Show, Eq)

-- each bytecode instruction w/o arguments
data ByteCode_Instr_woArgs =
    Nop
    | AConst_Null
    | IConst_m1
    | IConst_0
    | IConst_1
    | IConst_2
    | IConst_3
    | IConst_4
    | IConst_5
    | LConst_0
    | LConst_1
    | ALoad_0
    | ALoad_1
    | ALoad_2
    | ALoad_3
    | AStore_0
    | AStore_1
    | AStore_2
    | AStore_3
    | ILoad_0
    | ILoad_1
    | ILoad_2
    | ILoad_3
    | IStore_0
    | IStore_1
    | IStore_2
    | IStore_3
    | IAdd
    | ISub
    | IMul
    | IDiv
    | IRem
    | INeg
    | IShl
    | IShr
    | IUShr
    | IAnd
    | IOr
    | IXor
    | I2C
    | InstanceOf
    | New
    | GetField
    | PutField
    | GetStatic
    | PutStatic
    | InvokeVirtual
    | InvokeStatic
    | InvokeSpecial
    | InvokeDynamic
    | Goto
    | Goto_W
    | IfNull
    | IfNonNull
    | Return
    | IReturn
    | AReturn
    | Dup
    | Dup_X1
    | Dup_X2
    | Dup2
    | Dup2_X1
    | Dup2_X2
    | Pop
    | Pop2
    | Swap
    deriving (Show)


-- Convert bytecode instructions with arguments to opcodes
byteCodeToOpCode_wArgs :: ByteCode_Instr_wArgs -> Int -> Int
byteCodeToOpCode_wArgs instr arg = case instr of
    IConst -> 0x02 + arg
    BIPush -> 0x10 + arg  
    Ldc -> 0x12 + arg  
    Ldc_W -> 0x13 + arg  
    Ldc2_W -> 0x14 + arg  
    ALoad -> 0x19 + arg  
    AStore -> 0x3A + arg  
    ILoad -> 0x15 + arg  
    IStore -> 0x36 + arg  
    If -> 0x99 + arg  
    If_ICmpEq -> 0x9F + arg  
    If_ICmpNeq -> 0xA0 + arg  
    If_ICmpLeq -> 0xA1 + arg  
    If_ICmpLt -> 0xA2 + arg  
    If_ICmpGeq -> 0xA3 + arg  
    If_ICmpGt -> 0xA4 + arg  
    If_ACmpEq -> 0xA5 + arg  
    If_ACmpNeq -> 0xA6 + arg
    

-- Convert bytecode instructions w/o arguments to opcodes
byteCodeToOpCode_woArgs :: ByteCode_Instr_woArgs -> Int
byteCodeToOpCode_woArgs instr = case instr of
    Nop -> 0x00
    AConst_Null -> 0x01
    IConst_m1 -> 0x02
    IConst_0 -> 0x03
    IConst_1 -> 0x04
    IConst_2 -> 0x05
    IConst_3 -> 0x06
    IConst_4 -> 0x07
    IConst_5 -> 0x08
    LConst_0 -> 0x09
    LConst_1 -> 0x0A
    ALoad_0 -> 0x2A
    ALoad_1 -> 0x2B
    ALoad_2 -> 0x2C
    ALoad_3 -> 0x2D
    AStore_0 -> 0x4B
    AStore_1 -> 0x4C
    AStore_2 -> 0x4D
    AStore_3 -> 0x4E
    ILoad_0 -> 0x1A
    ILoad_1 -> 0x1B
    ILoad_2 -> 0x1C
    ILoad_3 -> 0x1D
    IStore_0 -> 0x3B
    IStore_1 -> 0x3C
    IStore_2 -> 0x3D
    IStore_3 -> 0x3E
    IAdd -> 0x60
    ISub -> 0x64
    IMul -> 0x68
    IDiv -> 0x6C
    IRem -> 0x70
    INeg -> 0x74
    IShl -> 0x78
    IShr -> 0x7A
    IUShr -> 0x7C
    IAnd -> 0x7E
    IOr -> 0x80
    IXor -> 0x82
    I2C -> 0x92
    InstanceOf -> 0xC1
    New -> 0xBB
    GetField -> 0xB4
    PutField -> 0xB5
    GetStatic -> 0xB2
    PutStatic -> 0xB3
    InvokeVirtual -> 0xB6
    InvokeStatic -> 0xB8
    InvokeSpecial -> 0xB7
    InvokeDynamic -> 0xBA
    Goto -> 0xA7
    Goto_W -> 0xC8
    IfNull -> 0xC6
    IfNonNull -> 0xC7
    Return -> 0xB1
    IReturn -> 0xAC
    AReturn -> 0xB0
    Dup -> 0x59
    Dup_X1 -> 0x5A
    Dup_X2 -> 0x5B
    Dup2 -> 0x5C
    Dup2_X1 -> 0x5D
    Dup2_X2 -> 0x5E
    Pop -> 0x57
    Pop2 -> 0x58
    Swap -> 0x5F


-- Convert opcode to bytecode instruction
opCodeToByteCode_wArgs :: Int -> ByteCode_Instr_wArgs
opCodeToByteCode_wArgs opcode = case opcode of
    0x02 -> IConst
    0x10 -> BIPush
    0x12 -> Ldc
    0x13 -> Ldc_W
    0x14 -> Ldc2_W
    0x19 -> ALoad
    0x3A -> AStore
    0x15 -> ILoad
    0x36 -> IStore
    0x99 -> If
    0x9F -> If_ICmpEq
    0xA0 -> If_ICmpNeq
    0xA1 -> If_ICmpLeq
    0xA2 -> If_ICmpLt
    0xA3 -> If_ICmpGeq
    0xA4 -> If_ICmpGt
    0xA5 -> If_ACmpEq
    0xA6 -> If_ACmpNeq
    _ -> NoInstr


-- Convert opcode to bytecode instruction
opCodeToByteCode_woArgs :: Int -> ByteCode_Instr_woArgs
opCodeToByteCode_woArgs opcode = case opcode of
    0x00 -> Nop
    0x01 -> AConst_Null
    0x01 -> AConst_Null
    0x02 -> IConst_m1
    0x03 -> IConst_0
    0x04 -> IConst_1
    0x05 -> IConst_2
    0x06 -> IConst_3
    0x07 -> IConst_4
    0x08 -> IConst_5
    0x09 -> LConst_0
    0x0A -> LConst_1
    0x2A -> ALoad_0
    0x2B -> ALoad_1
    0x2C -> ALoad_2
    0x2D -> ALoad_3
    0x4B -> AStore_0
    0x4C -> AStore_1
    0x4D -> AStore_2
    0x4E -> AStore_3
    0x1A -> ILoad_0
    0x1B -> ILoad_1
    0x1C -> ILoad_2
    0x1D -> ILoad_3
    0x3B -> IStore_0
    0x3C -> IStore_1
    0x3D -> IStore_2
    0x3E -> IStore_3
    0x60 -> IAdd
    0x64 -> ISub
    0x68 -> IMul
    0x6C -> IDiv
    0x70 -> IRem
    0x74 -> INeg
    0x78 -> IShl
    0x7A -> IShr
    0x7C -> IUShr
    0x7E -> IAnd
    0x80 -> IOr
    0x82 -> IXor
    0x92 -> I2C
    0xC1 -> InstanceOf
    0xBB -> New
    0xB4 -> GetField
    0xB5 -> PutField
    0xB2 -> GetStatic
    0xB3 -> PutStatic
    0xB6 -> InvokeVirtual
    0xB8 -> InvokeStatic
    0xB7 -> InvokeSpecial
    0xBA -> InvokeDynamic
    0xA7 -> Goto
    0xC8 -> Goto_W
    0xC6 -> IfNull
    0xC7 -> IfNonNull
    0xB1 -> Return
    0xAC -> IReturn
    0xB0 -> AReturn
    0x59 -> Dup
    0x5A -> Dup_X1
    0x5B -> Dup_X2
    0x5C -> Dup2
    0x5D -> Dup2_X1
    0x5E -> Dup2_X2
    0x57 -> Pop
    0x58 -> Pop2
    0x5F -> Swap
    _ -> error "no instruction"


-- Function to convert bytecode instruction to string
byteCodeToString_wArgs :: ByteCode_Instr_wArgs -> Int -> String
byteCodeToString_wArgs instr arg = case instr of
    IConst -> "iconst " ++ show arg
    BIPush -> "bipush " ++ show arg
    Ldc -> "ldc " ++ show arg
    Ldc_W -> "ldc_w " ++ show arg
    Ldc2_W -> "ldc2_w " ++ show arg
    ALoad -> "aload " ++ show arg
    AStore -> "astore " ++ show arg
    ILoad -> "iload " ++ show arg
    IStore -> "istore " ++ show arg
    If -> "if " ++ show arg
    If_ICmpEq -> "if_icmpeq " ++ show arg
    If_ICmpNeq -> "if_icmpne " ++ show arg
    If_ICmpLeq -> "if_icmple " ++ show arg
    If_ICmpLt -> "if_icmplt " ++ show arg
    If_ICmpGeq -> "if_icmpge " ++ show arg
    If_ICmpGt -> "if_icmpgt " ++ show arg
    If_ACmpEq -> "if_acmpeq " ++ show arg
    If_ACmpNeq -> "if_acmpne " ++ show arg
    


-- Function to convert bytecode instruction to string
byteCodeToString_woArgs :: ByteCode_Instr_woArgs -> String
byteCodeToString_woArgs instr = case instr of
    Nop -> "nop"
    AConst_Null -> "aconst_null"
    IConst_m1 -> "iconst_m1"
    IConst_0 -> "iconst_0"
    IConst_1 -> "iconst_1"
    IConst_2 -> "iconst_2"
    IConst_3 -> "iconst_3"
    IConst_4 -> "iconst_4"
    IConst_5 -> "iconst_5"
    LConst_0 -> "lconst_0"
    LConst_1 -> "lconst_1"
    ALoad_0 -> "aload_0"
    ALoad_1 -> "aload_1"
    ALoad_2 -> "aload_2"
    ALoad_3 -> "aload_3"
    AStore_0 -> "astore_0"
    AStore_1 -> "astore_1"
    AStore_2 -> "astore_2"
    AStore_3 -> "astore_3"
    ILoad_0 -> "iload_0"
    ILoad_1 -> "iload_1"
    ILoad_2 -> "iload_2"
    ILoad_3 -> "iload_3"
    IStore_0 -> "istore_0"
    IStore_1 -> "istore_1"
    IStore_2 -> "istore_2"
    IStore_3 -> "istore_3"
    IAdd -> "iadd"
    ISub -> "isub"
    IMul -> "imul"
    IDiv -> "idiv"
    IRem -> "irem"
    INeg -> "ineg"
    IShl -> "ishl"
    IShr -> "ishr"
    IUShr -> "iushr"
    IAnd -> "iand"
    IOr -> "ior"
    IXor -> "ixor"
    I2C -> "i2c"
    InstanceOf -> "instanceof"
    New -> "new"
    GetField -> "getfield"
    PutField -> "putfield"
    GetStatic -> "getstatic"
    PutStatic -> "putstatic"
    InvokeVirtual -> "invokevirtual"
    InvokeStatic -> "invokestatic"
    InvokeSpecial -> "invokespecial"
    InvokeDynamic -> "invokedynamic"
    Goto -> "goto"
    Goto_W -> "goto_w"
    IfNull -> "ifnull"
    IfNonNull -> "ifnonnull"
    Return -> "return"
    IReturn -> "ireturn"
    AReturn -> "areturn"
    Dup -> "dup"
    Dup_X1 -> "dup_x1"
    Dup_X2 -> "dup_x2"
    Dup2 -> "dup2"
    Dup2_X1 -> "dup2_x1"
    Dup2_X2 -> "dup2_x2"
    Pop -> "pop"
    Pop2 -> "pop2"
    Swap -> "swap"


