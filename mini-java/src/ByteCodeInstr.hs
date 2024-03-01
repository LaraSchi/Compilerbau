module ByteCodeInstr where

import Data.Bits


-- every byte code instruction with one argument
data ByteCode_Instr_w1Arg = 
    BIPush
    | ALoad
    | AStore
    | ILoad
    | IStore
    | Ldc
    | No1ArgInstr
    deriving (Show, Eq)


-- every byte code instruction with two arguments
data ByteCode_Instr_w2Args = 
    If    
    | If_ICmpEq
    | If_ICmpNeq
    | If_ICmpLeq
    | If_ICmpLt
    | If_ICmpGeq
    | If_ICmpGt
    | If_ACmpEq
    | If_ACmpNeq
    | IfNull
    | IfNonNull
    | InvokeVirtual
    | InvokeStatic
    | InvokeSpecial
    | Goto
    | GetField
    | PutField
    | GetStatic
    | PutStatic
    | InstanceOf 
    | Ldc_W
    | Ldc2_W
    | New
    | No2ArgInstr
    deriving (Show, Eq)


-- every byte code instruction with four argument
data ByteCode_Instr_w4Args = 
    Goto_W
    | InvokeDynamic
    | No4ArgInstr
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
    | NoInstr
    deriving (Show)



-- Convert bytecode instructions with arguments to opcodes
byteCodeToOpCode_w1Arg :: ByteCode_Instr_w1Arg -> Int -> Int
byteCodeToOpCode_w1Arg opc arg = case opc of
    BIPush -> (arg `shiftL` 8) + 0x10
    ALoad -> (arg `shiftL` 8) + 0x19
    AStore -> (arg `shiftL` 8) + 0x3A
    ILoad -> (arg `shiftL` 8) + 0x15
    IStore -> (arg `shiftL` 8) + 0x36
    Ldc -> (arg `shiftL` 8) + 0x12


-- Convert bytecode instructions with arguments to opcodes
byteCodeToOpCode_w2Args :: ByteCode_Instr_w2Args -> Int -> Int -> Int
byteCodeToOpCode_w2Args opc arg1 arg2 = case opc of
    If -> (arg1 `shiftL` 16) + (arg2 `shiftL` 8) + 0x99
    If_ICmpEq -> (arg1 `shiftL` 16) + (arg2 `shiftL` 8) + 0x9F
    If_ICmpNeq -> (arg1 `shiftL` 16) + (arg2 `shiftL` 8) + 0xA0
    If_ICmpLeq -> (arg1 `shiftL` 16) + (arg2 `shiftL` 8) + 0xA4
    If_ICmpLt -> (arg1 `shiftL` 16) + (arg2 `shiftL` 8) + 0xA1
    If_ICmpGeq -> (arg1 `shiftL` 16) + (arg2 `shiftL` 8) + 0xA2
    If_ICmpGt -> (arg1 `shiftL` 16) + (arg2 `shiftL` 8) + 0xA3
    If_ACmpEq -> (arg1 `shiftL` 16) + (arg2 `shiftL` 8) + 0xA5
    If_ACmpNeq -> (arg1 `shiftL` 16) + (arg2 `shiftL` 8) + 0xA6
    IfNull -> (arg1 `shiftL` 16) + (arg2 `shiftL` 8) + 0xC6
    IfNonNull -> (arg1 `shiftL` 16) + (arg2 `shiftL` 8) + 0xC7
    InvokeVirtual -> (arg1 `shiftL` 16) + (arg2 `shiftL` 8) + 0xB6
    InvokeStatic -> (arg1 `shiftL` 16) + (arg2 `shiftL` 8) + 0xB8
    InvokeSpecial -> (arg1 `shiftL` 16) + (arg2 `shiftL` 8) + 0xB7
    Goto -> (arg1 `shiftL` 16) + (arg2 `shiftL` 8) + 0xA7
    GetField -> (arg1 `shiftL` 16) + (arg2 `shiftL` 8) + 0xB4
    PutField -> (arg1 `shiftL` 16) + (arg2 `shiftL` 8) + 0xB5
    GetStatic -> (arg1 `shiftL` 16) + (arg2 `shiftL` 8) + 0xB2
    PutStatic -> (arg1 `shiftL` 16) + (arg2 `shiftL` 8) + 0xB3
    InstanceOf -> (arg1 `shiftL` 16) + (arg2 `shiftL` 8) + 0xC1
    Ldc_W -> (arg1 `shiftL` 16) + (arg2 `shiftL` 8) + 0x13
    Ldc2_W -> (arg1 `shiftL` 16) + (arg2 `shiftL` 8) + 0x14
    New -> (arg1 `shiftL` 16) + (arg2 `shiftL` 8) + 0xBB

    
-- Convert bytecode instructions with arguments to opcodes
byteCodeToOpCode_w4Args :: ByteCode_Instr_w4Args -> Int -> Int -> Int -> Int -> Int
byteCodeToOpCode_w4Args opc arg1 arg2 arg3 arg4 = case opc of
    InvokeDynamic -> (arg1 `shiftL` 32) 
                    + (arg2 `shiftL` 24) 
                    + (0x0 `shiftL` 16) 
                    + (0x0 `shiftL` 8) 
                    + 0xBA
    Goto_W -> (arg1 `shiftL` 24)
            + (arg2 `shiftL` 24) 
            + (arg3 `shiftL` 16) 
            + (arg4 `shiftL` 8) 
            + 0xC8
    
    

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
opCodeToByteCode_w1Arg :: Int -> ByteCode_Instr_w1Arg
opCodeToByteCode_w1Arg opc = case opc of
    0x10 -> BIPush
    0x19 -> ALoad
    0x3A -> AStore
    0x15 -> ILoad
    0x36 -> IStore
    0x12 -> Ldc
    _ -> No1ArgInstr


-- Convert opcode to bytecode instruction
opCodeToByteCode_w2Args :: Int -> ByteCode_Instr_w2Args
opCodeToByteCode_w2Args opc = case opc of
    0x99 -> If
    0x9F -> If_ICmpEq
    0xA0 -> If_ICmpNeq
    0xA4 -> If_ICmpLeq
    0xA1 -> If_ICmpLt
    0xA2 -> If_ICmpGeq
    0xA3 -> If_ICmpGt
    0xA5 -> If_ACmpEq
    0xA6 -> If_ACmpNeq
    0xC6 -> IfNull
    0xC7 -> IfNonNull
    0xB6 -> InvokeVirtual
    0xB8 -> InvokeStatic
    0xB7 -> InvokeSpecial
    0xA7 -> Goto
    0xB4 -> GetField
    0xB5 -> PutField
    0xB2 -> GetStatic
    0xB3 -> PutStatic
    0xC1 -> InstanceOf
    0x13 -> Ldc_W
    0x14 -> Ldc2_W
    0xBB -> New
    _ -> No2ArgInstr


-- Convert opcode to bytecode instruction
opCodeToByteCode_w4Args :: Int -> ByteCode_Instr_w4Args
opCodeToByteCode_w4Args opc = case opc of
    0xBA -> InvokeDynamic 
    0xC8 -> Goto_W
    _ -> No4ArgInstr


-- Convert opcode to bytecode instruction
opCodeToByteCode_woArgs :: Int -> ByteCode_Instr_woArgs
opCodeToByteCode_woArgs opc = case opc of
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
    _ -> NoInstr


-- Function to convert bytecode instruction to string
byteCodeToString_w1Arg :: ByteCode_Instr_w1Arg -> Int -> String
byteCodeToString_w1Arg opc arg = case opc of
    BIPush -> "bipush " ++ show arg
    ALoad -> "aload " ++ show arg
    AStore -> "astore " ++ show arg
    ILoad -> "iload " ++ show arg
    IStore -> "istore " ++ show arg
    Ldc -> "ldc " ++ show arg

-- Function to convert bytecode instruction to string
byteCodeToString_w2Args :: ByteCode_Instr_w2Args -> Int -> Int -> String
byteCodeToString_w2Args opc arg1 arg2 = case opc of
    If -> "if " ++ show ((arg1 `shiftL` 8) + arg2)
    If_ICmpEq -> "if_icmpeq " ++ show ((arg1 `shiftL` 8) + arg2)
    If_ICmpNeq -> "if_icmpne " ++ show ((arg1 `shiftL` 8) + arg2)
    If_ICmpLeq -> "if_icmple " ++ show ((arg1 `shiftL` 8) + arg2)
    If_ICmpLt -> "if_icmplt " ++ show ((arg1 `shiftL` 8) + arg2)
    If_ICmpGeq -> "if_icmpge " ++ show ((arg1 `shiftL` 8) + arg2)
    If_ICmpGt -> "if_icmpgt " ++ show ((arg1 `shiftL` 8) + arg2)
    If_ACmpEq -> "if_acmpeq " ++ show ((arg1 `shiftL` 8) + arg2)
    If_ACmpNeq -> "if_acmpne " ++ show ((arg1 `shiftL` 8) + arg2)
    IfNull -> "ifnull " ++ show ((arg1 `shiftL` 8) + arg2)
    IfNonNull -> "ifnonnull " ++ show ((arg1 `shiftL` 8) + arg2)
    InvokeVirtual -> "invokevirtual " ++ show ((arg1 `shiftL` 8) + arg2)
    InvokeStatic -> "invokestatic " ++ show ((arg1 `shiftL` 8) + arg2)
    InvokeSpecial -> "invokespecial " ++ show ((arg1 `shiftL` 8) + arg2)
    Goto -> "goto " ++ show ((arg1 `shiftL` 8) + arg2)
    GetField -> "getfield " ++ show ((arg1 `shiftL` 8) + arg2)
    PutField -> "putfield " ++ show ((arg1 `shiftL` 8) + arg2)
    GetStatic -> "getstatic " ++ show ((arg1 `shiftL` 8) + arg2)
    PutStatic -> "putstatic " ++ show ((arg1 `shiftL` 8) + arg2)
    InstanceOf -> "instanceof" ++ show ((arg1 `shiftL` 8) + arg2)
    Ldc_W -> "ldc_w " ++ show ((arg1 `shiftL` 8) + arg2)
    Ldc2_W -> "ldc2_w " ++ show ((arg1 `shiftL` 8) + arg2)
    New -> "new " ++ show ((arg1 `shiftL` 8) + arg2)



-- Function to convert bytecode instruction to string
byteCodeToString_w4Args :: ByteCode_Instr_w4Args -> Int -> Int -> Int -> Int -> String
byteCodeToString_w4Args opc arg1 arg2 arg3 arg4 = case opc of
    InvokeDynamic -> "invokedynamic " 
                    ++ show ((arg1 `shiftL` 24) 
                            + (arg2 `shiftL` 16) 
                            + (0x0 `shiftL` 8) 
                            + 0x0)
    Goto_W -> "goto_w "
                    ++ show ((arg1 `shiftL` 24) 
                            + (arg2 `shiftL` 16) 
                            + (arg3 `shiftL` 8) 
                            + arg4)   

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


