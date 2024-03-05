module ByteCodeInstr where

import Data.Bits


data ByteCodeInstrs = 
    BIPush Int
    | ALoad Int
    | AStore Int
    | ILoad Int
    | IStore Int
    | Ldc Int 
    | If Int Int
    | If_ICmpEq Int Int
    | If_ICmpNeq Int Int
    | If_ICmpLeq Int Int
    | If_ICmpLt Int Int
    | If_ICmpGeq Int Int
    | If_ICmpGt Int Int
    | If_ACmpEq Int Int
    | If_ACmpNeq Int Int
    | IfNull Int Int
    | IfNonNull Int Int
    | InvokeVirtual Int Int
    | InvokeStatic Int Int
    | InvokeSpecial Int Int
    | Goto Int Int
    | GetField Int Int
    | PutField Int Int
    | GetStatic Int Int
    | PutStatic Int Int
    | InstanceOf Int Int
    | Ldc_W Int Int
    | Ldc2_W Int Int
    | New Int Int
    | SIPush Int Int
    | Goto_W Int Int Int Int
    | InvokeDynamic Int Int Int Int
    | Nop
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
    deriving (Show, Eq)


-- function to call the different converters
convertInstrToByteCode :: ByteCodeInstrs -> [Int]
convertInstrToByteCode instr = case instr of
    (InvokeDynamic arg1 arg2 arg3 arg4) -> [0xBA, arg1, arg2, 0x0, 0x0]
    (Goto_W  arg1 arg2 arg3 arg4) -> [0xC8, arg1, arg2, arg3, arg4]
    (If arg1 arg2) -> [0x99, arg1, arg2]
    (If_ICmpEq arg1 arg2) -> [0x9F, arg1, arg2]
    (If_ICmpNeq arg1 arg2) -> [0xA0, arg1, arg2]
    (If_ICmpLeq arg1 arg2) -> [0xA4, arg1, arg2]
    (If_ICmpLt arg1 arg2) -> [0xA1, arg1, arg2]
    (If_ICmpGeq arg1 arg2) -> [0xA2, arg1, arg2]
    (If_ICmpGt arg1 arg2) -> [0xA3, arg1, arg2]
    (If_ACmpEq arg1 arg2) -> [0xA5, arg1, arg2]
    (If_ACmpNeq arg1 arg2) -> [0xA6, arg1, arg2]
    (IfNull arg1 arg2) -> [0xC6, arg1, arg2]
    (IfNonNull arg1 arg2) -> [0xC7, arg1, arg2]
    (InvokeVirtual arg1 arg2) -> [0xB6, arg1, arg2]
    (InvokeStatic arg1 arg2) -> [0xB8, arg1, arg2]
    (InvokeSpecial arg1 arg2) -> [0xB7, arg1, arg2]
    (Goto arg1 arg2) -> [0xA7, arg1, arg2]
    (GetField arg1 arg2) -> [0xB4, arg1, arg2]
    (PutField arg1 arg2) -> [0xB5, arg1, arg2]
    (GetStatic arg1 arg2) -> [0xB2, arg1, arg2]
    (PutStatic arg1 arg2) -> [0xB3, arg1, arg2]
    (InstanceOf arg1 arg2) -> [0xC1, arg1, arg2]
    (Ldc_W arg1 arg2) -> [0x13, arg1, arg2]
    (Ldc2_W arg1 arg2) -> [0x14, arg1, arg2]
    (New arg1 arg2) -> [0xBB, arg1, arg2]
    (SIPush arg1 arg2) -> [0x11, arg1, arg2]
    (BIPush arg1) -> [0x10, arg1]
    (ALoad arg1) -> [0x19, arg1]
    (AStore arg1) -> [0x3A, arg1]
    (ILoad arg1) -> [0x15, arg1]
    (IStore arg1) -> [0x36, arg1]
    (Ldc arg1) -> [0x12, arg1]
    Nop -> [0x00]
    AConst_Null -> [0x01]
    IConst_m1 -> [0x02]
    IConst_0 -> [0x03]
    IConst_1 -> [0x04]
    IConst_2 -> [0x05]
    IConst_3 -> [0x06]
    IConst_4 -> [0x07]
    IConst_5 -> [0x08]
    LConst_0 -> [0x09]
    LConst_1 -> [0x0A]
    ALoad_0 -> [0x2A]
    ALoad_1 -> [0x2B]
    ALoad_2 -> [0x2C]
    ALoad_3 -> [0x2D]
    AStore_0 -> [0x4B]
    AStore_1 -> [0x4C]
    AStore_2 -> [0x4D]
    AStore_3 -> [0x4E]
    ILoad_0 -> [0x1A]
    ILoad_1 -> [0x1B]
    ILoad_2 -> [0x1C]
    ILoad_3 -> [0x1D]
    IStore_0 -> [0x3B]
    IStore_1 -> [0x3C]
    IStore_2 -> [0x3D]
    IStore_3 -> [0x3E]
    IAdd -> [0x60]
    ISub -> [0x64]
    IMul -> [0x68]
    IDiv -> [0x6C]
    IRem -> [0x70]
    INeg -> [0x74]
    IShl -> [0x78]
    IShr -> [0x7A]
    IUShr -> [0x7C]
    IAnd -> [0x7E]
    IOr -> [0x80]
    IXor -> [0x82]
    I2C -> [0x92]
    Return -> [0xB1]
    IReturn -> [0xAC]
    AReturn -> [0xB0]
    Dup -> [0x59]
    Dup_X1 -> [0x5A]
    Dup_X2 -> [0x5B]
    Dup2 -> [0x5C]
    Dup2_X1 -> [0x5D]
    Dup2_X2 -> [0x5E]
    Pop -> [0x57]
    Pop2 -> [0x58]
    Swap -> [0x5F]


convertByteCodeToInstr :: [Int] -> ByteCodeInstrs
convertByteCodeToInstr (opc:arg1:arg2:arg3:[arg4]) = case opc of
    0xBA -> (InvokeDynamic arg1 arg2 0x0 0x0) 
    0xC8 -> (Goto_W arg1 arg2 arg3 arg4)
    _ -> NoInstr
convertByteCodeToInstr (opc:arg1:[arg2]) = case opc of
    0x99 -> (If arg1 arg2)
    0x9F -> (If_ICmpEq arg1 arg2)
    0xA0 -> (If_ICmpNeq arg1 arg2)
    0xA4 -> (If_ICmpLeq arg1 arg2)
    0xA1 -> (If_ICmpLt arg1 arg2)
    0xA2 -> (If_ICmpGeq arg1 arg2)
    0xA3 -> (If_ICmpGt arg1 arg2)
    0xA5 -> (If_ACmpEq arg1 arg2)
    0xA6 -> (If_ACmpNeq arg1 arg2)
    0xC6 -> (IfNull arg1 arg2)
    0xC7 -> (IfNonNull arg1 arg2)
    0xB6 -> (InvokeVirtual arg1 arg2)
    0xB8 -> (InvokeStatic arg1 arg2)
    0xB7 -> (InvokeSpecial arg1 arg2)
    0xA7 -> (Goto arg1 arg2)
    0xB4 -> (GetField arg1 arg2)
    0xB5 -> (PutField arg1 arg2)
    0xB2 -> (GetStatic arg1 arg2)
    0xB3 -> (PutStatic arg1 arg2)
    0xC1 -> (InstanceOf arg1 arg2)
    0x13 -> (Ldc_W arg1 arg2)
    0x14 -> (Ldc2_W arg1 arg2)
    0xBB -> (New arg1 arg2)
    0x11 -> (SIPush arg1 arg2)
    _ -> NoInstr
convertByteCodeToInstr (opc:[arg1]) = case opc of
    0x10 -> (BIPush arg1)
    0x19 -> (ALoad arg1)
    0x3A -> (AStore arg1)
    0x15 -> (ILoad arg1)
    0x36 -> (IStore arg1)
    0x12 -> (Ldc arg1)
    _ -> NoInstr
convertByteCodeToInstr [opc] = case opc of
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


convertToByteCode :: [ByteCodeInstrs] -> [Int]
convertToByteCode [] = []
convertToByteCode (i:is) = convertInstrToByteCode i ++ convertToByteCode is


convertInstrToString :: ByteCodeInstrs -> String
convertInstrToString instr = case instr of
    (InvokeDynamic arg1 arg2 arg3 arg4) -> "invokedynamic" ++ "\t#" ++ show arg1 ++ "\t#" ++ show arg2 ++ "\t#" ++ show arg3 ++ "\t#" ++ show arg4
    (Goto_W arg1 arg2 arg3 arg4) -> "goto_w" ++ "\t" ++ show arg1 ++ "\t" ++ show arg2 ++ "\t" ++ show arg3 ++ "\t" ++ show arg4
    (If arg1 arg2) -> "if" ++ "\t" ++ show arg1 ++ "\t" ++ show arg2
    (If_ICmpEq arg1 arg2) -> "if_icmpeq" ++ "\t" ++ show ((arg1 `shiftL` 8) + arg2)
    (If_ICmpNeq arg1 arg2) -> "if_icmpne" ++ "\t" ++ show ((arg1 `shiftL` 8) + arg2)
    (If_ICmpLeq arg1 arg2) -> "if_icmple" ++ "\t" ++ show ((arg1 `shiftL` 8) + arg2)
    (If_ICmpLt arg1 arg2) -> "if_icmplt" ++ "\t" ++ show ((arg1 `shiftL` 8) + arg2)
    (If_ICmpGeq arg1 arg2) -> "if_icmpge" ++ "\t" ++ show ((arg1 `shiftL` 8) + arg2)
    (If_ICmpGt arg1 arg2) -> "if_icmpgt" ++ "\t" ++ show ((arg1 `shiftL` 8) + arg2)
    (If_ACmpEq arg1 arg2) -> "if_acmpeq" ++ "\t" ++ show ((arg1 `shiftL` 8) + arg2)
    (If_ACmpNeq arg1 arg2) -> "if_acmpne" ++ "\t" ++ show ((arg1 `shiftL` 8) + arg2)
    (IfNull arg1 arg2) -> "ifnull" ++ "\t" ++ show arg1 ++ "\t#" ++ show arg2
    (IfNonNull arg1 arg2) -> "ifnonnull" ++ "\t" ++ show arg1 ++ "\t#" ++ show arg2
    (InvokeVirtual arg1 arg2) -> "invokevirtual" ++ "\t#" ++ show ((arg1 `shiftL` 8) + arg2)
    (InvokeStatic arg1 arg2) -> "invokestatic" ++ "\t#" ++ show ((arg1 `shiftL` 8) + arg2)
    (InvokeSpecial arg1 arg2) -> "invokespecial" ++ "\t#" ++ show ((arg1 `shiftL` 8) + arg2)
    (Goto arg1 arg2) -> "goto" ++ "\t\t" ++ show ((arg1 `shiftL` 8) + arg2)
    (GetField arg1 arg2) -> "getfield" ++ "\t#" ++ show ((arg1 `shiftL` 8) + arg2)
    (PutField arg1 arg2) -> "putfield" ++ "\t#" ++ show ((arg1 `shiftL` 8) + arg2)
    (GetStatic arg1 arg2) -> "getstatic" ++ "\t#" ++ show ((arg1 `shiftL` 8) + arg2)
    (PutStatic arg1 arg2) -> "putstatic" ++ "\t#" ++ show ((arg1 `shiftL` 8) + arg2)
    (InstanceOf arg1 arg2) -> "instanceof" ++ "\t#" ++ show arg1 ++ "\t#" ++ show arg2
    (Ldc_W arg1 arg2) -> "ldc_w" ++ "\t#" ++ show ((arg1 `shiftL` 8) + arg2)
    (Ldc2_W arg1 arg2) -> "ldc2_w" ++ "\t#" ++ show ((arg1 `shiftL` 8) + arg2)
    (SIPush arg1 arg2) -> "sipush" ++ "\t\t" ++ show ((arg1 `shiftL` 8) + arg2)
    (New arg1 arg2) -> "new" ++ "\t\t#" ++ show ((arg1 `shiftL` 8) + arg2)
    (BIPush arg1) -> "bipush" ++ "\t\t" ++ show arg1
    (ALoad arg1) -> "aload" ++ "\t" ++ show arg1
    (AStore arg1) -> "astore" ++ "\t" ++ show arg1
    (ILoad arg1) -> "iload" ++ "\t" ++ show arg1
    (IStore arg1) -> "istore" ++ "\t" ++ show arg1
    (Ldc arg1) -> "ldc" ++ "\t\t#" ++ show arg1
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
