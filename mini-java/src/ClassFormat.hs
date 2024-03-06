-- Entnommen aus der Vorlesung und angepasst
module ClassFormat where
import Syntax
import qualified Data.ByteString.Lazy as BS
import Data.Bits
import ByteCodeInstr

-- For colour printing
import System.Console.ANSI
import Control.Monad (zipWithM_)
import Debug.Trace (trace)

-- class file format
data ClassFile = ClassFile { magic            :: Magic
                           , minver           :: MinorVersion
                           , maxver           :: MajorVersion
                           , count_cp         :: ConstantPool_Count
                           , array_cp         :: CP_Infos
                           , acfg             :: AccessFlags
                           , this             :: ThisClass
                           , super            :: SuperClass
                           , count_interfaces :: Interfaces_Count
                           , array_interfaces :: Interfaces
                           , count_fields     :: Fields_Count
                           , array_fields     :: Field_Infos
                           , count_methods    :: Methods_Count
                           , array_methods    :: Method_Infos
                           , count_attributes :: Attributes_Count
                           , array_attributes :: Attribute_Infos
                           }
                    deriving Show       

type CP_Infos        = [CP_Info]
type Interfaces      = [Interface]
type Field_Infos     = [Field_Info]
type Method_Infos    = [Method_Info]
type Attribute_Infos = [Attribute_Info]

data Magic = Magic
        deriving Show

data MinorVersion = MinorVersion {
                        numMinVer :: Int
                    }
        deriving Show

data MajorVersion = MajorVersion {
                        numMaxVer :: Int
                    }
        deriving Show

data CP_Info = 
          Class_Info
                { tag_cp                :: Tag
                , index_cp              :: Index_Constant_Pool
                , desc                  :: String
                }
        | FieldRef_Info 
                { tag_cp                :: Tag
                , index_name_cp         :: Index_Constant_Pool
                , index_nameandtype_cp  :: Index_Constant_Pool
                , desc                  :: String
                }
        | MethodRef_Info 
                { tag_cp                :: Tag
                , index_name_cp         :: Index_Constant_Pool
                , index_nameandtype_cp  :: Index_Constant_Pool
                , desc                  :: String
                }
        | InterfaceMethodRef_Info 
                { tag_cp                :: Tag
                , index_name_cp         :: Index_Constant_Pool
                , index_nameandtype_cp  :: Index_Constant_Pool
                , desc                  :: String
                }
        | String_Info
                { tag_cp                :: Tag
                , index_cp              :: Index_Constant_Pool
                , desc                  :: String
                }
        | Integer_Info 
                { tag_cp                :: Tag
                , numi_cp               :: Int
                , desc                  :: String
                }
        | Float_Info 
                { tag_cp                :: Tag
                , numf_cp               :: Float
                , desc                  :: String
                }
        | Long_Info 
                { tag_cp                :: Tag
                , numi_l1_cp            :: Int
                , numi_l2_cp            :: Int
                , desc                  :: String
                }
        | Double_Info 
                { tag_cp                :: Tag
                , numi_d1_cp            :: Int
                , numi_d2_cp            :: Int
                , desc                  :: String
                }
        | NameAndType_Info 
                { tag_cp                :: Tag
                , index_name_cp         :: Index_Constant_Pool
                , index_descr_cp        :: Index_Constant_Pool
                , desc                  :: String
                }
        | Utf8_Info 
                { tag_cp                :: Tag
                , tam_cp                :: Int
                , cad_cp                :: String
                , desc                  :: String
                }
        deriving (Show, Eq)


data Tag = TagClass              
         | TagFieldRef
         | TagMethodRef
         | TagInterfaceMethodRef
         | TagString
         | TagInteger
         | TagFloat
         | TagLong
         | TagDouble
         | TagNameAndType
         | TagUtf8
        deriving (Show, Eq)

data AccessFlags = AccessFlags [Int]
            deriving Show

acc_Public     :: Int
acc_Public     = 1

acc_Private    :: Int
acc_Private    = 2

acc_Protected  :: Int
acc_Protected  = 4

acc_Static     :: Int
acc_Static     = 8

acc_Final      :: Int
acc_Final      = 16

acc_Super_Synchronized      :: Int
acc_Super_Synchronized      = 32

acc_Volatile_Bridge   :: Int
acc_Volatile_Bridge   = 64

acc_Transient_Varargs  :: Int
acc_Transient_Varargs  = 128

acc_Native :: Int
acc_Native = 256

acc_Interface  :: Int
acc_Interface  = 512

acc_Abstract   :: Int
acc_Abstract   = 1024

acc_Strict :: Int
acc_Strict = 2048

acc_Synthetic  :: Int
acc_Synthetic  = 4096

acc_Annotation :: Int
acc_Annotation = 8192

acc_Enum    :: Int
acc_Enum    = 16384

data ThisClass = ThisClass {
                    index_th :: Index_Constant_Pool
                 }
        deriving Show

data SuperClass = SuperClass {
                    index_sp :: Index_Constant_Pool
                  }
        deriving Show

data Interface = Interface {
                    index_if :: Index_Constant_Pool
                  }
        deriving Show

data Field_Info = Field_Info 
                        { af_fi          :: AccessFlags
                        , index_name_fi  :: Index_Constant_Pool     -- name_index
                        , index_descr_fi :: Index_Constant_Pool     -- descriptor_index
                        , tam_fi         :: Int                     -- count_attributte
                        , array_attr_fi  :: Attribute_Infos
                        }
            deriving Show

data Method_Info = Method_Info 
                        { af_mi          :: AccessFlags
                        , index_name_mi  :: Index_Constant_Pool       -- name_index
                        , index_descr_mi :: Index_Constant_Pool       -- descriptor_index
                        , tam_mi         :: Int                       -- attributes_count
                        , array_attr_mi  :: Attribute_Infos
                        }
                    deriving Show

data Attribute_Info =
        AttributeGeneric 
            { index_name_attr           :: Index_Constant_Pool
            , tam_len_attr              :: Int
            , rest_attr                 :: BS.ByteString
            }

      | AttributeConstantValue 
            { index_name_attr           :: Index_Constant_Pool              -- attribute_name_index
            , tam_attr                  :: Int                              -- attribute_length
            , index_value_attr          :: Index_Constant_Pool              -- constantvalue_index
            }
      | AttributeCode 
            { index_name_attr           :: Index_Constant_Pool              -- attribute_name_index
            , tam_len_attr              :: Int                              -- attribute_length
            , len_stack_attr            :: Int                              -- max_stack
            , len_local_attr            :: Int                              -- max_local
            , tam_code_attr             :: Int                              -- code_length
            , array_code_attr           :: ListaInt                         -- code como array de bytes
            , tam_ex_attr               :: Int                              -- exceptions_length
            , array_ex_attr             :: Tupla4Int                        -- no usamos
            , tam_atrr_attr             :: Int                              -- attributes_count
            , array_attr_attr           :: Attribute_Infos
            }
      
      | AttributeExceptions
            { index_name_attr           :: Index_Constant_Pool              -- attribute_name_index
            , tam_len_attr              :: Int                              -- attribute_length
            , tam_num_ex_attr           :: Int                              -- number of exceptions
            , exception_index_table     :: [Int]                            -- exception_index_table 
            }
      
      | AttributeInnerClasses
            { index_name_attr           :: Index_Constant_Pool              -- attribute_name_index
            , tam_len_attr              :: Int                              -- attribute_length
            , tam_classes               :: Int                              -- number_classes
            , array_classes             :: [(Int,Int,Int,AccessFlags)]       -- classes
            }
      
      | AttributeSynthetic
            { index_name_attr           :: Index_Constant_Pool              -- attribute_name_index
            , tam_len_attr              :: Int                              -- attribute_length
            }
      
      | AttributeSourceFile 
            { index_name_attr           :: Index_Constant_Pool              -- attribute_name_index
            , tam_len_attr              :: Int                              -- attribute_length
            , index_src_attr            :: Index_Constant_Pool              -- sourcefile_index
            }
            
      | AttributeLineNumberTable 
            { index_name_attr           :: Index_Constant_Pool              -- attribute_name_index
            , tam_len_attr              :: Int                              -- attribute_length
            , tam_table_attr            :: Int                              -- lineNumberTable_length
            , array_line_attr           :: Tupla2Int                        -- (start_pc, line_number)
            }
      | AttributeLocalVariableTable 
            { index_name_attr           :: Index_Constant_Pool              -- attribute_name_index
            , tam_len_attr              :: Int                              -- attribute_length
            , tam__table_attr           :: Int                              -- local_varible_table_length
            , array_var_attr            :: Tupla5Int                        -- (start_pc, length, name_index, descriptor_index, inlinedex)
            }
      | AttributeDeprecated
            { index_name_attr           :: Index_Constant_Pool              -- attribute_name_index
            , tam_len_attr              :: Int                              -- attribute_length
            }
         deriving Show
            

type Tupla5Int = [(Int, Int, Int, Int, Int)]
type Tupla2Int = [(Int, Int)]
type Tupla4Int = [(Int, Int, Int, Int)]
type ListaInt  = [Int]
type ConstantPool_Count  = Int
type Interfaces_Count    = Int
type Fields_Count        = Int
type Methods_Count       = Int
type Attributes_Count    = Int
type Index_Constant_Pool = Int



-- Function to set the text color based on the index and CP_Info type
setColor :: Int -> CP_Info -> String
setColor index (FieldRef_Info _ _ _ "java/lang/System.out:Ljava/io/PrintStream;") | index > 9 = setSGRCode [SetColor Foreground Vivid Yellow]
setColor index (MethodRef_Info _ _ _ "java/io/PrintStream.println:(Ljava/lang/String;)V") | index > 9 = setSGRCode [SetColor Foreground Vivid Yellow]
setColor index (String_Info _ _ "~HelloWorld~") | index > 9 = setSGRCode [SetColor Foreground Vivid Yellow]
setColor index (FieldRef_Info _ _ _ d) | index > 9 = trace (show d) $ setSGRCode [SetColor Foreground Vivid Blue]
setColor index (MethodRef_Info _ _ _ _) | index > 9 = setSGRCode [SetColor Foreground Vivid Magenta]
setColor index _ | index <= 9 = setSGRCode [SetColor Foreground Vivid Green]
setColor _ _ = setSGRCode [Reset]

-- Function to reset the text color
resetColor :: String
resetColor = setSGRCode [Reset]

-- Function to show CP_Info with color
showCP_InfoWithColor :: Int -> CP_Info -> String
showCP_InfoWithColor index info =
  let colorCode = setColor index info
      resetCode = resetColor
  in colorCode ++ (show index) ++ "|" ++ (show info) ++ "\n" ++ resetCode

showCP_InfosWithColor :: [CP_Info] -> String
showCP_InfosWithColor cpInfos = concat $ zipWith showCP_InfoWithColor [1 ..] cpInfos


-- Function to display CP_Info items with indices
showCP_Infos :: [CP_Info] -> Int -> String
showCP_Infos [] n = ""
showCP_Infos (x : xss) n = (show n) ++ "|" ++ (show x) ++ "\n" ++ (showCP_Infos xss (n+1))

-- Function to display Method_Info items with indices
showField_Infos :: [Field_Info] -> Int -> String
showField_Infos [] n = ""
showField_Infos (x : xss) n = (show n) ++ "|" ++ (show x) ++ "\n" ++ (showField_Infos xss (n+1))


-- Function to display Method_Info items with indices
showMethod_Infos :: [Method_Info] -> Int -> String
showMethod_Infos [] n = ""
showMethod_Infos (x : xss) n =
    (show n) ++ "| Method_Info{" ++
    "af_mi: " ++ show (af_mi x) ++ "\n" ++
    "index_name_mi: " ++ show (index_name_mi x) ++ "\n" ++
    "index_descr_mi: " ++ show (index_descr_mi x) ++ "\n" ++
    "tam_mi: " ++ show (tam_mi x) ++ "\n" ++
    "array_attr_mi:\n" ++ showAttribute_Infos (array_attr_mi x) 1 ++
    "}\n" ++ (showMethod_Infos xss (n + 1))

-- Function to check if an Attribute_Info instance is an AttributeCode
isAttributeCode :: Attribute_Info -> Bool
isAttributeCode (AttributeCode _ _ _ _ _ _ _ _ _ _) = True
isAttributeCode _                                  = False



-- Function to display Attribute_Info items with indices
showAttribute_Infos :: [Attribute_Info] -> Int -> String
showAttribute_Infos [] n = ""
showAttribute_Infos (x : xss) n = 
    if isAttributeCode x
        then (show n) ++ "|" ++ (showCodeAttrInfo x) ++ "\n" ++ (showAttribute_Infos xss (n+1))
        else show x

showCodeAttrInfo :: Attribute_Info -> String
showCodeAttrInfo (AttributeCode nameIndex attrLen maxStack maxLocal codeLen code exceptionsLen exceptions attributesCount attributes) =
    "AttributeCode {\n" ++
    "  attribute_name_index: " ++ show nameIndex ++ "\n" ++
    "  attribute_length: " ++ show attrLen ++ "\n" ++
    "  max_stack: " ++ show maxStack ++ "\n" ++
    "  max_local: " ++ show maxLocal ++ "\n" ++
    "  code_length: " ++ show codeLen ++ "\n" ++
    "  code: \n" ++ (showCode code 0) ++ "\n" ++
    "  exceptions_length: " ++ show exceptionsLen ++ "\n" ++
    "  exceptions: " ++ show exceptions ++ "\n" ++
    "  attributes_count: " ++ show attributesCount ++ "\n" ++
    "  attributes: " ++ showAttribute_Infos attributes 1 ++ "\n" ++
    "}\n"


showBytes :: ListaInt -> String
showBytes [] = ""
showBytes (x:xs) = "\t" ++ show x ++ "\n" ++ showBytes xs


-- Function to format ListaInt  TODO: better parsing: List only contains bytes -> when opcode with two arguments the next two list elements are the args
showCode :: ListaInt -> Int -> String
showCode [] idx = ""
showCode (opc:arg1:arg2:arg3:arg4:xs) idx =
    if (convertByteCodeToInstr (opc:arg1:arg2:arg3:[arg4])) /= NoInstr
        then "\t" ++ (convertInstrToString (convertByteCodeToInstr (opc:arg1:arg2:arg3:[arg4])) idx) ++ "\n " ++ (showCode xs (idx + 5))
        else if (convertByteCodeToInstr (opc:arg1:[arg2])) /= NoInstr
            then "\t" ++ (convertInstrToString (convertByteCodeToInstr (opc:arg1:[arg2])) idx) ++ "\n " ++ (showCode (arg3:arg4:xs) (idx + 3))
            else if (convertByteCodeToInstr (opc:[arg1])) /= NoInstr
                then "\t" ++ (convertInstrToString (convertByteCodeToInstr (opc:[arg1])) idx) ++ "\n " ++ (showCode (arg2:arg3:arg4:xs) (idx + 2))
                else "\t" ++ (convertInstrToString (convertByteCodeToInstr [opc]) idx)++ "\n " ++ (showCode (arg1:arg2:arg3:arg4:xs) (idx + 1))
showCode (opc:arg1:arg2:xs) idx =
    if (convertByteCodeToInstr (opc:arg1:[arg2])) /= NoInstr
        then "\t" ++ (convertInstrToString (convertByteCodeToInstr (opc:arg1:[arg2])) idx) ++ "\n " ++ (showCode xs (idx + 3))
        else if (convertByteCodeToInstr (opc:[arg1])) /= NoInstr
            then "\t" ++ (convertInstrToString (convertByteCodeToInstr (opc:[arg1])) idx) ++ "\n " ++ (showCode (arg2:xs) (idx + 2))
            else "\t" ++ (convertInstrToString (convertByteCodeToInstr [opc]) idx) ++ "\n " ++ (showCode (arg1:arg2:xs) (idx + 1))
showCode (opc:arg1:xs) idx = 
    if (convertByteCodeToInstr (opc:[arg1])) /= NoInstr
        then "\t" ++ (convertInstrToString (convertByteCodeToInstr (opc:[arg1])) idx) ++ "\n " ++ (showCode xs (idx + 2))
        else "\t" ++ (convertInstrToString (convertByteCodeToInstr [opc]) idx) ++ "\n " ++ (showCode (arg1:xs) (idx + 1))
showCode (opc:xs) idx = "\t" ++ (convertInstrToString (convertByteCodeToInstr [opc]) idx) ++ "\n " ++ (showCode xs (idx + 1))



-- Function to display a ClassFile
prettyPrintClassFile :: ClassFile -> String
prettyPrintClassFile classFile =
    "Class File:\n" ++
    "  Magic: " ++ show (magic classFile) ++ "\n" ++
    "  Version: " ++ show (minver classFile) ++ "." ++ show (maxver classFile) ++ "\n" ++
    "  Constant Pool Count: " ++ show (count_cp classFile) ++ "\n" ++
    "  Constant Pool:\n" ++ showCP_Infos (array_cp classFile) 1 ++ "\n" ++
    "  Access Flags: " ++ show (acfg classFile) ++ "\n" ++
    "  This Class: " ++ show (this classFile) ++ "\n" ++
    "  Super Class: " ++ show (super classFile) ++ "\n" ++
    "  Interfaces Count: " ++ show (count_interfaces classFile) ++ "\n" ++
    "  Interfaces: " ++ show (array_interfaces classFile) ++ "\n" ++
    "  Fields Count: " ++ show (count_fields classFile) ++ "\n" ++
    "  Fields:\n" ++ showField_Infos (array_fields classFile) 1 ++ "\n" ++
    "  Methods Count: " ++ show (count_methods classFile) ++ "\n" ++
    "  Methods:\n" ++ showMethod_Infos (array_methods classFile) 1 ++ "\n" ++
    "  Attributes Count: " ++ show (count_attributes classFile) ++ "\n" ++
    "  Attributes\n" ++ showAttribute_Infos (array_attributes classFile) 1





