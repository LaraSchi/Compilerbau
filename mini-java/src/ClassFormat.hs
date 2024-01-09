-- Entnommen aus der Vorlesung
module ClassFormat where
import Syntax
import qualified Data.ByteString.Lazy as BS

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

-- Constantpool is an array of CP_Info entries.
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
                , index_cp              :: Index_Constant_Pool -- index to class info (exmpl. to class name)
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
                , index_name_cp         :: Index_Constant_Pool -- index to class name
                , index_nameandtype_cp  :: Index_Constant_Pool -- index to Name and Type of method.
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

-- Function to display CP_Info items with indices
showCP_Infos :: [CP_Info] -> Int -> String
showCP_Infos [] n = ""
showCP_Infos (x : xss) n = (show n) ++ "|" ++ (show x) ++ "\n" ++ (showCP_Infos xss (n+1))

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


-- The class is accessible to any other class
acc_Public     :: Int
acc_Public     = 1

-- The class is only accessible within its own class.
acc_Private    :: Int
acc_Private    = 2

-- The class is accessible within its own package and by subclasses.
acc_Protected  :: Int
acc_Protected  = 4

-- The member is a class-level (static) member, rather than an instance-level member.
acc_Static     :: Int
acc_Static     = 8

-- The class or member cannot be subclassed or overridden.
acc_Final      :: Int
acc_Final      = 16

-- Superclass (for a class) or synchronized (for a method).
acc_Super_Synchronized      :: Int
acc_Super_Synchronized      = 32

-- Volatile (for a field) or bridge method (for a method).
acc_Volatile_Bridge   :: Int
acc_Volatile_Bridge   = 64

-- Transient (for a field) or variable-length argument method (for a method).
acc_Transient_Varargs  :: Int
acc_Transient_Varargs  = 128

-- The method is implemented in a language other than Java.
acc_Native :: Int
acc_Native = 256

-- The class is an interface.
acc_Interface  :: Int
acc_Interface  = 512

-- The class or method is abstract.
acc_Abstract   :: Int
acc_Abstract   = 1024

-- The method uses strict floating-point precision.
acc_Strict :: Int
acc_Strict = 2048

-- The class or member was generated by the compiler and is not present in the source code.
acc_Synthetic  :: Int
acc_Synthetic  = 4096

-- The class is an annotation type.
acc_Annotation :: Int
acc_Annotation = 8192

-- The class is an enumeration type.
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


