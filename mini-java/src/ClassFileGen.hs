module ClassFileGen where

import CodeGenerator
import ConstPoolGen
import Syntax
import Parser
import ClassFormat
import Data.Typeable
import Data.List (elemIndex, intercalate)

-- classfile anschauen mit decodeClassFile() und dann print(...)


-- main bct.class
-- x = decodeClassFile(bct.class)
-- print(x)


generateClassFile :: Program -> CP_Infos -> ClassFile
generateClassFile (Program (Class className fields methods) typed_bool) cpInfos =
    let -- Parse the Java class syntax and extract relevant information
        magicValue = Magic  
        minVer = MinorVersion 0  
        maxVer = MajorVersion 55 
        countCP = length cpInfos
        arrayCP = cpInfos
        accessFlag = AccessFlags [acc_Super_Synchronized]  -- 32 see example (6.1)  -- why the brackets (when integer is given [ ] is not needed)?
        thisClass = ThisClass {index_th = 7}  -- need to figure out the how to parse the index number
        superClass = SuperClass {index_sp = 2}
        numInterfaces = 0  
        arrayInterfaces = [] 
        numFields = length fields
        arrayFields = generateFieldsArray fields cpInfos
        numMethods = length methods
        arrayMethods = generateMethodsArray methods  cpInfos -- attribute Code
        numAttributes = 0
        arrayAttributes = []

    in ClassFile { 
        magic = magicValue,
        minver = minVer,
        maxver = maxVer,
        count_cp = countCP,
        array_cp = arrayCP,
        -- Populate other fields here based on the parsed syntax
        acfg = accessFlag,
        this = thisClass,
        super = superClass,
        count_interfaces = numInterfaces,
        array_interfaces = arrayInterfaces,
        count_fields = numFields,
        array_fields = arrayFields,
        count_methods = numMethods,
        array_methods = arrayMethods,
        count_attributes = numAttributes,
        array_attributes = arrayAttributes
    }


cpIndexFrom :: String -> [CP_Info] -> Int
cpIndexFrom searchName constPoolList =
    case elemIndex (Utf8_Info TagUtf8 (length searchName) searchName "") constPoolList of
        Just idx -> idx + 1  -- Constant pool begins at 1.
        Nothing  -> -1 --Todo Warning?

-- Iteration über FieldList aus der AST, um die Liste aus FieldInfos zu bauen. CP wird mit übergeben.
generateFieldsArray :: [Field] -> [CP_Info] -> Field_Infos
generateFieldsArray fields cpInfosList =
    let result = concatMap (\field -> buildFieldInfo field cpInfosList) fields
    in result

buildFieldInfo :: Field -> [CP_Info] -> [Field_Info]
buildFieldInfo (FieldDecl fieldType fieldName) cpInfosList =
    let newFieldInfos =
            [Field_Info
                { af_fi = AccessFlags [acc_Public]  -- 0x0000 dummy
                , index_name_fi = cpIndexFrom fieldName cpInfosList  -- name_index
                , index_descr_fi = cpIndexFrom (typeToString fieldType) cpInfosList -- descriptor_index (type)
                , tam_fi = 0                    -- count_attributte
                , array_attr_fi = [] -- Todo 'ConstantValue' attributes? (expl. final int i = 10;)
                }]
    in newFieldInfos
buildFieldInfo (FieldRef _ _) _ = []


-- how to parse the information?
generateMethodsArray :: [MethodDecl] -> [CP_Info] -> Method_Infos
generateMethodsArray methods cpInfosList =
    let methodInfos = concatMap (\method -> buildMethodInfo method cpInfosList) methods
    in methodInfos

buildMethodInfo :: MethodDecl -> [CP_Info] -> [Method_Info]
buildMethodInfo methodDecl@(MethodDecl _ outType methodName parameters blockStmt) cpInfosList =
    let methodType = ("(" ++ intercalate "" (concatMap getInputType parameters) ++ ")" ++ typeToString outType)
        newMethod_Info =
            [Method_Info
                { af_mi = AccessFlags [acc_Public]  -- 0x0000 dummy
                , index_name_mi = cpIndexFrom methodName cpInfosList  -- name_index
                , index_descr_mi = cpIndexFrom methodType cpInfosList -- descriptor_index (type)
                , tam_mi = 0                    -- count_attributte
                , array_attr_mi = generateAttributeCodeArray methodDecl cpInfosList -- Todo 'Code' Attributes
                }]
    in newMethod_Info


-- function to create attribute Infos?  -- nur für methods, normales Array ist leer (brauche ich das noch, wenn man nur AttributeCode hat?)
generateAttributeCodeArray :: MethodDecl -> [CP_Info] -> Attribute_Infos
generateAttributeCodeArray methodDecl cpInfosList =
    let newAttributeInfo :: Attribute_Infos
        newAttributeInfo =
            [AttributeCode
                { index_name_attr = cpIndexFrom "Code" cpInfosList  -- attribute_name_index
                , tam_len_attr = length("Code")  -- attribute_length
                , len_stack_attr = 0 -- Placeholder   -- max_stack
                , len_local_attr = 0 -- Placeholder  -- max_local
                , tam_code_attr = 0  -- Placeholder -- code_length
                , array_code_attr = [0, 0, 0, 0] -- generateCodeForMethod methodDecl -- Todo trun string to int (Bytes) -- Placeholder   -- code como array de bytes
                , tam_ex_attr = 0 -- Placeholder        -- exceptions_length
                , array_ex_attr = [(0, 0, 0, 0)] -- Placeholder               -- no usamos
                , tam_atrr_attr = 0 -- Placeholder                          -- attributes_count
                , array_attr_attr = []   -- Placeholder         :: Attribute_Infos
                }]
    in newAttributeInfo

