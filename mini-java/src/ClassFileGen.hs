module ClassFileGen where

import CodeGenerator
import ConstPoolGen
import Syntax
import Parser
import ClassFormat
import Data.Typeable
import Data.List (elemIndex, intercalate)
import ByteCodeInstr


generateClassFile :: Program -> CP_Infos -> ClassFile
generateClassFile (Program (Class className fields methods) typed_bool) cpInfos =
    let -- Parse the Java class syntax and extract relevant information
        magicValue = Magic  
        minVer = MinorVersion 0  
        maxVer = MajorVersion 55 
        countCP = length cpInfos
        arrayCP = cpInfos
        accessFlag = AccessFlags [acc_Super_Synchronized]  -- 32 see example (6.1)
        thisClass = ThisClass {index_th = 7}  -- TODO parse the index number
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
buildFieldInfo (FieldDecl fieldType fieldName maybeExpr) cpInfosList =
    let newFieldInfos =
            [Field_Info
                { af_fi = AccessFlags [acc_Public] 
                , index_name_fi = cpIndexFrom fieldName cpInfosList  -- name_index
                , index_descr_fi = cpIndexFrom (typeToString fieldType) cpInfosList -- descriptor_index (type)
                , tam_fi = 0                    -- count_attributte
                , array_attr_fi = [] -- Todo 'ConstantValue' attributes? (expl. final int i = 10;)
                }]
    in newFieldInfos


-- how to parse the information?
generateMethodsArray :: [MethodDecl] -> [CP_Info] -> Method_Infos
generateMethodsArray methods cpInfosList =
    let methodInfos = concatMap (\method -> buildMethodInfo method cpInfosList) methods
    in methodInfos

buildMethodInfo :: MethodDecl -> [CP_Info] -> [Method_Info]
buildMethodInfo methodDecl@(MethodDecl _ outType methodName parameters blockStmt) cpInfosList =
    let methodType = ("(" ++ intercalate "" (concatMap getInputType parameters) ++ ")" ++ typeToString outType)
        attributes_array = generateAttributeCodeArray methodDecl cpInfosList
        newMethod_Info =
            [Method_Info
                { af_mi = AccessFlags [acc_Public]  
                , index_name_mi = cpIndexFrom methodName cpInfosList  -- name_index
                , index_descr_mi = cpIndexFrom methodType cpInfosList -- descriptor_index (type)
                , tam_mi = length attributes_array
                , array_attr_mi = attributes_array
                }]
    in newMethod_Info


-- function to create attribute Infos?
generateAttributeCodeArray :: MethodDecl -> [CP_Info] -> Attribute_Infos
generateAttributeCodeArray methodDecl cpInfosList =
    let newAttributeInfo :: Attribute_Infos
        code = convertToByteCode (generateCodeForMethod methodDecl cpInfosList) -- [0, 0, 0, 0]                 -- Placeholder TODO: call function to build code
        newAttributeInfo =
            [AttributeCode
                { index_name_attr = cpIndexFrom "Code" cpInfosList  -- attribute_name_index
                , tam_len_attr = 0          -- Placeholder          -- attribute_length
                , len_stack_attr = 0        -- Placeholder          -- max_stack
                , len_local_attr = 0        -- Placeholder          -- max_local
                , tam_code_attr = length code
                , array_code_attr = code
                , tam_ex_attr = 0
                , array_ex_attr = []
                , tam_atrr_attr = 0
                , array_attr_attr = []
                }]
    in newAttributeInfo

convertToByteCode :: [ByteCodeInstrs] -> [Int]
convertToByteCode [] = []
convertToByteCode (i:is) = convertInstrToByteCode i ++ convertToByteCode is

