module ClassFileGen where

import CodeGenerator
import ConstPoolGen
import Syntax
import Parser
import ClassFormat
import Data.Typeable
import Data.List (elemIndex, intercalate)
import ByteCodeInstr
import Debug.Trace
import Data.List (foldl')


generateClassFile :: Program -> CP_Infos -> ClassFile
generateClassFile (Program (Class className fields methods) typed_bool) cpInfos =
    let -- Parse the Java class syntax and extract relevant information
        magicValue = Magic  
        minVer = MinorVersion 0
        maxVer = MajorVersion 55
        countCP = (length cpInfos) + 1
        arrayCP = cpInfos
        accessFlag = AccessFlags [acc_Super_Synchronized]  -- 32 see example (6.1)
        thisClass = ThisClass {index_th = 7}  -- TODO parse the index number
        superClass = SuperClass {index_sp = 2}
        numInterfaces = 0  
        arrayInterfaces = [] 
        numFields = length fields
        arrayFields = generateFieldsArray fields cpInfos
        numMethods = length methods
        arrayMethods = generateMethodsArray methods  cpInfos (newTypeToString className) -- attribute Code
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
generateMethodsArray :: [MethodDecl] -> [CP_Info] -> String -> Method_Infos
generateMethodsArray methods cpInfosList className =
    let methodInfos = concatMap (\method -> buildMethodInfo method cpInfosList className methods) methods
    in methodInfos

buildMethodInfo :: MethodDecl -> [CP_Info] -> String -> [MethodDecl] -> [Method_Info]
buildMethodInfo methodDecl@(MethodDecl _ outType methodName parameters blockStmt) cpInfosList className methods =
    let methodType = ("(" ++ intercalate "" (concatMap getInputType parameters) ++ ")" ++ typeToString outType)
        attributes_array = do
            generateAttributeCodeArray methodDecl cpInfosList className methods
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
generateAttributeCodeArray :: MethodDecl -> [CP_Info] -> String -> [MethodDecl] -> Attribute_Infos
generateAttributeCodeArray methodDecl cpInfosList className methods =
    let (result, maxStackSize, localVars) = startBuildGenCodeProcess methodDecl cpInfosList className methods
        maxStack = calcMaxStack result
        code = convertToByteCode result
        newAttributeInfo :: Attribute_Infos
        newAttributeInfo =
            [AttributeCode
                { index_name_attr = cpIndexFrom "Code" cpInfosList  -- attribute_name_index
                , tam_len_attr = fromIntegral (12 + (length code)) -- attribute_length: Plus 12 for own Header
                , len_stack_attr = maxStackSize
                , len_local_attr = (length localVars) + 1 -- for local variable 0 for reference to object on which instance method is being invoked (this)
                , tam_code_attr = length code
                , array_code_attr = code
                , tam_ex_attr = 0
                , array_ex_attr = []
                , tam_atrr_attr = 0
                , array_attr_attr = []
                }]
    in newAttributeInfo


-- Define a function to calculate the maximal stack size needed for a sequence of bytecode instructions
calcMaxStack :: [ByteCodeInstrs] -> Int
calcMaxStack instrs = snd $ foldl' updateStack (0, 0) instrs
    where
        updateStack :: (Int, Int) -> ByteCodeInstrs -> (Int, Int)
        updateStack (currentSize, maxSize) instr =
            let newSize = currentSize + stackChange instr
            in (newSize, max maxSize newSize)

        stackChange :: ByteCodeInstrs -> Int
        stackChange instr = case instr of
            BIPush _ -> 1
            ALoad _ -> 1
            AStore _ -> -1
            ILoad _ -> 1
            IStore _ -> -1
            Ldc _ -> 1
            If_Eq _ _ -> -1
            If_ICmpEq _ _ -> -2
            If_ICmpNeq _ _ -> -2
            If_ICmpLeq _ _ -> -2
            If_ICmpLt _ _ -> -2
            If_ICmpGeq _ _ -> -2
            If_ICmpGt _ _ -> -2
            If_ACmpEq _ _ -> -2
            If_ACmpNeq _ _ -> -2
            IfNull _ _ -> -1
            IfNonNull _ _ -> -1
            InvokeVirtual _ _ -> stackChangeForMethodInvocation
            InvokeStatic _ _ -> stackChangeForMethodInvocation
            InvokeSpecial _ _ -> stackChangeForMethodInvocation
            Goto _ _ -> 0
            GetField _ _ -> getFieldStackChange
            PutField _ _ -> putFieldStackChange
            GetStatic _ _ -> 1
            PutStatic _ _ -> -1
            InstanceOf _ _ -> 1
            Ldc_W _ _ -> 1
            Ldc2_W _ _ -> 2
            New _ _ -> 1
            SIPush _ _ -> 1
            Goto_W _ _ _ _ -> 0
            InvokeDynamic _ _ _ _ -> stackChangeForMethodInvocation
            Nop -> 0
            AConst_Null -> 1
            IConst_m1 -> 1
            IConst_0 -> 1
            IConst_1 -> 1
            IConst_2 -> 1
            IConst_3 -> 1
            IConst_4 -> 1
            IConst_5 -> 1
            LConst_0 -> 2
            LConst_1 -> 2
            ALoad_0 -> 1
            ALoad_1 -> 1
            ALoad_2 -> 1
            ALoad_3 -> 1
            AStore_0 -> -1
            AStore_1 -> -1
            AStore_2 -> -1
            AStore_3 -> -1
            ILoad_0 -> 1
            ILoad_1 -> 1
            ILoad_2 -> 1
            ILoad_3 -> 1
            IStore_0 -> -1
            IStore_1 -> -1
            IStore_2 -> -1
            IStore_3 -> -1
            IAdd -> -1
            ISub -> -1
            IMul -> -1
            IDiv -> -1
            IRem -> -1
            INeg -> 0
            IShl -> -1
            IShr -> -1
            IUShr -> -1
            IAnd -> -1
            IOr -> -1
            IXor -> -1
            I2C -> 0
            Return -> 0
            IReturn -> -1
            AReturn -> -1
            Dup -> 1
            Dup_X1 -> 1
            Dup_X2 -> 1
            Dup2 -> 2
            Dup2_X1 -> 2
            Dup2_X2 -> 2
            Pop -> -1
            Pop2 -> -2
            Swap -> 0
            where
            stackChangeForMethodInvocation = -1  -- Assuming method invocation pops arguments and pushes return value
            getFieldStackChange = 1  -- Assuming getField pops an object reference and pushes a value
            putFieldStackChange = -2  -- Assuming putField pops an object reference and a value
        