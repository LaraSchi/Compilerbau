module ClassFileGen where

import Syntax
import ClassFormat

generateClassFile :: Program -> CP_Infos -> ClassFile
generateClassFile (Program (Class className fields methods) typed_bool) cpInfos =
    let -- Parse the Java class syntax and extract relevant information
        magicValue = Magic  -- Where to get the information
        minVer = MinorVersion 0  
        maxVer = MajorVersion 55 
        countCP = length cpInfos
        arrayCP = cpInfos
        accessFlag = AccessFlags [acc_Super_Synchronized]  -- 32 see example (6.1)  -- why the brackets (when integer is given [ ] is not needed)?
        thisClass = ThisClass {index_th = 7}  -- need to figure out the how to parse the index number
        superClass = SuperClass {index_sp = 2}
        numInterfaces = 0  -- no interfaces exist in minijava
        arrayInterfaces = []  -- no interfaces exist in minijava
        numFields = length fields
        arrayFields = generateFieldsArray fields
        numMethods = length methods
        arrayMethods = generateMethodsArray methods  
        numAttributes = length methods  -- what? where to get information
        arrayAttributes = generateAttributesArray  []  -- what? where to get information

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


-- iterieren über fielddecls und für jede fielddecl neue FieldInfo machen
generateFieldsArray :: [FieldDecl] -> Field_Infos
generateFieldsArray fields = 
    let newFieldInfos :: Field_Infos
        newFieldInfos = [Field_Info   -- dummy
                            { af_fi = AccessFlags [acc_Public]  -- 0x0000 dummy
                            , index_name_fi = 1   -- name_index  getindex method in CodeGen.hs (info bauen mit gebauter Info aus FieldDecl)
                            , index_descr_fi = 1     -- descriptor_index (type) -> von FieldDecl bekommen und dann im Konstanten pool danach suchen und dann index angeben
                            , tam_fi = 0                    -- count_attributte
                            , array_attr_fi = []
                            }]
    in newFieldInfos


-- how to parse the information?
generateMethodsArray :: [MethodDecl] -> Method_Infos
generateMethodsArray methods = 
    let newMethodInfos :: Method_Infos
        newMethodInfos = []
    in newMethodInfos

-- access flag bei MethodDecl
-- name index: Name von MethodDecl -> dann suchen im Konstantenpool
-- index über name index im Konstantenpool finden


-- function to create attribute Infos?
generateAttributesArray :: [String] -> Attribute_Infos
generateAttributesArray attributes = 
    let newAttributeInfos :: Attribute_Infos
        newAttributeInfos = []
    in newAttributeInfos