module CodeGen where

import Parser (parse)
import Syntax
import ClassFormat
import Syntax
import Control.Monad.State (gets, modify, State, runState)
import Control.Monad.State (liftIO)

-- Function to generate constant pool entries from the AST
--[CP_Info]
generateConstantPool :: Program -> IO Char
generateConstantPool ast = do
   -- Print the input AST
  liftIO $ do
    putStrLn "Input AST:"
    putStrLn $ show ast
  return 'a'
--generateConstantPool (Program classes a) = do
 -- print(Program classes a)
  --concatMap generateClassConstantPool classes a

-- Function to generate constant pool entries for a class
--generateClassConstantPool :: CP_Info -> [CP_Info]
--generateClassConstantPool (CP_Info className fields methods) =
--  createClassEntry className :
--  concatMap generateFieldConstantPool fields ++
--  concatMap generateMethodConstantPool methods

-- Function to generate constant pool entries for a field
--generateFieldConstantPool :: Field_Info -> [CP_Info]
--generateFieldConstantPool (Field_Info fieldName fieldType) =
--  createUtf8Entry fieldName :
--  createUtf8Entry fieldType :
--  [createNameAndTypeEntry fieldName fieldType]

-- Function to generate constant pool entries for a method
--generateMethodConstantPool :: Method_Info -> [CP_Info]
--generateMethodConstantPool (Method_Info methodName returnType parameters _) =
--  createUtf8Entry methodName :
--  createUtf8Entry returnType :
--  createUtf8Entry (show parameters) :  -- Adjust this based on your parameter representation
--  [createNameAndTypeEntry methodName (show parameters)]  -- Adjust this based on your parameter representation

-- Helper functions to create specific constant pool entries
--createClassEntry :: String -> CP_Info
--createClassEntry className = Class_Info TagClass 1 className

--createUtf8Entry :: String -> CP_Info
--createUtf8Entry str = Utf8_Info TagUtf8 (length str) str

--createNameAndTypeEntry :: String -> String -> CP_Info
--createNameAndTypeEntry name descriptor =
--  NameAndType_Info TagNameAndType 1 2  -- Assuming indices 1 and 2 correspond to the Utf8 entries for name and descriptor
