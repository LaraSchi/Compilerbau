module ClassFileGen where

import Syntax
import ClassFormat
import CodeGen

generateConstantPool :: Program -> CP_Infos -> ClassFile
generateConstantPool (Program classes typed_bool) CP_Infos = do


