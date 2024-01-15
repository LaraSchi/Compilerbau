module ClassFileGen where

import Syntax
import ClassFormat
import CodeGen

generateClassFile :: Program -> CP_Infos -> ClassFile
generateClassFile (Program classes typed_bool) CP_Infos = do


