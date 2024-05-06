module Interpreter where

import Prelude (FilePath, IO, readFile, (>>=), String, ($), print)
import Lexer.ParTortex (pProgram, myLexer)

interpretFile :: FilePath -> IO ()
interpretFile file = readFile file >>= interpret

interpret :: String -> IO ()
interpret program = print 
