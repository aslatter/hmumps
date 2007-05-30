module Main where

import qualified Data.Map as Map
import Text.ParserCombinators.Parsec

import MValue


-- AST data structures
-- These structures are what MUMPS will look like AFTER parsing,
-- but these structures do NOT describe the execution environment.
--
-- EXCPETION: The MValue data type is both used as literals in
-- the AST structures, and in the run-time environment.
--
-- These data structures, taken together, should be trvially
-- isomorphic to unparsed MUMPS
--
-- It's intended that the execution environment will execute these
-- structures - so that it's a step removed from parsing.


data CommandLabel = Break | Close | Do | Else | For | Goto | Halt | Hang
                  | If | Job | Kill | Lock | Merge | New | Open | Quit
                  | Read | Set | TCommit | TRestart | TRollback | TStart 
                  | Use | View | Write | Xecute | ZUnspecified String
  deriving (Eq, Show)

type CommandLine = [(CommandLabel,(Maybe Condition),[CommandArg])]
data CommandArg = IndirectArg String | SimpleArg ArgLabel [Expression] | ComplexArg ArgLabel [CommandArg]

data ArgLabel = Exclussive | SetLabel | NoLabel

type Condition = Expression


data Vn = Lvn String [Expression]
        | Gvn String [Expression]
        | IndirectVn String [Expression]

type Expression = [ExprAtom]
data ExprAtom = ExprVn Vn 