module Main where

-- Copyright 2007 Antoine Latter
-- aslatter@gmail.com

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


-- NOTES: I expect I'll be scraping everything between this
--  and "Data Vn ..."


-- these commandlabels are going, and are only here for reference (as
-- it is a complete list).
data CommandLabel = Break | Close | Do | Else | For | Goto | Halt | Hang
                  | If | Job | Kill | Lock | Merge | New | Open | Quit
                  | Read | Set | TCommit | TRestart | TRollback | TStart 
                  | Use | View | Write | Xecute | ZUnspecified String
  deriving (Eq, Show)

-- I feel like this is going to turn into an explosion of type contructors
data Command = Break (Maybe Condition)
             | Do (Maybe Condition) Location
             | Else
             | Goto (Maybe Condition) Location
             | Halt (Maybe Condition)
             | Hang (Maybe Condition) Expression
             | If [Condition]
             | Kill (Maybe Condition) [KillArg]
             | New (Maybe Condition) [NewArg]
             | Set (Maybe Condition) [SetArg]




type Condition = Expression


data Vn = Lvn String [Expression]
        | Gvn String [Expression]
        | IndirectVn String [Expression]

-- there's somehting I'm not groking wrt the standard and expressions
type Expression = [ExprAtom]
data ExprAtom = ExprVn Vn 