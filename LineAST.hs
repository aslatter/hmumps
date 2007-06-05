module LineAST where

-- Copyright 2007 Antoine Latter
-- aslatter@gmail.com

-- import Text.ParserCombinators.Parsec
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
-- structures - so that it's a step removed from parsing and line
-- reading, even if the base execution environment still has a
-- concept of "line"


-- these commandlabels are going, and are only here for reference (as
-- it is a complete list).
data CommandLabel = Break | Close | Do | Else | For | Goto | Halt | Hang
                  | If | Job | Kill | Lock | Merge | New | Open | Quit
                  | Read | Set | TCommit | TRestart | TRollback | TStart 
                  | Use | View | Write | Xecute | ZUnspecified String
  deriving (Eq, Show)

-- I feel like this is going to turn into an explosion of type contructors
data Command = Break (Maybe Condition)
             | Do (Maybe Condition) [(Maybe Condition,Location,[FunArg])]
             | Else
             | Goto (Maybe Condition) [(Maybe Condition,Location)]
             | Halt (Maybe Condition)
             | Hang (Maybe Condition) Expression
             | If [Condition]
             | Kill (Maybe Condition) [KillArg]
             | Merge (Maybe Condition) [MergeArg]
             | New (Maybe Condition) [NewArg]
             | Set (Maybe Condition) [SetArg]


-- Location stuff used by Do, Goto &c. Section 8.1.6.1
data Location = Routine Routineref
              | Subroutine DLabel (Maybe Integer) Routineref

data DLabel = DLabel String 
            | DLabelIndirect String

data Routineref = Routineref (Maybe String) String
                | RoutinerefIndirect String


type Condition = Expression


-- Variable names: 7.1.2
data Vn = Lvn String [Expression]
        | Gvn String [Expression]
        | IndirectVn String [Expression]


data FunArg = FunArgExp Expression
            | FunArgName Name

data Name = Name String | NameIndirect String

-- there's somehting I'm not groking wrt the standard and expressions.
-- The "Exression' type will ikely be the last thing I define.
type Expression = [ExprAtom]
data ExprAtom = ExprVn Vn 