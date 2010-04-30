{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS -Wall -Werror #-}

module HMumps.Types where

-- import Text.Regex
import Data.MValue

type Routine     = String -> Maybe Subroutine
type Line        = [Command]
type File        = [(Tag, Line)]
type OldFile     = [(Tag, Int, Line)]
type Subroutine  = ([Name],[Line]) -- Arguments and commands
type Tag         = Maybe (Name, [Name]) -- Label and arguments

instance Show Routine where
    show _ = "[Routine]"


{- Definition of lexical tokens follows:
   routinehead -> name eol
   name -> startname [alphanumeric]
   startname -> % | ['A'..'Z']++['a'..'z']
   digit -> ['0'..'9']
   control -> 127:[0..31] 
   graphic -> [x | x<-[0..255], not (elem x control)]
   eol -> '\n'

-}

-- Where we put this may change, but I'm going to just include the pre
-- processing here so that we can know where we stand.  The fundamental
-- assumptions that can go into the true parsing is that 
-- 1. There are no comments
-- 2. There are no trailing whitespaces
-- 3. There are no newlines
-- Leaving in stupid version until I can think of a good way to fuse the traversals.


-- AST data structures
-- These structures are what MUMPS will look like AFTER parsing,
-- but these structures do NOT describe the execution environment.
--
-- EXCEPTION: The MValue data type is both used as literals in
-- the AST structures, and in the run-time environment.
--
-- These data structures, taken together, should be trvially
-- isomorphic to unparsed MUMPS
--
-- It's intended that the execution environment will execute these
-- structures - so that it's a step removed from parsing and line
-- reading, even if the base execution environment still has a
-- concept of "line"


-- I feel like this is going to turn into an explosion of type contructors
--
-- | These commands make up the initial sub-set of commands I'd like
-- to implement.  I'm not sure if the ASTs described here will make
-- up the optimizable representation, but they will make up what's
-- executed by my first stab at a run-time environment.
data Command = Break (Maybe Condition)
             | Do (Maybe Condition) [DoArg]
             | Else
             | For Vn ForArg
             | ForInf
             | Goto (Maybe Condition) [GotoArg]
             | Halt (Maybe Condition)
             | Hang (Maybe Condition) Expression
             | If [Condition]
             | Kill (Maybe Condition) [KillArg]
             | Merge (Maybe Condition) [MergeArg]
             | New (Maybe Condition) [NewArg]
             | Quit (Maybe Condition) (Maybe Expression)
             | Read (Maybe Condition) [WriteArg] Vn
             | Set (Maybe Condition) [SetArg]
             | Write (Maybe Condition) [WriteArg]
             -- The following commands will not have parsers written for them
             | Nop
             | Block (Maybe Condition) Routine [[Command]]
 deriving (Show)


data DoArg = DoArg (Maybe Condition) EntryRef [FunArg]
           | DoArgIndirect Expression
 deriving (Show)

data ForArg = ForArg1 Expression
            | ForArg2 Expression Expression
            | ForArg3 Expression Expression Expression
 deriving (Show)

data GotoArg = GotoArg (Maybe Condition) EntryRef
             | GotoArgIndirect Expression
 deriving (Show)

-- | "EntryRef" is a thing that can be pointed to by a DO or a GOTO,
-- it may be specify a subroutine or a routine. This datatype should
-- be equivalent to the "entryref" of the MUMPS spec.
data EntryRef = Routine Routineref
              | Subroutine Label (Maybe Integer) (Maybe Routineref)
 deriving (Show)

-- | The DLabel is tag pointed to by an enytryref, if the entryref
-- specifies a label.
data Label = Label Name
            | LabelInt Integer -- ^Labels can be given as integers
            | LabelIndirect Expression
 deriving (Show)

-- | The Routineref specifies a routine and an optional
-- environment.  May be indirect.
data Routineref = Routineref Name
                | RoutinerefIndirect Expression
 deriving (Show)

type Condition = Expression
type Subscript = Expression

-- | Each argument to KILL may be
--  1) A variable name
--  2) A list containing the names of variables
--     not to kill (the remainder are killed)
--  3) An expression, evaluating to a list of 
--     valid kill arguments
-- See 8.2.11
data KillArg = KillSelective Vn
             | KillExclusive [Name]
             | KillIndirect  Expression
 deriving (Show)

-- |An argument to merge specifies a source and a target.
-- See 8.2.13
data MergeArg = MergeArg Vn Vn | MergeArgIndirect Expression
 deriving (Show)


-- New should probably be broken up into more primative commands
-- such as PushNewframe and AddNewframeEntry or something,
-- for optimization.  I'll new to have designed the run-time
-- environment first.
-- | The arguments to NEW are pretty much the same as the arguments
-- to KILL.
-- See 8.2.14
data NewArg = NewSelective Name
            | NewExclusive [Name]
            | NewIndirect  Expression
 deriving (Show)

data WriteArg = WriteExpression Expression
              | WriteFormat [WriteFormatCode]
              | WriteIndirect Expression
 deriving (Show)

data WriteFormatCode = Formfeed
                     | Newline
                     | Tab Int
 deriving (Show)

-- |Vn describes the name of a variable, which may be local, global,
-- or indirect.  Each form may optionally indicate a Subscript.
-- See 7.1.2
data Vn = Lvn Name [Subscript] -- these two will only ever
        | Gvn Name [Subscript] -- be direct names.  Maybe.
        | IndirectVn Expression [Subscript]
 deriving (Show)

-- | A funarg can be an expression, or the name of a local to pass
-- in by reference.
data FunArg = FunArgExp Expression
            | FunArgName Name
 deriving (Show)

type Name = String


-- there's somehting I'm not groking wrt the standard and expressions.
-- The "Expression" type will ikely be the last thing I define.
-- | An expression is something which evaluates to an MValue.
data Expression
    -- |An expression may be a literal MValue
    = ExpLit MValue
    -- |or a variable to be fetched
    | ExpVn Vn
    -- |Any expression may be precedded by one of the
    -- unary operators
    | ExpUnop UnaryOp Expression
    -- |Binary operators may be used to combine expressions.
    | ExpBinop BinOp Expression Expression
    -- |MUMPS provides many builtin functions, some of which
    -- are of arity zero (so are more like builtin constants)
    | ExpBifCall BifCall
    -- |You can even call your own functions!  Locally defined
    -- functions need not specify the parent routine.
    | FunCall  String String [FunArg]
    -- |A pattern match is similar to a regular expression match.
    -- This binary operator returns either 0 or 1.
--    | Pattern Expression Regex
 deriving (Show)

type PatCode = ()

{-
instance Show Regex where
    show _ = "<Regex>"
-}

data UnaryOp = UNot | UPlus | UMinus
 deriving (Show)

data BinOp   = Concat | Add | Sub | Mult | Div | Rem | Quot | Pow | And | Or
             | Equal | LessThan | GreaterThan | Follows | Contains | SortsAfter
 deriving (Show)

data BifCall
    = BifChar [Expression]
    | BifX
    | BifY
    | BifTest
 deriving Show

-- I don't know why I hadn't defined this earlier.
-- I'm glad I hadn't - it liekly would've been
-- more complicated.
-- |A set argument consists of list of variable names that are to be
-- set to the supplied expression.  Even though the SetArg as a whole
-- may not be indirect, the Vn or Expression may allow for indirection.
type SetArg=([Vn],Expression)
