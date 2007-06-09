module LineAST where

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
-- structures - so that it's a step removed from parsing and line
-- reading, even if the base execution environment still has a
-- concept of "line"


-- I feel like this is going to turn into an explosion of type contructors
--
-- These commands make up the initial sub-set of commands I'd like
-- to implement.  I'm not sure if the ASTs described here will make
-- up the optimizable representation, but they will make up what's
-- executed by my first stab at a run-time environment.
data Command = Break (Maybe Condition)
             | Do (Maybe Condition) [(Maybe Condition,Location,[FunArg])]
             | Else
             | For (Maybe (Vn, ForArg)) -- note lack of postcondition
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
            | DLabelIndirect Expression

data Routineref = Routineref (Maybe String) String
                | RoutinerefIndirect Expression


type Condition = Expression
type Subscript = Expression

-- See 8.2.5
data ForArg = For1 Expression
            | For2 Expression Expression
            | For3 Expression Expression Expression

-- See 8.2.11
data KillArg = KillSelective Vn
             | KillExclusive [Name]
             | KillIndirect  Expression

-- See 8.2.13
type MergeArg = (Vn,Vn)


-- See 8.2.14
-- New should probably be broken up into more primative commands
-- such as PushNewframe and AddNewframeEntry or something,
-- for optimization.  I'll new to have designed the run-time
-- environment first.
data NewArg = NewSelective Name
            | NewExclusive [Name]
            | NewIndirect  Expression

-- Variable names: 7.1.2
data Vn = Lvn Name [Subscript] -- these two will only ever
        | Gvn Name [Subscript] -- be direct names.  Maybe.
        | IndirectVn Expression [Subscript]


data FunArg = FunArgExp Expression
            | FunArgName Name

-- Titles of routines, tags and variables.
-- Name is what appears in the symbol table or
-- whatever, LName is an expression which must
-- evaluate to a Name.
data Name = Name String | LName Expression


-- there's somehting I'm not groking wrt the standard and expressions.
-- The "Expression" type will ikely be the last thing I define.
data Expression = ExpLit MValue 
                | ExpVn Vn
                | ExpUnary UnaryOp Expression
                | BinaryOp BinOp Expression Expression
                | BIFCall String [Expression]
                | Funcal  String (Maybe String) [Expression]
                | Pattern Expression Pattern

type Pattern = () -- I'm hoping that MUMPS patterns can be mapped
                  -- directly onto regexs.  I can't find any useful
                  -- documentation on regexs in Haskell.  Even if
                  --  I could, I don't know regexs anyway.

data UnaryOp = Not | UPlus | UMinus
data BinOp   = Concat | Add | Sub | Mult | Div | Rem | Quot | Pow
-- missing a few binops.  not sure where ], [, and ]] are in the spec

type SetArg=()

-- Parse Commands is fed a LINE of MUMPS
parseCommands :: Parser [Command]
parseCommands = do many spaces
                   (do x <- command;
                       xs <- parseCommands;
                       return (x:xs)) <|> (do comment;
                                              return []) <|> (return [])
-- I think I do this wrong, because I'm not sure what happens on
-- mal-formed input.  anyway, I think it's better than it was.


-- munch comments
comment :: Parser String
comment = do char ';'
             -- How do I match everything?
             undefined

command :: Parser Command
command = parseBreak
      <|> parseDo
      <|> parseElse
      <|> parseFor
      <|> parseGoto
      <|> parseHa  -- left factored halt or hang
      <|> parseIf
      <|> parseKill
      <|> parseMerge
      <|> parseNew
      <|> parseSet

parseBreak :: Parser Command
parseBreak = do stringOrPrefix "break"
                cond <- postCondition
                return $ Break cond

postCondition :: Parser (Maybe Expression)
postCondition = do char ':'
                   cond <- parseExp
                   return $ Just cond
      <|> return Nothing

parseDo :: Parser Command
parseDo = do stringOrPrefix "do"
             cond <- postCondition
             char ' '
             args <- mlist (do loc <- parseLocation
                               args <- arglist parseFunArg
                               cond <- postCondition
                               return (cond,loc,args))
             return $ Do cond args

parseElse = do
  stringOrPrefix "else"
  cond <- postCondition
  char ' '
  char ' '
  return Else
parseFor = undefined
parseGoto = undefined

parseHa = (try parseHang) <|> parseHalt

parseHang = do
  stringOrPrefix "hang"
  cond <- postCondition
  char ' '
  exp <- parseExp
  return $ Hang cond exp

parseHalt = do
  stringOrPrefix "halt"
  cond <- postCondition
  return $ Halt cond

parseIf = undefined
parseKill = do 
  stringOrPrefix "kill"
  cond <- postCondition
  killers <- sepBy killarg (char ',')
  return $ Kill cond killers
               
parseMerge = undefined
parseNew = undefined
parseSet = undefined

killarg = undefined

stringOrPrefix :: String -> Parser String
stringOrPrefix [] = return []
stringOrPrefix (x:xs) = do y <- char x
                           ys <- stringOrPrefix xs
                                 <|> return []
                           return (y:ys)


parseExp=undefined
parseLocation=undefined
parseFunArg=undefined
parseVn=undefined

-- Given a parser, parse a comma separated list of these.
mlist :: Parser a -> Parser [a]
mlist pa = mlist1 pa <|> return []

-- Similar to mlist, but must grab at least one
mlist1 :: Parser a -> Parser [a]
mlist1 pa = do 
              x <- pa
              xs <- (do char ','
                        mlist pa) <|> return []
              return (x:xs)

-- Given a parser, parse a comma separated list of these surrounded by parens
arglist :: Parser a -> Parser [a]
arglist pa = do char '('
                xs <- mlist pa
                char ')'
                return xs
         <|> return []