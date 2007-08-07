-- |This module contains everything needed to do the initial parsing
-- of either a MUMPS routine or MUMPS commands entered at a REPL
module LineAST (
                -- * Syntax-Tree Types
                -- ** Commands
                Command(..),
                EntryRef(..),
                FunArg(..),
                Vn(..),
                Label(..),
                Routineref(..),
                DoArg(..),
                KillArg(..),
                GotoArg(..),
                MergeArg,
                NewArg(..),
                SetArg,
                WriteArg,
                Name(..),
                -- ** Expressions
                Expression(..),
                Condition,
                Subscript,
                UnaryOp(..),
                BinOp(..),
                -- * Parsering related functionality
                initLex,
                strip,
                comment,
                parseCommands,
                command,
                parseExp,
                mlist,
                mlist1,
                arglist,
                arglist1
               ) where

-- Copyright 2007 Antoine Latter
-- aslatter@gmail.com

import Control.Monad
import Text.ParserCombinators.Parsec
import Text.Regex

import MValue

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

-- | The "initLex" function takes in a string representing all of the code
-- to be parsed (say, an entire routine) and:
--  * Breaks the code into lines
--  * Removes comments
--  * Removes trailing whitespace
initLex :: String -> [String]
initLex = map strip . lines

strip :: String -> String
strip = (dropWhile whitespace) . reverse . (dropWhile whitespace) . reverse . (takeWhile (/=';'))
    where whitespace x = any (==x) [' ','\t','\r']

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
             | For
             | For1 Vn Expression
             | For2 Vn Expression Expression
             | ForEach Vn [Expression]
             | Goto (Maybe Condition) [GotoArg]
             | Halt (Maybe Condition)
             | Hang (Maybe Condition) Expression
             | If [Condition]
             | Kill (Maybe Condition) [KillArg]
             | Merge (Maybe Condition) [MergeArg]
             | New (Maybe Condition) [NewArg]
             | Read (Maybe Condition) [WriteArg] Vn
             | Set (Maybe Condition) [SetArg]
             | Write (Maybe Condition) [WriteArg]
 deriving Show


data DoArg = DoArg (Maybe Condition) EntryRef [FunArg]
           | DoArgIndirect Expression
 deriving Show

data GotoArg = GotoArg (Maybe Condition) EntryRef
             | GotoArgIndirect Expression
 deriving Show

-- | "EntryRef" is a thing that can be pointed to by a DO or a GOTO,
-- it may be specify a subroutine or a routine. This datatype should
-- be equivalent to the "entryref" of the MUMPS spec.
data EntryRef = Routine Routineref
              | Subroutine Label (Maybe Integer) (Maybe Routineref)
 deriving Show

-- | The DLabel is tag pointed to by an enytryref, if the entryref
-- specifies a label.
data Label = Label Name
            | LabelInt Integer -- ^Labels can be given as integers
            | LabelIndirect Expression
 deriving Show

-- | The Routineref specifies a routine and an optional
-- environment.  May be indirect.
data Routineref = Routineref Name
                | RoutinerefIndirect Expression
 deriving Show

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
 deriving Show

-- |An argument to merge specifies a source and a target.
-- See 8.2.13
type MergeArg = (Vn,Vn)


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
 deriving Show

type WriteArg = ()

-- |Vn describes the name of a variable, which may be local, global,
-- or indirect.  Each form may optionally indicate a Subscript.
-- See 7.1.2
data Vn = Lvn Name [Subscript] -- these two will only ever
        | Gvn Name [Subscript] -- be direct names.  Maybe.
        | IndirectVn Expression [Subscript]
 deriving Show

-- | A funarg can be an expression, or the name of a local to pass
-- in by reference.
data FunArg = FunArgExp Expression
            | FunArgName Name
 deriving Show

-- |A Name is the title of a routine, tag or variable.
data Name
    -- | This is the string which would appear in the symbol
    -- table.
    = Name String
    -- |An LName is the indirect form of a name, and should
    -- evaluate to a valid Name.
    | LName Expression
 deriving Show


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
    | BIFCall String [Expression]
    -- |You can even call your own functions!  Locally defined
    -- functions need not specify the parent routine.
    | Funcal  String (Maybe String) [Expression]
    -- |A pattern match is similar to a regular expression match.
    -- This binary operator returns either 0 or 1.
    | Pattern Expression Regex
 deriving Show

instance Show Regex where
    show _ = "<Regex>"

data UnaryOp = UNot | UPlus | UMinus
 deriving Show
data BinOp   = Concat | Add | Sub | Mult | Div | Rem | Quot | Pow | And | Or
             | Equal | LessThan | GreaterThan | Follows | Contains | SortsAfter
 deriving Show
-- missing a few binops.  not sure where ], [, and ]] are in the spec


-- I don't know why I hadn't defined this earlier.
-- I'm glad I hadn't - it liekly would've been
-- more complicated.
-- |A set argument consists of list of variable names that are to be
-- set to the supplied expression.  Even though the SetArg as a whole
-- may not be indirect, the Vn or Expression may allow for indirection.
type SetArg=([Vn],Expression)

-- |Parse Commands is fed a LINE of MUMPS (after line-level has been detrimined).
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
             many anyChar

-- |Parses a single command.
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
      <|> parseRead
      <|> parseSet
      <|> parseWrite <?> "MUMPS command"

parseBreak :: Parser Command
parseBreak = do stringOrPrefix1 "break"
                cond <- postCondition
                return $ Break cond

postCondition :: Parser (Maybe Expression)
postCondition = do char ':'
                   cond <- parseExpAtom
                   return $ Just cond
      <|> return Nothing

-- Should work for end-of-line do statements.  Need more tests though.
parseDo :: Parser Command
parseDo = do stringOrPrefix1 "do"
             cond <- postCondition
             do char ' '
                args <- mlist parseDoArg
                return $ Do cond args
              <|> do eof
                     return $ Do cond []

parseDoArg :: Parser DoArg
parseDoArg = (do char '@'; expr <- parseExpAtom; return $ DoArgIndirect expr)
         <|> (do loc <- parseEntryRef
                 args <- arglist parseFunArg
                 cond <- postCondition
                 return $ DoArg cond loc args)

-- very similar to the DO parser - which makes sense, as they do
-- similar things.
parseGoto :: Parser Command
parseGoto = do stringOrPrefix1 "goto"
               cond <- postCondition
               do char ' '
                  args <- mlist parseGotoArg
                  return $ Goto cond args
                <|> do eof
                       return $ Goto cond []

parseGotoArg :: Parser GotoArg
parseGotoArg = (do char '@'; expr <- parseExpAtom; return $ GotoArgIndirect expr)
           <|> (do loc <- parseEntryRef
                   cond <- postCondition
                   return $ GotoArg cond loc)

-- Will not work for an end-of-line do statment
parseElse :: Parser Command
parseElse = do
  stringOrPrefix1 "else"
  char ' '
  char ' '
  return Else

parseFor :: Parser Command
parseFor = do stringOrPrefix1 "for"
              error "No parser for FOR"


parseHa :: Parser Command
parseHa = do stringOrPrefix1 "ha"
             (parseHang <|> parseHalt)
             
--Not sufficiently left factored
parseHang :: Parser Command
parseHang = do
  stringOrPrefix "ng"
  cond <- postCondition
  char ' '
  expr <- parseExp
  return $ Hang cond expr

parseHalt :: Parser Command
parseHalt = do
  stringOrPrefix "lt"
  cond <- postCondition
  return $ Halt cond

parseIf :: Parser Command
parseIf = do stringOrPrefix1 "if"
             (char ' ' >> If `liftM` mlist parseExp)
              <|> (eof >> (return $ If []))

parseKill :: Parser Command
parseKill = do 
  stringOrPrefix1 "kill"
  cond <- postCondition
  (do char ' '
      args <- mlist parseKillArg
      return $ Kill cond args)
   <|> (eof >> (return $ Kill cond []))

parseMerge :: Parser Command               
parseMerge = do stringOrPrefix1 "merge"
                error "No parser for MERGE"

parseNew :: Parser Command
parseNew = do stringOrPrefix1 "new"
              _ <- postCondition
              error "No parser for NEW"

parseRead :: Parser Command
parseRead = do stringOrPrefix1 "read"
               error "No parser for READ"


parseSet :: Parser Command
parseSet = do stringOrPrefix1 "set"
              error "No parser for SET"

parseWrite :: Parser Command
parseWrite = do stringOrPrefix1 "write"
                error "No parser for WRITE"

parseKillArg :: Parser KillArg
parseKillArg = (KillIndirect `liftM` (char '@' >> parseExpAtom))
           <|> (KillExclusive `liftM` arglist1 litName)
           <|> (KillSelective `liftM` parseVn)

stringOrPrefix :: String -> Parser String
stringOrPrefix str = stringOrPrefix1 str <|> return []

stringOrPrefix1 :: String -> Parser String
stringOrPrefix1 [] = return []
stringOrPrefix1 (x:xs) = do y <- char x
                            ys <- stringOrPrefix xs
                            return (y:ys)

parseExpAtom :: Parser Expression
parseExpAtom = (parseExpUnop <|> parseExpVn <|> parseExpFuncall <|> parseSubExp <|> parseExpLit)

-- |Parse an expression.  Is not at all forgiving about extraneous whitespace.
parseExp :: Parser Expression
parseExp = do let parseWrapper :: Parser ((Expression -> Expression) -> Expression -> Expression)
                  parseWrapper = (do char '\''; return $ \f x -> ExpUnop UNot (f x)) <|> (return id)

                  parseTailItem :: Parser (Expression -> Expression)
                  parseTailItem = do wrapper <- parseWrapper;
                                     ((do binop <- parseBinop;
                                          expr <- parseExpAtom;
                                          return $ wrapper  $ \x -> ExpBinop binop x expr)
                                      <|>(do char '?';
                                             pat <- parsePattern;
                                             return $ wrapper $ \x -> Pattern x pat))

              exp1 <- parseExpAtom
              tails <- many parseTailItem

              return $ foldl (flip (.)) id tails $ exp1 

parseExpUnop :: Parser Expression
parseExpUnop = (do unop <- parseUnop; expr <- parseExpAtom; return $ ExpUnop unop expr)

parseUnop :: Parser UnaryOp
parseUnop = (do char '\''; return UNot)
        <|> (do char '+';  return UPlus)
        <|> (do char '-';  return UMinus)

parseExpVn :: Parser Expression
parseExpVn = do vn <- parseVn
                return $ ExpVn vn

parseExpFuncall :: Parser Expression
parseExpFuncall = do char '$'
                     error "parseExpFuncall undefined"

parseSubExp :: Parser Expression
parseSubExp = do char '('
                 expr <- parseExp
                 char ')'
                 return expr

-- Take a positive number or a string.  Any leading -/+ signs should've
-- been picked up by parseExpUnop by now.
parseExpLit :: Parser Expression
parseExpLit = parseNumLit <|> parseStringLit

-- Does not work  with scientific notation yet
parseNumLit :: Parser Expression
parseNumLit = do xs <- many1 digit
                 (do char '.'; ys <- many1 digit; (return . ExpLit . Float . read)  (xs ++ ['.'] ++ ys))
                  <|> (return . ExpLit. Number .read) xs

-- Does not work for quote-marks inside a string
parseStringLit :: Parser Expression
parseStringLit = do char '"'
                    xs <- many $ (try $ do string "\"\"";return '\"' ) <|> (noneOf "\"")
                    char '"'
                    (return . ExpLit . String) xs

-- No guarantees that the list of binops is complete.
parseBinop :: Parser BinOp
parseBinop = (char '_'  >> return Concat)
         <|> (char '+'  >> return Add)
         <|> (char '-'  >> return Sub)
         <|> (char '*'  >> ((char '*' >> return Pow) <|> return Mult))
         <|> (char '/'  >> return Div)
         <|> (char '#'  >> return Rem)
         <|> (char '\\' >> return Quot)
         <|> (char '&'  >> return And)
         <|> (char '!'  >> return Or)
         <|> (char '='  >> return Equal)
         <|> (char '<'  >> return LessThan)
         <|> (char '>'  >> return GreaterThan)
         <|> (char ']'  >> ((char ']' >> return SortsAfter) <|> return Follows))
         <|> (char '['  >> return Contains)
         <?> "binary operator"

parsePattern :: Parser Regex
parsePattern = error "No pattern parser"

-- Used in DoArg and GotoArg
parseRoutineRef :: Parser Routineref
parseRoutineRef = (do char '^'
                      (do char '@'
                          RoutinerefIndirect `liftM` parseExpAtom)
                        <|> Routineref `liftM` litName)

parseEntryRef :: Parser EntryRef
parseEntryRef = (Routine `liftM` parseRoutineRef)
            <|> (do lbl <- parseLabel
                    offset <- parseOffset
                    routine <- parseRoutine
                    return $ Subroutine lbl offset routine)
 where
   parseLabel = (char '@' >> LabelIndirect `liftM` parseExpAtom)
            <|> (Label `liftM` litName)
   parseOffset = (char '+' >> (Just . read) `liftM` many1 (oneOf "1234567890"))
             <|> (return Nothing)
   parseRoutine = (Just `liftM` parseRoutineRef)
              <|> (return Nothing)
                      
                   
-- Differs from parseExp because a funarg may be either:
--  1) An Expression
--  2) A (local?) variable passed by ref
parseFunArg :: Parser FunArg
parseFunArg = error "parseFunArg not implemented"

-- |Parses the name of a variable (with subscripts)
parseVn :: Parser Vn
parseVn = (do char '@'
              expr <- parseExpAtom
              args <- (do char '@'
                          arglist parseExp) <|> return []
              return $ IndirectVn expr args)
      <|> (do char '^'
              name <- litName
              args <- arglist parseExp
              return $ Gvn name args)
      <|> (do name <- litName
              args <- arglist parseExp
              return $ Lvn name args)

-- |Parses a literal name.
litName :: Parser Name
litName = do x <- oneOf (return '%' ++ ident)
             xs <- many (oneOf (ident ++ digits))
             return . Name $ x:xs
 where ident = ['a'..'z'] ++ ['A'..'Z']
       digits = ['0'..'9']
                         

-- |Given a parser, parse a comma separated list of these.
mlist :: Parser a -> Parser [a]
mlist pa = mlist1 pa <|> return []


-- |Similar to mlist, but must grab at least one element
mlist1 :: Parser a -> Parser [a]
mlist1 pa = do 
              x <- pa
              xs <- (do char ','
                        mlist pa) <|> return []
              return (x:xs)

-- |Given a parser, parse a comma separated list of these surrounded by parens
arglist :: Parser a -> Parser [a]
arglist pa = do char '('
                xs <- mlist pa
                char ')'
                return xs
         <|> return []

-- |Given a parser, parse a comma separated non-empty list of these
-- surounded by parens
arglist1 :: Parser a -> Parser [a]
arglist1 pa = do char '('
                 xs <- mlist1 pa
                 char ')'
                 return xs
