module LineAST where

-- Copyright 2007 Antoine Latter
-- aslatter@gmail.com

import Text.ParserCombinators.Parsec
import Text.Regex

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
 deriving Show

-- Location stuff used by Do, Goto &c. Section 8.1.6.1
data Location = Routine Routineref
              | Subroutine DLabel (Maybe Integer) Routineref
 deriving Show

data DLabel = DLabel String 
            | DLabelIndirect Expression
 deriving Show

data Routineref = Routineref (Maybe String) String
                | RoutinerefIndirect Expression
 deriving Show


type Condition = Expression
type Subscript = Expression

-- See 8.2.5
data ForArg = For1 Expression
            | For2 Expression Expression
            | For3 Expression Expression Expression
 deriving Show

-- See 8.2.11
data KillArg = KillSelective Vn
             | KillExclusive [Name]
             | KillIndirect  Expression
 deriving Show

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
 deriving Show

-- Variable names: 7.1.2
data Vn = Lvn Name [Subscript] -- these two will only ever
        | Gvn Name [Subscript] -- be direct names.  Maybe.
        | IndirectVn Expression [Subscript]
 deriving Show

-- A funarg can be an expression, or the name of a local to pass
-- in by reference (I think this is what I meant?)
data FunArg = FunArgExp Expression
            | FunArgName Name
 deriving Show

-- Titles of routines, tags and variables.
-- Name is what appears in the symbol table or
-- whatever, LName is an expression which must
-- evaluate to a Name.
data Name = Name String | LName Expression
 deriving Show


-- there's somehting I'm not groking wrt the standard and expressions.
-- The "Expression" type will ikely be the last thing I define.
data Expression = ExpLit MValue 
                | ExpVn Vn
                | ExpUnop UnaryOp Expression
                | ExpBinop BinOp Expression Expression
                | BIFCall String [Expression]
                | Funcal  String (Maybe String) [Expression]
                | Pattern Expression Regex
 deriving Show

instance Show Regex where
    show _ = "<Regex>"

data UnaryOp = UNot | UPlus | UMinus
 deriving Show
data BinOp   = Concat | Add | Sub | Mult | Div | Rem | Quot | Pow
 deriving Show
-- missing a few binops.  not sure where ], [, and ]] are in the spec


-- I don't know why I hadn't defined this earlier.
-- I'm glad I hadn't - it liekly would've been
-- more complicated.
type SetArg=([Vn],Expression)

-- Parse Commands is fed a LINE of MUMPS (after line-level is determined)
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
parseBreak = do stringOrPrefix1 "break"
                cond <- postCondition
                return $ Break cond

postCondition :: Parser (Maybe Expression)
postCondition = do char ':'
                   cond <- parseExp
                   return $ Just cond
      <|> return Nothing

-- Will not work for an end-of-line statement
parseDo :: Parser Command
parseDo = do stringOrPrefix1 "do"
             cond <- postCondition
             char ' '
             args <- mlist (do loc <- parseLocation
                               args <- arglist parseFunArg
                               cond <- postCondition
                               return (cond,loc,args))
             return $ Do cond args

-- Will not work for an end-of-line do statment
parseElse = do
  stringOrPrefix1 "else"
  cond <- postCondition
  char ' '
  char ' '
  return Else

parseFor = undefined
parseGoto = undefined

parseHa :: Parser Command
parseHa = do stringOrPrefix1 "ha"
             (parseHang <|> parseHalt)
             

parseHang = do
  stringOrPrefix "ng"
  cond <- postCondition
  char ' '
  exp <- parseExp
  return $ Hang cond exp

parseHalt = do
  stringOrPrefix "lt"
  cond <- postCondition
  return $ Halt cond

parseIf = do stringOrPrefix1 "if"
             undefined

parseKill = do 
  stringOrPrefix1 "kill"
  cond <- postCondition
  killers <- sepBy killarg (char ',')
  return $ Kill cond killers
               
parseMerge = do stringOrPrefix1 "merge"
                undefined

parseNew = do stringOrPrefix1 "new"
              undefined

parseSet = do stringOrPrefix1 "set"
              undefined

killarg = undefined

stringOrPrefix :: String -> Parser String
stringOrPrefix str = stringOrPrefix1 str <|> return []

stringOrPrefix1 :: String -> Parser String
stringOrPrefix1 [] = return []
stringOrPrefix1 (x:xs) = do y <- char x
                            ys <- stringOrPrefix xs
                            return (y:ys)

-- Parse an expression.  Is not at all forgiving about extraneous whitespace.
parseExp :: Parser Expression
parseExp = do let parseExpAtom :: Parser Expression
                  parseExpAtom = (parseExpUnop <|> parseExpVn <|> parseExpFuncall <|> parseSubExp <|> parseExpLit)

                  parseExpUnop :: Parser Expression
                  parseExpUnop = (do unop <- parseUnop; exp <- parseExpAtom; return $ ExpUnop unop exp)

                  parseWrapper :: Parser ((Expression -> Expression) -> Expression -> Expression)
                  parseWrapper = (do char '\''; return $ \f x -> ExpUnop UNot (f x)) <|> (return id)

                  parseTailItem :: Parser (Expression -> Expression)
                  parseTailItem = do wrapper <- parseWrapper;
                                     ((do binop <- parseBinop;
                                          exp <- parseExpAtom;
                                          return $ wrapper  $ \x -> ExpBinop binop x exp)
                                      <|>(do char '?';
                                             pat <- parsePattern;
                                             return $ wrapper $ \x -> Pattern x pat))

              exp1 <- parseExpAtom
              tails <- many parseTailItem

              return $ foldl (flip (.)) id tails $ exp1 

parseUnop :: Parser UnaryOp
parseUnop = (do char '\''; return UNot)
        <|> (do char '+';  return UPlus)
        <|> (do char '-';  return UMinus)

parseExpVn :: Parser Expression
parseExpVn = do vn <- parseVn
                return $ ExpVn vn

parseExpFuncall = do char '$'
                     error "parseExpFuncall undefined"

parseSubExp :: Parser Expression
parseSubExp = do char '('
                 exp <- parseExp
                 char ')'
                 return exp

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
parseStringLit = do char '"'
                    xs <- many (noneOf "\"")
                    char '"'
                    (return . ExpLit . String) xs

-- No guarantees that the list of binops is complete.
parseBinop = (char '_'  >> return Concat)
         <|> (char '+'  >> return Add)
         <|> (char '-'  >> return Sub)
         <|> (char '*'  >> ((char '*' >> return Pow) <|> return Mult))
         <|> (char '/'  >> return Div)
         <|> (char '#'  >> return Rem)
         <|> (char '\\' >> return Quot)

parsePattern = undefined

-- I don't remember where I use this
parseLocation = error "parseLocation not implemented"

-- Differs from parseExp because a funarg may be either:
--  * An Expression
--  * A (local?) variable passed by ref
parseFunArg :: Parser FunArg
parseFunArg = error "parseFunArg not implemented"

-- Parses the name of a variable (with subscripts)
parseVn :: Parser Vn
parseVn = (do char '@'
              exp <- parseExp
              args <- (do char '@'
                          arglist parseExp) <|> return []
              return $ IndirectVn exp args)
      <|> (do char '^'
              name <- litName
              args <- arglist parseExp
              return $ Gvn (Name name) args)
      <|> (do name <- litName
              args <- arglist parseExp
              return $ Lvn (Name name) args)

-- Parses a literal name.
litName :: Parser String
litName = do x <- oneOf (return '%' ++ ident)
             xs <- many (oneOf (ident ++ digits))
             return $ x:xs
 where ident = ['a'..'z'] ++ ['A'..'Z']
       digits = ['0'..'9']
                         


             

-- Given a parser, parse a comma separated list of these.
mlist :: Parser a -> Parser [a]
mlist pa = mlist1 pa <|> return []


-- Similar to mlist, but must grab at least one element
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