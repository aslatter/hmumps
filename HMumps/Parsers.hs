{-# OPTIONS -Wall -Werror #-}

-- |This module contains everything needed to do the initial
-- parsing of either a MUMPS routine or MUMPS commands
-- entered at a REPL

module HMumps.Parsers (
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

import Data.MValue
import HMumps.SyntaxTree

import Control.Monad
import Text.ParserCombinators.Parsec
import Text.Regex

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

-- |Parse Commands is fed a LINE of MUMPS (after line-level has been detrimined).
parseCommands :: Parser [Command]
parseCommands = do spaces
                   (do x <- command;
                       xs <- parseCommands;
                       return (x:xs)) <|> (do comment;
                                              return []) <|> (eof >> return [])
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
                cond <- postCondition
                char ' '
                args <- mlist1 parseMergeArg
                return $ Merge cond args

parseMergeArg :: Parser MergeArg
parseMergeArg = (do char '@'
                    expr <- parseExpAtom
                    return $ MergeArgIndirect expr)
            <|> (liftM2 MergeArg parseVn (char '=' >> parseVn))
            <?> "MERGE argument or indirection"

parseNew :: Parser Command
parseNew = do stringOrPrefix1 "new"
              cond <- postCondition
              (char ' ' >>  New cond `liftM` (mlist parseNewArg))
               <|> (eof >> (return $ New cond []))

parseNewArg :: Parser NewArg
parseNewArg = (do char '('
                  args <- mlist litName
                  char ')'
                  return $ NewExclusive args)
          <|> (NewIndirect `liftM` (char '@' >> parseExpAtom))
          <|> NewSelective `liftM` litName
          

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
                     (do name <- parseValidName
                         args <- arglist parseExp
                         return $ BIFCall name args)
                      <|> (do char '$'
                              name1 <- parseValidName
                              (do char '^'
                                  name2 <- parseValidName
                                  args <- arglist parseFunArg
                                  return $ FunCall name1 (Just name2) args)
                               <|> (do args <- arglist parseFunArg
                                       return $ FunCall name1 Nothing args))
                      

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
                    xs <- many $ (try $ do string "\"\"";return '\"') <|> (noneOf "\"")
                    char '"'
                    (return . ExpLit . String) xs

-- I consider every "try" in a parsec parser a personal failure.

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
parseFunArg = (do char '.'
                  FunArgName `liftM` litName)
          <|> (FunArgExp `liftM` parseExp)

-- |Parses the name of a variable (with subscripts)
parseVn :: Parser Vn
parseVn = (do char '@'
              expr <- parseExpAtom
              args <- (do char '@'
                          arglist parseExp) <|> return []
              return $ IndirectVn expr args)
      <|> (do char '^'
              name <- litName <|> return ""
              args <- arglist parseExp
              return $ Gvn name args)
      <|> (do name <- litName
              args <- arglist parseExp
              return $ Lvn name args)
      <?> "variable name"

-- |Parses a literal name.
litName :: Parser Name
litName = parseValidName
                         
parseValidName :: Parser String
parseValidName = do x <- oneOf (return '%' ++ ident)
                    xs <- many (oneOf (ident ++ digits))
                    return (x:xs)
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
