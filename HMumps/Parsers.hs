{-# OPTIONS_GHC -Wall #-}

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
             parseVn,
             parseWriteArg,
             mlist,
             mlist1,
             arglist,
             arglist1,
             parse,
             parseFile,
             eol,
                       ) where

import Data.MValue
import HMumps.Routine
import HMumps.SyntaxTree

import Data.Char
import Control.Monad
import Text.Parsec hiding (spaces)
import Text.Parsec.String
-- import Text.Regex

spaces :: Parser ()
spaces = (many $ oneOf " \t\r") >> return ()

parseFile :: Parser OldFile
parseFile = many $
            do tag <- parseTag
               spaces
               linelevel <- length `liftM` many (do spaces; x <- char '.'; spaces; return x)
               cmds <- parseCommands
               optional comment
               char '\n'
               return (tag, linelevel, cmds)

parseTag :: Parser Tag
parseTag = do name <- parseValidName
              args <- arglist parseValidName
              return $ Just (name,args)              
       <|> return Nothing

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
parseCommands = (many $ do c <- command
                           spaces
                           return c)
            <|> (do comment;
                    return [])
            <|> (eol >> return [])
            <|> (spaces >> parseCommands)

-- I think I do this wrong, because I'm not sure what happens on
-- mal-formed input.  anyway, I think it's better than it was.


-- munch comments
comment :: Parser String
comment = do char ';'
             cmt <- many $ noneOf "\n"
             return cmt

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
      <|> parseQuit
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
              <|> do eol
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
                <|> do eol
                       return $ Goto cond []

parseGotoArg :: Parser GotoArg
parseGotoArg = (try (do char '@'; expr <- parseExpAtom; return $ GotoArgIndirect expr))
           <|> (do loc <- parseEntryRef
                   cond <- postCondition
                   return $ GotoArg cond loc)

parseElse :: Parser Command
parseElse = do
  stringOrPrefix1 "else"
  eol <|> do char ' '
             eol <|> (char ' ' >> return ())
  return Else

parseFor :: Parser Command
parseFor = do stringOrPrefix1 "for"
              (eol >> return ForInf)
               <|>
                (do char ' '
                    (do vn <- parseLvn
                        char '='
                        arg <- forArg
                        return $ For vn arg)
                      <|> (do eol <|> (char ' ' >> return ())
                              return ForInf))

 where forArg :: Parser ForArg
       forArg = do args <- colonlist parseExp
                   case length args of
                     1 -> return $ ForArg1 (head args)
                     2 -> return $ ForArg2 (args !! 0) (args !! 1)
                     3 -> return $ ForArg3 (args !! 0) (args !! 1) (args !! 2)
                     _ -> fail "Wrong number of arguments to FOR"


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
              <|> (eol >> (return $ If []))

parseKill :: Parser Command
parseKill = do 
  stringOrPrefix1 "kill"
  cond <- postCondition
  (do char ' '
      args <- mlist parseKillArg
      return $ Kill cond args)
   <|> (eol >> (return $ Kill cond []))

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
               <|> (eol >> (return $ New cond []))

parseNewArg :: Parser NewArg
parseNewArg = (do char '('
                  args <- mlist litName
                  char ')'
                  return $ NewExclusive args)
          <|> (NewIndirect `liftM` (char '@' >> parseExpAtom))
          <|> NewSelective `liftM` litName

parseQuit :: Parser Command
parseQuit = do stringOrPrefix1 "quit"
               return Quit `ap` postCondition `ap` quitArg
 where quitArg = (char ' ' >> (Just `liftM` parseExp <|> (eol >> return Nothing) <|> (char ' ' >> return Nothing)))
             <|> (eol >> return Nothing)

eol :: Parser ()
eol = notFollowedBy $ noneOf "\n;"

parseRead :: Parser Command
parseRead = do stringOrPrefix1 "read"
               cond <- postCondition
               char ' '
               args <- mlist1 parseWriteArg
               case last args of
                 WriteExpression (ExpVn vn) -> return $ Read cond (init args) vn
                 _ -> fail "last argument to READ must be a variable name"


parseSet :: Parser Command
parseSet = do stringOrPrefix1 "set"
              return Set `ap` postCondition `ap` (char ' ' >> mlist1 setArg)
 where setArg = do lhs <- arglist1 parseVn <|> liftM (\x->[x]) parseVn
                   char '='
                   rhs <- parseExp
                   return (lhs,rhs)

parseWrite :: Parser Command
parseWrite = do stringOrPrefix1 "write"
                return Write `ap` postCondition `ap` (char ' ' >> mlist1 parseWriteArg)

parseWriteArg :: Parser WriteArg
parseWriteArg = (WriteFormat `liftM` many1 parseWriteFormatCode)
            <|> do char '@'
                   expr <- parseExpAtom
                   (char '@' >> do args <- arglist parseExp
                                   return $ WriteExpression $ ExpVn $ IndirectVn expr args)
                    <|> (return $ WriteIndirect expr)
            <|> (WriteExpression `liftM` parseExp)

parseWriteFormatCode :: Parser WriteFormatCode
parseWriteFormatCode = (char '#' >> return Formfeed)
                   <|> (char '!' >> return Newline)
                   <|> (char '?' >> return Tab `ap` parseInt)
 where parseInt :: Parser Int
       parseInt = return read `ap` many1 (oneOf ['0'..'9'])

parseKillArg :: Parser KillArg
parseKillArg = (KillIndirect `liftM` (char '@' >> parseExpAtom))
           <|> (KillExclusive `liftM` arglist1 litName)
           <|> (KillSelective `liftM` parseVn)

stringOrPrefix :: String -> Parser String
stringOrPrefix str = stringOrPrefix1 str <|> return []

stringOrPrefix1 :: String -> Parser String
stringOrPrefix1 [] = return []
stringOrPrefix1 (x:xs) = do y <- char (toUpper x) <|> char (toLower x) <|> char x
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
                                             pat <- undefined;
                                             return $ pat))
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
                      <|> parseExFun

parseExFun :: Parser Expression
parseExFun = do
  char '$'
  (do char '^'
      name2 <- parseValidName
      args  <- arglist parseFunArg
      return $ FunCall [] name2 args)
   <|> (do name1 <- parseValidName
           (do char '^'
               name2 <- parseValidName
               args <- arglist parseFunArg
               return $ FunCall name1 name2 args)
             <|> (do args <- arglist parseFunArg
                     return $ FunCall name1 "" args))
                      

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
      <|> parseLvn
      <?> "variable name"

parseLvn :: Parser Vn
parseLvn = return Lvn `ap` litName `ap` arglist parseExp

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

colonlist :: Parser a -> Parser [a]
colonlist pa = do x <- pa
                  xs <- many (char ':' >> pa)
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
