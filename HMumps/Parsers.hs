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
             parseKillArg,
             parseNewArg,
             parseDoArg,
             parseRoutineRef,
             parseLabel,
             parseGotoArg,
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
import Data.String
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
               char_ '\n'
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
comment :: Parser ()
comment = do char_ ';'
             _ <- many $ noneOf "\n"
             return ()

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
      <|> parseWrite
      <|> parseXecute <?> "MUMPS command"

parseBreak :: Parser Command
parseBreak = do stringOrPrefix1 "break"
                cond <- postCondition
                return $ Break cond

postCondition :: Parser (Maybe Expression)
postCondition = do char_ ':'
                   cond <- parseExpAtom
                   return $ Just cond
      <|> return Nothing

-- Should work for end-of-line do statements.  Need more tests though.
parseDo :: Parser Command
parseDo = do stringOrPrefix1 "do"
             cond <- postCondition
             do char_ ' '
                args <- mlist parseDoArg
                return $ Do cond args
              <|> do eol
                     return $ Do cond []

parseDoArg :: Parser DoArg
parseDoArg = (do char_ '@'; expr <- parseExpAtom; return $ DoArgIndirect expr)
         <|> (do loc <- parseEntryRef
                 args <- arglist parseFunArg
                 cond <- postCondition
                 return $ DoArg cond loc args)

-- very similar to the DO parser - which makes sense, as they do
-- similar things.
parseGoto :: Parser Command
parseGoto = do stringOrPrefix1 "goto"
               cond <- postCondition
               do char_ ' '
                  args <- mlist parseGotoArg
                  return $ Goto cond args
                <|> do eol
                       return $ Goto cond []

parseGotoArg :: Parser GotoArg
parseGotoArg = (try (do char_ '@'; expr <- parseExpAtom; return $ GotoArgIndirect expr))
           <|> (do loc <- parseEntryRef
                   cond <- postCondition
                   return $ GotoArg cond loc)

parseElse :: Parser Command
parseElse = do
  stringOrPrefix1 "else"
  eol <|> do char_ ' '
             eol <|> char_ ' '
  return Else

parseFor :: Parser Command
parseFor = do stringOrPrefix1 "for"
              (eol >> return ForInf)
               <|>
                (do char_ ' '
                    (do vn <- parseLvn
                        char_ '='
                        arg <- forArg
                        return $ For vn arg)
                      <|> (do eol <|> char_ ' '
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
             (try parseHang <|> parseHalt)
             
--Not sufficiently left factored
parseHang :: Parser Command
parseHang = do
  stringOrPrefix "ng"
  cond <- postCondition
  char_ ' '
  expr <- parseExp
  return $ Hang cond expr

parseHalt :: Parser Command
parseHalt = do
  stringOrPrefix "lt"
  cond <- postCondition
  return $ Halt cond

parseIf :: Parser Command
parseIf = do stringOrPrefix1 "if"
             (char_ ' ' >> If `liftM` mlist parseExp)
              <|> (eol >> (return $ If []))

parseKill :: Parser Command
parseKill = do 
  stringOrPrefix1 "kill"
  cond <- postCondition
  (do char_ ' '
      args <- mlist parseKillArg
      return $ Kill cond args)
   <|> (eol >> (return $ Kill cond []))

parseMerge :: Parser Command               
parseMerge = do stringOrPrefix1 "merge"
                cond <- postCondition
                char_ ' '
                args <- mlist1 parseMergeArg
                return $ Merge cond args

parseMergeArg :: Parser MergeArg
parseMergeArg = (do char_ '@'
                    expr <- parseExpAtom
                    return $ MergeArgIndirect expr)
            <|> (liftM2 MergeArg parseVn (char_ '=' >> parseVn))
            <?> "MERGE argument or indirection"

parseNew :: Parser Command
parseNew = do stringOrPrefix1 "new"
              cond <- postCondition
              (char_ ' ' >>  New cond `liftM` (mlist parseNewArg))
               <|> (eol >> (return $ New cond []))

parseNewArg :: Parser NewArg
parseNewArg = (do char_ '('
                  args <- mlist litName
                  char_ ')'
                  return $ NewExclusive args)
          <|> (NewIndirect `liftM` (char_ '@' >> parseExpAtom))
          <|> NewSelective `liftM` litName

parseQuit :: Parser Command
parseQuit = do stringOrPrefix1 "quit"
               return Quit `ap` postCondition `ap` quitArg
 where quitArg = (char_ ' ' >> (Just `liftM` parseExp <|> (eol >> return Nothing) <|> (char_ ' ' >> return Nothing)))
             <|> (eol >> return Nothing)

eol :: Parser ()
eol = notFollowedBy $ noneOf "\n;"

parseRead :: Parser Command
parseRead = do stringOrPrefix1 "read"
               cond <- postCondition
               char_ ' '
               args <- mlist1 parseWriteArg
               case last args of
                 WriteExpression (ExpVn vn) -> return $ Read cond (init args) vn
                 _ -> fail "last argument to READ must be a variable name"


parseSet :: Parser Command
parseSet = do stringOrPrefix1 "set"
              return Set `ap` postCondition `ap` (char_ ' ' >> mlist1 setArg)
 where setArg = do lhs <- arglist1 parseVn <|> liftM (\x->[x]) parseVn
                   char_ '='
                   rhs <- parseExp
                   return (lhs,rhs)

parseWrite :: Parser Command
parseWrite = do stringOrPrefix1 "write"
                return Write `ap` postCondition `ap` (char_ ' ' >> mlist1 parseWriteArg)

parseWriteArg :: Parser WriteArg
parseWriteArg = (WriteFormat `liftM` many1 parseWriteFormatCode)
            <|> do char_ '@'
                   expr <- parseExpAtom
                   (char_ '@' >> do args <- arglist parseExp
                                    return $ WriteExpression $ ExpVn $ IndirectVn expr args)
                    <|> (return $ WriteIndirect expr)
            <|> (WriteExpression `liftM` parseExp)

parseWriteFormatCode :: Parser WriteFormatCode
parseWriteFormatCode = (char_ '#' >> return Formfeed)
                   <|> (char_ '!' >> return Newline)
                   <|> (char_ '?' >> return Tab `ap` parseInt)
 where parseInt :: Parser Int
       parseInt = return read `ap` many1 (oneOf ['0'..'9'])

parseXecute :: Parser Command
parseXecute = do
  stringOrPrefix1 "x"
  cond <- postCondition
  char_ ' '
  arg <- parseExp
  return $ Xecute cond arg

parseKillArg :: Parser KillArg
parseKillArg = (KillIndirect `liftM` (char_ '@' >> parseExpAtom))
           <|> (KillExclusive `liftM` arglist1 litName)
           <|> (KillSelective `liftM` parseVn)

stringOrPrefix :: String -> Parser ()
stringOrPrefix str = stringOrPrefix1 str <|> return ()

stringOrPrefix1 :: String -> Parser ()
stringOrPrefix1 [] = return ()
stringOrPrefix1 (x:xs) = do char_ (toUpper x) <|> char_ (toLower x) <|> char_ x
                            stringOrPrefix xs

parseExpAtom :: Parser Expression
parseExpAtom = (parseExpUnop <|> parseExpVn <|> parseExpFuncall <|> parseSubExp <|> parseExpLit)

-- |Parse an expression.  Is not at all forgiving about extraneous whitespace.
parseExp :: Parser Expression
parseExp = do let parseWrapper :: Parser ((Expression -> Expression) -> Expression -> Expression)
                  parseWrapper = (do char_ '\''; return $ \f x -> ExpUnop UNot (f x)) <|> (return id)

                  parseTailItem :: Parser (Expression -> Expression)
                  parseTailItem = do wrapper <- parseWrapper;
                                     ((do binop <- parseBinop;
                                          expr <- parseExpAtom;
                                          return $ wrapper  $ \x -> ExpBinop binop x expr)
                                      <|>(do char_ '?';
                                             pat <- undefined;
                                             return $ pat))
              exp1 <- parseExpAtom
              tails <- many parseTailItem

              return $ foldl (flip (.)) id tails $ exp1 

parseExpUnop :: Parser Expression
parseExpUnop = (do unop <- parseUnop; expr <- parseExpAtom; return $ ExpUnop unop expr)

parseUnop :: Parser UnaryOp
parseUnop = (do char_ '\''; return UNot)
        <|> (do char_ '+';  return UPlus)
        <|> (do char_ '-';  return UMinus)

parseExpVn :: Parser Expression
parseExpVn = do vn <- parseVn
                return $ ExpVn vn

parseExpFuncall :: Parser Expression
parseExpFuncall = char_ '$' >>
                  (parseBif <|> parseExFun)

parseBif :: Parser Expression
parseBif = ExpBifCall `liftM` (parseBifC <|> parseBifX <|> parseBifY <|> parseBifT <|> parseBifO)

parseBifC :: Parser BifCall
parseBifC = do
  stringOrPrefix1 "char"
  args <- arglist1 parseExp
  return $ BifChar args

parseBifX :: Parser BifCall
parseBifX = char_ 'x' >> return BifX

parseBifY :: Parser BifCall
parseBifY = char_ 'y' >> return BifY

parseBifT :: Parser BifCall
parseBifT = stringOrPrefix1 "test" >> return BifTest

parseBifO :: Parser BifCall
parseBifO = do
  stringOrPrefix1 "order"
  (vn, dir) <- parse2args parseVn parseExp
  return $ BifOrder vn dir

-- | parse two function arguments where the second is optional
parse2args :: Parser a -> Parser b -> Parser (a, Maybe b)
parse2args a1 a2 = do
  char_ '('
  v1 <- a1
  v2 <- ((char_ ',' >> liftM Just a2) <|> return Nothing)
  char_ ')'
  return (v1, v2)

parseExFun :: Parser Expression
parseExFun = do
  char_ '$'
  (do char_ '^'
      name2 <- parseValidName
      args  <- arglist parseFunArg
      return $ FunCall "" name2 args)
   <|> (do name1 <- parseValidName
           (do char_ '^'
               name2 <- parseValidName
               args <- arglist parseFunArg
               return $ FunCall name1 name2 args)
             <|> (do args <- arglist parseFunArg
                     return $ FunCall name1 "" args))
                      

parseSubExp :: Parser Expression
parseSubExp = do char_ '('
                 expr <- parseExp
                 char_ ')'
                 return expr

-- Take a positive number or a string.  Any leading -/+ signs should've
-- been picked up by parseExpUnop by now.
parseExpLit :: Parser Expression
parseExpLit = parseNumLit <|> parseStringLit

-- Does not work  with scientific notation yet
parseNumLit :: Parser Expression
parseNumLit = do xs <- many1 digit
                 (do char_ '.'; ys <- many1 digit; (return . ExpLit . fromDouble . read)  (xs ++ ['.'] ++ ys))
                  <|> (return . ExpLit. fromInteger . read) xs

-- parse a string literal - uses one char of look-ahead
parseStringLit :: Parser Expression
parseStringLit = do char_ '"'
                    xs <- many $ (try $ do string_ "\"\"";return '\"') <|> (noneOf "\"")
                    char_ '"'
                    (return . ExpLit . fromString) xs


-- No guarantees that the list of binops is complete.
parseBinop :: Parser BinOp
parseBinop = (char_ '_'  >> return Concat)
         <|> (char_ '+'  >> return Add)
         <|> (char_ '-'  >> return Sub)
         <|> (char_ '*'  >> ((char_ '*' >> return Pow) <|> return Mult))
         <|> (char_ '/'  >> return Div)
         <|> (char_ '#'  >> return Rem)
         <|> (char_ '\\' >> return Quot)
         <|> (char_ '&'  >> return And)
         <|> (char_ '!'  >> return Or)
         <|> (char_ '='  >> return Equal)
         <|> (char_ '<'  >> return LessThan)
         <|> (char_ '>'  >> return GreaterThan)
         <|> (char_ ']'  >> ((char_ ']' >> return SortsAfter) <|> return Follows))
         <|> (char_ '['  >> return Contains)
         <?> "binary operator"


-- Used in DoArg and GotoArg
parseRoutineRef :: Parser Routineref
parseRoutineRef = (do char_ '^'
                      (do char_ '@'
                          RoutinerefIndirect `liftM` parseExpAtom)
                        <|> Routineref `liftM` litName)

parseEntryRef :: Parser EntryRef
parseEntryRef = (Routine `liftM` parseRoutineRef)
            <|> (do lbl <- parseLabel
                    offset <- parseOffset
                    routine <- parseRoutine
                    return $ Subroutine lbl offset routine)
 where
   parseOffset = (char_ '+' >> (Just . read) `liftM` many1 (oneOf "1234567890"))
             <|> (return Nothing)
   parseRoutine = (Just `liftM` parseRoutineRef)
              <|> (return Nothing)
                      

parseLabel :: Parser Label
parseLabel = (char_ '@' >> LabelIndirect `liftM` parseExpAtom)
         <|> (Label `liftM` litName)

                   
-- Differs from parseExp because a funarg may be either:
--  1) An Expression
--  2) A (local?) variable passed by ref
parseFunArg :: Parser FunArg
parseFunArg = (do char_ '.'
                  FunArgName `liftM` litName)
          <|> (FunArgExp `liftM` parseExp)

-- |Parses the name of a variable (with subscripts)
parseVn :: Parser Vn
parseVn = (do char_ '@'
              expr <- parseExpAtom
              args <- (do char_ '@'
                          arglist parseExp) <|> return []
              return $ IndirectVn expr args)
      <|> (do char_ '^'
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
              xs <- (do char_ ','
                        mlist pa) <|> return []
              return (x:xs)

colonlist :: Parser a -> Parser [a]
colonlist pa = do x <- pa
                  xs <- many (char_ ':' >> pa)
                  return (x:xs)
                            


-- |Given a parser, parse a comma separated list of these surrounded by parens
arglist :: Parser a -> Parser [a]
arglist pa = do char_ '('
                xs <- mlist pa
                char_ ')'
                return xs
         <|> return []

-- |Given a parser, parse a comma separated non-empty list of these
-- surounded by parens
arglist1 :: Parser a -> Parser [a]
arglist1 pa = do char_ '('
                 xs <- mlist1 pa
                 char_ ')'
                 return xs

char_ :: Char -> Parser ()
char_ c = char c >>= \_ -> return ()

string_ :: String -> Parser ()
string_ str = string str >>= \_ -> return ()
