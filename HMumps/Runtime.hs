{-# OPTIONS_GHC -Wall -fglasgow-exts #-}
{-# LANGUAGE FlexibleContexts #-}

module HMumps.Runtime(RunState(..),
                      Env(..),
                      emptyState,
                      eval,
                      exec,
                      Normalizable(..),
                      setX, setY,
                      addX, addY,
                      runFunctionCall,
                      -- RunMonad, RunStateMonad,
                     )
where

import Prelude hiding (lookup,break,map)
import qualified Prelude as P

import Data.Map
import Data.MValue
import Data.MArray

import HMumps.Routine
import HMumps.SyntaxTree
import HMumps.Parsers

import Control.Monad.State
import Control.Monad.Maybe

import System(exitWith)
import System.Exit(ExitCode(..))

type RunStateMonad a = MonadState [RunState] m => m a
type RunMonad a      = (MonadIO m, MonadState [RunState] m) => m a

-- |Anything you may ever want to strip indirection off of should
--  be an instance of this class
class Normalizable a where
    normalize :: a -> RunMonad a

instance Normalizable Vn where
    normalize (IndirectVn expr subs)
     = do result <- eval expr
          let String str = mString result
          case parse parseVn "Indirect VN" str of
            Right (IndirectVn expr' subs') -> normalize $ IndirectVn expr' (subs' ++ subs)
            Right (Lvn label subs') -> return $ Lvn label (subs' ++ subs)
            Right (Gvn label subs') -> return $ Gvn label (subs' ++ subs)
            Left err -> (liftIO . putStrLn . show $ err) >> fail ""
    normalize x = return x

instance Normalizable WriteArg where
    normalize (WriteIndirect expr)
     = do result <- eval expr
          let String str = mString result
          case parse parseWriteArg "Indirect Write Argument" str of
            Right wa -> case wa of
                          w@(WriteIndirect _) -> normalize w
                          w -> return w
            Left err -> (liftIO . putStrLn . show $ err) >> fail ""
    normalize x = return x



data RunState = RunState {env       :: Maybe Env,
                          tags      :: Routine}

emptyState :: [RunState]
emptyState = [RunState Nothing (\_ -> Nothing)]

data Env = Env EnvTag (Map String EnvEntry)

data EnvTag = NormalEnv
            | StopEnv

data EnvEntry = LookBack (Maybe Name)
              | Entry MArray

fetch' :: String -> [RunState] -> Maybe MArray
fetch' str xs = join . fst $ foldl helper (Nothing,str) [x | Just x <- fmap env xs] where

 helper :: (Maybe (Maybe MArray),Name) -> Env -> (Maybe (Maybe MArray), Name)

 helper rhs@(Just _, _) _ = rhs
 helper (_,name) (Env tag m) = case tag of
                   NormalEnv -> case name `lookup` m of
                                  Nothing -> (Nothing, name)
                                  Just (Entry ma) -> (Just (Just ma), name)
                                  Just (LookBack newname') -> case newname' of
                                                             Just newname -> (Nothing, newname)
                                                             Nothing      -> (Nothing, name)
                   StopEnv   -> case name `lookup` m of
                                  Nothing -> (Just Nothing, name)
                                  Just (Entry ma) -> (Just (Just ma), name)
                                  Just (LookBack newname') -> case newname' of
                                                             Just newname -> (Nothing, newname)
                                                             Nothing      -> (Nothing, name)

-- |Returns the MArray associated with the named local var, or the empty MArray
fetch :: String -> RunStateMonad MArray
fetch str = do result <- (fetch' str) `liftM` get
               case result of
                  Just x  -> return x
                  Nothing -> return mEmpty


put' :: String -> MArray -> [RunState] -> [RunState]
put' _ _ [] = error "SET called with an empty stack"
put' str ma (x:[]) = case (env x) of
                      Nothing          -> x {env = Just $ Env NormalEnv (insert str (Entry ma) empty)} : []
                      Just (Env tag m) -> x {env = Just $ Env tag       (insert str (Entry ma) m)} : []
put' str ma (x:xs) = case (env x) of
                      Nothing          -> x : (put' str ma xs)
                      Just (Env tag m) -> let enter = x {env = Just $ Env tag (insert str (Entry ma) m)} : xs in
                                          case str `lookup` m of
                                            Nothing -> case tag of
                                                         NormalEnv -> x : (put' str ma xs)
                                                         StopEnv   -> enter

                                            Just (Entry _)              ->  enter
                                            Just (LookBack Nothing)     -> x : (put' str  ma xs)
                                            Just (LookBack (Just str')) -> x : (put' str' ma xs)

setVar :: MonadState [RunState] m => String -> MArray -> m ()
setVar str ma = modify (put' str ma)

change :: MonadState [RunState] m => String -> [MValue] -> MValue -> m ()
change name subs val = do ma <- fetch name
                          setVar name (arrayUpdate ma subs val)

orM :: Monad m => [m Bool] -> m Bool
orM [] = return False
orM (x:xs) = do x' <- x
                if x'
                 then return True
                 else orM xs

-- |A return value of 'Nothing' indicates we did not quit, and should not unroll the stack.
-- A return value of 'Just Nothing' means we should quit with no return value.
-- A return value of 'Just (Just mv)' means that we should quit with a return value of mv.
exec :: (MonadState [RunState] m, MonadIO m) => Line -> m (Maybe (Maybe MValue))
exec []  = return Nothing
exec (Nop:cmds) = exec cmds
exec (ForInf:cmds) = forInf (cycle cmds)
exec ((For vn farg):cmds) = case farg of
                              ForArg1 expr -> exec $ (Set Nothing [([vn],expr)]) : ForInf : cmds
                              ForArg2 exprStart exprInc ->
                               do mStart <- eval exprStart
                                  mInc   <- eval exprInc
                                  exec $ (Set Nothing [([vn],ExpLit mStart)]) : ForInf : cmds ++
                                    [Set Nothing [([vn],ExpBinop Add (ExpVn vn) (ExpLit mInc))]]
                              ForArg3 exprStart exprInc exprTest -> 
                               do mStart <- eval exprStart
                                  mInc   <- eval exprInc
                                  mTest  <- eval exprTest
                                  exec $ (Set Nothing [([vn],ExpLit mStart)]) : ForInf : cmds ++

                                   [Quit (Just $ if mToBool (mTest `mLT` 0)
                                      then ExpBinop LessThan    (ExpVn vn) (ExpLit (mTest + 1))
                                      else ExpBinop GreaterThan (ExpVn vn) (ExpLit (mTest - 1))) Nothing,

                                    Set Nothing [([vn],ExpBinop Add (ExpVn vn) (ExpLit mInc))]]

exec ((Break cond):cmds) = case cond of
    Nothing -> break >> exec cmds
    Just expr -> do mv <- eval expr
                    if mToBool mv
                     then break >> exec cmds
                     else exec cmds
exec (Else:cmds) = do t <- getTest
                      if not t
                       then exec cmds
                       else return Nothing
exec ((If xs):cmds) = do let xs' =  fmap eval xs
                         cond <- orM $ (liftM . liftM) mToBool xs'
                         if cond
                          then setTest True  >> exec cmds
                          else setTest False >> return Nothing
exec ((Write cond ws):cmds) = case cond of
                                Nothing   -> write ws >> exec cmds
                                Just expr -> do mcond <- eval expr
                                                (if mToBool mcond then write ws else return ()) >> exec cmds
exec (cmd:cmds) = case cmd of
   Set cond sas -> case cond of
     Nothing   -> set sas >> exec cmds
     Just expr -> do mTest <- eval expr
                     if mToBool mTest
                      then set sas >> exec cmds
                      else exec cmds
   Halt cond -> case cond of
                  Nothing -> liftIO (exitWith ExitSuccess) >> return Nothing
                  Just expr -> do mv <- eval expr
                                  if mToBool mv
                                   then liftIO (exitWith ExitSuccess) >> return Nothing
                                   else exec cmds
   Quit cond arg -> do case cond of
                         Nothing   -> case arg of
                                        Nothing   -> return $ Just Nothing
                                        Just expr -> do mv <- eval expr
                                                        return $ Just $ Just mv
                         Just cond' -> do cond'' <- eval cond'
                                          if mToBool cond''
                                           then case arg of
                                                Nothing   -> return $ Just Nothing
                                                Just expr -> do mv <- eval expr
                                                                return $ Just $ Just mv
                                           else return Nothing

   c -> (liftIO $ putStrLn $ "Sorry, I don't know how to execute: " ++ (takeWhile (\x -> not (x==' ')) $ show c)) >> return Nothing

set :: (MonadState [RunState] m, MonadIO m) => [SetArg] -> m ()
set [] = return ()
set ((vns,expr):ss) = do vns' <- mapM normalize vns
                         mv <- eval expr
                         mapM_ (setHelper mv) vns' >> set ss
 where setHelper mv (Lvn name subs) = do subs' <- mapM eval subs
                                         change name subs' mv
       setHelper _ (Gvn _ _)        = fail "We don't supposrt global variables yet.  sorry."
       setHelper _ (IndirectVn _ _) = fail "Variable name should be normalized"

write :: (MonadIO m, MonadState [RunState] m) => [WriteArg] -> m ()
write = mapM_ f
 where f wa = do
         wa' <- normalize wa
         case wa' of
           WriteExpression expr -> do m <- eval expr
                                      let String s = mString m
                                      liftIO $ putStr s
                                      addX $ fromIntegral $ length s
           WriteFormat fs -> writeFormat fs
           WriteIndirect _ -> fail "write argument should be normalized"

writeFormat :: (MonadIO m, MonadState [RunState] m)=> [WriteFormatCode] -> m ()
writeFormat = mapM_ f
 where
  f Formfeed = liftIO (putChar '\f') >> setY 1
  f Newline  = liftIO (putChar '\n') >> setX 1 >> addY 1
  f (Tab n)  = do x <- getX
                  let n' = fromIntegral n
                  if x >= n'
                   then return ()
                   else do liftIO (putStr $ (replicate . floor) (n'-x) ' ')
                           setX n'

setX :: MonadState [RunState] m => MValue -> m ()
setX = change "$x" []

setY :: MonadState [RunState] m => MValue -> m ()
setY = change "$y" []

getX :: MonadState [RunState] m => m MValue
getX = getLocal "$x" []

getY :: MonadState [RunState] m => m MValue
getY = getLocal "y" []

addX :: MonadState [RunState] m => Int -> m ()
addX n = do x <- getX
            setX (x + fromIntegral n)

addY :: MonadState [RunState] m => Int -> m ()
addY n = do y <- getY
            setY (y + fromIntegral n)

getLocal :: MonadState [RunState] m => String -> [MValue] -> m MValue
getLocal label subs = do ma <- fetch label
                         return $ case mIndex ma subs of
                           Just mv -> mv
                           Nothing -> String ""                         


forInf ::  (MonadState [RunState] m, MonadIO m) => Line -> m (Maybe (Maybe MValue))
forInf ((Quit cond Nothing):xs) = case cond of
    Nothing  -> return Nothing
    Just expr -> do mv <- eval expr
                    if mToBool mv
                     then return Nothing
                     else forInf xs
forInf ((Quit _ _):_) = fail "QUIT with argument in a for loop"
forInf (cmd:xs) = exec [cmd] >> forInf xs
forInf [] = forInf []  -- dumb

break ::  (MonadState [RunState] m, MonadIO m) => m ()
break = fail "BREAK not working"

getTest :: MonadState [RunState] m => m Bool
getTest = mToBool `liftM` getLocal "$test" []

setTest :: MonadState [RunState] m => Bool -> m ()
setTest =  change "$test" [] . boolToM


eval :: (MonadState [RunState] m, MonadIO m) => Expression -> m MValue
eval (ExpLit m) = return m
eval (ExpVn vn) = do vn' <- normalize vn
                     case vn' of
                       Lvn label subs -> do mvs <- mapM eval subs
                                            getLocal label mvs
                       Gvn _ _        -> fail "Globals not yet implemented"
                       IndirectVn _ _ -> fail "normalized VNs should not be indirect"

eval (ExpUnop unop expr) = do mv <- eval expr
                              return $ case unop of
                                UNot   -> mNot   mv
                                UPlus  -> mNum   mv
                                UMinus -> negate mv
eval (ExpBinop binop lexp rexp) 
 = do lv <- eval lexp
      rv <- eval rexp
      return $ case binop of
        Concat      -> lv `mConcat` rv
        Add         -> lv + rv
        Sub         -> lv - rv
        Mult        -> lv * rv
        Div         -> lv / rv
        Rem         -> lv `mRem` rv
        Quot        -> lv `mQuot` rv
        Pow         -> lv `mPow` rv
        And         -> lv `mAnd` rv
        Or          -> lv `mOr` rv
        Equal       -> boolToM $ lv == rv
        LessThan    -> lv `mLT` rv
        GreaterThan -> lv `mGT` rv
        Follows     -> lv `follows` rv
        Contains    -> lv `contains` rv
        SortsAfter  -> boolToM $ lv > rv
-- eval (Pattern _ _)   = fail "Can't evaluate pattern matches"
eval (FunCall label routine args) = case routine of
                                      [] -> localCall label args
                                      _  -> remoteCall label routine args
eval (BIFCall _ _)   = fail "Can't evaluate built-in function calls"
 
localCall :: Name -> [FunArg] -> RunMonad MValue
localCall label args = do (r :: Routine) <- (tags . head) `liftM` get
                          case r label of
                            Nothing -> fail $ "Noline: " ++ label
                            Just (argnames, cmds) -> funcall args argnames cmds r


remoteCall :: (MonadIO m, MonadState [RunState] m) =>
                Name -> Name -> [FunArg] -> m MValue
remoteCall label routine args = let filename = routine ++ ".hmumps" in
  do text <- liftIO $ readFile filename
     case parse parseFile filename text of
       Left a ->  (fail . show) a
       Right f -> let r = pack $ transform f in
                  case r label of
                    Nothing -> fail $ "Noline: " ++ label ++ "^"  ++ routine
                    Just (argnames, cmds) -> funcall args argnames cmds r

funcall :: [FunArg] -> [Name] -> [Line] -> Routine -> RunMonad MValue
funcall args' argnames cmds r = 
    let (pairs, remainder) = zipRem args' argnames in
    case remainder of
      Just (Left _) -> fail "Supplied too many parameters to function"
      _ -> do m <- foldM helper empty pairs
              let newframe = RunState (Just $ Env NormalEnv m) r
              modify (newframe:)
              x <- runFunctionCall cmds
              case x of
                Just mv -> modify tail >> return mv
                Nothing -> fail "Function quit without returning a value"

 where helper :: Map Name EnvEntry -> (FunArg, Name) -> RunMonad (Map Name EnvEntry)
       helper m (arg,name) = case arg of
             FunArgExp expr -> do mval <- eval expr
                                  let entry = Entry $ arrayUpdate mEmpty [] mval
                                  return $ insert name entry m
             FunArgName name' -> return $ insert name (LookBack $ Just name') m
             
runFunctionCall :: [Line] -> RunMonad (Maybe MValue)
runFunctionCall [] = return Nothing
runFunctionCall (x:xs) = do result <- exec x
                            case result of
                              Nothing -> runFunctionCall xs
                              Just x' -> return x'

zipRem :: [a] -> [b] -> ([(a,b)],Maybe (Either [a] [b]))
zipRem [] []         = ([],Nothing)
zipRem [] xs         = ([],Just $ Right xs)
zipRem xs []         = ([],Just $ Left  xs)
zipRem (x:xs) (y:ys) = let (pairs, remainder) = zipRem xs ys
                       in ((x,y):pairs,remainder)