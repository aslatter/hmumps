{-# OPTIONS -fglasgow-exts -Wall -Werror #-}

module HMumps.Runtime where

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

-- |Anything you may ever want to strip indirection off of should
--  be an instance of this class
class Normalizable a where
    normalize :: (MonadState [RunState] m, MonadIO m) => a -> m a

instance Normalizable Vn where
    normalize (IndirectVn expr subs)
     = do result <- eval expr
          let String str = mString result
          case parse parseVn "Indirect VN" str of
            Right (Lvn label subs') -> return $ Lvn label (subs' ++ subs)
            Right (Gvn label subs') -> return $ Gvn label (subs' ++ subs)
            Right (IndirectVn expr' subs') -> normalize $ IndirectVn expr' (subs' ++ subs)
            Left err -> fail . show $ err
    normalize v = return v
          

data RunState = RunState {env       :: Env,
                          tags      :: Routine}

emptyState :: [RunState]
emptyState = [RunState NoFrame empty]

data Env = NoFrame
         | NormalFrame (Map String MArray)
         | StopFrame (Map String (Maybe MArray))


fetch :: String -> [RunState] -> Maybe MArray
fetch str xs = join $ foldr helper Nothing (P.map (look str . env) xs)

 where helper :: Maybe (Maybe MArray) -> Maybe (Maybe MArray) -> Maybe (Maybe MArray)
       helper x Nothing      = x
       helper _ j@(Just _)   = j

look :: String -> Env -> Maybe (Maybe MArray)
look _ NoFrame = Nothing
look str (NormalFrame m) = case lookup str m of
                             Nothing -> Nothing
                             j       -> Just j
look str (StopFrame m) = case lookup str m of
                           Nothing      -> Just Nothing
                           Just Nothing -> Nothing
                           x            -> x

-- |Returns the MArray associated with the named local var, or the empty MArray
fetch' :: MonadState [RunState] m => String -> m MArray
fetch' str = do result <- (fetch str) `liftM` get
                case result of
                  Just x  -> return x
                  Nothing -> return mEmpty


set' :: String -> MArray -> [RunState] -> [RunState]
set' _ _ []        = error "set called with an empty stack"
set' str ma (x:[]) = case (env x) of
                      NoFrame -> x {env = (NormalFrame (insert str ma empty))} : []
                      NormalFrame m -> x {env = (NormalFrame (insert str ma m))} : []
                      StopFrame m -> x {env = (StopFrame (insert str (Just ma) m))} : []
set' str ma (x:xs) = case (look str . env) x of 
                      Nothing -> x : (set' str ma xs)
                      Just _ -> case env x of
                                  NoFrame -> error "something bad happened in HMumps.Runtime.set"
                                  NormalFrame m -> x {env = (NormalFrame (insert str ma m))} : xs
                                  StopFrame m -> x {env = (StopFrame (insert str (Just ma) m))} : xs


set :: MonadState [RunState] m => String -> MArray -> m ()
set str ma = modify (set' str ma)

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
                                    [Set Nothing [([vn],ExpBinop Add (ExpVn vn) (ExpLit mInc))],
                                     Quit (Just $ ExpBinop (if mToBool (mTest `mLT` 0) then LessThan else GreaterThan)
                                                   (ExpVn vn) (ExpLit mTest)) Nothing]
exec ((Break cond):cmds) = case cond of
    Nothing -> break >> exec cmds
    Just expr -> do mv <- eval expr
                    if mToBool mv
                     then break >> exec cmds
                     else exec cmds
exec (Else:cmds) = do t <- getTest
                      if t
                       then exec cmds
                       else return Nothing
exec ((If xs):cmds) = do ms <- mapM eval xs
                         if or $ P.map mToBool ms
                          then setTest True  >> exec cmds
                          else setTest False >> return Nothing
exec ((Write cond ws):cmds) = case cond of
                                Nothing   -> write ws >> exec cmds
                                Just expr -> do mcond <- eval expr
                                                (if mToBool mcond then write ws else return ()) >> exec cmds


exec _ = undefined

write :: (MonadIO m, MonadState [RunState] m) => [WriteArg] -> m ()
write [] = return ()
write (wa:ws) = case wa of
                  WriteExpression expr -> do m <- eval expr
                                             let String s = mString m
                                             liftIO $ putStr s
                                             write ws
                  WriteFormat fs -> writeFormat fs >> write ws

writeFormat :: MonadIO m => [WriteFormatCode] -> m ()
writeFormat [] = return ()
writeFormat (Formfeed : fs) = liftIO (putChar '\f') >> writeFormat fs
writeFormat (Newline  : fs) = liftIO (putChar '\n') >> writeFormat fs
writeFormat (Tab n    : fs) = liftIO (putStr $ replicate n ' ') >> writeFormat fs  -- Not quite right, should align to nth column

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
break = undefined

getTest :: (MonadState [RunState] m) => m Bool
getTest = undefined

setTest ::  (MonadState [RunState] m) => Bool -> m ()
setTest = undefined

eval :: (MonadState [RunState] m, MonadIO m) => Expression -> m MValue
eval (ExpLit m) = return m
eval (ExpVn vn) = do vn' <- normalize vn
                     case vn' of
                       Lvn label subs -> do ma <- fetch' label
                                            mvs <- mapM eval subs
                                            case mIndex ma mvs of
                                              Just mv -> return mv
                                              Nothing -> return $ String ""
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
        Pow         -> lv ** rv
        And         -> lv `mAnd` rv
        Or          -> lv `mOr` rv
        Equal       -> lv `mEqual` rv
        LessThan    -> lv `mLT` rv
        GreaterThan -> lv `mGT` rv
        Follows     -> lv `follows` rv
        Contains    -> lv `contains` rv
        SortsAfter  -> boolToM $ lv > rv
eval (Pattern _ _)   = fail "Can't evaluate pattern matches"
eval (FunCall _ _ _) = fail "Can't evaluate function calls"
eval (BIFCall _ _)   = fail "Can't evaluate built-in function calls"

