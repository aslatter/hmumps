{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleContexts, OverloadedStrings, ViewPatterns, ScopedTypeVariables,
             Rank2Types, GeneralizedNewtypeDeriving
  #-}

module HMumps.Runtime(RunState(..),
                      Env(..),
                      emptyState,
                      eval,
                      exec,
                      Normalizable(..),
                      setX, setY,
                      addX, addY,
                      RunMonad,
                      step
                     )
where

import Prelude hiding (lookup,break,map)

import Data.Char (chr)
import Data.String
import Data.Map
import Data.MValue hiding (join)
import qualified Data.MValue as M
import Data.MArray
import Data.Monoid

import HMumps.Routine
import HMumps.SyntaxTree
import HMumps.Parsers

import Control.Applicative hiding (empty)
import Control.Monad.State
import Control.Monad.Error

import System(exitWith)
import System.Exit(ExitCode(..))

newtype RunMonad a = RM {runRunMonad :: ErrorT String (StateT [RunState] IO) a}
    deriving (Functor, Monad, MonadIO, MonadState [RunState], MonadError String)

step :: (MonadState [RunState] m, MonadIO m) => RunMonad a -> m (Either String a)
step k
    = do
  s <- get
  (a, s') <- liftIO $ flip runStateT s $ runErrorT $ runRunMonad k
  put s'
  return a

-- |Anything you may ever want to strip indirection off of should
--  be an instance of this class
class Normalizable a where
    normalize :: a -> RunMonad a

instance Normalizable Vn where
    normalize (IndirectVn expr subs)
     = do result <- eval expr
          let str = asString result
          case parse parseVn "Indirect VN" str of
            Right (IndirectVn expr' subs') -> normalize $ IndirectVn expr' (subs' ++ subs)
            Right (Lvn label subs') -> return $ Lvn label (subs' ++ subs)
            Right (Gvn label subs') -> return $ Gvn label (subs' ++ subs)
            Left err -> normalizeError err
    normalize x = return x

instance Normalizable WriteArg where
    normalize (WriteIndirect expr)
     = do result <- eval expr
          let str = asString result
          case parse parseWriteArg "Indirect Write Argument" str of
            Right wa -> case wa of
                          w@(WriteIndirect _) -> normalize w
                          w -> return w
            Left err -> normalizeError err
    normalize x = return x

instance Normalizable KillArg where
    normalize (KillIndirect expr)
        = do
      result <- eval expr
      let str = asString result
      case parse (mlist1 parseKillArg) "Indirect KILL argument" str of
        Right args -> do
          args' <- mapM normalize args
          case args' of
            [arg] -> return arg
            _     -> return $ KillArgList args'
        Left err -> normalizeError err
    normalize x = return x

instance Normalizable NewArg where
    normalize (NewIndirect expr)
        = do
      result <- eval expr
      let str = asString result
      case parse (mlist1 parseNewArg) "Indirect NEW argument" str of
        Right args -> do
          args' <- mapM normalize args
          case args' of
            [arg] -> return arg
            _ -> return $ NewArgList args'
        Left err -> normalizeError err
    normalize x = return x

instance Normalizable DoArg where
    normalize (DoArgIndirect expr)
        = do
      result <- eval expr
      let str = asString result
      case parse (mlist1 parseDoArg) "Indirect DO argument" str of
        Right args -> do
          args' <- mapM normalize args
          case args' of
            [arg] -> return arg
            _ -> return $ DoArgList args'
        Left err -> normalizeError err
    normalize x = return x

instance Normalizable Routineref where
    normalize (RoutinerefIndirect expr)
        = do
      result <- eval expr
      let str = asString result
      case parse parseRoutineRef "Indirect routine ref" str of
        Right ref -> normalize ref
        Left err -> normalizeError err
    normalize x = return x

instance Normalizable Label where
    normalize (LabelIndirect expr)
        = do
      str <- asString <$> eval expr
      case parse parseLabel "Indirect label" str of
        Right lbl -> normalize lbl
        Left err -> normalizeError err
    normalize x = return x

instance Normalizable GotoArg where
    normalize (GotoArgIndirect expr)
        = do
      str <- asString <$> eval expr
      case parse parseGotoArg "GOTO argument" str of
        Right arg -> return arg
        Left err -> normalizeError err
    normalize x = return x

normalizeError :: (Show a, MonadIO m) => a -> m b
normalizeError err = (liftIO . putStrLn . show $ err) >> fail ""

-- | Remove any KillArgList constructors
flattenKillArgs :: [KillArg] -> [KillArg]
flattenKillArgs (KillArgList args':args) = flattenKillArgs args' ++ flattenKillArgs args
flattenKillArgs [] = []
flattenKillArgs (x:xs) = x : flattenKillArgs xs

-- | Remove any NewArgList constructors
flattenNewArgs :: [NewArg] -> [NewArg]
flattenNewArgs (NewArgList args':args) = flattenNewArgs args' ++ flattenNewArgs args
flattenNewArgs [] = []
flattenNewArgs (x:xs) = x : flattenNewArgs xs

data RunState = RunState { env       :: Maybe Env
                         , tags      :: Routine
                         , gotoTags  :: Routine
                         }

emptyState :: [RunState]
emptyState = [emptyFrame]

emptyFrame :: RunState
emptyFrame = RunState Nothing (\_ -> Nothing) (\_ -> Nothing)

data Env = Env EnvTag (Map String EnvEntry)

data EnvTag = NormalEnv
            | StopEnv
 deriving Eq

data EnvEntry = LookBack (Maybe Name)
              | Entry MArray


killLocal :: Name -> RunMonad ()
killLocal = modify . go

 where go _ [] = []
       go label (f:fs)
           | noEnvFrame f = f : go label fs
           | otherwise
               = case f of
                   RunState (Just (Env envTag envMap)) rou gRou
                       -> case label `lookup` envMap of
                            Nothing
                                | envTag == StopEnv -> f:fs
                                | otherwise -> f : go label fs
                            Just (Entry _ary)
                                -> (RunState (Just (Env envTag (label `delete` envMap))) rou gRou) : fs
                            Just (LookBack Nothing) -> f : go label fs
                            Just (LookBack (Just newLabel)) -> f : go newLabel fs
                   _ -> error "Fatal error in KILL"

       noEnvFrame (RunState Nothing _ _) = True
       noEnvFrame _ = False

new :: Name -> RunMonad ()
new label
    = modify $ \state ->
      case state of
        [] -> fail "NEW called with an empty stack!"
        (x:xs) -> go x : xs

 where
   go (RunState Nothing r gr) = RunState (Just (Env NormalEnv (insert label (Entry mEmpty) empty))) r gr
   go (RunState (Just ev) r gr)
       = let newEnv
                 = case ev of
                    Env NormalEnv eMap -> Env NormalEnv $ insert label (Entry mEmpty) eMap
                    Env StopEnv eMap -> Env StopEnv $ delete label eMap
         in RunState (Just newEnv) r gr

newExclusive :: [Name] -> RunMonad ()
newExclusive labels
    = modify $ \state ->
      case state of
        [] -> fail "NEW called with an empty stack!"
        (x:xs) -> go x : xs

 where
   go (RunState oldEnv r gr)
       = let newEnv = foldr addLabel (Env StopEnv mempty) labels
         in RunState (Just newEnv) r gr
    where
      addLabel label@(inEnv oldEnv -> Just entry) (Env _StopEnv eMap)
          = Env StopEnv $ insert label entry eMap
      addLabel label (Env _StopEnv eMap)
          = Env StopEnv $ insert label (LookBack Nothing) eMap

      inEnv Nothing _ = Nothing
      inEnv (Just (Env _ eMap)) lbl
          = lbl `lookup` eMap


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
fetch :: String -> RunMonad MArray
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

setVar :: String -> MArray -> RunMonad ()
setVar str ma = modify (put' str ma)

change :: String -> [MValue] -> MValue -> RunMonad ()
change name subs val = do ma <- fetch name
                          setVar name (arrayUpdate ma subs val)

kill :: Name -> [MValue] -> RunMonad ()
kill label [] = killLocal label
kill label subs = do
  ma <- fetch label
  setVar label (killSub ma subs)

orM :: Monad m => [m Bool] -> m Bool
orM [] = return False
orM (x:xs) = do x' <- x
                if x'
                 then return True
                 else orM xs

-- |A return value of 'Nothing' indicates we did not quit, and should not unroll the stack.
-- A return value of 'Just Nothing' means we should quit with no return value.
-- A return value of 'Just (Just mv)' means that we should quit with a return value of mv.
exec :: Line -> RunMonad (Maybe (Maybe MValue))
exec []  = return Nothing

-- special commamds which (may) use the rest of the command list, or may
-- return without processing the entire list
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

exec ((Break cond):cmds) = do
  condition <- evalCond cond
  when condition $ break
  exec cmds

exec (Else:cmds) = do t <- getTest
                      if not t
                       then exec cmds
                       else return Nothing
exec ((If xs):cmds) = do let xs' =  fmap eval xs
                         cond <- orM $ (liftM . liftM) mToBool xs'
                         if cond
                          then setTest True  >> exec cmds
                          else setTest False >> return Nothing
exec ((Halt cond):cmds)
    = do
  condition <- evalCond cond
  if condition
     then liftIO (exitWith ExitSuccess) >> return Nothing
     else exec cmds

exec ((Quit cond arg):cmds)
    = do
  condition <- evalCond cond
  if condition
     then case arg of
            Nothing -> return $ Just Nothing
            Just expr -> do
                     mv <- eval expr
                     return $ Just $ Just mv
     else exec cmds

exec ((Goto cond args):cmds)
    = do
  condition <- evalCond cond
  if condition
   then execGotoArgs args
   else exec cmds

 where
   execGotoArgs [] = exec cmds
   execGotoArgs (arg:rest)
       = do
     GotoArg argCond entryRef <- normalize arg
     condition <- evalCond argCond
     if condition
      then do
         (rou,tag) <- unpackEntryRef entryRef
         liftM Just $ goto rou tag
      else execGotoArgs rest

   unpackEntryRef :: EntryRef -> RunMonad (Maybe Name, Name)
   unpackEntryRef entryRef =
       case entryRef of
         Routine rRef -> do
                       Routineref name <- normalize rRef
                       return (Just name, "")
         Subroutine label' Nothing Nothing -> do
                       label <- labelName label'
                       return (Nothing, label)
         Subroutine label' Nothing (Just rRef) -> do
                       label <- labelName label'
                       Routineref name <- normalize rRef
                       return (Just name, label)
         Subroutine _ Just{} _ -> fail "unable to process numberic offsets for DO or GOTO"

   labelName :: Label -> RunMonad Name
   labelName label' = do
     label <- normalize label'
     case label of
       Label name -> return name
       LabelInt{} -> fail "Unable to handle numeric labels"
       _ -> error "Fatal error handling entry reference"
     

-- regular commands go through the command driver

exec (cmd:cmds)
    = do
  go cmd
  exec cmds

 where
   go Nop = return ()

   go (Write cond ws) = do
     condition <- evalCond cond
     when condition $ write ws

   go (Set cond sas) = do
     condition <- evalCond cond
     when condition $ set sas

   go (Xecute cond arg) = do
     condition <- evalCond cond
     when condition $ do
       str <- asString `liftM` eval arg
       case parse parseCommands "XECUTE" str of
         Left _err -> fail "" -- todo, better error message
         Right xcmds -> do
          modify (emptyFrame:)
          res <- exec $ xcmds ++ [Quit Nothing Nothing]
          case res of
            Just (Just{}) -> fail "XECUTE cannot return with a value"
            _ -> return ()
          modify tail

   -- the "routine" argument is only for use with GOTO,
   -- so we ignore it for now
   go (Block cond rou doLines) = do
     condition <- evalCond cond
     when condition $ do
       RunState _ r _ <- gets head
       modify (emptyFrame {tags = r,gotoTags=rou}:)
       doBlockLines doLines
       modify tail
    where
      doBlockLines [] = return ()
      doBlockLines (doCmds:rest)
          = do
        res <- exec doCmds
        case res of
          Nothing -> doBlockLines rest
          Just Nothing -> return ()
          Just Just{} -> fail "Argumentless DO block cannot quit with a value"


   go (Kill cond args) = do
     condition <- evalCond cond
     when condition $ case args of
         [] -> fail "Sorry, I don't know how to kill everything"
         _ -> do
           args' <- flattenKillArgs `liftM` (mapM normalize args)
           forM_ args' $ \arg ->
               case arg of
                 KillSelective vn'
                     -> do
                         vn <- normalize vn'
                         case vn of
                           Lvn name subs' -> do
                                    subs <- mapM eval subs'
                                    kill name subs
                           _ -> fail "I can only kill locals, sorry"
                 _ -> fail "I can only do selective kills, sorry!"

   go (New cond args) = do
     condition <- evalCond cond
     when condition $ case args of
         [] -> newExclusive []
         _ -> do
           args' <- flattenNewArgs `liftM` (mapM normalize args)
           forM_ args' $ \arg ->
               case arg of
                 NewSelective name -> new name
                 NewExclusive names -> newExclusive names
                 _ -> error "Fatal error processing arguments to NEW"

   go (Do cond args) = do
      condition <- evalCond cond
      when condition $ forM_ args $ \arg' -> do
          arg <- normalize arg'
          case arg of
            DoArgList argList -> mapM_ processDo argList
            _ -> processDo arg

   go c =  fail $ "Sorry, I don't know how to execute: " ++ (takeWhile (\x -> not (x==' ')) $ show c)

processDo :: DoArg -> RunMonad ()
processDo (DoArg cond entryRef args)
    = do
  condition <- evalCond cond
  when condition $ do
    case entryRef of
      Routine routineRef'
          -> do
        Routineref rou <- normalize routineRef'
        sub (Just rou) "" args
      Subroutine label' Nothing Nothing
          -> do
        label <- normalize label'
        case label of
          Label name -> sub Nothing name args
          LabelInt{} -> fail "Cannot use numeric labels"
          _ -> error "fatal error in DO"
      Subroutine label' Nothing (Just rouRef')
          -> do
        Routineref rou <- normalize rouRef'
        label <- normalize label'
        case label of
          Label name -> sub (Just rou) name args
          LabelInt{} -> fail "Cannot use numeric labels"
          _ -> error "fatal error in DO"
      Subroutine _ Just{} _ -> fail "Unable to execute DO with a numeric offset"
processDo _ = error "fatal error in DO"

evalCond :: Maybe Expression -> RunMonad Bool
evalCond Nothing = return True
evalCond (Just e) = mToBool `liftM` eval e

set :: [SetArg] -> RunMonad ()
set [] = return ()
set ((vns,expr):ss) = do vns' <- mapM normalize vns
                         mv <- eval expr
                         mapM_ (setHelper mv) vns' >> set ss
 where setHelper mv (Lvn name subs) = do subs' <- mapM eval subs
                                         change name subs' mv
       setHelper _ (Gvn _ _)        = fail "We don't supposrt global variables yet.  sorry."
       setHelper _ (IndirectVn _ _) = fail "Variable name should be normalized"

write :: [WriteArg] -> RunMonad ()
write = mapM_ f
 where f wa = do
         wa' <- normalize wa
         case wa' of
           WriteExpression expr -> do m <- eval expr
                                      let s = asString m
                                      liftIO $ putStr s
                                      addX $ fromIntegral $ length s
           WriteFormat fs -> writeFormat fs
           WriteIndirect _ -> fail "write argument should be normalized"

writeFormat :: [WriteFormatCode] -> RunMonad ()
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

setX :: MValue -> RunMonad ()
setX = change "$x" []

setY :: MValue -> RunMonad ()
setY = change "$y" []

getX :: RunMonad MValue
getX = getLocal "$x" []

getY :: RunMonad MValue
getY = getLocal "y" []

addX :: Int -> RunMonad ()
addX n = do x <- getX
            setX (x + fromIntegral n)

addY :: Int -> RunMonad ()
addY n = do y <- getY
            setY (y + fromIntegral n)

getLocal :: String -> [MValue] -> RunMonad MValue
getLocal label subs = do ma <- fetch label
                         return $ case mIndex ma subs of
                           Just mv -> mv
                           Nothing -> fromString ""                         

getLocalArray :: String -> [MValue] -> RunMonad (Maybe MArray)
getLocalArray label subs = do
  ma <- fetch label
  return $ mSubArray ma subs

forInf ::  Line -> RunMonad (Maybe (Maybe MValue))
forInf ((Quit cond Nothing):xs) = case cond of
    Nothing  -> return Nothing
    Just expr -> do mv <- eval expr
                    if mToBool mv
                     then return Nothing
                     else forInf xs
forInf ((Quit _ _):_) = fail "QUIT with argument in a for loop"
forInf (cmd:xs) = exec [cmd] >> forInf xs
forInf [] = forInf []  -- dumb

break ::  RunMonad ()
break = fail "BREAK not working"

getTest :: RunMonad Bool
getTest = mToBool `liftM` getLocal "$test" []

setTest :: Bool -> RunMonad ()
setTest =  change "$test" [] . boolToM


eval :: Expression -> RunMonad MValue
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
                                UPlus  -> mv+0
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
eval (FunCall label "" args) = function Nothing label args 
eval (FunCall label rou args) = function (Just rou) label args
eval (ExpBifCall bif)   = evalBif bif

function :: Maybe Name -> Name -> [FunArg] -> RunMonad MValue
function routine tag args
    = do
  retVal <- call routine tag args
  case retVal of
    Nothing -> fail "Function quit without returning a value"
    Just v -> return v


sub :: Maybe Name -> Name -> [FunArg] -> RunMonad ()
sub routine tag args
    = do
  retVal <- call routine tag args
  case retVal of
    Nothing -> return ()
    Just{} -> fail "Subroutine quit with a value!"


call :: Maybe Name -> Name -> [FunArg] -> RunMonad (Maybe MValue)
call Nothing tag args = localCall tag args
call (Just routine) "" args = call (Just routine) routine args
call (Just routine) tag args = remoteCall tag routine args


 
localCall :: Name -> [FunArg] -> RunMonad (Maybe MValue)
localCall label args = do (r :: Routine) <- (tags . head) `liftM` get
                          case r label of
                            Nothing -> fail $ "Noline: " ++ label
                            Just (argnames, cmds) -> funcall args argnames cmds r


remoteCall :: Name -> Name -> [FunArg] -> RunMonad (Maybe MValue)
remoteCall label routine args
    = openRemote routine $ \r ->
      case r label of
        Nothing -> fail $ "Noline: " ++ label ++ "^"  ++ routine
        Just (argnames, cmds) -> funcall args argnames cmds r

goto :: Maybe Name -> Name -> RunMonad (Maybe MValue)
goto Nothing tag
    = do
  s <- gets head
  doGoto tag (tags s) (gotoTags s)
goto (Just rouName) tag
    = openRemote rouName $ \r -> doGoto tag r r

doGoto :: Name -> Routine -> Routine -> RunMonad (Maybe MValue)
doGoto tag r gr
    = case gr tag of
        Nothing -> fail $ "Noline: " ++ tag
        Just ([], cmds) -> do
                   modify $ \(s:ss) -> s {tags=r,gotoTags=gr} : ss
                   runLines cmds
        Just{} -> fail "Error in GOTO: tag should not take arguments"

openRemote :: MonadIO m => Name -> (Routine -> m a) -> m a 
openRemote routine k
    = do
  let filename = routine ++ ".hmumps"
  text <- liftIO $ readFile filename
  case parse parseFile filename text of
    Left a -> (fail . show) a
    Right f -> let r = pack $ transform f in
               k r

evalBif :: BifCall -> RunMonad MValue
evalBif (BifChar args') = do
  args <- mapM eval args'
  let str = fmap (chr . asInt) args
  return $ fromString str

evalBif BifX = getX
evalBif BifY = getY
evalBif BifTest = boolToM `liftM` getTest
evalBif (BifOrder vn' expForward) = do
  vn <- normalize vn'
  case vn of
    Lvn label subs' -> do
            subs <- mapM eval subs'
            case unSnoc subs of
              Nothing -> fail "Cannot $ORDER with no subscripts"
              Just (rest,lastSub)
                  -> do
                ma <- getLocalArray label rest
                case ma of
                  Nothing -> return ""
                  Just a -> do
                      forward <- case expForward of
                                   Nothing -> return True
                                   Just ex -> mToBool `liftM` eval ex
                      case order a forward lastSub of
                        Nothing -> return ""
                        Just v -> return v
    Gvn{} -> fail "$ORDER on globals is not supported"
    _ -> error "Fatal error in ORDER"
evalBif (BifReplace haystack' needle' replacement') = do
  haystack <- eval haystack'
  needle <- eval needle'
  replacement <- eval replacement'
  return $ M.join replacement $ M.split needle haystack

-- evalBif bif = fail $ "oops! I don't know what to do with " ++ show bif


-- | returns the front of a list plus the last element.
-- returns Nothing if the list is empty.
unSnoc :: [a] -> Maybe ([a],a)
unSnoc [] = Nothing
unSnoc (x:xs) = Just $ case unSnoc xs of
                         Nothing -> ([],x)
                         Just ~(ys,y) -> (x:ys,y)

funcall :: [FunArg] -> [Name] -> [Line] -> Routine -> RunMonad (Maybe MValue)
funcall args' argnames cmds r = 
    let (pairs, remainder) = zipRem args' argnames in
    case remainder of
      Just (Left _) -> fail "Supplied too many parameters to function"
      _ -> do m <- foldM helper empty pairs
              let newframe = RunState (Just $ Env NormalEnv m) r r
              modify (newframe:)
              x <- runLines cmds
              modify tail
              return x

 where helper :: Map Name EnvEntry -> (FunArg, Name) -> RunMonad (Map Name EnvEntry)
       helper m (arg,name) = case arg of
             FunArgExp expr -> do mval <- eval expr
                                  let entry = Entry $ arrayUpdate mEmpty [] mval
                                  return $ insert name entry m
             FunArgName name' -> return $ insert name (LookBack $ Just name') m

runLines :: [Line] -> RunMonad (Maybe MValue)
runLines [] = return Nothing
runLines (x:xs) = do result <- exec x
                     case result of
                       Nothing -> runLines xs
                       Just x' -> return x'

zipRem :: [a] -> [b] -> ([(a,b)],Maybe (Either [a] [b]))
zipRem [] []         = ([],Nothing)
zipRem [] xs         = ([],Just $ Right xs)
zipRem xs []         = ([],Just $ Left  xs)
zipRem (x:xs) (y:ys) = let (pairs, remainder) = zipRem xs ys
                       in ((x,y):pairs,remainder)

