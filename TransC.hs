{-# LANGUAGE FlexibleInstances #-}

module TransC where

import Control.Monad (ap, liftM, (>=>))
import qualified Control.Monad.State as S
import Control.Monad.Trans
import Data.Foldable (toList)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import System.IO (hReady, stdin)
import Test.HUnit hiding (State)

-- a)

class Monad m => Output m where
  write :: String -> m ()

class Monad m => Input m where
  input :: m (Maybe String) -- only return input if it is ready

instance Output IO where
  write = putStr

instance Input IO where
  input = do
    x <- hReady stdin
    if x then Just <$> getLine else return Nothing

-- | Wait for some input to appear, and when it does, repeat it.
echo :: (Input m, Output m) => m ()
echo = do
  ms <- input
  case ms of
    Just str -> write str >> write "\n"
    Nothing -> echo

type FakeIO = S.State FakeState

data FakeState = FS
  { fsWrite :: Seq String, -- what has been written
    fsInput :: [Maybe String] -- what to read from
  }

instance Output FakeIO where
  write s = do
    st <- S.get
    let oldLog = fsWrite st
    let newLog = oldLog <> Seq.singleton s
    S.put $ st {fsWrite = newLog}

instance Input FakeIO where
  input = undefined

runFakeIO :: FakeIO () -> [Maybe String] -> [String]
runFakeIO comp inputs =
  toList (fsWrite (S.execState comp initState))
  where
    initState = FS {fsWrite = Seq.empty, fsInput = inputs}

testEcho :: Test
testEcho =
  runFakeIO
    echo
    [Nothing, Nothing, Just "hello"]
    ~?= ["hello", "\n"]

-- >>> runTestTT testEcho

testEcho2 :: Test
testEcho2 =
  runFakeIO
    (echo >> echo)
    [Just "hello", Nothing, Just "CIS 552"]
    ~?= ["hello", "\n", "CIS 552", "\n"]

-- >>> runTestTT testEcho2

test3 :: Test
test3 = runFakeIO undefined undefined ~?= undefined

-- >>> runTestTT test3

-- b)

data Action m
  = Atom (m (Action m)) -- an atomic computation, returning a new action
  | Fork (Action m) (Action m) -- create a new thread
  | Stop -- terminate this thread

newtype C m a = C {runC :: (a -> Action m) -> Action m}

instance Monad m => Monad (C m) where
  return x = undefined
  m >>= f = undefined

instance Monad m => Applicative (C m) where
  pure = return
  (<*>) = ap

instance Monad m => Functor (C m) where
  fmap = liftM

-- c)

atom m = C $ \k -> Atom (liftM k m)

run m = sched [runC m $ const Stop]

fork m = C $ \k -> Fork (runC m $ const Stop) (k ())

sched [] = return ()
sched (Atom m : cs) = m >>= \a -> sched (cs ++ [a])
sched (Fork a1 a2 : cs) = sched (cs ++ [a1, a2])
sched (Stop : cs) = sched cs

-- d)

instance Input m => Input (C m) where
  input = undefined

instance Output m => Output (C m) where
  write = undefined

instance MonadTrans C where
  lift = atom

example :: Output m => C m ()
example = do
  fork (write "Hello " >> write "552")
  write "CIS"

runCFakeIO :: C FakeIO () -> [Maybe String] -> [String]
runCFakeIO x inputs = undefined

testWrite :: Test
testWrite = runCFakeIO example [] ~?= ["Hello ", "CIS", "552"]

-- >>> runTestTT testWrite

example2 :: (Input m, Output m) => C m ()
example2 = undefined

testExample2 :: Test
testExample2 = undefined

-- >>> runTestTT testExample2
