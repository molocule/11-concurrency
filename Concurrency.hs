{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Concurrency where

import Control.Monad
import Data.IORef
import Network.Socket -- from the `network` library
import System.IO

data Action
  = Atom (IO Action) -- an atomic computation, returning a new action
  | Fork Action Action -- create a new thread
  | Stop -- terminate this thread

writeAction :: String -> Action
writeAction = undefined

prog :: Action
prog = Fork (writeAction "Hello\n") (writeAction "CIS 552\n")

sched :: [Action] -> IO ()
sched = undefined

--    *Concurrency> hSetBuffering stdout NoBuffering
--    *Concurrency> hSetBuffering stdin NoBuffering

--    *Concurrency> sched [ prog ]

sequenceAction :: Action -> Action -> Action
sequenceAction a1 a2 = error "Hypothetical. Don't try to write me."

hello552 :: Action
hello552 = writeAction "Hello" `sequenceAction` writeAction "CIS 552"

writeComputation :: String -> Action -> Action
writeComputation "" k = k
writeComputation (c : cs) k = undefined

prog3 :: Action
prog3 = writeComputation "Hello" (writeComputation " CIS 552\n" Stop)

--    *Concurrency> sched [prog3]

sequenceComputation ::
  (Action -> Action) ->
  (Action -> Action) ->
  (Action -> Action)
sequenceComputation = undefined

hello552Computation :: Action -> Action
hello552Computation =
  writeComputation "Hello"
    `sequenceComputation` writeComputation "CIS 552\n"

{-

   *Concurrency> sched [ hello552Computation Stop ]
   *Concurrency> sched [ Fork (hello552Computation Stop) (hello552Computation Stop) ]
   *Concurrency> let bomb = writeComputation "bomb" bomb
   *Concurrency> sched [ bomb ]

-}

readComputation :: (Char -> Action) -> Action
readComputation = undefined

sequenceComp ::
  ((a -> Action) -> Action) -> -- last action takes an arg.
  (a -> (b -> Action) -> Action) -> -- pass to another
  (b -> Action) ->
  Action
sequenceComp m f = undefined

--      |
--      |  spoiler space, no peeking
--      |
--      |
--      |
--      |
--      |
--      |
--      |
--      |
--      |
--      |
--      |
--      |
--      |
--      |
--      |
--      |
--      |
--      |
--      |
--      |
--      |
--      |

type CM a = (a -> Action) -> Action

sequenceCompM :: CM a -> (a -> CM b) -> CM b
sequenceCompM m f = \k -> m (`f` k)

returnCompM :: a -> ((a -> Action) -> Action)
returnCompM x = undefined

newtype C a = C {runC :: (a -> Action) -> Action}

instance Monad C where
  (>>=) :: C a -> (a -> C b) -> C b
  m >>= f = C $ \k -> runC m (\v -> runC (f v) k)

  return :: a -> C a
  return x = C $ \k -> k x

instance Applicative C where
  pure = return
  (<*>) = ap

instance Functor C where
  fmap = liftM

atom :: IO a -> C a
atom = undefined

fork :: C () -> C ()
fork m = C $ \k -> Fork (runC m (const Stop)) (k ())

run :: C a -> IO ()
run m = sched [runC m (const Stop)]

class Monad m => OutputMonad m where
  write :: String -> m ()

instance OutputMonad IO where
  write = putStr

infloop :: OutputMonad m => String -> m ()
infloop = undefined

--     *Concurrency> infloop "CIS 552"

instance OutputMonad C where
  write s = atom (write s)

example :: C ()
example = do
  write "It's raining..."
  fork (infloop "dog\n")
  fork (infloop "cat\n")

--    *Concurrency> run example

-- instance OutputMonad C where
--    write []     = atom (write [])
--    write (x:xs) = atom (write [x]) >> write xs

class Monad m => InputMonad m where
  input :: m (Maybe String)

instance InputMonad IO where
  input = do
    x <- hReady stdin
    if x then Just <$> getLine else return Nothing

ioloop :: (InputMonad m, OutputMonad m) => String -> m String
ioloop s = do
  i <- input
  case i of
    Just x -> return $ "Thread " ++ s ++ ":" ++ x
    Nothing -> do
      write s
      ioloop s

--    *Concurrency> ioloop "CIS 552"   -- defaults to IO monad

instance InputMonad C where
  input = atom input

example2 :: C ()
example2 = do
  fork $ ioloop "a" >>= write
  ioloop "b" >>= write

--    *Main> run example2

type MVar a = IORef (Maybe a)

class Monad m => MVarMonad m where
  newMVar :: m (MVar a)
  writeMVar :: MVar a -> a -> m ()
  takeMVar :: MVar a -> m (Maybe a)

instance MVarMonad IO where
  newMVar = newIORef Nothing
  writeMVar v a = writeIORef v (Just a)
  takeMVar = undefined

instance MVarMonad C where
  newMVar = atom newMVar
  writeMVar v a = atom (writeMVar v a)
  takeMVar v = atom (takeMVar v)

readMVar :: (MVarMonad m) => MVar a -> m a
readMVar = undefined

data Msg
  = Add
  | Reset
  | Print
  | Quit

simulation :: MVar Msg -> Integer -> C ()
simulation mv i = do
  x <- takeMVar mv
  case x of
    Just Add -> do
      write "Adding...\n"
      simulation mv (i + 1)
    Just Reset -> do
      write "Resetting...\n"
      simulation mv 0
    Just Print -> do
      write ("Current value is " ++ show i ++ "\n")
      simulation mv i
    Just Quit -> do write "Done\n"
    Nothing -> simulation mv i

interface :: MVar Msg -> C (Maybe String) -> C ()
interface mv getInput = loop
  where
    loop = do
      maybeKey <- getInput
      case maybeKey of
        Just "a" -> writeMVar mv Add >> loop
        Just "r" -> writeMVar mv Reset >> loop
        Just "p" -> writeMVar mv Print >> loop
        Just "q" -> writeMVar mv Quit
        Just s -> write ("Unknown command: " ++ s ++ "\n") >> loop
        Nothing -> loop

example6 :: C ()
example6 = do
  mv <- newMVar
  fork $ simulation mv 0
  interface mv input

--   *Concurrency> run example6

-- | Create an interface to the server that communicates via a socket
-- on the specified port
network :: String -> MVar Msg -> C ()
network port mv = do
  handle <- atom $
    withSocketsDo $ do
      putStrLn "Opening a socket."
      addr <- resolve
      sock <- open addr
      (conn, _peer) <- accept sock
      putStrLn "Connected to socket."
      socketToHandle conn ReadMode
  interface
    mv
    ( atom $ do
        x <- hReady handle
        if x
          then Just <$> hGetLine handle
          else return Nothing
    )
  atom $ do
    hClose handle
    putStrLn "Socket closed."
  where
    resolve = do
      let hints =
            defaultHints
              { addrFlags = [AI_PASSIVE],
                addrSocketType = Stream
              }
      addrInfos <- getAddrInfo (Just hints) Nothing (Just port)
      case addrInfos of
        [] -> error "resolve returned no results"
        (addrInfo : _) -> return addrInfo
    open addr = do
      sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      setSocketOption sock ReuseAddr 1
      withFdSocket sock setCloseOnExecIfNeeded
      bind sock $ addrAddress addr
      listen sock 1024
      return sock

example7 :: String -> C ()
example7 port = do
  mv <- newMVar
  fork (simulation mv 0)
  fork (interface mv input)
  network port mv

--        *Client> h <- client local "1025"

--        *Client> send h "p"
--        *Client> send h "a"   -- no output here, all effects are shown on the server
--        *Client> send h "r"
