module MetaFeatures.LazinessDemo (runLazinessDemo) where

import Control.Monad.State.Lazy (State, StateT, evalState, evalStateT, get, put)
import Control.Monad.Identity (Identity)
import Control.Concurrent (forkIO, threadDelay, killThread)


runLazinessDemo :: IO ()
runLazinessDemo = do
    putStrLn "Laziness of Identity, Maybe and IO monads on top of lazy State monad transformer:"
    putStrLn "================================================================================="
    putStr " -  laziness of the lazy State monad: "
    putStr (show $ take 12 $ evalState machine  0) >> putStr "\r ✓\n"
    putStr " -  laziness of Identity monad on top of the lazy State monad transformer (StateT s Identity a): "
    putStr (show $ fmap (take 12) (evalStateT machineId 0)) >> putStr "\r ✓\n"
    putStr " -  laziness of Maybe monad on top of the lazy State monad transformer (StateT s Maybe a): "
    mbThreadId <- forkIO $ print (fmap (take 12) (evalStateT machineMaybe 0)) >> putStr "\r ✓\n"
    threadDelay 2000000
    putStr "<<!!-INFINITE-LOOP-!!>>\r ✕\n"
    killThread mbThreadId
    putStr "  - laziness of IO monad on top of the lazy State monad transformer (StateT s IO a): "
    ioThreadId <- forkIO $ fmap (take 12) (evalStateT machineIO 0) >>= print >> putStr "\r ✓\n"
    threadDelay 2000000
    putStr "<<!!-INFINITE-LOOP-!!>>\r ✕\n"
    killThread ioThreadId

machine :: State Int [Int]
machine = do
    counter <- get
    put $ succ counter
    (counter :) <$> machine

machineId :: StateT Int Identity [Int]
machineId = do
    counter <- get
    put $ succ counter
    (counter :) <$> machineId

machineMaybe :: StateT Int Maybe [Int]
machineMaybe = do
    counter <- get
    put $ succ counter
    (counter :) <$> machineMaybe

machineIO :: StateT Int IO [Int]
machineIO = do
    -- lift getLine
    counter <- get
    put $ succ counter
    (counter :) <$> machineIO
