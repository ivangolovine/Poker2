module PokerTester where

    -- Place this PokerTester file into the same directory as your 
    -- Poker.hs file. Load it into ghci, call testAll function.
    import Poker (deal)
    import System.Random
    import Data.Array.IO
    import Control.Monad
  
    --deal p = ["2C", "3C", "4C", "5C", "6C"] 
  
    shuffle :: [a] -> IO [a]
    shuffle xs = do
        ar <- newArray n xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
        where
            n = length xs
            newArray :: Int -> [a] -> IO (IOArray Int a)
            newArray n xs =  newListArray (1,n) xs

    testPerm p = do 
        --let res = deal p 
        let res = Poker.deal p 
        -- Will cause error if returned list doesn't contain strings
        let str = foldl (++) "" res 
        -- Ensure return list has correct number of cards
        length res == 5   

    runTests :: Int -> IO (Int) -> IO (Int)
    runTests 0 nPass = nPass
    runTests n nPass = do
        shuf <- shuffle [1..52]
        np <- nPass
        if testPerm (take 9 shuf) 
        then runTests (n-1) (return $ np+1) 
        else runTests (n-1) (return np)
    
    -- Load this module into ghci, call testAll, adjust nTests as desired.
    testAll = do 
        let nTests = 1000 
        nPassed <- runTests nTests $ return 0 
        putStrLn $ "Succeeded " ++ show(nPassed) ++ " out of " ++ show(nTests)
        
            

    



    