module Main where
import qualified Text.Pronounce as Pronounce
import Debug.Trace as Debug
import Control.Exception (evaluate)
import Control.DeepSeq (rnf)

main :: IO ()
main = do
    putStrLn "Hello, Haskell!"
    evaluate $ cmuDict
    return ()

cmuDict = Debug.trace "Got dict" $! Pronounce.myDict
