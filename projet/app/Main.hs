module Main where

import Moteur

main :: IO ()
main = etat1 >>= lance >> return ()
