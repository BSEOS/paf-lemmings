module Moteur where

import Control.Concurrent
import Etat ( tourEtat, Etat(lrestantsE, lvivantsE), Fin )

--import System.Console.ANSI

tourMoteur :: Int -> Etat -> IO (Either Fin (Int, Etat))
tourMoteur n e = case tourEtat n e of
  Left f -> return $ Left f
  Right ne -> do
    print ne
    putStrLn ("Tour " <> show n <> " restants : " <> show (lrestantsE ne) <> ", vivants : " <> show (lvivantsE ne))
    putStrLn ""
    threadDelay 500000
    --System.Console.ANSI.clearScreen
    return $ Right (n + 1, ne)

tourne :: Int -> Etat -> IO String
tourne n e = do
  e1 <- tourMoteur n e
  case e1 of
    Left f -> return "Fini"
    Right (i, n) -> tourne i n

lance :: Etat -> IO String
lance = tourne 0
