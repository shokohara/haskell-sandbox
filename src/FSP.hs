module FSP where

import Control.Monad.Writer
import Data.Char
import Control.Applicative
import Control.Arrow

data Operator = Plus | Minus | Mult | Div deriving (Show, Eq)

stringToOperator :: Integral a => String -> Maybe (a -> a -> a)
stringToOperator x
  | x == "+" = Just (+)
  | x == "-" = Just ( - )
  | x == "*" = Just (*)
  | x == "/" = Just div
  | otherwise = Nothing

stringToInteger :: String -> Maybe Int
stringToInteger x
  | all isDigit x = Just (read x :: Int)
  | otherwise = Nothing

rpn :: String -> Int
rpn = flip abc [] . fmap (stringToInteger &&& stringToOperator) . words where
  abc :: [(Maybe t, Maybe (t -> t -> t))] -> [t] -> t
  abc ((Just v, Nothing):xs) s = abc xs (v : s)
  abc ((Nothing, Just o):xs) s = abc xs (o (head s) (head . tail $ s) : (tail . tail $ s))
  abc [] s = head s

