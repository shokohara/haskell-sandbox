module LYGHReader where

import System.Random

addStuff :: Int -> Int
--addStuff = do
--  a <- (*2)
--  b <- (+10)
--  return (a+b)
addStuff x = let
  a = (*2) x
  b = (+10) x
  in a+b

threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen =
  let (firstCoin, newGen) = random gen
      (secondCoin, newGen') = random newGen
      (thirdCoin, newGen'') = random newGen'
  in (firstCoin, secondCoin, thirdCoin)

type Stack = [Int]

pop :: Stack -> (Int, Stack)
pop (x:xs) = (x,xs)

push :: Int -> Stack -> ((),Stack)
push a xs = ((), a:xs)

stackManip :: Stack -> (Int, Stack)
--stackManip stack = let
--  ((), newStack1) = push 3 stack
--  (a, newStack2) = pop newStack1
--  in pop newStack2
stackManip = do
  push 3
  _ <- pop
  pop

