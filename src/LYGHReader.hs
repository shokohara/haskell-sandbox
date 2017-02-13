module LYGHReader where

import System.Random
import Control.Monad.State

addStuff :: Int -> Int
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

pop :: State Stack Int
pop = state $ \(x:xs) -> (x,xs)

push :: Int -> State Stack ()
push a = state $ \xs -> ((), a:xs)

stackManip :: State Stack Int
stackManip = do
  push 3
  pop
  pop

stackStuff :: State Stack ()
stackStuff = do
  a <- pop
  if a == 5
     then push 5
     else do
       push 3
       push 8

moreStack :: State Stack ()
moreStack = do
  a <- stackManip
  if a == 100
     then stackStuff
     else return ()

