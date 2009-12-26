module Main where

import Ast
import Stringy
import Operations


problem1, problem2 :: Term
problem1 = read "(let (a = λq.q) (b = λo.o) a b c) P Q"
problem2 = read "(λf.(λw.f (w w)) λu.f (u u)) (λsx.s) P Q R"

test3, test4 :: Term -> IO ()
test3 = mapM_ print . take 50 . whnf1
test4 = mapM_ print . take 50 . whnf2


main = print (whnf2 problem2 !! 1000)
