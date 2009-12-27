module Main where

import Ast
import Stringy
import Operations


problem1, problem2, problem3, problem4 :: Term
problem1 = read "(let (a = λq.q) (b = λo.o) a b c) P Q"
problem2 = read "(λf.(λw.f (w w)) λu.f (u u)) (λsx.s) P Q R"
problem3 = read "let (y = λf.f (y f)) y (λsx.s x x) A"
problem4 = read "(λf.(λw.f (w w)) λu.f (u u)) (λsx.s x x) A"

test1, test2 :: Term -> IO ()
test1 = mapM_ print . take 50 . whnf1
test2 = mapM_ print . take 50 . whnf2


main = print (whnf2 problem2 !! 1000)
