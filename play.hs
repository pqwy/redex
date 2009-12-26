module Main where

import Ast
import Stringy
import Operations


problem1, problem2, problem3 :: Term
problem1 = read "(let (a = λq.q) let (b = λo.o) a b c) P Q"
problem2 = read "(λf.(λu.f (u u)) λu.f (u u)) (λsx.s) P Q R"
problem3 = read "(λf.(λu.f (u u)) λw.f (w w)) (λsx.s) P Q R"

test3, test4 :: Term -> IO ()
test3 = mapM_ print . take 50 . whnf1
test4 = mapM_ print . take 50 . whnf2

