{-# LANGUAGE ViewPatterns, PatternGuards  #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Main where 

import Ast
import Stringy
import Operations
import Reductions
import Primitives
import Graphs

import System.Environment

import Control.Parallel.Strategies


instance NFData Term where
    rnf (ast -> Var x) = x `seq` ()
    rnf (ast -> Lam x m) = x `seq` rnf m
    rnf (ast -> Let x e m) = x `seq` rnf e `seq` rnf m
    rnf (ast -> App a b) = rnf a `seq` rnf b
    rnf (ast -> Prim p) = p `seq` ()


headWin :: (NFData a) => Strategy a -> [a] -> [a]
headWin st [] = []
headWin st (x:xs) = st x `seq` x : headWin st xs



problem1, problem2, problem3, problem4 :: Term
problem1 = read "(let (a = λq.q) (b = λo.o) a b c) P Q"
problem2 = read "(λf.(λw.f (w w)) λu.f (u u)) (λsx.s) P Q R"
problem3 = read "let (y = λf.f (y f)) y (λsx.s x x) A"
problem4 = read "(λf.(λw.f (w w)) λu.f (u u)) (λsx.s x x) A"

test1, test2 :: Term -> IO ()
test1 = anTest . whnf1
test2 = anTest . whnf2

anTest r | length run == 1000 = mapM_ print (take 50 run) >> putStrLn "\n** LOOP"
         | otherwise = mapM_ print run
    where run = take 1000 r


main = do
    a : n : _ <- getArgs
    let fun = case a of "one" -> whnf1
                        "two" -> whnf2
                        "thr" -> whnf3
                        "fou" -> whnf4 
    print $ (rnf `headWin` fun problem3) !! (read n)


globok =
    " let (o = λfgx.f (g x))                                    \
    \     (I = λx.x)                                            \
    \     (K = λxy.x)                                           \
    \     (S = λfgx.f x (g x))                                  \

    \     (Y1 = λf.(λu.f(u u)) λu.f(u u))                       \
    \     (Y2 = let (A = λxy.y (x x y)) A A)                    \
    \     (Y3 = let                                             \
    \           (L = λabcdefghijklmnopqstuvwxyzr.r(t h i s i s a f i x e d p o i n t c o m b i n a t o r))   \
    \           L L L L L L L L L L L L L L L L L L L L L L L L L L)    \
    \     (Y4 = λf.f (Y4 f))                                    \

    \     (kons = λabs.s a b)                                   \
    \     (kar = λk.k λab.a)                                    \
    \     (kdr = λk.k λab.b)                                    \

    \     (cons = λxlfa.f x (l f a))                            \
    \     (nil = λfa.a)                                         \
    \     (car = λl.l (λab.a) X)                                \
    \     (cdr = λlfa.l (λxgh.h x (g f)) (K a) (λab.b))         \
    \     (isnil = λl.l (λxg.false) true)                       \

    \     (zero = λfx.x)                                        \
    \     (succ = λnfx.f (n f x))                               \
    \     (plus = λmnf.o (m f) (n f))                           \
    \     (plus = λmnfx.m f (n f x))                            \
    \     (pred = λnfx.n (λgh.h (g f)) (K x) I)                 \
    \     (minus = λmn.n pred m)                                \
    \     (mul = λmnf.m (n f))                                  \
    \     (exp = λmn.n m)                                       \

    \     (true = λab.a)                                        \
    \     (false = λab.b)                                       \
    \     (not = λfab.f b a)                                    \

    \     (iszero = λn.n (K false) true)                        \
    \     (lteq = λnm.iszero (m pred n))                        \

    \     (decode = λn.n (λx.x A) (λx.x))                       \

    \     (iota = λn.cons n (iota (succ n)))                    \
    \     (lsum = λl.l plus zero)                               \
    \     (take = λnl.l (λabm.iszero m nil (cons a (b (pred m)))) (λm.nil) n) \
    \     (map = λglf.l (λa.f(g a)))                            \
\ "

exp1, exp2, exp3, exp4, exp5 :: Term
exp1 = read (globok ++ "iszero zero Y N")
exp2 = read (globok ++ "iszero (pred (pred (pred (succ (succ (succ zero)))))) Y N")
exp3 = read (globok ++ "iszero (plus (succ (succ (succ (succ zero)))) (succ (succ zero))) Y N")
exp4 = read (globok ++ "iszero (minus zero zero) Y N")
exp5 = read (globok ++ "(λn.iszero (minus n n)) (plus (succ (succ (succ (succ zero)))) (succ (succ zero))) Y N")

backtrack1 :: Term
backtrack1 = read "(let (K = |xy.x) (let (zero = |fx.x) zero (|nfx.n (|gh.h (g f)) (K x) (|x.x)) zero) (K (|ab.b))) (|ab.a) Y N"


