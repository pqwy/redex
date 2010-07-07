{-# LANGUAGE ViewPatterns, PatternGuards  #-}
{-# LANGUAGE PackageImports  #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Main where 

import Ast
import Stringy
import Operations
import Reductions
import Primitives
import Graphs
import Typer

import Control.Arrow
import Data.Generics

import System.Environment

import Control.Parallel.Strategies


instance NFData Term where
    rnf (ast -> Var x)     = x `seq` ()
    rnf (ast -> Lam x m)   = x `seq` rnf m
    rnf (ast -> Let x e m) = x `seq` rnf e `seq` rnf m
    rnf (ast -> App a b)   = rnf a `seq` rnf b
    rnf (ast -> Prim p)    = p `seq` ()


headWin :: (NFData a) => Strategy a -> [a] -> [a]
headWin st []     = []
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
         | otherwise          = mapM_ print run
    where run = take 1000 r


main = undefined
-- main = do
--     a : n : _ <- getArgs
--     let fun = case a of "one" -> whnf1
--                         "two" -> whnf2
--                         "thr" -> whnf3
--                         "fou" -> whnf4 
--     print $ (rnf `headWin` fun problem3) !! (read n)


globok =
    " let (o = λfgx.f (g x))                                    \
    \     (I = λx.x)                                            \
    \     (K = λxy.x)                                           \
    \     (S = λfgx.f x (g x))                                  \

    \     (Y1 = λf.(λu.f(u u)) λu.f(u u))                       \
    \     (Y2 = (λuf.f (u u f)) λuf.f (u u f))                  \
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



Right ty = Typer.typeOf predefEnv . read $
        "let (map = |fl.if (isNil l) nil (cons (f (head l)) (map f (tail l))) ) map (|xf.f (pair foo (bop x)))"


idx :: Ident -> [Int]
idx (IDD _ n) = [n]
idx _         = []

unIdx :: Ident -> Ident
unIdx (IDD s _) = ID s
unIdx x         = x

wee' :: Data t => t -> ([Int] -> [Int], t)
wee' = gfoldl inj (\x -> (id, x))
    where
        inj (l, f) t = let (tis, tt) = wee' t in
                ( l . tis . idx' tt , f (mkT unIdx tt) )

        idx' :: Typeable t => t -> [Int] -> [Int]
        idx' = mkQ id ((++) . idx)

wee :: Data t => t -> ([Int], t)
wee = first ($ []) . wee'


-- {-# LANGUAGE RankNTypes  #-}

--  woo :: Data t => t -> ([Int], t)
--  woo = first ($ []) . fun
--      where
--          
--          fun :: Data t => t -> ([Int] -> [Int], t)
--          fun = gfoldl inj (\x -> (id, x))

--          inj :: Data a => forall b. ([Int] -> [Int], a -> b) -> a -> ([Int] -> [Int], b)
--          inj (l, f) t = ( l . tis . idx' tt, f (mkT unIdx tt) )
--              where
--                  (tis, tt) = fun t

--          idx' :: Typeable t => t -> [Int] -> [Int]
--          idx' = mkQ id ((++) . idx)

