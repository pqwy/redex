module Main where 
import Ast
import Stringy
import Operations
import Reductions
import Primitives


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


main = print (whnf2 problem3 !! 1000)


globok =
    " \
    \ let (o = λfgx.f (g x))                     \
    \     (id = λx.x)                            \
    \     (K = λxy.x)                            \
    \     (S = λfgx.f x (g x))                   \

    \     (kons = λabs.s a b)                    \
    \     (kar = λk.k λab.a)                     \
    \     (kdr = λk.k λab.b)                     \

    \     (zero = λfx.x)                         \
    \     (add1 = λnfx.f (n f x))                \
    \     (plus = λmnf.o (m f) (n f))            \
    \     (sub1 = λnfx.n (λgh.h (g f)) (K x) id) \
    \     (minus = λmn.n sub1 m)                 \

    \     (true = λab.a)                         \
    \     (false = λab.b)                        \

    \     (iszero = λn.n (K false) true)         \
\ "

exp1, exp2, exp3, exp4, exp5 :: Term
exp1 = read (globok ++ "iszero zero Y N")
exp2 = read (globok ++ "iszero (sub1 (sub1 (sub1 (add1 (add1 (add1 zero)))))) Y N")
exp3 = read (globok ++ "iszero (plus (add1 (add1 (add1 (add1 zero)))) (add1 (add1 zero))) Y N")
exp4 = read (globok ++ "iszero (minus zero zero) Y N")
exp5 = read (globok ++ "(λn.iszero (minus n n)) (plus (add1 (add1 (add1 (add1 zero)))) (add1 (add1 zero))) Y N")

backtrack1 :: Term
backtrack1 = read "(let (K = |xy.x) (let (zero = |fx.x) zero (|nfx.n (|gh.h (g f)) (K x) (|x.x)) zero) (K (|ab.b))) (|ab.a) Y N"


