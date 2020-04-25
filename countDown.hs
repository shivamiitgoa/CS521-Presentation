-- The CountDown Problem 
-- Written by - Pratyay, Shivam, Ashish, Shreyash and Sandesh 

import Prelude

-- We are creating a new data type called Op
-- Since we can not create Op from the already available types
-- and it is a completely new type, we have used `data`
-- It may come to us that, we could have used `newtype` also
-- but we also needed to have multiple constructors, that is 
-- the reason why `data` is the suitable method to create 
-- a new data type for our purpose 
data Op = Add | Sub | Mul | Div 


-- Now, we are making our new type `Op`, an instance of `Show` class
-- For this we have supply some definition about how show will behave 
-- on the objects of type `Op`
instance Show Op where 
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"


-- Checks if the result of the operation will be a (strictly) positive number 
-- Input will be an operation with two positive numbers 
valid :: Op -> Int -> Int -> Bool 
valid Add _ _ = True 
valid Sub x y = x > y 
valid Mul _ _ = True 
valid Div x y = x `mod` y == 0


-- Computes the result
apply :: Op -> Int -> Int -> Int 
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y 
apply Div x y = x `div` y 


-- New data type for expressions
-- Note 1: Example of constructor with parameter 
-- Note 2: Example of recursive data type 
data Expr = Val Int | App Op Expr Expr 


-- Show definitions for `Expr` data type 
instance Show Expr where 
    show (Val n) = show n 
    show (App o l r) = bracketShow l ++ show o ++ bracketShow r 
                        where 
                            bracketShow (Val n) = show n 
                            bracketShow e = "(" ++ show e ++ ")"


-- Retrieves the list of numbers used in the expression
values :: Expr -> [Int] 
values (Val n) = [n]
values (App _ l r) = values l ++ values r 


-- Evaluates the expression
evaluate :: Expr -> [Int]
evaluate (Val n) = [n | n > 0]
evaluate (App o l r) = [apply o x y | x <- evaluate l,
                                      y <- evaluate r,
                                      valid o x y]


-- Computes power set of the given set 
subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = yss ++ map (x:) yss 
              where yss = subs xs 


-- Computes all possible ways of inserting a new element into a list 
interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)


-- Computes all the permutation of a list 
perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))


-- Computes all the possible ways to selecting zero or more elements in any order 
choices :: [a] -> [[a]]
choices = concat . map perms . subs 


-- Checks if given expression is a solution of the given count down problem
solution :: Expr -> [Int] -> Int -> Bool 
solution e ns n = elem (values e) (choices ns) && evaluate e == [n]


-- Given a list, it splits the list in all possible ways into two list which are not empty 
split :: [a] -> [([a], [a])]
split [] = []
split [_] = []
split (x:xs) = ([x], xs) : [(x:ls,rs) | (ls, rs) <- split xs]


-- A function which returns all the operations
ops :: [Op]
ops = [Add, Sub, Mul, Div]


-- Given two expressions, it combines these two expression with all possible operations 
combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]


-- Given a list of numbers, it computes all the expressions which are formed with these numbers 
exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e | (ls ,rs) <- split ns,
                l <- exprs ls,
                r <- exprs rs,
                e <- combine l r]


-- Brute force algorithm to get solution of count down problem
solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e | ns' <- choices ns, e <- exprs ns', evaluate e == [n]]


-- Result data type to implement the optimized count down algorithm
type Result = (Expr, Int) 


-- Combines two Result's with all possible operations while removing invalid Result's (which Result contains an invalid expression) 
combine' :: Result -> Result -> [Result]
combine' (l,x) (r,y) = [(App o l r, apply o x y) | o <- ops, valid o x y]


-- Given a list of numbers, it computes all the valid Result's (expression with its final value) which are formed with these numbers 
results :: [Int] -> [Result]
results [] = []
results [n] = [(Val n, n) | n > 0]
results ns = [res | (ls, rs) <- split ns,
                    lx <- results ls,
                    ry <- results rs,
                    res <- combine' lx ry]


-- Optimized algorithm one to get solution of count down problem
solutions' ns n = [e | ns' <- choices ns, (e, m) <- results ns', m == n]


-- Optimized operation validator 
-- Since we are considering all the permutations,
--   If (Expr1, Expr2) is considered, then (Expr2, Expr1) will also be considered
--     So, we remove all Add x, y such that x > y 
--         we remove all Mul x, y such that x > y 
-- Since we are considering all the power set,
--   If set S is considered, then all subset of S will also be considered
--     So, we remove all Mul x, 1 and Mul 1, y
--         we remove all Div x, 1
valid'' :: Op -> Int -> Int -> Bool 
valid'' Add x y = x <= y
valid'' Sub x y = x > y 
valid'' Mul x y = x /= 1 && y /= 1 && x <= y 
valid'' Div x y = y /= 1 && x `mod` y == 0


-- Optimized combine function 
combine'' :: Result -> Result -> [Result]
combine'' (l,x) (r,y) = [(App o l r, apply o x y) | o <- ops, valid'' o x y]


-- Optimized results function
results'' :: [Int] -> [Result]
results'' [] = []
results'' [n] = [(Val n, n) | n > 0]
results'' ns = [res | (ls, rs) <- split ns,
                    lx <- results'' ls,
                    ry <- results'' rs,
                    res <- combine'' lx ry]


-- Optimized solution' function
solutions'' ns n = [e | ns' <- choices ns, (e, m) <- results'' ns', m == n]


main :: IO ()
main = do
  print "**************************************************************************"
  print "Example for 'Show Op'"
  print "print Add "
  print Add 
  print "print Sub "
  print Sub 
  print "Sub is printed below"
  print "**************************************************************************"
  print "Example of 'valid'"
  print "print (valid Add 1 2)"
  print (valid Add 1 2)
  print "print (valid Sub 1 2)"
  print (valid Sub 1 2)
  print "**************************************************************************"
  print "Example of 'apply'"
  print "print (apply Add 1 2)"
  print (apply Add 1 2)
  print "print (apply Div 50 5)"
  print (apply Div 50 5)
  print "**************************************************************************"
  print "Example of 'Expr'"
  print "(App Add (Val 1) (App Mul (Val 2) (Val 3)))"
  print (App Add (Val 1) (App Mul (Val 2) (Val 3)))
  print "**************************************************************************"
  print "Example of 'values'"
  print "print (values (App Add (Val 2) (Val 3)))"
  print (values (App Add (Val 2) (Val 3)))
  print "**************************************************************************"
  print "Example of 'evaluate'"
  print "print (evaluate (App Add (Val 2) (Val 3)))"
  print (evaluate (App Add (Val 2) (Val 3)))
  print "print (evaluate (App Sub (Val 2) (Val 3)))"
  print (evaluate (App Sub (Val 2) (Val 3)))
  print "**************************************************************************"
  print "Example of 'subs'"
  print "print (subs [1,2,3])"
  print (subs [1,2,3])
  print "Example of 'interleave'"
  print "print (interleave 1 [2,3,4])"
  print (interleave 1 [2,3,4])
  print "Example of 'perms'"
  print "print (perms [1,2,3])"
  print (perms [1,2,3])
  print "**************************************************************************"
  print "Example of 'choices'"
  print "print (choices [1,2,3])"
  print (choices [1,2,3])
  print "**************************************************************************"
  print "Example of 'split'"
  print "print (split [1,2,3,4])"
  print (split [1,2,3,4])
  print "**************************************************************************"
  print "Example of 'combine'"
  print "print (combine (Val 1) (Val 2))"
  print (combine (Val 1) (Val 2))
  print "**************************************************************************"
  print "Example of 'exprs'"
  print "print (exprs [1, 2, 3])"
  print (exprs [1, 2, 3])
  print "**************************************************************************"
  print "Example of 'combine''"
  print "print (combine' ((Val 1), 1) ((Val 2), 2))"
  print (combine' ((Val 1), 1) ((Val 2), 2))
  print "**************************************************************************"
  print "Brute Force Algorithm"
  print "print (take 5 (reverse (solutions [1,3,7,10,25,50] 765)))"
  print (take 5 (reverse (solutions [1,3,7,10,25,50] 765)))
  print "**************************************************************************"
  print "Optimized Algorithm 1"
  print "print (take 5 (reverse (solutions' [1,3,7,10,25,50] 765)))"
  print (take 5 (reverse (solutions' [1,3,7,10,25,50] 765)))
  print "**************************************************************************"
  print "Optimized Algorithm 2"
  print "print (take 5 (reverse (solutions'' [1,3,7,10,25,50] 765)))"
  print (take 5 (reverse (solutions'' [1,3,7,10,25,50] 765)))
  print "**************************************************************************"

