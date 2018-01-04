module LVM.Reduce where
import LVM.Parse
import Data.Map as Map

type Env = Map.Map String Expr

reduce :: Env -> Expr -> Expr
reduce env (Var n)      = case Map.lookup n env of
                            Nothing -> Var n
                            Just x -> x
reduce env (App a b)    = case beta env a b of
                            App a b -> App a b
                            x -> reduce env x
reduce env (Nat n)      = Nat n
reduce env (Lam a (Lam b x))
    | x == Var a        = Boolean True
    | x == Var b        = Boolean False
    | otherwise         = case funToNat (Lam a (Lam b (x))) of
                                    Nat (-1) -> q
                                    Nat n -> Nat n
                                where q = case Map.lookup a env of
                                        Nothing -> Lam a (reduce env (Lam b x))
                                        Just old -> Lam newName (reduce env (refact a newName (Lam b x)))
                                      newName = '_':a
reduce env (Lam a b)    = case Map.lookup a env of
                            Nothing -> Lam a (reduce env b)
                            Just old -> Lam newName (reduce env (refact a newName b))
                        where newName = '_':a
--reduce env (Boolean True)  = Lam "a" (Lam "b" (Var "a"))
--reduce env (Boolean False) = Lam "a" (Lam "b" (Var "b"))
reduce env x            = x

refact :: String -> String -> Expr -> Expr
refact oldName newName (Var n) = if n == oldName then Var newName else Var n
refact oldName newName (App p q) = App (refact oldName newName p) (refact oldName newName q)
refact oldName newName (Nat n) = Nat n
refact oldName newName (Lam p q) = if p == oldName then Lam p q else Lam p (refact oldName newName q)
refact oldName newName (Boolean b) = Boolean b

contains :: Expr -> String -> Int -- Checks how deep lambdas use s as a free variable
contains (Var a) s = 0
contains (App a b) s = max (contains a s) (contains b s)
contains (Lam a b) s = 1 + contains b s
contains (Nat n) _ = 0
contains (Boolean b) _ = 0

reduceL env x = case reduce env x of -- Reduces without converting to Nat
                    Boolean b -> boolToFun b
                    Nat n -> natToFun n
                    z -> z

boolToFun :: Bool -> Expr -- Converts Bool to Church encoding
boolToFun True = Lam "a" (Lam "b" (Var "a"))
boolToFun False = Lam "a" (Lam "b" (Var "b"))

natToFun :: Int -> Expr -- Converts a number to its Church encoding
natToFun 0 = Lam "f" (Lam "x" (Var "x"))
natToFun x = succ (natToFun (x - 1))
    where succ a = reduce (Map.fromList [("aaaaaah", Nat 42)]) (Lam "f" (Lam "x" (App (Var "f") (App (App a (Var "f")) (Var "x")))))

funToNat :: Expr -> Expr
funToNat (Lam a (Lam b (App f x)))
    | f == (Var a) && x == (Var b) = Nat 1
    | f == (Var a) = let Nat n = funToNat (Lam a (Lam b (x))) in if n == -1 then Nat (-1) else Nat (n + 1)
    | otherwise = Nat (-1) -- -1 means could not convert
funToNat (Lam a (Lam b (x)))
    | x == (Var b) = Nat 0
    | otherwise = Nat (-1)

beta :: Env -> Expr -> Expr -> Expr -- Perform beta reduction
beta env (Lam free body) x = let newEnv = Map.insert free' (refact free' ((take numContains (repeat '_')) ++ free') (reduce env x)) env
                                 Lam free' body' = reduceL env (Lam free body)
                                 numContains = x `contains` free'
                             in refact ((take numContains (repeat '_')) ++ free') free' (reduce newEnv body')
beta env (Var a) x = case Map.lookup a env of
                        Nothing -> App (Var a) (reduce env x)
                        Just (Var n) -> if n == a then App (Var n) (reduce env x) else beta env (Var n) x
                        Just l -> beta env l x
beta env (App (Var a) b) c = case Map.lookup a env of
                                Nothing -> App (App (Var a) b) c
                                Just (Var n) -> if n == a then App (App (Var n) (reduce env b)) (reduce env c) else beta env (beta env (Var n) b) c
                                Just l -> beta env (App l b) c
beta env (App a b) c = beta env (beta env a b) (reduce env c)
beta env (Boolean b) x = App (boolToFun b) (reduce env x)
beta env (Nat n) x = App (natToFun n) (reduce env x)