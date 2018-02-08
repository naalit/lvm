module LVM.Reduce2 where -- Redo of LVM.Reduce
import LVM.Parse
import Data.Map as M

type Env = M.Map String Expr -- Stores variables

testFunToNat :: Env -> String -> String -> Expr -> Expr
testFunToNat env f x (Var z) -- Tries to convert an expression to a Nat,
    | z == x = Nat 0         -- reduces on failure.
    | otherwise = aux env f x $ Var z
testFunToNat env f x (App (Var e) z)
    | e == f = case testFunToNat env f x z of
        Nat n -> Nat (n + 1)
        a -> aux env f x (App (Var e) z)
    | otherwise = aux env f x (App (Var e) z)
testFunToNat env f x z = aux env f x z

aux :: Env -> String -> String -> Expr -> Expr
aux env f x z
    | new == z  = Lam f $ Lam x z
    | otherwise = testFunToNat env f x new
    where new   = reduce (M.delete f (M.delete x env)) z
    
reduce :: Env -> Expr -> Expr
reduce env (IR expr) = IR expr
reduce env (Var name) = case M.lookup name env of
    Nothing -> Var name
    Just x  -> x
reduce env (Lam f (Lam x z)) = testFunToNat env f x z
reduce env (Lam free body) = case M.lookup free env of
    Nothing -> Lam free (reduce env body)
    Just x  -> Lam free (reduce (M.delete free env) body)
reduce env (Nat n) = Nat n
reduce env (Boolean b) = Boolean b
reduce env (App f x) = let rx = reduce env x in case f of
    IR e      -> IR $ App e rx
    Var name  -> case M.lookup name env of
        Nothing  -> IR $ App (Var name) rx
        Just val -> reduce env (App val rx)
    Nat n     -> reduce env $ App (natToFun n) rx
    Boolean b -> reduce env $ App (boolToFun b) rx
    App f2 x2 -> reduce env $ App (reduce env $ App f2 x2) rx
    Lam v b   -> case M.lookup v env of
        Just old -> reduce (M.insert newName x env) (refact v newName b)
        Nothing -> if x `contains` (Var v) then reduce (M.insert newName x env) (refact v newName b') else reduce newEnv b'
      where newEnv = M.insert v x env
            b' = unir b
            newName = if x `contains` (Var $ rename v b env) then rename (rename v b env) x env else rename v b env

unir (IR x) = x -- Removes IR
unir (App a b) = App (unir a) (unir b)
unir (Lam a b) = Lam a (unir b)
unir x = x

rename :: String -> Expr -> Env -> String -- Rename a free variable
rename v b env = let new = '_':v in case M.lookup new env of
    Nothing -> if b `contains` (Var new) then rename new b env else new
    Just _ -> rename new b env

refact :: String -> String -> Expr -> Expr
refact old new (Var x) = if x == old then Var new else Var x
refact old new (App f x) = App (refact old new f) (refact old new x)
refact old new (Lam v b) = if v == old || v == new then Lam v b else Lam v (refact old new b)
refact old new x = x

contains :: Expr -> Expr -> Bool -- a `contains` b
contains (App a b) z = z == (App a b) || contains a z || contains b z
contains (Lam a b) z = z == (Lam a b) || contains b z
contains (Nat _) z = contains (natToFun 1) z
contains x z = z == x

natToFun :: Int -> Expr
natToFun x = Lam "f" $ Lam "x" $ iterate (App $ Var "f") (Var "x") !! x

boolToFun :: Bool -> Expr
boolToFun True  = Lam "a" $ Lam "b" $ Var "a"
boolToFun False = Lam "a" $ Lam "b" $ Var "b"