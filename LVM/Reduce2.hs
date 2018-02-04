module LVM.Reduce2 where -- Redo of LVM.Reduce
import LVM.Parse
import Data.Map as M

type Env = M.Map String Expr -- Stores variables

reduce :: Env -> Expr -> Expr
reduce env (IR expr) = IR expr
reduce env (Var name) = case M.lookup name env of
    Nothing -> Var name
    Just x  -> x
reduce env (Lam f (Lam x z)) = case z of
    Var a -> if a == x then Nat 0 else Lam f (Lam x z)
    App (Var a) b -> if a == f then case b of
            Var n -> if n == x then Nat 1 else Lam f (Lam x z)
            App a' b' -> let n = reduce (M.delete f (M.delete x env)) (Lam f (Lam x (App a' b'))) in case n of
                Nat i -> Nat (i + 1)
                i -> Lam f (Lam x (App (Var a) n))
            q -> Lam f (Lam x z)
        else Lam f (Lam x z)
    q -> Lam f (Lam x z)
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
    Lam v b   -> reduce newEnv b
        where newEnv = M.insert v x env

natToFun :: Int -> Expr
natToFun x = Lam "f" $ Lam "x" $ iterate (App $ Var "f") (Var "x") !! x

boolToFun :: Bool -> Expr
boolToFun True  = Lam "a" $ Lam "b" $ Var "a"
boolToFun False = Lam "a" $ Lam "b" $ Var "b"