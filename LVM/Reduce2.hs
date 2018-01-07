module LVM.Reduce2 where -- Redo of LVM.Reduce
import LVM.Parse
import Data.Map as M

type Env = M.Map String Expr -- Stores variables

reduce :: Env -> Expr -> Expr
reduce env (IR expr) = IR expr
reduce env (Var name) = case M.lookup name env of
    Nothing -> Var name
    Just x  -> x
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