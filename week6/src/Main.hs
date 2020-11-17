module Main where
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State
import qualified Text.PrettyPrint as PP

data Exp = Var String
         | Const Val
         | App Exp Exp
         | Abs String Exp
         | Let String Exp Exp
     deriving (Eq,Ord)

data Val = I Integer | B Bool
     deriving (Eq,Ord)

data Type = TVar String
          | TInt | TBool
          | TFun Type Type
     deriving (Eq,Ord)

data Scheme = Scheme [String] Type

type Subst = Map.Map String Type

nullSubst :: Subst
nullSubst = Map.empty

class Types a where
  ftv   :: a -> Set.Set String
  apply :: Subst -> a -> a

composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2 = (Map.map (apply s1) s2) `Map.union` s1

instance Types Type where
    ftv (TVar n)      =  Set.singleton n
    ftv TInt          =  Set.empty
    ftv TBool         =  Set.empty
    ftv (TFun t1 t2)  =  ftv t1 `Set.union` ftv t2

    apply s (TVar n)      =  case Map.lookup n s of
                               Nothing  -> TVar n
                               Just t   -> t
    apply s (TFun t1 t2)  = TFun (apply s t1) (apply s t2)
    apply _ t             =  t

instance Types Scheme where
    ftv (Scheme vars t)
        =  (ftv t) `Set.difference` (Set.fromList vars)

    apply s (Scheme vars t)
        =  Scheme vars (apply (foldr Map.delete s vars) t)

instance Types a => Types [a] where
    apply s  =  map (apply s)
    ftv l    =  foldr Set.union Set.empty (map ftv l)

newtype TypeEnv = TypeEnv (Map.Map String Scheme)

remove                    ::  TypeEnv -> String -> TypeEnv
remove (TypeEnv env) var  =  TypeEnv (Map.delete var env)

instance Types TypeEnv where
    ftv (TypeEnv env)      =  ftv (Map.elems env)
    apply s (TypeEnv env)  =  TypeEnv (Map.map (apply s) env)

generalize        ::  TypeEnv -> Type -> Scheme
generalize env t  =   Scheme vars t
  where vars = Set.toList ((ftv t) `Set.difference` (ftv env))

type TI a = State TIState a

data TIState = TIState {  tiSupply :: Int }

runTI :: TI a -> (a, TIState)
runTI t = runState t initTIState
  where initTIState = TIState{ tiSupply = 0 }

newTyVar :: String -> TI Type
newTyVar prefix =
    do  s <- get
        put s{tiSupply = tiSupply s + 1}
        return (TVar (prefix ++ show (tiSupply s)))

instantiate :: Scheme -> TI Type
instantiate (Scheme vars t) =
     do  nvars <- mapM (\ _ -> newTyVar "a") vars
         let s = Map.fromList (zip vars nvars)
         return $ apply s t

mgu :: Type -> Type -> TI (Either String Subst)
mgu (TFun l r) (TFun l' r')
   =  do  e1 <- mgu l l'
          case e1 of
            Right s1 -> do
                e2 <- mgu (apply s1 r) (apply s1 r')
                return $ case e2 of
                    Right s2 -> Right (s1 `composeSubst` s2)

mgu (TVar u) t               =  varBind u t
mgu t (TVar u)               =  varBind u t

mgu TInt TInt                =  return $ Right nullSubst
mgu TBool TBool              =  return $ Right nullSubst
mgu t1 t2                    =  return $ Left $ "types do not unify: " ++ show t1 ++
                                " vs. " ++ show t2

varBind :: String -> Type -> TI (Either String Subst)
varBind u t
   | t == TVar u           =  return $ Right nullSubst
   | u `Set.member` ftv t  =  return $ Left $
                                "occur check fails: " ++ u ++
                                " vs. " ++ show t
   | otherwise             =  return $ Right (Map.singleton u t)

tiConst :: TypeEnv -> Val -> TI (Subst, Type)
tiConst _ (I _)   = return (nullSubst, TInt)
tiConst _ (B _)  =  return (nullSubst, TBool)

ti        ::  TypeEnv -> Exp -> TI (Subst, Type)

ti (TypeEnv env) (Var n) =
    case Map.lookup n env of
       Nothing     ->  error $ "unbound variable: " ++ n
       Just sigma  ->  do  t <- instantiate sigma
                           return (nullSubst, t)

ti env (Const l) = tiConst env l

ti env (Abs n e) =
 do tv <- newTyVar "a"
    let TypeEnv env' = remove env n
        env'' = TypeEnv (env' `Map.union`
                    (Map.singleton n (Scheme [] tv)))
    (s1, t1) <- ti env'' e
    return (s1, TFun (apply s1 tv) t1)

ti env (App e1 e2) =
    do  tv <- newTyVar "a"
        (s1, t1) <- ti env e1
        (s2, t2) <- ti (apply s1 env) e2

        e <- mgu (apply s2 t1) (TFun t2 tv)
        case e of
            Left msg -> error msg
            Right s3 -> return (s3 `composeSubst` s2
                   `composeSubst` s1, apply s3 tv)

ti env (Let x e1 e2) =
    do  (s1, t1) <- ti env e1
        let TypeEnv env' = remove env x
            t' = generalize (apply s1 env) t1
            env'' = TypeEnv (Map.insert x t' env')
        (s2, t2) <- ti (apply s1 env'') e2
        return (s1 `composeSubst` s2, t2)

typeInference :: Map.Map String Scheme -> Exp -> TI Type
typeInference env e =
    do  (s, t) <- ti (TypeEnv env) e
        return (apply s t)


------------
-- Prettyprinter
------------
{- This section uses the Text.PrettyPrint library to
   format the output of Type and Exp values more pleasingly
   than the default Show instances.
   In particular, lambda abstractions and let expressions
   are printed more like traditional functional languages.
-}


instance Show Type where
    showsPrec _ x = shows (prType x)

prType             ::  Type -> PP.Doc
prType (TVar n)    =   PP.text n
prType TInt        =   PP.text "Int"
prType TBool       =   PP.text "Bool"
prType (TFun t s)  =   prParenType t PP.<+> PP.text "->" PP.<+> prType s

prParenType     ::  Type -> PP.Doc
prParenType  t  =   case t of
                      TFun _ _  -> PP.parens (prType t)
                      _         -> prType t

instance Show Exp where
    showsPrec _ x = shows (prExp x)

prExp                  ::  Exp -> PP.Doc
prExp (Var name)      =   PP.text name
prExp (Const lit)     =   prVal lit
prExp (Let x b body)  =   PP.text "let" PP.<+>
                          PP.text x PP.<+> PP.text "=" PP.<+>
                          prExp b PP.<+> PP.text "in" PP.$$
                          PP.nest 2 (prExp body)
prExp (App e1 e2)     =   prExp e1 PP.<+> prParenExp e2
prExp (Abs n e)       =   PP.char '\\' PP.<+> PP.text n PP.<+>
                          PP.text "->" PP.<+>
                          prExp e

prParenExp    ::  Exp -> PP.Doc
prParenExp t  =   case t of
                    Let _ _ _  -> PP.parens (prExp t)
                    App _ _    -> PP.parens (prExp t)
                    Abs _ _    -> PP.parens (prExp t)
                    _          -> prExp t

instance Show Val where
    showsPrec _ x = shows (prVal x)

prVal            ::  Val -> PP.Doc
prVal (I i)   =  PP.integer i
prVal (B b)  =   if b then PP.text "True" else PP.text "False"

instance Show Scheme where
    showsPrec _ x = shows (prScheme x)

prScheme                  ::  Scheme -> PP.Doc
prScheme (Scheme vars t)  =   PP.text "All" PP.<+>
                              PP.hcat
                                (PP.punctuate PP.comma (map PP.text vars))
                              PP.<> PP.text "." PP.<+> prType t

------------------------------

------------
-- Test cases
------------

ex0  =  Let "id" (Abs "x" (Var "x"))
        (Var "id")

ex1  =  Let "id" (Abs "x" (Var "x"))
        (App (Var "id") (Var "id"))

ex2  =  Let "id" (Abs "x" (Let "y" (Var "x") (Var "y")))
        (App (Var "id") (Var "id"))

ex3  =  Let "id" (Abs "x" (Let "y" (Var "x") (Var "y")))
        (App (App (Var "id") (Var "id")) (Const (I 2)))

ex4  =  Let "id" (Abs "x" (App (Var "x") (Var "x")))
        (Var "id")

ex5  =  Abs "m" (Let "y" (Var "m")
                 (Let "x" (App (Var "y") (Const (B True)))
                       (Var "x")))
ex6 = Abs "f" (Abs "g" (Abs "x" (App (App (Var "f") (Var "x")) (App (Var "g") (Var "x")))))

test e = runTI $ typeInference Map.empty e

  ------------
-- Main
------------


main = let (inferredtype, _) = test ex3 in do
          putStr $ show ex6
          putStr " :: "
          print inferredtype
