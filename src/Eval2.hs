module Eval2
  ( eval
  , State
  )
where

import           AST
import qualified Data.Map.Strict               as M
import           Data.Strict.Tuple
import           Control.Monad

-- Estados
type State = M.Map Variable Int

-- Estado nulo
initState :: State
initState = M.empty

-- Busca el valor de una variable en un estado
lookfor :: Variable -> State -> Either Error Int
lookfor v s = case M.lookup v s of
                Nothing -> Left UndefVar
                Just n -> Right n

-- Cambia el valor de una variable en un estado
update :: Variable -> Int -> State -> State
update = M.insert

-- Evalua un programa en el estado nulo
eval :: Comm -> Either Error State
eval p = stepCommStar p initState

-- Evalua multiples pasos de un comnado en un estado,
-- hasta alcanzar un Skip
stepCommStar :: Comm -> State -> Either Error State
stepCommStar Skip s = return s
stepCommStar c    s = do
  (c' :!: s') <- stepComm c s
  stepCommStar c' s'

-- Evalua un paso de un comando en un estado dado
stepComm :: Comm -> State -> Either Error (Pair Comm State)
stepComm (Let v intexp) s = do n <- evalExp intexp s
                               return (Skip :!: update v n s)
stepComm (Seq Skip c2) s = Right (c2 :!: s)
stepComm (Seq c1 c2) s = do (e1 :!: s') <- stepComm c1 s
                            return ((Seq e1 c2) :!: s')
stepComm (IfThenElse boolexp c1 c2) s = do b <- evalExp boolexp s 
                                           if b then return (c1 :!: s) else return (c2 :!: s)
stepComm w@(While boolexp c1) s = do b <- evalExp boolexp s  
                                     if b then return ((Seq c1 w) :!: s) else return (Skip :!: s)

-- Evalua una expresion
evalExp :: Exp a -> State -> Either Error a 
evalExp (Const n) s = Right n
evalExp (Var v) s = lookfor v s
evalExp (UMinus e) s = liftM (negate) (evalExp e s)
evalExp (Plus e1 e2) s = liftM2 (+) (evalExp e1 s) (evalExp e2 s)
evalExp (Minus e1 e2) s = liftM2 (-) (evalExp e1 s) (evalExp e2 s)
evalExp (Times e1 e2) s = liftM2 (*) (evalExp e1 s) (evalExp e2 s)
evalExp (Div e1 e2) s = do n1 <- evalExp e1 s 
                           n2 <- evalExp e2 s
                           if n2 == 0 then Left DivByZero else return (div n1 n2)
evalExp (ECond b e1 e2) s = do b <- evalExp b s
                               case b of
                                False -> evalExp e2 s
                                _ -> evalExp e1 s
evalExp (BTrue) s = Right True
evalExp (BFalse) s = Right False
evalExp (Lt e1 e2) s = liftM2 (<) (evalExp e1 s) (evalExp e2 s)
evalExp (Gt e1 e2) s = liftM2 (>) (evalExp e1 s) (evalExp e2 s)
evalExp (Eq e1 e2) s = liftM2 (==) (evalExp e1 s) (evalExp e2 s)
evalExp (NEq e1 e2) s = liftM2 (/=) (evalExp e1 s) (evalExp e2 s)
evalExp (And e1 e2) s = liftM2 (&&) (evalExp e1 s) (evalExp e2 s)
evalExp (Or e1 e2) s = liftM2 (||) (evalExp e1 s) (evalExp e2 s)
evalExp (Not e1) s = liftM (not) (evalExp e1 s)
