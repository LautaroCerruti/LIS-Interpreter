module Eval1
  ( eval
  , State
  )
where

import           AST
import qualified Data.Map.Strict               as M
import           Data.Strict.Tuple

-- Estados
type State = M.Map Variable Int

-- Estado nulo
initState :: State
initState = M.empty

-- Busca el valor de una variable en un estado
lookfor :: Variable -> State -> Int
lookfor v s = s M.! v

-- Cambia el valor de una variable en un estado
update :: Variable -> Int -> State -> State
update = M.insert

-- Evalua un programa en el estado nulo
eval :: Comm -> State
eval p = stepCommStar p initState

-- Evalua multiples pasos de un comnado en un estado,
-- hasta alcanzar un Skip
stepCommStar :: Comm -> State -> State
stepCommStar Skip s = s
stepCommStar c    s = Data.Strict.Tuple.uncurry stepCommStar $ stepComm c s

-- Evalua un paso de un comando en un estado dado
stepComm :: Comm -> State -> Pair Comm State
stepComm (Let v intexp) s = let n = evalExp intexp s
                            in Skip :!: update v n s
stepComm (Seq Skip c2) s = c2 :!: s 
stepComm (Seq c1 c2) s = let (e1 :!: s') = stepComm c1 s
                         in ((Seq e1 c2) :!: s')
stepComm (IfThenElse boolexp c1 c2) s = if evalExp boolexp s then (c1 :!: s) else (c2 :!: s)
stepComm w@(While boolexp c1) s = if evalExp boolexp s then ((Seq c1 w) :!: s) else (Skip :!: s)

-- Evalua una expresion
evalExp :: Exp a -> State -> a 
evalExp (Const n) s = n 
evalExp (Var v) s = lookfor v s
evalExp (UMinus e) s = -(evalExp e s)
evalExp (Plus e1 e2) s = (evalExp e1 s) + (evalExp e2 s)
evalExp (Minus e1 e2) s = (evalExp e1 s) - (evalExp e2 s)
evalExp (Times e1 e2) s = (evalExp e1 s) * (evalExp e2 s)
evalExp (Div e1 e2) s = div (evalExp e1 s) (evalExp e2 s)
evalExp (ECond b e1 e2) s = if evalExp b s then evalExp e1 s else evalExp e2 s
evalExp (BTrue) s = True
evalExp (BFalse) s = False
evalExp (Lt e1 e2) s = (evalExp e1 s) < (evalExp e2 s)
evalExp (Gt e1 e2) s = (evalExp e1 s) > (evalExp e2 s)
evalExp (Eq e1 e2) s = (evalExp e1 s) == (evalExp e2 s)
evalExp (NEq e1 e2) s = (evalExp e1 s) /= (evalExp e2 s)
evalExp (And e1 e2) s = (evalExp e1 s) && (evalExp e2 s)
evalExp (Or e1 e2) s = (evalExp e1 s) || (evalExp e2 s)
evalExp (Not e1) s = not (evalExp e1 s)
