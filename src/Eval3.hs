module Eval3
  ( eval
  , State
  )
where

import           AST
import qualified Data.Map.Strict               as M
import           Data.Strict.Tuple hiding (fst)

-- Estados
type State = (M.Map Variable Int, Integer)

-- Estado nulo
initState :: State
initState = (M.empty, 0)

-- Busca el valor de una variable en un estado
lookfor :: Variable -> State -> Either Error Int
lookfor v s = case M.lookup v (fst s) of
                Nothing -> Left UndefVar
                Just n -> Right n

-- Cambia el valor de una variable en un estado
update :: Variable -> Int -> State -> State
update x v (map, n) = (M.insert x v map, n)

-- Suma un costo dado al estado
addWork :: Integer -> State -> State
addWork n (map, p) = (map, p + n)

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
stepComm (Let v intexp) s = do (n :!: s') <- evalExp intexp s
                               return (Skip :!: update v n s')
stepComm (Seq Skip c2) s = Right (c2 :!: s)
stepComm (Seq c1 c2) s = do (e1 :!: s') <- stepComm c1 s
                            return ((Seq e1 c2) :!: s')
stepComm (IfThenElse boolexp c1 c2) s = do (b :!: s') <- evalExp boolexp s 
                                           if b then return (c1 :!: s') else return (c2 :!: s')
stepComm w@(While boolexp c1) s = do (b :!: s') <- evalExp boolexp s  
                                     if b then return ((Seq c1 w) :!: s') else return (Skip :!: s')

-- Evalua una expresion
evalExp :: Exp a -> State -> Either Error (Pair a State)
evalExp (Const n) s = Right (n :!: s)
evalExp (Var v) s = do n <- lookfor v s
                       return (n :!: s)
evalExp (UMinus e) s = do (n :!: s') <- evalExp e s
                          return ((negate n) :!: (addWork 1 s'))
evalExp (Plus e1 e2) s = do (n1 :!: s1) <- evalExp e1 s
                            (n2 :!: s2) <- evalExp e2 s1
                            return ((n1 + n2) :!: (addWork 2 s2))
evalExp (Minus e1 e2) s = do (n1 :!: s1) <- evalExp e1 s
                             (n2 :!: s2) <- evalExp e2 s1
                             return ((n1 - n2) :!: (addWork 2 s2))
evalExp (Times e1 e2) s = do (n1 :!: s1) <- evalExp e1 s
                             (n2 :!: s2) <- evalExp e2 s1
                             return ((n1 * n2) :!: (addWork 3 s2))
evalExp (Div e1 e2) s = do (n1 :!: s1) <- evalExp e1 s
                           (n2 :!: s2) <- evalExp e2 s1
                           if n2 == 0 then Left DivByZero else return ((div n1 n2) :!: (addWork 3 s2))
evalExp (ECond b e1 e2) s = do (b :!: s') <- evalExp b s
                               case b of
                                False -> evalExp e2 s'
                                _ -> evalExp e1 s'
evalExp (BTrue) s = Right (True :!: s)
evalExp (BFalse) s = Right (False :!: s)
evalExp (Lt e1 e2) s = do (n1 :!: s1) <- evalExp e1 s
                          (n2 :!: s2) <- evalExp e2 s1
                          return ((n1 < n2) :!: (addWork 2 s2))
evalExp (Gt e1 e2) s = do (n1 :!: s1) <- evalExp e1 s
                          (n2 :!: s2) <- evalExp e2 s1
                          return ((n1 > n2) :!: (addWork 2 s2))
evalExp (Eq e1 e2) s = do (n1 :!: s1) <- evalExp e1 s
                          (n2 :!: s2) <- evalExp e2 s1
                          return ((n1 == n2) :!: (addWork 2 s2))
evalExp (NEq e1 e2) s = do (n1 :!: s1) <- evalExp e1 s
                           (n2 :!: s2) <- evalExp e2 s1
                           return ((n1 /= n2) :!: (addWork 2 s2))
evalExp (And e1 e2) s = do (b1 :!: s1) <- evalExp e1 s
                           (b2 :!: s2) <- evalExp e2 s1
                           return ((b1 && b2) :!: (addWork 2 s2))
evalExp (Or e1 e2) s = do (b1 :!: s1) <- evalExp e1 s
                          (b2 :!: s2) <- evalExp e2 s1
                          return ((b1 || b2) :!: (addWork 2 s2))
evalExp (Not e1) s = do (b :!: s') <- evalExp e1 s
                        return ((not b) :!: (addWork 1 s'))