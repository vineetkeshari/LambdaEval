-- 02/24/2014
-- Big-step and small-step evaluation for the lambda calculus in Haskell
-- Author: Vineet Keshari (EID: vk3226)

module Lambda where
import Base

data Exp = Var String
         | Abs String Exp
         | App Exp Exp
--         deriving Show

-- If this expression cannot be reduced further, it's a value. Values can be:
-- -- Variables OR
-- -- Lambda abstractions whose body cannot be further evaluated OR
-- -- Applications where the first term is not a lambda abstraction.
isval :: Exp -> Bool
isval (Var _) = True
isval (Abs _ e) = isval e
isval (App (Abs _ _) _) = False
isval (App e1 e2) = isval e1 && isval e2

-- Is this expression a fully evaluated lambda abstraction? Useful for deciding whether or not to call substitute
islam :: Exp -> Bool
islam (Abs s e) = isval (Abs s e)
islam exp = False

-- Substitute a string for an expression in an expression
-- -- seen : A list of variable names that exist in 'e'
-- Capture is avoided by renaming the bound variable of Abs s' e' if it occurs in seen
subst :: [String] -> String -> Exp -> Exp -> Exp
subst seen s e (App e1 e2) = App (subst seen s e e1) (subst seen s e e2)
subst seen s e (Abs s' e') = if s == s' then Abs s' e'
                                        else if (s' `elem` seen) then (let s1' = makeNewVariable 1 s' seen in Abs s1' (subst seen s e (subst seen s' (Var s1') e')))
                                                                else Abs s' (subst seen s e e')
subst _ s e (Var s') = if s == s' then e
                                    else (Var s')

-- Create a new variable that hasn't been bound yet
makeNewVariable :: Int -> String -> [String] -> String
makeNewVariable n s seen = if (s ++ (show n)) `elem` seen then makeNewVariable (n+1) s seen
                                            else (s ++ (show n))

-- Substitute an expression in a lambda abstraction using whatever variable it binds (protects call to subst)
substInAbs :: Exp -> Exp -> Exp
substInAbs (Abs s e) exp2 = subst (findAllVars exp2) s exp2 e
substInAbs exp1 _ = error ("Tried to substitute into non-abstraction: " ++ show exp1)

-- Finds all variables mentioned in an expression and its subexpressions
findAllVars :: Exp -> [String]
findAllVars (Var s) = [s]
findAllVars (Abs s e) = findAllVars e
findAllVars (App e1 e2) = (findAllVars e1) ++ (findAllVars e2)

-- BIG STEP
evalb :: Exp -> Exp
-- This is the only substitution rule in big step.
-- First evaluate both expressions completely.
-- Then, if both are values (variables or fully evaluated lambda abstractions), perform a substitution.
-- Then, evaluate the resulting expression.
evalb (App exp1 exp2) = if isval exp1Val && isval exp2Val then if islam exp1Val then evalb (substInAbs exp1Val exp2Val)
                                                                                else (App exp1Val exp2Val)
                                                            else error ("Cannot evaluate using big step: " ++ show (App exp1 exp2))
                        where exp1Val = evalb exp1
                              exp2Val = evalb exp2
-- We may need to evaluate a lambda expression if it's body isn't fully evaluated yet e.g. just after substitution (same as small-step rule)
evalb (Abs s e) = if isval e then (Abs s e)
                                else Abs s (evalb e)
--Anything else is either already a value, or trying to evaluate it is an error.
evalb exp = if isval exp then exp
                            else error ("Cannot evaluate using big step: " ++ show exp)

-- SMALL STEP
evals :: Exp -> Exp
-- This covers all three substitution rules of small step.
-- If exp1 is not evaluated fully, take a step to evaluate it.
-- Otherwise, if exp2 is not evaluated fully, take a step to evaluate it.
-- Otherwise, do a substitution if exp1 is a lambda abstraction, and do nothing otherwise.
evals (App exp1 exp2) = if isval exp1 then if isval exp2 then if islam exp1 then substInAbs exp1 exp2
                                                                            else (App exp1 exp2)
                                                            else App exp1 (evals exp2)
                                        else App (evals exp1) exp2
-- We may need to evaluate a lambda expression if it's body isn't fully evaluated yet e.g. just after substitution (same as big-step rule)
evals (Abs s e) = if isval e then (Abs s e)
                                else Abs s (evals e)
-- Anything else should already be a value.
evals exp = if isval exp then exp
                            else error ("Cannot take small step from " ++ show exp)

-- REPEATED SMALL STEP
evals' :: Exp -> Exp
-- Call evals repeatedly until a value is found.
evals' exp = if isval exp then exp
                            else evals' (evals exp)


instance Show Exp where
    show exp = showExp False exp

showExp parens (Var x) = x
showExp parens (Abs x t) = 
   if parens then "(" ++ result ++ ")" else result
      where result = "\\" ++ x ++ "." ++ showExp False t
showExp parens (App (Abs s t) t2) = 
   if parens then "(" ++ result ++ ")" else result
      where result = showExp True (Abs s t) ++ " " ++ showExp True t2
showExp parens (App t1 t2) = 
   if parens then "(" ++ result ++ ")" else result
      where result = showExp False t1 ++ " " ++ showExp True t2

