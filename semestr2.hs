data Term = Variable String 
                | Abstract String Term
                | Apply Term Term
                deriving Show

--Interp. 3 type of term for b-reduction

vaeEx, abstrEx, applyEx :: Term
vaeEx = Variable "var"
abstrEx = Abstract "var" vaeEx
applyEx = Apply abstrEx vaeEx

fn_for_Term :: Term -> a
fn_for_Term term = case term of 
					Variable str -> undefined str
					Abstract str1 term3 -> undefined str1 (fn_for_Term term3)
					Apply term1 term2 -> undefined (fn_for_Term term1) (fn_for_Term term2)


type Context = [(String, Term)]
--Interp. Dictionary entry for b-reduction

contextEx :: Context
contextEx = ("var", vaeEx):[]

fn_for_Context :: Context -> a
fn_for_Context cont = undefined (head cont) fn_for_Context (tail cont)



eval :: Term -> Term
eval t = eval'[] t

eval' :: Context -> Term -> Term
eval' context term = case term of
				l@(Variable a) 	-> maybe l id (lookup a context)
				(Abstract s t) 	-> Abstract s (eval' context t)
				(Apply t1 t2) 	-> apply context (eval' context t1) (eval' context t2)

apply :: Context -> Term -> Term -> Term
apply context t1 t2 = case t1 of
					(Abstract s t) 	-> 	eval' ((s, t2):context) t
					otherwise 		-> 	Apply t1 t2


ex2 =Apply (Abstract "x" (Variable "x")) (Apply (Abstract "x" (Variable "x")) (Apply (Abstract "z" (Abstract "x" (Variable "x"))) (Variable "z")))