type PropAtom = String
data Proposicion = Atom PropAtom
		| Proposicion
                | Not Proposicion
                | Proposicion `And` Proposicion --And--
                | Proposicion `Or`  Proposicion --Or--
                | Proposicion `Imp` Proposicion --Implicacion--
              	| Proposicion `Iff` Proposicion --Si y solo si--
		deriving Show
p = Atom "p"
q = Atom "q"
r = Atom "r"
prop1 :: Proposicion
prop1 = (p `And` q) `Imp` r
prop3 = (p `Imp` (p `And` q))

e = [("p", True), ("q", True), ("r", False)]


--Funcion evaluadora
type Valuacion = [(PropAtom, Bool)] 
eval :: Valuacion -> Proposicion -> Bool
eval i (Atom  x)    	= busca x i 
eval i (Not p)      	= not (eval i p) 
eval i (p `And` q ) 	= eval i p && eval i q
eval i (p `Imp` q ) 	= eval i p <= eval i q
eval i (p `Or` q ) 	= eval i p || eval i q

busca :: Eq c => c -> [(c,v)] -> v
busca c t = head [v | (c',v) <- t, c == c']


--Funcion encargada de obtener las propocisiones que tenemos
variables :: Proposicion -> [PropAtom]
variables (Atom x) 		= [x]
variables (Not p )		= variables p
variables (p `And` q ) 	= variables p ++ variables q
variables (p `Imp` q ) 	= variables p ++ variables q


--Funcion encargada de generar los valores booleanos de 2^n 
--n:= no_argumentos
interpretacionesVar :: Int -> [[Bool]]
interpretacionesVar 0  = [[]]
interpretacionesVar n  = 
	map (False:) bss ++ map (True:) bss
	where bss = interpretacionesVar (n -1)


--Funcion que combina interpretaciones y a varaibles 
interpretaciones :: Proposicion -> [Valuacion]
interpretaciones p =
	map (zip vs) (interpretacionesVar (length vs))
	where vs = (variables p)

esTautologia :: Proposicion -> Bool
esTautologia p =
	and [eval i p | i <- interpretaciones p]



