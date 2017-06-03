dobleimpl :: (Ord a) => a -> a -> Bool
alpha `dobleimpl` beta
      |(alpha == beta) = True
      | otherwise = False 

type PropAtom = String
data Proposicion = Atom PropAtom
		            | Proposicion
                | Not Proposicion
                | Proposicion `And` Proposicion --And--
                | Proposicion `Or`  Proposicion --Or--
                | Proposicion `Imp` Proposicion --Implicacion--
              	| Proposicion `Iff` Proposicion --Si y solo si--
		            --deriving Show

p = Atom "p"
q = Atom "q"
r = Atom "r"
prop1,prop2,prop3 :: Proposicion
prop1 = (p `And` q) `Imp` r
prop2 = (p `And` q) `Iff` ( Not $ (Not p) `Or` (Not q) )
prop3 = (Not p) `And` p

--Funcion evaluadora
type Valuacion = [(PropAtom, Bool)] 
eval :: Valuacion -> Proposicion -> Bool
eval i (Atom  x)    	= busca x i 
eval i (Not p)      	= not (eval i p) 
eval i (p `And` q ) 	= eval i p && eval i q
eval i (p `Or` q ) 	= eval i p || eval i q
eval i (p `Imp` q )   = eval i p <= eval i q
eval i (p `Iff` q) = eval i p `dobleimpl` eval i q

--Funcion que obtiene los valores  booleanos
--Idea obtenida de:https://www.haskell.org/hoogle en el apartado de listar y filtros

busca :: Eq c => c -> [(c,v)] -> v
busca c t = head [v | (c',v) <- t, c == c']

--Funcion encargada de obtener las propocisiones que tenemos
variables :: Proposicion -> [PropAtom]
variables (Atom x) 		  = [x]
variables (Not p )		  = variables p
variables (p `And` q ) 	= variables p ++ variables q
variables (p `Imp` q ) 	= variables p ++ variables q
variables (p `Or`  q )  = variables p ++ variables q
variables (p `Iff` q )  = variables p ++ variables q 

--Funcion encargada de generar los valores booleanos de 2^n 
--n:= no_argumentos
--Idea obtnida de :https://www.haskell.org/hoogle/?hoogle=map
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

tautologia :: Proposicion -> Bool
tautologia p = and [eval i p | i <- interpretaciones p]

contradiccion :: Proposicion -> Bool
contradiccion  p = and [not (eval i p) | i <- interpretaciones p]

contigencia :: Proposicion -> Bool
contigencia p = (tautologia p == False) && (contradiccion p == False)
