type PropAtom = String
data Proposicion = Atom PropAtom
                 | Not Proposicion
                 | Proposicion `And` Proposicion
                 | Proposicion `Or`  Proposicion
                 | Proposicion `Imp` Proposicion
                 | Proposicion `Iff` Proposicion

p = Atom "p"
q = Atom "q"
r = Atom "r"
prop1 = (p `And` q) `Imp` r

type Valuacion = [(PropAtom, Bool)] 
e = [("p", True), ("q", True), ("r", False)]
eval :: Valuacion -> Proposicion -> Bool
eval _ (PropAtom b) = b
eval i (Atom  x)    = busca x i 
eval i (Neg p)      = not (valor i p) 
eval i (q `And` p ) = valor i p && valor i q
eval i (q `Imp` p ) = valor i p <= valor i q
eval i (q `Or` p ) = valor i p || valor i q


