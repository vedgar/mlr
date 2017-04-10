data Slovo = Samo Char | Crtica Slovo
   deriving (Eq)

instance Show Slovo where
   show (Samo c) = [c]
   show (Crtica s) = (show s) ++ "'"

data Lambda = Varijabla Slovo
   | Aplikacija Lambda Lambda
   | Apstrakcija Slovo Lambda
   
instance Show Lambda where
   show (Varijabla s) = show s
   show (Aplikacija funkcija argument) = show funkcija ++ " " ++ show argument
   show (Apstrakcija slovo povratna_vrijednost) =
      "(lambda " ++ show slovo ++ " . " ++ show povratna_vrijednost ++ ")"

lam (Varijabla v) izraz = Apstrakcija v izraz
f # arg = Aplikacija f arg

supst :: Lambda -> Lambda -> Lambda -> Lambda
supst (Varijabla c) (Varijabla v) cime | c == v = cime
                                       | otherwise = (Varijabla c)
supst (Aplikacija f arg) sto@(Varijabla v) cime =
   Aplikacija (supst f sto cime) (supst arg sto cime)
supst (Apstrakcija x f) sto@(Varijabla v) cime
   | x == v = Apstrakcija x f
   | otherwise = Apstrakcija x (supst f sto cime)
 -- (\ x -> x + y) [y |-> x] ne radi kako treba
problem = beta (lam y (lam x (f # x # y)) # x)
 -- trebalo bi dati (\ x' -> x' + x)
 
beta (Aplikacija (Apstrakcija x f) y) = supst f (Varijabla x) y
 
x = Varijabla (Samo 'x')
x' = Varijabla (Crtica (Samo 'x'))
x'' = Varijabla (Crtica (Crtica (Samo 'x')))
y = Varijabla (Samo 'y')
y' = Varijabla (Crtica (Samo 'y'))
z = Varijabla (Samo 'z')
a = Varijabla (Samo 'a')
b = Varijabla (Samo 'b')
f = Varijabla (Samo 'f')
