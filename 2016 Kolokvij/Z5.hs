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
   | x `sePojavljujeU` cime = let
            nova_apstrakcija = Apstrakcija (novi x f) (supst f (Varijabla x) (Varijabla (novi x f)))
                in supst nova_apstrakcija sto cime
   | otherwise = Apstrakcija x (supst f sto cime)
novi x f | x `sePojavljujeU` f = novi (Crtica x) f
             | otherwise = x
sePojavljujeU :: Slovo -> Lambda -> Bool
x `sePojavljujeU` Varijabla v = x == v
x `sePojavljujeU` Aplikacija f arg = x `sePojavljujeU` f || x `sePojavljujeU` arg
x `sePojavljujeU` Apstrakcija y f = x == y || x `sePojavljujeU` f
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

test1 = problem
test2 = beta (lam y (lam x (f # x # x')) # x)
