data Lambda = Slovo Char
   | Aplikacija Lambda Lambda
   | Apstrakcija Char Lambda
   
instance Show Lambda where
   show (Slovo c) = [c]
   show (Aplikacija funkcija argument) = show funkcija ++ " " ++ show argument
   show (Apstrakcija slovo povratna_vrijednost) =
      "(lambda " ++ [slovo] ++ " . " ++ show povratna_vrijednost ++ ")"

supst :: Lambda -> Char -> Lambda -> Lambda
supst (Slovo c) sto cime | c == sto = cime
                         | otherwise = (Slovo c)
supst (Aplikacija f arg) sto cime = Aplikacija (supst f sto cime) (supst arg sto cime)
supst (Apstrakcija x f) sto cime | x == sto = Apstrakcija x f
                                 | otherwise = Apstrakcija x (supst f sto cime)
 -- problem: (\ x -> x + y) [y |-> x] ne radi kako treba
 -- rjesenje: umjesto Char treba neki "mesnatiji" tip, u kojem treba smisliti novo "slovo"
 -- i vratiti (\ x' -> x' + x)
 
beta (Aplikacija (Apstrakcija x f) y) = supst f x y
 
x = Slovo 'x'
y = Slovo 'y'
z = Slovo 'z'
a = Slovo 'a'
b = Slovo 'b'
f = Slovo 'f'