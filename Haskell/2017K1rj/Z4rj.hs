data HFormula = P Integer | Cond HFormula HFormula
    deriving (Eq)

f --> g = Cond f g
p1 = P 1
p2 = P 2

instance Show HFormula where
    show (P n) = 'P' : show n
    show (Cond f1 f2) = "(" ++ show f1 ++ "->" ++ show f2 ++ ")"

data HDokaz = A1 HFormula HFormula
            | A2 HFormula HFormula HFormula
            | ModPon HDokaz HDokaz

teorem (A1 a b) = a --> (b --> a)
teorem (A2 a b c) = (a --> (b --> c)) --> ((a --> b) --> (a --> c))
teorem (ModPon d1 d2) | a == teorem d2 = b where Cond a b = teorem d1

dokaz1 = A2 p1 p2 p1
dokaz2 = A1 p1 p2
test1 = teorem dokaz1
test2 = teorem dokaz2
test3 = teorem (ModPon dokaz1 dokaz2)

idokaz a = ModPon (ModPon (A2 a i a) (A1 a i)) (A1 a a) where i = a --> a

test formula = teorem (idokaz formula) == (formula --> formula)

test4 = test p1
test5 = test (p1 --> p2)

