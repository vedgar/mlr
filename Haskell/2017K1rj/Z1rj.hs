import qualified Data.Set as Set

data Atom = At Char (Maybe Integer)
    deriving (Eq, Ord)

instance Show Atom where
    show (At p Nothing) = [p]
    show (At p (Just i)) = [p] ++ show i

data Connective = Conn Integer ([Bool] -> Bool)

logconst b = Conn 0 (\ [] -> b)
unary op = Conn 1 (\ [x] -> op x)
binary op = Conn 2 (\ [x, y] -> x `op` y)

truec = logconst True
falsec = logconst False
notc = unary not
andc = binary (&&)
orc = binary (||)
condc = binary (\ x y -> not x || y)
bicondc = binary (==)

and3c = Conn 3 (\ [x, y, z] -> x && y && z)
and4c = Conn 4 (\ [x, y, z, t] -> x && y && z && t)
or3c = Conn 3 (\ [x, y, z] -> x || y || z)
or4c = Conn 4 (\ [x, y, z, t] -> x || y || z || t)
bicond3c = Conn 3 (\ [x, y, z] -> (x == y) == z)

table (Conn 0 f) = [f []]
table (Conn n f) = [b | start <- [False, True],
    b <- table (Conn (n-1) (\ rest -> f([start] ++ rest)))]

instance Eq Connective where
    c1 == c2 = table c1 == table c2

data Formula = Atomic Atom
    | Compound Connective [Formula]
    deriving (Eq)

p  = Atomic (At 'P' Nothing )
p0 = Atomic (At 'P' (Just 0))
p1 = Atomic (At 'P' (Just 1))
p2 = Atomic (At 'P' (Just 2))
p3 = Atomic (At 'P' (Just 3))
m1 = Atomic (At 'p' (Just 1))
q1 = Atomic (At 'Q' (Just 1))

f & g = Compound andc [f, g]
ne f = Compound notc [f]
true = Compound truec []
false = Compound falsec []
f ||| g = Compound orc [f, g]
f --> g = Compound condc [f, g]
f <-> g = Compound bicondc [f, g]

instance Show Connective where
    show c | c == notc = "!"
           | c == andc = "&"
           | c == orc = "|"
           | c == condc = "->"
           | c == bicondc = "<->"
           | otherwise = error ("Unnamed operator with table "
                                ++ show (table c))

instance Show Formula where
    show (Atomic p) = show p
    show (Compound c@(Conn 0 b) []) = show (b [])
    show (Compound c@(Conn 1 _) [f]) = show c ++ show f
    show (Compound c@(Conn 2 _) [f, g]) = "("
        ++ show f ++ show c ++ show g ++ ")"

mvs::Formula->Bool
mvs (Atomic (At slovo (Just broj))) = slovo == 'P' && broj > 0
mvs (Atomic (At slovo Nothing)) = False
mvs (Compound c fs) = dozvoljen c && all mvs fs

dozvoljen c = c == notc || c == condc || c == bicondc || c == andc || c == orc

f1 = (p1 & p2) ||| ne p3
f2 = (p1 & Compound or4c [p1, p2, p1, p1]) <-> p2
f3 = (p1 & Compound bicond3c [p1, p2, p3]) <-> p2
f4 = true & false

test1 = mvs p  {- False -}
test2 = mvs p0 {- False -}
test3 = mvs p1 {- True  -}
test4 = mvs m1 {- False -}
test5 = mvs q1 {- False -}
test6 = mvs f1 {- True  -}
test7 = mvs f2 {- False -}
test8 = mvs f3 {- False -}
test9 = mvs f4 {- False -}
