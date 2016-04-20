import qualified Data.Set as Set

data Atom = At Char (Maybe Integer)
    deriving (Eq, Ord)

instance Show Atom where
    show (At p Nothing) = [p]
    show (At p (Just i)) = p : show i

data Connective = Conn Integer ([Bool] -> Bool)

logconst b = Conn 0 (\ [] -> b)
unary op = Conn 1 (\ [x] -> op x)
binary op = Conn 2 (\ [x, y] -> x `op` y)

true_c = logconst True
false_c = logconst False
not_c = unary not
and_c = binary (&&)
or_c = binary (||)
cond_c = binary (\ x y -> not x || y)
bicond_c = binary (==)
ternary_c = Conn 3 (\[x,y,z] -> if x then y else z)

table (Conn 0 f) = [f []]
table (Conn n f) = [b | start <- [False, True],
    b <- table (Conn (n-1) (\ rest -> f([start] ++ rest)))]

fullTable (Conn 0 f) = [[f []]]
fullTable (Conn n f) = [start : b | start <- [False, True],
    b <- fullTable (Conn (n-1) (\rest -> f([start] ++ rest)))]

instance Eq Connective where
    c1 == c2 = table c1 == table c2

data Formula = Atomic Atom
    | Compound Connective [Formula]
    deriving (Eq)

p = Atomic (At 'p' Nothing)
q = Atomic (At 'q' Nothing)
p1 = Atomic (At 'p' (Just 1))
p2 = Atomic (At 'p' (Just 2))
p3 = Atomic (At 'p' (Just 3))

f & g = Compound and_c [f, g]
ne f = Compound not_c [f]
true = Compound true_c []
false = Compound false_c []
f ||| g = Compound or_c [f, g]
f --> g = Compound cond_c [f, g]
f <-> g = Compound bicond_c [f, g]

instance Show Connective where
    show c | c == not_c = "!"
           | c == and_c = "&"
           | c == or_c = "|"
           | c == cond_c = "->"
           | c == bicond_c = "<->"
           | otherwise = error ("Unnamed operator with table "
                                ++ show (table c))

instance Show Formula where
    show (Atomic p) = show p
    show (Compound c@(Conn 0 b) []) = show (b [])
    show (Compound c@(Conn 1 _) [f]) = show c ++ show f
    show (Compound c'@(Conn 3 _) [a, b, c]) =  if c' == ternary_c
        then "(" ++ show a ++ " ? " ++ show b ++ " : " ++ show c ++ ")"
        else error "Unknown ternary operator"
    show (Compound c@(Conn 2 _) [f, g]) = "(" ++ show f ++ show c ++ show g ++ ")"

test1 = Compound ternary_c [q,p&q,p1] & p
test2 = fullTable true_c
test3 = fullTable not_c
test4 = fullTable and_c
test5 = fullTable ternary_c