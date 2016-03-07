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

table (Conn 0 f) = [f []]
table (Conn n f) = [b | start <- [False, True],
    b <- table (Conn (n-1) (\ rest -> f([start] ++ rest)))]
{- table (Conn n f) = table (Conn (n-1) (partial False))
    ++ table (Conn (n-1) (partial True)) where
    partial prefix rest = f ([prefix] ++ rest) -}

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

vars (Atomic p) = Set.singleton p
vars (Compound _ fs) = Set.unions (map vars fs)

complexity (Atomic p) = 0
complexity (Compound _ fs) = 1 + sum (map complexity fs)
