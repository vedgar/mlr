import Data.List
import Data.Set hiding (map)

data Variable = Var Char (Maybe Integer)
    deriving (Eq, Ord)
instance Show Variable where
    show (Var c Nothing) = [c]
    show (Var c (Just n)) = c : show n

data Funcsymb = FuncS Char (Maybe Integer)
    deriving (Eq)
instance Show Funcsymb where
    show (FuncS c Nothing) = [c]
    show (FuncS c (Just n)) = c : show n

data Term = Single Variable | Application Funcsymb [Term]
    deriving (Eq)
instance Show Term where
    show (Single v) = show v
    show (Application f []) = show f
    show (Application f ts) = show f ++ "(" ++ inner ++ ")"
        where inner = intercalate "," (map show ts)

x = Single (Var 'x' Nothing)
y = Single (Var 'y' Nothing)
z = Single (Var 'z' Nothing)
x1 = Single (Var 'x' (Just 1))
x2 = Single (Var 'x' (Just 2))
f = Application (FuncS 'f' Nothing)
g = Application (FuncS 'g' Nothing)
h = Application (FuncS 'h' Nothing)
f1 = Application (FuncS 'f' (Just 1))
f2 = Application (FuncS 'f' (Just 2))
c = Application (FuncS 'c' Nothing) []
c1 = Application (FuncS 'c' (Just 1)) []
c2 = Application (FuncS 'c' (Just 2)) []

data Relsymb = RelS Char (Maybe Integer)
    deriving (Eq)
instance Show Relsymb where
    show (RelS c Nothing) = [c]
    show (RelS c (Just n)) = c : show n

data Atom = Related Relsymb [Term]
    deriving (Eq)
instance Show Atom where
    show (Related r []) = show r
    show (Related r ts) = show r ++ "(" ++ inner ++ ")"
        where inner = intercalate "," (map show ts)

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

table (Conn 0 f) = [f []]
table (Conn n f) = [b | start <- [False, True],
    b <- table (Conn (n-1) (\ rest -> f([start] ++ rest)))]
instance Eq Connective where
    c1 == c2 = table c1 == table c2

instance Show Connective where
    show c | c == not_c = "!"
           | c == and_c = "&"
           | c == or_c = "|"
           | c == cond_c = "-->"
           | c == bicond_c = "<->"
           | otherwise = error ("Unnamed operator with table "
                                ++ show (table c))

data Quantifier = Universal | Existential
    deriving (Eq)
data Formula = Atomic Atom
    | Compound Connective [Formula]
    | Quantified Quantifier Variable Formula
    deriving (Eq)

true = Compound true_c []
false = Compound false_c []
ne f = Compound not_c [f]
f & g = Compound and_c [f, g]
f ||| g = Compound or_c [f, g]
f --> g = Compound cond_c [f, g]
f <-> g = Compound bicond_c [f, g]

instance Show Formula where
    show (Atomic p) = show p
    show (Compound (Conn 0 b) []) = show (b [])
    show (Compound c@(Conn 1 _) [f]) = show c ++ show f
    show (Compound c@(Conn 2 _) [f, g]) = "( "
        ++ show f ++ " " ++ show c ++ " " ++ show g ++ " )"
    show (Quantified Universal v f) = "forall "
        ++ show v ++ " " ++ show f
    show (Quantified Existential v f) = "exists "
        ++ show v ++ " " ++ show f

p = Atomic . Related (RelS 'p' Nothing)
q = Atomic . Related (RelS 'q' Nothing)
r = Atomic . Related (RelS 'r' Nothing)
p1 = Atomic . Related (RelS 'p' (Just 1))
p2 = Atomic . Related (RelS 'p' (Just 2))
fa (Single v) = Quantified Universal v
ex (Single v) = Quantified Existential v
fi1 = fa x (p[x] --> q[x]) --> (fa x (p[x]) --> fa x (q[x]))
fi2 = fa x (p[x] & q[x]) <-> (fa x2 (p[x2]) & fa x1 (q[x1]))
