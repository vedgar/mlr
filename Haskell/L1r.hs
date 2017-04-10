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

free' (Single v) = singleton v
free' (Application _ ts) = unions (map free' ts)
free (Atomic (Related _ ts)) = unions (map free' ts)
free (Compound _ fs) = unions (map free fs)
free (Quantified _ v f) = Data.Set.delete v (free f)
qdepth (Atomic _) = 0
qdepth (Compound _ []) = 0
qdepth (Compound _ fs) = maximum (map qdepth fs)
qdepth (Quantified _ _ f) = 1 + qdepth f

{- renameVar None v2 : replace first bound you find with v2
   renameVar (Just v1) v2 : found v1, replacing it with v2 -}
{- renameVar :: Maybe Variable -> Variable -> Formula -> Formula
renameVar Nothing v (Atomic a) = a
renameVar Nothing v (Compound c fs) = Compound c
    (map (renameVar Nothing v) -}

natMax [] = -1
natMax xs = maximum xs
maxIndex'' (Var _ Nothing) = -1
maxIndex'' (Var _ (Just n)) = n
maxIndex' (Single v) = maxIndex'' v
maxIndex' (Application _ ts) = natMax (map maxIndex' ts)
maxIndex (Atomic (Related _ ts)) = natMax (map maxIndex' ts)
maxIndex (Compound _ fs) = natMax (map maxIndex fs)
maxIndex (Quantified _ v f) = max (maxIndex'' v) (maxIndex f)

{- replaceFirst p f l :
    replaces first element x of l satisfying p x with f x -}
replaceFirst p f [] = []
replaceFirst p f (x:xs) | p x = f x : xs
                        | otherwise = x : replaceFirst p f xs

renameVar' v1 v2 (Single v3) | v3 == v1 = Single v2
                             | otherwise = Single v3
renameVar' v1 v2 (Application f ts) =
    Application f (map (renameVar' v1 v2) ts)
renameVar v1 v2 (Atomic (Related r ts)) =
    Atomic (Related r (map (renameVar' v1 v2) ts))
renameVar v1 v2 (Compound c fs) =
    Compound c (map (renameVar v1 v2) fs)
renameVar v1 v2 (Quantified q v f)
    | v == v1 = Quantified q v f
    | otherwise = Quantified q v (renameVar v1 v2 f)

containsBelow' n (Single v) = maxIndex'' v <= n
containsBelow' n (Application f ts) = any (containsBelow' n) ts
renameBelow' n v2 (Single v3) | maxIndex'' v3 <= n = Single v2
                              | otherwise = Single v3
renameBelow' n v2 (Application f ts) =
    Application f (replaceFirst (containsBelow' n) (renameBelow' n v2) ts)

containsBelow n (Atomic (Related _ ts)) = any (containsBelow' n) ts
containsBelow n (Compound c fs) = any (containsBelow n) fs
containsBelow n (Quantified q v f) = maxIndex'' v <= n || containsBelow n f
renameBelow n v2 (Atomic (Related r ts)) =
    Atomic (Related r
        (replaceFirst (containsBelow' n) (renameBelow' n v2) ts))
renameBelow n v2 (Compound c fs) =
    Compound c (replaceFirst (containsBelow n) (renameBelow n v2) fs)
renameBelow n v2 (Quantified q v f)
    | maxIndex'' v <= n = Quantified q v2 (renameVar v v2 f)
    | otherwise = Quantified q v (renameBelow n v2 f)
