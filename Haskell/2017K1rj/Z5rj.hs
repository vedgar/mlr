import Data.Set hiding (map)
import qualified Data.List

data Atom = At Char (Maybe Integer) deriving (Eq, Ord)
instance Show Atom where
    show (At p Nothing) = [p]
    show (At p (Just i)) = p : show i

p1 = At 'P' (Just 1)
p2 = At 'P' (Just 2)
p3 = At 'P' (Just 3)

type Aset = Set Atom
showAset r = Data.List.intercalate "," (map show (elems r))

data Clause = Aset :< Aset deriving (Eq, Ord)
instance Show Clause where
    show (r :< q) = "(" ++ showAset r ++ " <- " ++ showAset q ++ ")"
pos <~ neg = fromList pos :< fromList neg

data HFormula = P Integer | Cond HFormula HFormula deriving (Eq)
f --> g = Cond f g

instance Show HFormula where
    show (P n) = 'P' : show n
    show (Cond f1 f2) = "(" ++ show f1 ++ "->" ++ show f2 ++ ")"

noviatom i = (At 'Q' (Just i), i + 1)

visit :: HFormula -> Integer -> (Set Clause, Atom, Integer)
visit (P i) n = (empty, At 'P' (Just i), n)
visit (Cond f g) n1 = let (cf, kf, n2) = visit f n1 in
                      let (cg, kg, n3) = visit g n2 in
                      let (k, n4) = noviatom n3 in
    (unions [fromList [[kg]<~[k,kf], [k]<~[kg], [k,kf]<~[]], cf, cg], k, n4)

cejtin f = let (cf, kf, _) = visit f 1 in insert ([kf]<~[]) cf
