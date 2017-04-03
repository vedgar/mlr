import Data.Set hiding (map)
import qualified Data.List

data Atom = At Char (Maybe Integer)
    deriving (Eq, Ord)
instance Show Atom where
    show (At p Nothing) = [p]
    show (At p (Just i)) = p : show i
p = At 'p' Nothing
p1 = At 'p' (Just 1)
p2 = At 'p' (Just 2)
q = At 'q' Nothing
r = At 'r' Nothing
s = At 's' Nothing

type Aset = Set Atom
showAset r = Data.List.intercalate "," (map show (elems r))

data Clause = Aset :< Aset
    deriving (Eq, Ord)
instance Show Clause where
    show (r :< q) = "(" ++ showAset r ++ " <- " ++ showAset q ++ ")"
pos <~ neg = fromList pos :< fromList neg

resolve (a :< b) (c :< d) = 
    let isec a b = elems (intersection a b)
        undel a b q = union a (delete q b)
    in case (isec a d, isec b c) of
        ([], [q]) -> Just (undel a c q :< undel d b q)
        _ -> Nothing
expand clf = fromList ([k | c1 <- t, c2 <- t, Just k <- [resolve c1 c2]] ++ t)
    where t = elems clf
converge = until =<< ((==) =<<)  -- http://stackoverflow.com/a/23924238/1875565
contradictory clf = []<~[] `member` converge expand clf

test1 = contradictory (fromList [[q]<~[p], [p]<~[q], [p,q]<~[], []<~[p,q]])
test2 = contradictory (fromList [[q]<~[p], [p]<~[q], [p,q]<~[]])

 -- prva zadaca, peti zadatak
z1z5 = fromList [[p,q,s]<~[r], [r,s]<~[p], []<~[q,r], [p]<~[s], []<~[p,r], [r]<~[]]
test3 = contradictory z1z5
