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

data Clause = BackArrow (Set Atom) (Set Atom)
    deriving (Eq, Ord)
instance Show Clause where
    show (BackArrow r q) = "(" ++ showset r ++ " <- " ++ showset q ++ ")"
showset r = Data.List.intercalate "," (map show (elems r))

cl pos neg = BackArrow (fromList pos) (fromList neg)

type ClausalForm = Set Clause

clashing (BackArrow a b) (BackArrow c d) = (elems (intersection a d), elems (intersection b c))

resolve c1@(BackArrow a b) c2@(BackArrow c d) = case (clashing c1 c2) of
    ([q], []) -> [resolveBy q c1 c2]
    ([], [q]) -> [resolveBy q c1 c2]
    _ -> []

resolveBy q (BackArrow a b) (BackArrow c d) = BackArrow (delete q (union a c)) (delete q (union b d))

expand t = fromList ([k | c1 <- elems t, c2 <- elems t, k <- resolve c1 c2] ++ elems t)
fixpoint clf | clf == expand clf = clf
             | otherwise = fixpoint (expand clf)

contradictory clf = emptyclause `member` fixpoint clf
emptyclause = BackArrow empty empty

test1 = contradictory (fromList [[q]`cl`[p], [p]`cl`[q], [p,q]`cl`[], []`cl`[p,q]])
test2 = contradictory (fromList [[q]`cl`[p], [p]`cl`[q], [p,q]`cl`[]])

 -- prva zadaca, peti zadatak
z1z5 = fromList [[p,q,s]`cl`[r], [r,s]`cl`[p], []`cl`[q,r], [p]`cl`[s], []`cl`[p,r], [r]`cl`[]]
test3 = contradictory z1z5