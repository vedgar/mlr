import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad (guard)
import Data.Tree (Tree)
import qualified Data.Tree as Tree

data Expr = Atom String
          | Neg Expr
          | And Expr Expr
          | Or  Expr Expr
          | Eqv Expr Expr
          | Imp Expr Expr
          deriving (Eq,Ord)

instance Show Expr where
  show (Atom x) = x
  show (Neg p) = "~" ++ (show p)
  show (And p q) = "(" ++ (show p) ++ " and " ++ (show q) ++ ")"
  show (Or p q)  = "(" ++ (show p) ++ " or " ++ (show q) ++ ")"
  show (Imp p q) = "(" ++ (show p) ++ " -> " ++ (show q) ++ ")"
  show (Eqv p q) = "(" ++ (show p) ++ " <-> " ++ (show q) ++ ")"

type Tableau = Tree (Set Expr,Bool)

drawNode :: (Set Expr,Bool) -> String
drawNode (u,b) = show (Set.toList u) ++ (if b then " Unsat" else " Sat")

draw :: Tableau -> String
draw = Tree.drawTree . fmap drawNode

isLiteral :: Expr -> Bool
isLiteral (Atom _) = True
isLiteral (Neg (Atom _)) = True
isLiteral _ = False

isAllLiterals :: Set Expr -> Bool
isAllLiterals u = Set.null (Set.filter (not . isLiteral) u)

complement :: Expr -> Expr
complement (Neg x) = x
complement x = Neg x

hasComplements :: Set Expr -> Bool
hasComplements u | Set.size u <= 1 = False
hasComplements u = 
  if Set.member (complement x) u' then True else hasComplements u'
  where x  = Set.findMin u
        u' = Set.deleteMin u

closed :: Set Expr -> Maybe Bool
closed u = do
  guard (isAllLiterals u)
  return (hasComplements u)

isClosed :: Tableau -> Bool
isClosed (Tree.Node (_,b) _) = b

alpha :: Expr -> [Expr]
alpha (Neg (Neg p))     = [p]
alpha (And p q)       = [p,q]
alpha (Neg (Or p q))  = [Neg p,Neg q]
alpha (Neg (Imp p q)) = [p,Neg q]
alpha (Eqv p q)       = [Imp p q,Imp q p]
alpha _                 = []

beta :: Expr -> [Expr]
beta (Neg (And p q))  = [Neg p,Neg q]
beta (Or p q)         = [p,q]
beta (Imp p q)        = [Neg p,q]
beta (Neg (Eqv p q))  = [Neg (Imp p q),Neg (Imp q p)]
beta _                  = []

buildTableau :: Set Expr -> Tableau
buildTableau u = 
  case closed u of
    Just b -> Tree.Node (u,b) []
    Nothing -> let (_,w) = Set.partition isLiteral u in
      let x = Set.findMin w in
      case alpha x of
        [] -> case beta x of
            [b1,b2] -> 
              let 
                u1 = (Set.delete x u) `Set.union` (Set.singleton b1)
                u2 = (Set.delete x u) `Set.union` (Set.singleton b2)
                t1 = buildTableau u1
                t2 = buildTableau u2
                b = and (map isClosed [t1,t2])
              in
                Tree.Node (u,b) [t1,t2]
        a -> 
          let 
            u' = (Set.delete x u) `Set.union` (Set.fromList a)
            t = buildTableau u'
            b = isClosed t
          in
            Tree.Node(u,b) [t]

tableau :: Expr -> Tableau
tableau = buildTableau . Set.singleton

sat :: Tableau -> Bool
sat = not . isClosed

f = And (Atom "p") (Or (Neg(Atom "q")) (Neg(Atom "p")))
f1 = And (Or (Atom "p") (Atom "q")) (And (Neg (Atom "p")) (Neg (Atom "q")))

t = tableau f
t1 = tableau f1

{-
putStrLn (show f)
putStrLn (sat t)
putStrLn (draw t)

putStrLn (show f1)
putStrLn (sat t1)
putStrLn (draw t1)
-}