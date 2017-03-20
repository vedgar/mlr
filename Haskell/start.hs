data Formula = Atom Char 
  | LogConst Bool
  | Unary Char Formula
  | Binary Char Formula Formula
  deriving (Eq)

instance Show Formula where
  show (Atom c) = [c]
  show (LogConst True) = "T"
  show (LogConst False) = "F"
  show (Unary c f) = [c] ++ show f
  show (Binary c f g) = "(" ++ show f ++ [c] ++ show g ++ ")"

p = Atom 'p'
q = Atom 'q'
f & g = Binary '&' f g
ne f = Unary '!' f

type Interp = (Char -> Bool)
vrijednost i (Atom c) = i c
vrijednost i (Unary '!' f) = not (vrijednost i f)
vrijednost i (Binary '&' f g) = min (vrijednost i f) (vrijednost i g)
vrijednost i (LogConst b) = b

i1 c = c == 'p' || c == 'q'
test1 = vrijednost i1 $ Binary '&' (Atom 'p') (Unary '!' $ Atom 'q')
