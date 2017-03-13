data Formula = Atom Char 
  | LogConst Bool
  | Unary Char Formula
  | Binary Char Formula Formula
  deriving (Eq)

instance Show Formula where
  show (Atom c) = [c]
  show (Unary c f) = [c] ++ show f
  show (Binary c f g) = "(" ++ show f ++ [c] ++ show g ++ ")"

p = Atom 'p'
q = Atom 'q'
f & g = Binary '&' f g
ne f = Unary '!' f

type Interp = (Char -> Bool)
interp i (Atom c) = i c
interp i (LogConst b) = b
interp i (Unary '!' f) = not (interp i f)
interp i (Binary '&' f g) = min (interp i f) (interp i g)

i1 c = c == 'p' || c == 'q'
test1 = interp i1 $ Binary '&' (Atom 'p') (Unary '!' $ Atom 'q')
