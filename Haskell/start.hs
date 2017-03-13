data Formula = Atom Char 
  | LogConst Bool
  | Unary Char Formula
  | Binary Char Formula Formula
  deriving (Eq)

instance Show Formula where
  show (Atom c) = [c]
  show (Unary c f) = [c] ++ show f
  show (Binary c f g) = "(" ++ show f
    ++ [c] ++ show g ++ ")"

p = Atom 'p'
q = Atom 'q'
f & g = Binary '&' f g
ne f = Unary '!' f

Interp = (Atom -> Bool)
interp I (Atom c) = I c
interp I (LogConst b) = b
interp I (Unary '!' f) = not I f
interp I (Binary '&' f g) = max (I f) (I g)