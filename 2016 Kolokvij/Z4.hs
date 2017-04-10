data RegEx = Eset | Estr | Literal Char | Concat RegEx RegEx | Alter RegEx RegEx | Star RegEx

instance Show RegEx where
    show Eset = "0"
    show Estr = "$"
    show (Literal c) = [c]
    show (Concat r1 r2) = (show r1) ++ (show r2)
    show (Alter r1 r2) = "(" ++ show r1 ++ "|" ++ show r2 ++ ")"
    show (Star r) = show r ++ "*"

test1 = Concat (Alter (Literal 'a') Estr) (Star (Literal 'b'))

exactly "" = Estr
exactly (c : cs) = Concat (Literal c) (exactly cs)

test2 = exactly "MLR"

singular Eset = False
singular Estr = True
singular (Literal c) = False
singular (Concat r1 r2) = singular r1 && singular r2
singular (Alter r1 r2) = singular r1 || singular r2
singular (Star r) = True

test3 = singular test1
test4 = singular test2