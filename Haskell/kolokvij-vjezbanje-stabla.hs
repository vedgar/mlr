-- Peti zadatak

data IntTree1 = Leaf1 Int | Node1 IntTree1 IntTree1

depth :: IntTree1 -> Int
depth (Leaf1 n) = 1
depth (Node1 t1 t2) = 1 + max (depth t1) (depth t2)

findDeepest :: (Int -> Bool) -> IntTree1 -> [Int]

-- findDeepest' p tree d : vraæa najdublji element koji 
-- zadovoljava p, skupa s dubinom
findDeepest' p (Leaf1 n) d | p n = [(n, d)]
                           | otherwise = []
findDeepest' p (Node1 t1 t2) d =
    deeper (findDeepest' p t1 (d+1)) (findDeepest' p t2 (d+1))

deeper [] [] = []
deeper [(n, d)] [] = [(n, d)]
deeper [] [(n', d')] = [(n', d')]
deeper [(n, d)] [(n', d')] | d >= d' = [(n, d)]
                           | otherwise = [(n', d')]

findDeepest p t = map fst (findDeepest' p t 0)

level 1 (Leaf1 n) = [n]
level _ (Leaf1 n) = []
level n (Node1 t1 t2) = level (n-1) t1 ++ level (n-1) t2

data IntTree2 = Leaf2 Int | Node2 Int IntTree2 IntTree2
	deriving Show

pruneTree d t1 = odsijeci d (pozbroji t1)
pozbroji (Leaf1 n) = Leaf2 n
pozbroji (Node1 t1l t1d) =
	Node2 (korijen t2l + korijen t2d) t2l t2d
		where t2l = pozbroji t1l
		      t2d = pozbroji t1d

korijen (Leaf2 n) = n
korijen (Node2 n _ _) = n

odsijeci d (Leaf2 n) = Leaf2 n
odsijeci 1 (Node2 n t2l t2d) = Leaf2 n
odsijeci d (Node2 n t2l t2d) =
	Node2 n (odsijeci (d-1) t2l) (odsijeci (d-1) t2d)