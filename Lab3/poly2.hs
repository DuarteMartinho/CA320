evalPoly :: Int -> [Int] -> Int
evalPoly _ [p] = p
evalPoly x (p:ps) = p + (x * (evalPoly x ps))