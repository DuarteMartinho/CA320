sumPoly :: [Int] -> [Int] -> [Int]
sumPoly x [] = x
sumPoly [] y = y
sumPoly (x:xs) (y:ys) = x +y : sumPoly xs ys