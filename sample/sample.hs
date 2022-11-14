three :: [Int]
three = [ x | x <- [1,3..99], mod x 3 == 0]

evalNum :: Int -> Int -> Int
evalNum x y = if mod x 2 == 1 || mod y 2 == 1 
    then x + y
    else x - y

join :: Eq a => [a] -> [a] -> [a]
join xs ys = [x | x <- xs,not (elem x ys)] ++ ys

data Tree a = Empty | Node a (Tree a) (Tree a)

preOrder :: (Ord a) => Tree a -> [a]
preOrder Empty = []
preOrder (Node x l r)= x : (preOrder l ++ preOrder r)