myAppend :: Eq a => [a] -> [a] -> [a]
myAppend [] xs = xs
myAppend (x) (y) = x ++ y

myHead :: Eq a => [a] -> a
myHead [] = error "Called myHead with Empty List"
myHead (x:xs) = x

myLast :: Eq a => [a] -> a
myLast [] = error "Called myLast with Empty List"
myLast [x] = x
myLast (x:xs) = myLast xs

myTail :: Eq a => [a] -> [a]
myTail [] = error "Called myTail with Empty List"
myTail (x:xs) = xs

myInit ::  Eq a => [a] -> [a]
myInit [] = error "Called myInit with Empty List"
myInit [x] = []
myInit (x:xs) = x : myInit(xs)

myLength ::  Eq a => [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

myReverse ::  Eq a => [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

myConcat :: Eq a => [[a]] -> [a]
myConcat [] = []
myConcat (x:xs) = x ++ myConcat xs

mySum :: Num a => [a] -> a
mySum [] = 0
mySum (x:xs) = x + mySum xs

myProduct :: Num a => [a] -> a
myProduct [] = 0
myProduct [x] = x
myProduct (x:xs) = x * myProduct xs

myMaximum :: (Ord a) => [a] -> a
myMaximum [] = error "Called myMaximum with an Empty List"
myMaximum [x] = x
myMaximum (x:xs) = if x > (myMaximum(xs))
    then x
    else myMaximum xs

myMinimum :: (Ord a) => [a] -> a
myMinimum [] = error "Called myMinimum with an Empty List"
myMinimum [x] = x
myMinimum (x:xs) = if x < (myMinimum(xs))
    then x
    else myMinimum xs

myElem :: Eq a => a -> [a] -> Bool
myElem x [] = False
myElem x (y:ys) = if y == x 
    then True
    else myElem x ys

myDelete :: Eq a => a -> [a] -> [a]
myDelete x (y:ys) = if y == x
    then ys 
    else y : myDelete x ys 

myUnion :: Eq a => [a] -> [a] -> [a]
myUnion xs [] = xs
myUnion xs (y:ys) = if (myElem y xs) || (myElem y ys)
                    then myUnion xs ys
                    else myUnion (myAppend xs [y]) ys