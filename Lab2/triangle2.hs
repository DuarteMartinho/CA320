findS :: Float -> Float -> Float -> Float
findS a b c = abs ((a+b+c)/2)

isTriangle :: Float -> Float -> Float -> Bool
isTriangle x y z = (x + y) > z &&
    (y + z) > x &&
    (x + z) > y

triangleArea2 :: Float -> Float -> Float -> Float
triangleArea2 a b c = if isTriangle a b c == False 
    then error "Not a triangle!"
    else sqrt (s*(s-a)*(s-b)*(s-c))
        where s = findS a b c