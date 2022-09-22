findS :: Float -> Float -> Float -> Float
findS a b c = abs ((a+b+c)/2)

triangleArea :: Float -> Float -> Float -> Float
triangleArea a b c = sqrt (s*(s-a)*(s-b)*(s-c))
    where s = findS a b c