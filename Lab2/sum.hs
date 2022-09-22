isSum :: Int -> Int -> Int -> Bool

isSum x y z = x + y == z ||
    y + z == x ||
    x + z == y