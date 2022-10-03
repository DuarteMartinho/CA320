shortestList :: Eq a => [a] -> [[a]] -> [a]
shortestList s a =
    if (length(a)) == 1 then s
    else
        if length s == 0 then shortestList (head(a)) a
        else if length (head(a)) < length s
            then shortestList (head(a)) (tail(a)) 
            else shortestList s (tail(a))


shortest :: Eq a => [[a]] -> [a]
shortest a = shortestList [] a