checkPalindrome :: Eq a => [a] -> Bool
checkPalindrome a = head a == last a


isPalindrome :: Eq a => [a] -> Bool
isPalindrome a = 
    length a < 2 || checkPalindrome a && isPalindrome (init(tail(a)))