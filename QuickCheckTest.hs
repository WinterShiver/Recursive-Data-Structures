import Test.QuickCheck

propFalse :: [Int] -> [Int] -> Bool
propFalse xs ys = reverse (xs++ys) == reverse xs ++ reverse ys

propTrue :: [Int] -> [Int] -> Bool
propTrue xs ys = reverse (xs++ys) == reverse ys ++ reverse xs

mainT = quickCheck propTrue
mainF = quickCheck propFalse