--Jesus Magana
--No partner

--1)The function makes foldl using recursion
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl oper base [] = base
myFoldl oper base (x:xs) = oper (myFoldl oper base xs) x

--2)This function does the reverse using a foldl function
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ foldl (++) [] [[x]]

--3) Make a foldr using a foldl
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr oper base (x) = (foldl (flip (oper)) base x)

--4)Use foldr to make myfoldl2
myFoldl2 :: (a -> b -> a) -> a -> [b] -> a
myFoldl2 oper base (x) = (foldr (flip (oper)) base x)

--5)Check if a char is uppercase
isUpper :: Char -> Bool
isUpper x = x `elem` ['A'..'Z']

--6) return only capital letters of a string
onlyCapitals1 :: String -> String
onlyCapitals1 x = filter (isUpper) x

--7) same as 6 but use list comprehension instead
onlyCapitals2 :: String -> String
onlyCapitals2 x = [x | x <- x, isUpper x] 

--8) do the same but with recursion
onlyCapitals3 :: String -> String
onlyCapitals3 [] = []
onlyCapitals3 (x:xs)
	| isUpper x = x: onlyCapitals3 xs
	| otherwise = onlyCapitals3 xs

--9) This is a function that takes in 2 ints, divides them and returns a tuple
divRemainder :: Int -> Int -> (Int, Int)
divRemainder x y = ((div x y), (mod x y))

--10) This function returns the sum of the digits of a given integer
digitSum :: Int -> Int
digitSum x
	| x < 10 = x
	| otherwise = digitSum (x `div` 10) + x `mod` 10 
