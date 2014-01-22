--Jesus Magana
--No partner
--homework 2

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

--11) This function takes in a number and spells it out in english
--made lists of words that will be needed and are not 1 time use
ones = ["", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
teens = ["","eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]
tens = ["","ten","twenty","thirty","fourty","fifty","sixty","seventy","eighty","ninety"]

--This function is to convert exactly one letter to 
sayOnes :: String -> String
sayOnes x = ones!!(read x)

--This function splits two numbers into their word form. It also checks for teens since they are a special case
twoNumSplit :: String -> String
twoNumSplit (y:z)
	| ((read (y:z)) > 10) && ((read (y:z)) < 20) = " " ++ teens!!(read z)
	| ((read [y]) == 0) = " " ++ ones!!(read z)
	| ((read z) == 0) = " " ++ tens!!(read [y])
	| (read (y:z)) > 0 = " " ++ tens!!(read [y]) ++ " " ++ ones!!(read z)
	| otherwise = " "

--Here I check to see if its smaller than 100 and smaller than 10 and redirect the string accordingly
--If it is bigger than 100, then it will say the number plus hundred and then call its child function
threeNumSplit :: String -> String
threeNumSplit (x:y:z) 
	| ((read(x:y:z)) < 100) = twoNumSplit (x:y:z)
	| otherwise = ones!!(read [x]) ++ " hundred" ++ twoNumSplit(y:z)
threeNumSplit x = sayOnes x


--Here i split the number down to smaller numbers based on cases and add the correct ending.
sayNum :: String -> String
sayNum x
	| (x == "") = ""
	| (head x) == '0' = sayNum (tail x)
	| ((read x) `div` (10^63) > 0) = threeNumSplit (show ((read x) `div` (10^63))) ++ " vigintillion " ++ sayNum (show ((read x) `mod` (10^63)))
	| ((read x) `div` (10^60) > 0) = threeNumSplit (show ((read x) `div` (10^60))) ++ " novemdecillion " ++ sayNum (show ((read x) `mod` (10^60)))
	| ((read x) `div` (10^57) > 0) = threeNumSplit (show ((read x) `div` (10^57))) ++ " octodecillion " ++ sayNum (show ((read x) `mod` (10^57)))
	| ((read x) `div` (10^54) > 0) = threeNumSplit (show ((read x) `div` (10^54))) ++ " septendecillion " ++ sayNum (show ((read x) `mod` (10^54)))
	| ((read x) `div` (10^51) > 0) = threeNumSplit (show ((read x) `div` (10^51))) ++ " sexdecillion " ++ sayNum (show ((read x) `mod` (10^51)))
	| ((read x) `div` (10^48) > 0) = threeNumSplit (show ((read x) `div` (10^48))) ++ " quindecillion " ++ sayNum (show ((read x) `mod` (10^48)))
	| ((read x) `div` (10^45) > 0) = threeNumSplit (show ((read x) `div` (10^45))) ++ " quattuordecillion " ++ sayNum (show ((read x) `mod` (10^45)))
	| ((read x) `div` (10^42) > 0) = threeNumSplit (show ((read x) `div` (10^42))) ++ " tredecillion " ++ sayNum (show ((read x) `mod` (10^42)))
	| ((read x) `div` (10^39) > 0) = threeNumSplit (show ((read x) `div` (10^39))) ++ " duodillion " ++ sayNum (show ((read x) `mod` (10^39)))
	| ((read x) `div` (10^36) > 0) = threeNumSplit (show ((read x) `div` (10^36))) ++ " undecillion " ++ sayNum (show ((read x) `mod` (10^36)))
	| ((read x) `div` (10^33) > 0) = threeNumSplit (show ((read x) `div` (10^33))) ++ " decillion " ++ sayNum (show ((read x) `mod` (10^33)))
	| ((read x) `div` (10^30) > 0) = threeNumSplit (show ((read x) `div` (10^30))) ++ " nonillion " ++ sayNum (show ((read x) `mod` (10^30)))
	| ((read x) `div` (10^27) > 0) = threeNumSplit (show ((read x) `div` (10^27))) ++ " octillion " ++ sayNum (show ((read x) `mod` (10^27)))
	| ((read x) `div` (10^24) > 0) = threeNumSplit (show ((read x) `div` (10^24))) ++ " septillion " ++ sayNum (show ((read x) `mod` (10^24)))
	| ((read x) `div` (10^21) > 0) = threeNumSplit (show ((read x) `div` (10^21))) ++ " sextillion " ++ sayNum (show ((read x) `mod` (10^21)))
	| ((read x) `div` (10^18) > 0) = threeNumSplit (show ((read x) `div` (10^18))) ++ " quintillion " ++ sayNum (show ((read x) `mod` (10^18)))
	| ((read x) `div` (10^15) > 0) = threeNumSplit (show ((read x) `div` (10^15))) ++ " quadrillion " ++ sayNum (show ((read x) `mod` (10^15)))
	| ((read x) `div` (10^12) > 0) = threeNumSplit (show ((read x) `div` (10^12))) ++ " trillion " ++ sayNum (show ((read x) `mod` (10^12)))
	| ((read x) `div` (10^9) > 0) = threeNumSplit (show ((read x) `div` (10^9))) ++ " billion " ++ sayNum (show ((read x) `mod` (10^9)))
	| ((read x) `div` (10^6) > 0) = threeNumSplit (show ((read x) `div` (10^6))) ++ " million " ++ sayNum (show ((read x) `mod` (10^6)))
	| ((read x) `div` (10^3) > 0) = threeNumSplit (show ((read x) `div` (10^3))) ++ " thousand " ++ sayNum (show ((read x) `mod` (10^3)))
	| otherwise = threeNumSplit x