units, teens, tens :: [String]

units = ["zero","one","two","three","four","five",
         "six","seven","eight","nine"]
teens = ["ten","eleven","twelve","thirteen","fourteen",
         "fifteen","sixteen","seventeen","eighteen",
         "nineteen"]
tens  = ["twenty","thirty","forty","fifty","sixty",
         "seventy","eighty","ninety"]


convert1 :: Int -> String
convert1 n = units !! n


digits2 :: Int -> (Int, Int)
digits2 n = (div n 10, mod n 10)

-- convert 308000 = "three hundred and eight thousand"
-- convert 369027 = "three hundred and sixty-nine thousand and
                    -- twenty-seven"
-- convert 369401 = "three hundred and sixty-nine thousand
--                      four hundred and one"

-- combine2 :: (Int, Int) ->  String
-- combine2 (t, u)
--   | t == 0 = units !! u
--   | t == 1 = teens !! u
--   | u == 0 = tens !! (t-2)
--   | otherwise = tens !! (t-2) ++ "-" ++ units !! u

convert2 :: Int ->  String
convert2 n
  | t == 0 = units !! u
  | t == 1 = teens !! u
  | u == 0 = tens !! (t-2)
  | otherwise = tens !! (t-2) ++ "-" ++ units !! u
  where (t, u) = digits2 n


convert3 :: Int ->  String
convert3 n
  | h == 0 = convert2 t
  | h == 1 = units !! h ++ " hundred"
  | otherwise = units !! h ++ " hundred and " ++ convert2 t
  where (h, t) = (div n 100, mod n 100)


convert6 :: Int -> String
convert6 n
  | t == 0 = convert3 u
  | u == 0 = convert3 t ++ " thousand"
  | otherwise = convert3 t ++ " thousand" ++ link u ++ convert3 u
  where (t, u) = (div n 1000, mod n 1000)

link :: Int -> String
link h = if h < 100 then " and " else " "

myzip (x:xs) (y:ys) = (x,y) : (myzip xs ys)
myzip [] [] = []
myzip xs [] = []
myzip [] xs = []

myspan p [] = ([], [])
myspan p (x:xs) = if p x then (x:ys, zs)
                else ([], x:xs)
                where (ys, zs) = myspan p xs

