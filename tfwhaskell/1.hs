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
