module MP2a where
import Data.Char


--makeWorld :: (Int,Int) -> ((Int,Int), [[Bool]])
--makeWorld dims@(w,h) = (dims, replicate h (replicate w False))




{-
   Creates N cycles (repetitions) of the input list.


   Examples:

   cycleN 3 [1..4]
   => [1,2,3,4,1,2,3,4,1,2,3,4]

   cycleN 0 "hello?"
   => ""
-}
cycleN :: Int  -- N
       -> [a]  -- input list
       -> [a] 
cycleN 0 _ = []
cycleN 1 x = x
cycleN n x = (cycleN 1 x) ++ (cycleN (n-1) x)


{-
   Partitions the input list into sublists of maximum size N.


   Examples:

   chunksOf 3 "hello world"
   => ["hel","lo ","wor","ld"]

   chunksOf 5 [1..3]
   => [[1,2,3]]
-}
chunksOf :: Int  -- N
         -> [a]  -- input list
         -> [[a]]
chunksOf n [] = []
chunksOf n ls = [take' n ls] ++ chunksOf n (drop' n ls)


{-
   "Unzips" a list of 4-tuples into a tuple of 4 lists.


   Examples:

   unzip4 [(1,2,3,4),(5,6,7,8),(9,10,11,12),(13,14,15,16)]
   => ([1,5,9,13],[2,6,10,14],[3,7,11,15],[4,8,12,16])

   unzip4 [(1,'h',True,3.14), (2,'i',False,2.7), (3,'!',True,9.8)]
   => ([1,2,3],"hi!",[True,False,True],[3.14,2.7,9.8])
-}

unzip4  :: [(a,b,c,d)] -> ([a],[b],[c],[d])
unzip4  =  foldr' (\(a,b,c,d) (as,bs,cs,ds) -> (a:as,b:bs,c:cs,d:ds))
                  ([],[],[],[]) -- partial application
                  where
                     foldr' :: (a -> b -> b) -> b -> [a] -> b
                     foldr' _ v [] = v                        -- anything where second argument is empty list returns the first argument
                     foldr' f v (x:xs) = f x (foldr' f v xs)  -- applies function to first element and then to remaining elements

{-
   Concatenates the lists in an input list, with element X interspersed.


(\ [x]++[xs] -> (take' 1 [xs]) ++ (drop' 1 [xs])) [[1,2,3],[2,3,4],[4,4,5]]

   Examples:

   intersperse ',' ["tom","dick","harry"]
   => "tom,dick,harry"

   intersperse '!' ["hi"]
   => "hi"

   intersperse 0 [[1..5],[6..10]]
   => [1,2,3,4,5,0,6,7,8,9,10]
-}

intersperse :: a -> [[a]] -> [a]
intersperse xs xss = drop' 1 (concat (prepend [xs] xss))
   where
      prepend            :: a -> [a] -> [a]
      prepend sep_list  []     = []
      prepend sep_list (x:xs)  = sep_list : x : prepend sep_list xs

{-
   Removes all values found in the list `candidates` from the input list.


   Examples:

   removeAll [1..3] [0..10]
   => [0,4,5,6,7,8,9,10]

   removeAll "aeiou" "supercalifragilisticexpialidocious"
   => "sprclfrglstcxpldcs"
-}
removeAll :: (Eq a) 
          => [a]  -- candidates list
          -> [a]  -- input list
          -> [a]
removeAll _ [] = []
removeAll x (y:ys)
    | inList y x = removeAll x ys
    | otherwise = y:removeAll x ys
    where
      inList :: Eq a => a -> [a] -> Bool
      inList _ [] = False
      inList x (y:ys) | x == y = True
                      | otherwise = inList x ys


{-
   Extracts a sublist of elements in range [M,N) from the input list. 
   Returns a tuple containing the sublist and the input list with the 
   sublist removed.


   Examples:

   sublist (2,7) [0..10]
   => ([2,3,4,5,6],[0,1,7,8,9,10])

   sublist (3,4) [0..10]
   => ([3],[0,1,2,4,5,6,7,8,9,10])

   sublist (5,5) [0..10]
   => ([],[0,1,2,3,4,5,6,7,8,9,10])

   sublist (0,12) "hello world!"
   => ("hello world!","")

   sublist (6,100) "hello world!"
   => ("world!","hello ")
-}


sublist :: (Int,Int)  -- sublist range (M,N); M is inclusive, N exclusive
        -> [a]        -- input list
        -> ([a],[a])
sublist (a,b) xs = (drop' a (take' b xs), (take' a xs) ++ (drop' b xs))

{-
   Applies Luhn's algorithm for numeric ID verification:

   The Luhn algorithm is used to verify the validity of numeric identifiers
   commonly used for credit/debit card numbers, government IDs, IMEI numbers,
   etc. Given a list of one-digit numbers, it processes them as follows:
   
   1. From right to left, double the value of every other digit. If a
      product is greater than 9, subtract 9 from that result.
   
   2. Sum up all the digits (i.e., the results from step 1 and the given values
      of the other digits)
   
   3. If the result is evenly divisible by 10, the identifier is valid.
   

   E.g., given the identifier consisting of the numbers [2,7,5,8]:
   
   1. We start by doubling the value of every other number starting from the
      right, getting [4,7,10,8]. Since 10 > 9, we subtract 9 from it,
      giving us the list [4,7,1,8]
   
   2. Sum up all the digits, giving us: 4+7+1+8 = 20
   
   3. 20 is evenly divisible by 10, so the identifier is valid.
   

   E.g., given the identifier consisting of the numbers [4,6,1,8,5,3,8]
   
   1. Doubling every other value and subtracting 9 when needed gets us 
      [4,3,1,7,5,6,8]
   
   2. Summing them gets us 34
   
   3. 34 is not evenly divisible by 10, so the identifier is invalid.
   
   
   Examples:
   
   luhn [2,7,5,8]
   => True
   
   luhn [4,3,1,7,5,6,8]
   => False
   
   luhn [3,9,2,8,6,4,1,7,2,0,5,2]
   => True
-}


luhn :: [Int]  -- numeric ID
     ->  Bool  -- True if valid, False otherwise
luhn cc | (mod . sum' . replace_over_nine (start_idx' cc)) cc 10 == 0 = True
        | otherwise = False
   where 
----------------
      replace_over_nine idx cc | idx < 1    = cc
                               | otherwise  = replace_over_nine (idx - 2) (swap idx cc)
         where
            swap i l | (l !! (i-1) * 2) <= 9  = (take' (i-1) l)  ++  [l !! (i-1) * 2    ]  ++  (drop' i l)
                     | otherwise              = (take' (i-1) l)  ++  [l !! (i-1) * 2 - 9]  ++  (drop' i l)
----------------
      start_idx' cc = start_idx'' (-1) cc
         where
            start_idx'' count x | x == []    = count
                                | otherwise  = start_idx'' (count + 1) (drop' 1 x)
----------------
      sum' (x:xs) | drop' 1 (x:xs) == [] = x
                  | otherwise           = x + sum' xs

{-
   Carries out run-length encoding on input string. 
   
   Run-length encoding is a simple form of data compression that replaces
   characters in a stream with the count of adjacent occurrences of that
   character and just a single instance of the character itself. Write a
   function that takes a string and returns a list of tuples reprenting the
   run-length encoding of that string.
   

   Examples:
   
   runLengthEncode "aaaaaaabbb"
   => [(7,'a'),(3,'b')]
   
   runLengthEncode "happy daaay"
   => [(1,'h'),(1,'a'),(2,'p'),(1,'y'),(1,' '),(1,'d'),(3,'a'),(1,'y')]
-}

-- get length of string


runLengthEncode :: String -> [(Int,Char)]
runLengthEncode input = rle [] input
   where
----------------
      rle tups ""                                          = tups
      rle []   letters                                     = rle [(1, letters !! 0)]                             (drop' 1 letters)
      rle tups letters | new_letter tups letters == False  = rle (all_but_last ++ [(count + 1, letters !! 0)])   (drop' 1 letters)
                       | otherwise                         = rle (tups         ++ [(1        , letters !! 0)])   (drop' 1 letters)
         where
            first        = letters !! 0
            all_but_last = take' ((length' tups) - 1) tups
            count        = fst (tups !! (length' (tups) - 1))
----------------
      new_letter tups letters | snd (tups !! ((length' tups) - 1)) == letters !! 0 = False
                              | otherwise                                          = True
      

{-
   Decodes the run-length encoding of a string.


   Examples:

   runLengthDecode [(1,'h'), (5,'i')]
   => "hiiiii"
    
   runLengthDecode (runLengthEncode "whhhhaaaaat?")
   => "whhhhaaaaat?"
-}
runLengthDecode :: [(Int,Char)]  -- run-length encoded string
                -> String  -- original string
runLengthDecode tups = rld [] tups
   where
----------------
      repeat_n (n, c) = repeat_n' [] (n,c)
         where
            repeat_n' lst  (0,c) = lst
            repeat_n' lst  (n,c) = repeat_n' (lst ++ [c]) (n-1, c)
----------------
      rld lst []   = lst
      rld lst tups = rld (lst ++ repeat_n (tups !! 0)) (drop' 1 tups)

{- 
   Applies the Vigenere encryption scheme to the input string.

   The Vigenere encryption scheme is similar to the Caesar cipher presented in
   class in that it makes use of shifting, but instead of a single numeric key
   applied uniformly to the entire plain text, a string of characters is used as
   the key. The numeric value of each character (i.e., its position in the
   alphabet) is used as a shift value, and if the key is shorter than the length
   of the plain text it is simply repeated.


   E.g., to encrypt the plain text "FOOBAR" with the key "BAZ", we can proceed
   as follows:

   1. Pair each letter of the plain text with a letter from the key:

         F  O  O  B  A  R B  A  Z  B  A  Z

   2. Convert each letter to its numeric value (A=0, B=1 ... Z=25)

         5  14  14  1  0  17 1   0  25  1  0  25

   3. Add them together:

         6  14  39  2  0  42

   4. "Wrap" the numbers around so they're in the range 0-25:

         6  14  13  2  0  16

   5. Convert the numbers back into letters:

         G  O  N  C  A  Q

         -- convert to numbers
         -- 

   Plain text can contain a mix of lowercase and uppercase letters and
   punctuation, but all letters will be interpreted as uppercase. Punctuation
   will not be encrypted. The key will contain only letters (lower or upper
   case), but again will only be interpreted as uppercase.


   Examples:

   vigenere "baz" "foobar" => "GONCAQ"

   vigenere "Yadda" "Hello, world!" => "FEOOO, UOUOD!"
-}

vigenere :: String  -- input string (plain text)
         -> String  -- encryption key
         -> String  -- encrypted string
vigenere [] input = "Error: empty key"
vigenere key [] = []
vigenere key input = apply_at_strings [] (split_at_punc [[]] input) key
   where
      apply_at_strings xs []     key = concat xs
      apply_at_strings xs (y:ys) key | is_punctuation (head y)   = apply_at_strings (xs ++ [y])            ys key
                                     | otherwise                 = apply_at_strings (xs ++ [(vig' key y)]) ys key

      split_at_punc xs   [] = xs
      split_at_punc [[]] (y:ys)            = split_at_punc [[y]] ys
      split_at_punc xs (y:ys) | scenario_1 = split_at_punc first_list_1 ys
                              | scenario_2 = split_at_punc first_list_2 ys
         where
            scenario_1 = is_punctuation ((head . reverse) ((head . reverse) xs)) == (is_punctuation y)
            scenario_2 = is_punctuation ((head . reverse) ((head . reverse) xs)) /= (is_punctuation y)
            first_list_1 = ((reverse . drop 1 . reverse) xs) ++ [((head . reverse) xs) ++ [y]]
            first_list_2 = ((reverse . drop 1 . reverse) xs) ++ [((head . reverse) xs)] ++ [[y]]

      is_punctuation x | ((ord (toUpper (x)) - 65) >= 0) && ((ord (toUpper (x)) - 65) <= 25) = False
                       | otherwise = True

      --vig' []  input = "Error: empty key"
      vig' key []    = []
      vig' key input = (numbers_to_letters . wrap' . sum_counterparts . halve' . letters_to_numbers) (pair_with_key key input)

      --pair_with_key []  input = "Error: empty key"
      pair_with_key key []    = []
      pair_with_key key input = input ++ (take' (length' input) (cycleN (length' input) key))

      letters_to_numbers []     = []
      letters_to_numbers (x:xs) = (ord (toUpper(x))-65) : (letters_to_numbers xs)

      halve' lst = ((take' n lst), (drop' n lst))
         where n = (length' lst) `div` 2

      sum_counterparts ([],[])         = []
      sum_counterparts ((x:xs),(y:ys)) = [x+y] ++ sum_counterparts (xs,ys)

      wrap' [] = []
      wrap' (x:xs) | x > 25    = (x `mod` 26) : (wrap' xs)
                   | otherwise =            x : (wrap' xs)

      numbers_to_letters []     = []
      numbers_to_letters (x:xs) = (['A'..'Z'] !! x) : (numbers_to_letters xs)


--------------------------------------
------ general helper functions ------
--------------------------------------

drop' :: Int -> [a] -> [a]
drop' _ [] = []
drop' n xs@(_:xs')
   | n > 0     = drop' (n-1) xs'
   | otherwise = xs

take' :: Int -> [a] -> [a]
take' n _      | n <= 0 =  []
take' _ []              =  []
take' n (x:xs)          =  x : take' (n-1) xs

length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs