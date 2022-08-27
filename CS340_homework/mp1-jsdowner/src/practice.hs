--a = [\lambda x | x in [1..10]]

evens = [2*x - 2*x + 3 | x <- [1..]]

five_ev = take 5 evens

mystery3 :: (a -> b -> c) -> [a] -> b -> [c]
mystery3 _ [] _ = []
mystery3 f (x:xs) y = f x y : mystery3 f xs y

sec' :: (a,b) -> b
sec' (a, b) = b

fst' :: a -> b -> a
fst' a b = a


--f g x y = f (g x y) x
--f: a > b > c
--g: b > d > a
--x: b
--y: d

--(b > d) > c

--outside functions -> inside functions -> ... -> inside arguments -> final output

--(a > b > c) > (b > d > a) > b > d > c

