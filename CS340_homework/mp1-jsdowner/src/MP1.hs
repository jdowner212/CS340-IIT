module MP1 where

import Data.Char
import Graphics.Gloss

-- Part 1: Polymorphic functions from types

p1_1 :: a -> (b -> b)
p1_1 x y = snd (x, y)

p1_2 :: (a -> b -> c) -> (a, b) -> c
p1_2 f (x, y)= f x y


p1_3 :: (a -> b) ->  (b -> c) ->  a -> c
p1_3 g h = h . g

p1_4 :: (a -> b -> c)  ->  a ->  (d -> b)  ->  d -> c
p1_4 f a b d = f a (b d)

-- Part 2: Function implementations 


-- 1. Transposes a 2-row x 2-column tuple.
--
--    e.g., transposeTup ((1,2),(3,4)) = ((1,3),(2,4))
transposeTup :: ((a,a),(a,a)) -> ((a,a),(a,a))
transposeTup ((a,b),(c,d)) = ((a,c),(b,d))


-- 2. Sorts the elements of a 3-tuple.
--
--    e.g., sort3Tup (2,1,3) = (1,2,3)
--          sort3Tup (3,2,1) = (1,2,3)
sort3Tup :: Ord a => (a,a,a) -> (a,a,a)
sort3Tup (a,b,c) | a <= b && b <= c = (a,b,c)
                 | a > b = sort3Tup (b,a,c)
                 | b > c = sort3Tup (a,c,b)



-- 3. Computes the compound interest earned.
--
--    e.g., compoundInterest 100 0.2 1 = 20
--          compoundInterest 100 0.2 2 = 44
compoundInterest :: Floating a => a -> a -> Int -> a
compoundInterest a b c | c == 1 = a * b
                       | otherwise = cI' a b 2 (a*b) c
                       where
                           cI' :: Floating a => a -> a-> Int -> a -> Int -> a
                           cI' a b c prev c0 | c == c0 = prev + ((a + prev) * b)
                           cI' a b c prev c0 | otherwise = cI' a b (c+1) (prev + (a + prev)*b) c0




-- 4. Computes the length of the Collatz sequence starting at the input.
--
--    e.g., collatzLen 10 = 7
--          collatzLen 27 = 112
collatzLen :: Integer -> Integer
collatzLen a | a == 1 = 1
             | a `mod` 2 == 0 = 1 + (collatzLen (a `div` 2))
             | a `mod` 2 == 1 = 1 + (collatzLen ((a * 3) + 1))
             | otherwise = undefined

-- 5. Computes the square root of the input using Newton's method.
--
--    e.g., newtonsSqrt 2 ~= 1.4142...
--          newtonsSqrt 1000 ~= 31.6227...
newtonsSqrt :: (Floating a, Ord a) => a -> a
newtonsSqrt n | abs(0.5*(n+1) - n) < 0.00001 = abs(n)
              | otherwise = newtonSqrt' (0.5*(n+1) - n) n
              where
                  newtonSqrt' :: (Floating a, Ord a) => a -> a -> a
                  newtonSqrt' x n | abs(0.5*(x+n/x) - x) < 0.00001 = abs(x)
                                  | otherwise = newtonSqrt' (0.5*(x + n/x)) n
   

-- 6. Draws a planet in a circular orbit given an orbital radius and period.
drawOrbit :: Float -> Float -> Float -> Picture
drawOrbit r p t = translate (x r p t) (y r p t) (circleSolid 10)
     where  
        x :: Float -> Float -> Float -> Float
        x r p t = r * cos ((2 * pi * t) / p)
        y :: Float -> Float -> Float -> Float
        y r p t = r * sin ((2 * pi * t) / p)

-- 7. Draws a planet in an elliptical orbit based on Kepler's equation.
drawOrbit' :: Float -> Float -> Float -> Float -> Picture
drawOrbit' a e p t = translate (x' r (theta' e big_E)) (y' r (theta' e big_E)) (circleSolid 10)
     where
        -- helper functions
        big_E' :: Float -> Float -> Float
        big_E' e m | abs(0.5 * (m + (m + e * sin m)) - m) < 0.00001 = m    -- set initial guess to m
                   | otherwise = big_E'' e (0.5 * (m + (m + e * sin m)))
            where
                big_E'' :: Float -> Float -> Float
                big_E'' e g | abs(0.5 * (g + (m + e * sin g)) - g) < 0.00001 = g
                            | otherwise = bYig_E'' e (0.5 * (g + (m + e * sin g)))
        theta' :: Float -> Float -> Float
        theta' a b | a < 1 = 2 * atan (newtonsSqrt ((1 + a)/(1 - a)) * tan (b / 2))
                   | otherwise = 0
        x' :: Float -> Float -> Float
        x' a b = a * cos b
        y' :: Float -> Float -> Float
        y' a b = a * sin b 

        -- constants
        n = 2 * pi / p
        big_M = n * t
        big_E = big_E' e big_M
        r = a * (1 - e * cos big_E)







