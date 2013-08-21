-- | Main entry point to the application.
{-
*****************************************************

This is my private repository, documenting my progress on http://projecteuler.net/

You are technically and legally allowed to read and use this code as you please.

But you should not look in the code and rather try to solve the problems yourself.

This repository is just an off-site backup of my code. It is not meant to spoil somenone 
else the fun of solving the problems.

Current state:
http://projecteuler.net/profile/stefan.schwetschke.png

*****************************************************
-}
module Main where

{-# OPTIONS_GHC -O2 #-}
import Data.Array.Unboxed
import Data.Foldable (maximumBy, toList)
import qualified Data.Sequence as Seq
 
-- Problem 1
problem1 = foldl (+) 0 [x | x <- [1 .. 999], x `mod` 3 == 0 || x `mod` 5 == 0 ] 

-- Problem 2
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
problem2 = foldl (+) 0 $ filter (\x -> x `mod` 2 == 0) $ takeWhile (<= 4000000) fibs

-- Problem 3
-- primes = 2 : 3 : ([5,7..] `minus` unionAll [[p*p, p*p+2*p..] | p <- tail primes])
primesToA m = sieve 3 (array (3,m) [(i,odd i) | i<-[3..m]]
                        :: UArray Int Bool)
  where
    sieve p a 
      | p*p > m   = 2 : [i | (i,True) <- assocs a]
      | a!p       = sieve (p+2) $ a//[(i,False) | i <- [p*p, p*p+2*p..m]]
      | otherwise = sieve (p+2) a

problem3 = head $ reverse $ filter (\x -> 600851475143 `mod` x == 0) $ primesToA $ floor  $ sqrt 600851475143

-- Problem 4
isPalindrom x = (show x) == (reverse $ show x)

problem4 = maximum $ [x*y | x <- [100 .. 999], y <- [100 .. 999], 
    isPalindrom $ x * y]

-- Problem 5
ggT y 0 = y
ggT x y = ggT y (x `mod` y)
kgV x y = x*y `div` (ggT x y)

problem5 = foldl (kgV) 1 [1..20]

-- Problem 6
sumOfSquares n = (2*n*n*n + 3*n*n + n) / 6
squareSum n = sqr((n+1)*n/2)
    where
        sqr n = n*n

problem6 = (squareSum 100) - (sumOfSquares 100)

-- Problem 7
nPrimes n = toList $ nPrimes' (Seq.fromList [2,3,5,7,11,13,17]) 19 n

nPrimes' :: Seq.Seq Integer -> Integer -> Int -> Seq.Seq Integer
nPrimes' primes i n 
    | isPrime' primes i && (Seq.length primes)+1>=n = primes Seq.|> i
    | isPrime' primes i = nPrimes' (primes Seq.|> i) (i+2) n
    | otherwise = nPrimes' primes (i+2) n

isPrime' primes n = 0 == Seq.length (Seq.dropWhileL (\i -> n `mod` i /= 0) $ Seq.takeWhileL (\i -> i*i<=n) primes)

problem7 = head $ reverse $ nPrimes 10001

-- Problem 8

string8 = "73167176531330624919225119674426574742355349194934\
\96983520312774506326239578318016984801869478851843\
\85861560789112949495459501737958331952853208805511\
\12540698747158523863050715693290963295227443043557\
\66896648950445244523161731856403098711121722383113\
\62229893423380308135336276614282806444486645238749\
\30358907296290491560440772390713810515859307960866\
\70172427121883998797908792274921901699720888093776\
\65727333001053367881220235421809751254540594752243\
\52584907711670556013604839586446706324415722155397\
\53697817977846174064955149290862569321978468622482\
\83972241375657056057490261407972968652414535100474\
\82166370484403199890008895243450658541227588666881\
\16427171479924442928230863465674813919123162824586\
\17866458359124566529476545682848912883142607690042\
\24219022671055626321111109370544217506941658960408\
\07198403850962455444362981230987879927244284909188\
\84580156166097919133875499200524063689912560717606\
\05886116467109405077541002256983155200055935729725\
\71636269561882670428252483600823257530420752963450"

-- paramorphism (generalizes catamorphisms, i.e. foldr)
--
paramorphism :: (a -> [a] -> b -> b) -> b -> [a] -> b
paramorphism phi base = step
  where step []     = base
        step (x:xs) = phi x xs (step xs)

problem8 = foldr (max) 0 $ map (foldr (*) 1) $ map (take 5) $ paramorphism phi [] $  0 : map (\c -> read [c] :: Int) string8
    where
        phi _ xs suffxs = xs : suffxs

-- Problem 9

problem9 = [(a,b,c a b, round(a*b*c a b)) | a <- [1..1000], b <- [1..1000], sqr(a)+sqr(b)==sqr(1000-a-b)]
    where
        c a b = sqrt(sqr(a)+sqr(b)) 
        sqr x = x*x

-- Problem10

problem10 = foldr (+) 0 $ primesToA 2000000

-- | The main entry point.
main :: IO ()
main = putStrLn $ show $ problem10

