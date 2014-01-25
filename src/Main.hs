-- | Main entry point to the application.
{-
*****************************************************

This is my private repository, documenting my progress on http://projecteuler.net/

You are technically and legally allowed to read and use this code as you please.

But you should not look in the code and rather try to solve the problems yourself.

Oh and btw. I might have inserted some "errors", so the results might be not 100% correct :-)

This repository is just an off-site backup of my code. It is not meant to spoil somenone 
else the fun of solving the problems.

Current state:
http://projecteuler.net/profile/stefan.schwetschke.png

*****************************************************
-}
module Main where

{-# OPTIONS_GHC -O2 #-}
import Data.Int (Int64)
import Data.Array.Unboxed
import Data.Foldable (maximumBy, toList)
import qualified Data.Sequence as Seq

import Data.List (maximumBy)
import Data.Function (on)

import Data.List.Split (splitOn, chunksOf)
--import qualified Data.Array.Repa as R -- Currently no repa on FPcomplete available
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Data.List (nub)
import qualified Data.HashMap.Strict as Map

import Data.List
import Data.Ord
import qualified Data.MemoCombinators as Memo

import Data.Array.ST
import Data.Array.Base
import Control.Monad.ST
import Data.Bits

import Data.Char (digitToInt)

import Numeric.SpecFunctions (factorial)

import System.IO.Unsafe

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

problem9 = [(a,b,dist a b, round(a*b*dist a b)) | a <- [1..1000], b <- [1..1000], sqr(a)+sqr(b)==sqr(1000-a-b)]
    where
        dist a b = sqrt(sqr(a)+sqr(b)) 
        sqr x = x*x

-- Problem10

problem10 = foldr (+) 0 $ primesToA 2000000

-- Problem11

input11raw="\
\08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08|\
\49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00|\
\81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65|\
\52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91|\
\22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80|\
\24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50|\
\32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70|\
\67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21|\
\24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72|\
\21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95|\
\78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92|\
\16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57|\
\86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58|\
\19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40|\
\04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66|\
\88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69|\
\04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36|\
\20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16|\
\20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54|\
\01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48"



problem11 = maximum $ map (product) $ map (map (\(x,y) -> inputMatrix V.! y V.! x)) indexes
    where
        d = V.length inputMatrix
        inputList = map (map (\s -> read s :: Int)) $ map (splitOn " ") $ splitOn "|" input11raw
        inputMatrix = V.fromList $ map (V.fromList) inputList
        directDeltas=[[(0,0),(0,1),(0,2),(0,3)],[(0,0),(1,0),(2,0),(3,0)],[(0,0),(1,1),(2,2),(3,3)], [(3,0),(2,1),(1,2),(0,3)]]
        allIndexes = [ map (\(dx,dy) -> (x+dx, y+dy)) delta | x <- [0..d-1], y <- [0..d-1], delta <- directDeltas]
        indexes = filter (all (\(x,y) -> x<d && y<d)) allIndexes

-- Problem 12

-- primeFactors x = primeFactors (nPrimes (sqrt x) x
intSquareRoot :: Int -> Int
intSquareRoot = floor . sqrt . (fromIntegral :: Int -> Double)

-- Result is a list of tuple (factor, multiplicity)
-- primeFactors n = primeFactors' n (primesToA $ intSquareRoot n) []
primeFactors' :: Int ->  [Integer] ->  [(Int,Int)] -> [(Int,Int)]
primeFactors' 1 _ fs = fs
primeFactors' _ [] _ = error "Not enough prime factors given"
primeFactors' n primes ((f,m):fs)
    |  n `mod` f == 0 = primeFactors'  (n `div` f) primes ((f, m+1):fs)
primeFactors' n (p:ps) factors
    | n `mod` (fromIntegral p) == 0 = primeFactors' n ps ((fromIntegral p,0):factors)
    | otherwise = primeFactors' n ps factors


problem12 = head . filter (\ n -> numOfDivisors n > 500) $ triangulars
    where
        primes = nPrimes 5000
        primeFactors x = primeFactors' x primes []
        numOfDivisors = product . map (1+) . map snd . primeFactors
        triangulars = [n*(n+1) `div` 2 | n <- [1..]]

-- Problem 13
input13raw="\
\37107287533902102798797998220837590246510135740250\
\62177842752192623401942399639168044983993173312731\
\32924185707147449566916674687634660915035914677504\
\99518671430235219628894890102423325116913619626622\
\73267460800591543471830798392868535206946944540724\
\76841822524674417141514036427982273348055556214818\
\97142617910342598647204516893989422179826088076852\
\87783646182799346313767754307809363333018982642090\
\10848802521674670883715120185883543223812876952786\
\71329612474782464538636993009049310363619763878039\
\62184073572399794823406235393808339651327408011116\
\66627891981488087797941876876144230030984490851411\
\60661826293682836764744779239180335110989069790714\
\85786944089552960653640447425576083659976645795096\
\66024396409905389607120198219976047599490197230297\
\64913982680032973156037520041377903785566085089252\
\16730939319872750275468906903707539413042652315011\
\94809377245058795150954100921645863754710598436791\
\78639167021187492431995700641917969777599028300699\
\15368713711936614952811305876380278410754449733078\
\40789923115535562561142322423255033685442488917353\
\44889911501440648020369068063960672322193204149535\
\41503128880339536053299340368006977710650566631954\
\81234880673210146739058568457934581403627822703280\
\82616570773948327592232845341706525094512325230608\
\22918802058777319719839450280888072429661980811197\
\77158542502016545090413245809786882778948721859617\
\72107838435069186155435662884062257473692284509516\
\20849603980134001723930671666823555245252804609722\
\53503534226472524250874054075591789781264330331690"


input13 = map (\s -> read s :: Integer) $ chunksOf 50 input13raw
problem13 = take 10 $ show $ sum input13

-- Problem 14
toInt i = (fromIntegral i) :: Int
toInt64 i = (fromIntegral i) :: Int64

collatzLength :: Integer -> Integer -> Integer
collatzLength cacheSize n = collatzLengthCached n
    where
        collatzLengthCached :: Integer -> Integer
        collatzLengthCached = (Memo.arrayRange $! (1,cacheSize)) collatzLengthCached'
        collatzLengthCached' n
            | n < 1  = 0 -- error "Number too low"
            | n == 1 = 1
            | odd n  = 1 + collatzLengthCached (3 * n + 1)
            | even n = 1 + collatzLengthCached (n `div` 4)

collatzLengthCached :: Integer -> Integer
collatzLengthCached = Memo.arrayRange (1,10000000) collatzLengthCached'
    where
        collatzLengthCached' n
            | n < 1  = 0 -- error "Number too low"
            | n == 1 = 1
            | odd n  = 1 + collatzLengthCached (3 * n + 1)
            | even n = 1 + collatzLengthCached (n `div` 4)

problem14 =
    let size = 999999
        in
        foldl1' max $ [(collatzLengthCached n, n) | n <- [1..size]]

-- Problem 15
problem15 = map numMoves [1,2,3,20]
    where
        numMoves i = factorialInt (i*2) `div` sqr (factorialInt i)
        sqr i = i * i
        factorialInt i = product [0..i]

-- Problem 16
problem16 = sum $ map digitToInt $ show $ foldl (*) 1 $ replicate 10000 2

-- Problem 17
input17raw="75|\
\25 34|\
\17 47 82|\
\18 35 87 10|\
\20 04 82 47 65|\
\19 01 23 75 03 34|\
\88 02 77 73 07 63 67|\
\99 65 04 28 06 16 70 92|\
\41 41 26 56 83 40 80 70 33|\
\41 48 72 33 47 32 37 16 94 29|\
\53 71 44 65 25 43 91 52 97 51 14|\
\70 11 33 28 77 73 17 78 39 68 17 57|\
\91 71 52 38 17 14 91 43 58 50 27 29 48|\
\63 66 04 68 89 53 67 30 73 16 69 87 40 31|\
\04 62 98 27 23 09 70 28 73 93 38 53 60 04 23"

input17rawSmall = "3|\
\7 4|\
\2 4 6|\
\8 5 9 3"

parseMatrix lineSplit input= map (map (\s -> read s :: Int)) $ map (splitOn " ") $ splitOn lineSplit input

input17 = parseMatrix "|" input17raw

maxPathSum m = maximum $ foldl maxLine (head m) (tail m)
    where
        maxLine a b = map maxSum $ zip3 (0:a) (a ++ [0]) b
        maxSum (left, right, down) = maximum [left+down, right+down]

problem17 = maxPathSum input17

-- Problem 67
input67 = unsafePerformIO readTriangle
    where
        readTriangle = do
            input <- readFile "triangle.txt"
            return (parseMatrix "\n" input)

problem67 = maxPathSum input67

-- | The main entry point.
main :: IO ()
main = putStrLn $ show $ problem67


