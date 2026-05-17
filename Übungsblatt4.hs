-- Aufgabe 1: Palindrom

isPalindrome :: String -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome xs =
  head xs == last xs && isPalindrome (init (tail xs))


-- Aufgabe 2: Sieb des Eratosthenes

sieve' :: [Integer] -> [Integer]
sieve' [] = []
sieve' (x:xs) =
  x : sieve' [y | y <- xs, y `mod` x /= 0]


-- Aufgabe 3: Quicksort

quicksort :: [Integer] -> [Integer]
quicksort [] = []
quicksort (x:xs) =
  quicksort [y | y <- xs, y <= x]
  ++ [x]
  ++ quicksort [y | y <- xs, y > x]


-- Aufgabe 4a

f :: Int -> Int
f 0 = 1
f 9 = 10
f n = n * f (n - 2)


-- Aufgabe 4b

g :: [Int] -> Int
g xs = gg xs 0
  where
    gg xs acc
      | null xs = acc
      | otherwise = gg xs (2 * head xs + acc)


-- Aufgabe 4c

summation :: Int -> Int
summation 0 = 0
summation n = n + summation (n - 1)


-- Aufgabe 4d

maximal :: [Int] -> Int
maximal (x:xs) = max' x xs
  where
    max' cmax [] = cmax
    max' cmax (x:xs)
      | x > cmax = max' x xs
      | otherwise = max' cmax xs