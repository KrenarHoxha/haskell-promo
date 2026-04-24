-- Übungsblatt 2: List Comprehension

-- Aufgabe 2a
alleGleich :: Integer -> Integer -> Integer -> Bool
alleGleich x y z = x == y && y == z

-- Aufgabe 2b
ungerade :: Integer -> Bool
ungerade x = x `mod` 2 /= 0

-- Aufgabe 3a
ende :: String -> Char
ende [x] = x
ende (_:xs) = ende xs

-- Aufgabe 3b
rest :: String -> String
rest (_:xs) = xs

-- Aufgabe 4a
laenge :: String -> Integer
laenge xs = sum [1 | _ <- xs]

-- Aufgabe 4b
vokale :: String -> String
vokale xs = [x | x <- xs, elem x "aeiouAEIOU"]

-- Aufgabe 4c
faktoren :: Integer -> [Integer]
faktoren n = [x | x <- [1..n], n `mod` x == 0]

-- Aufgabe 4d
div8Rest4 :: Integer -> Integer -> [Integer]
div8Rest4 a b = [x | x <- [a..b], x `mod` 8 == 4]

-- Aufgabe 4e
geradeZahlen2 :: [Integer] -> [Integer]
geradeZahlen2 xs = [if x `mod` 2 == 0 then x else x * 2 | x <- xs]

-- Aufgabe 4f
pytri :: Integer -> [(Integer, Integer, Integer)]
pytri n = [(a,b,c) | a <- [1..n], b <- [1..n], c <- [1..n], a^2 == b^2 + c^2]

-- Aufgabe 4g
fermat :: Integer -> [(Integer, Integer)]
fermat p =
  [(x,y) |
    x <- [-(floor (sqrt (fromIntegral p))) .. floor (sqrt (fromIntegral p))],
    y <- [-(floor (sqrt (fromIntegral p))) .. floor (sqrt (fromIntegral p))],
    x^2 + y^2 == p
  ]