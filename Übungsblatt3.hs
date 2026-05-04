-- Aufgabe 1: Notenstufen

note :: Double -> (Integer, String)
note x
  | x >= 87.5 = (1, "sehr gut")
  | x >= 75   = (2, "gut")
  | x >= 62.5 = (3, "befriedigend")
  | x >= 50   = (4, "ausreichend")
  | otherwise = (5, "nicht ausreichend")

  -- Aufgabe 2: Steuerberechnung

tax :: Double -> Double
tax income
  | income <= 10000 = 0
  | income <= 30000 = (income - 10000) * 0.10
  | income <= 70000 = 20000 * 0.10
                    + (income - 30000) * 0.20
  | otherwise       = 20000 * 0.10
                    + 40000 * 0.20
                    + (income - 70000) * 0.30

-- Aufgabe 3: Substitutionsmodell

-- Gegeben:

foo [] = []
foo (x:xs) =
  if x `mod` 2 == 0
  then x : foo xs
  else foo xs

bar y [] = [y]
bar y (x:xs) = x : bar y xs

baz z = z + 1

bar (baz 4) [9, 12, 18]

foo [9, 12, baz 17]

bar 18 (foo [9, 12])

-- Aufgabe 4: Rekursion mit Listen

length' :: [a] -> Integer
length' [] = 0
length' (_:xs) = 1 + length' xs

length'' :: [a] -> Integer
length'' xs = go xs 0
  where
    go [] acc = acc
    go (_:ys) acc = go ys (acc + 1)

    reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

reverse'' :: [a] -> [a]
reverse'' xs = go xs []
  where
    go [] acc = acc
    go (y:ys) acc = go ys (y:acc)

maximum' :: Ord a => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs) =
  let m = maximum' xs
  in if x > m then x else m

maximum'' :: Ord a => [a] -> a
maximum'' [] = error "maximum of empty list"
maximum'' (x:xs) = go xs x
  where
    go [] acc = acc
    go (y:ys) acc
      | y > acc   = go ys y
      | otherwise = go ys acc

take' :: Integer -> [a] -> [a]
take' n _
  | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n - 1) xs

take'' :: Integer -> [a] -> [a]
take'' n xs = reverse'' (go n xs [])
  where
    go k _ acc
      | k <= 0 = acc
    go _ [] acc = acc
    go k (y:ys) acc = go (k - 1) ys (y:acc)

