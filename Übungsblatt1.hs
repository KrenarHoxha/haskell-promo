module Main where

-- Aufgabe 3a
double :: Integer -> Integer
double x = x * 2

-- Aufgabe 3b
vierfach :: Integer -> Integer
vierfach x = double (double x)

main :: IO ()
main = do
    putStrLn "Übungsblatt 1 - Beispielprogramm"
    putStrLn ""

    putStrLn ("double 5 = " ++ show (double 5))
    putStrLn ("vierfach 5 = " ++ show (vierfach 5))

    putStrLn ""
    putStrLn "Listen Beispiele:"
    print ([1,2,3,4,5,6,7,8] !! 5 == 5)
    print (tail [0,1,10])
    print (take 3 "icecream")
    print (drop 4 "peacock")
    print (length [5,4,3,2,1])
    print (null [9,12,18])