import Control.Monad


-- Aufgabe 3

chainAction1 :: Monad m => a -> [a -> m a] -> m a
chainAction1 x [] =
    return x

chainAction1 x (f : fs) = do
    y <- f x
    chainAction1 y fs


chainAction2 :: Monad m => a -> [a -> m a] -> m a
chainAction2 x [] =
    return x

chainAction2 x (f : fs) =
    f x >>= \y -> chainAction2 y fs


chainAction3 :: Monad m => a -> [a -> m a] -> m a
chainAction3 =
    foldM (flip ($))


tellOp :: (Show a, Show b) => (a -> b) -> a -> IO b
tellOp f x =
    let fx = f x
    in do
        putStrLn $
            show x ++ " -> " ++ show fx

        return fx


test :: [Int -> IO Int]
test =
    map tellOp
        [ (*3)
        , (+1)
        , (`mod` 7)
        , (+5)
        , (*2)
        ]


-- Aufgabe 4

data Logger a =
    Logger a [String]


instance Functor Logger where
    fmap f (Logger x logs) =
        Logger (f x) logs


instance Applicative Logger where
    pure x =
        Logger x []

    Logger f logs1 <*> Logger x logs2 =
        Logger
            (f x)
            (logs1 ++ logs2)


instance Monad Logger where
    Logger x logs1 >>= f =
        let Logger y logs2 = f x
        in Logger
            y
            (logs1 ++ logs2)

    return = pure


instance Show a => Show (Logger a) where
    show (Logger value logs) =
        show value
            ++ "\n"
            ++ unlines (reverse logs)


data Match =
    Match
        { homeTeam  :: String
        , awayTeam  :: String
        , homeScore :: Int
        , awayScore :: Int
        }


instance Show Match where
    show m =
        home ++ " - " ++ away
      where
        home =
            homeTeam m
                ++ " "
                ++ show (homeScore m)

        away =
            show (awayScore m)
                ++ " "
                ++ awayTeam m


startMatch :: String -> String -> Logger Match
startMatch h a =
    Logger
        (Match h a 0 0)
        ["Das Spiel beginnt."]


endMatch :: Match -> Logger Match
endMatch m =
    Logger
        m
        ["Das Spiel ist zu Ende."]


scoreHome :: String -> Int -> Match -> Logger Match
scoreHome player minute match =
    let newMatch =
            match
                { homeScore =
                    homeScore match + 1
                }
    in Logger
        newMatch
        [ show minute
            ++ ". Minute: Tor für "
            ++ homeTeam match
            ++ " durch "
            ++ player
            ++ "."
        ]


scoreAway :: String -> Int -> Match -> Logger Match
scoreAway player minute match =
    let newMatch =
            match
                { awayScore =
                    awayScore match + 1
                }
    in Logger
        newMatch
        [ show minute
            ++ ". Minute: Tor für "
            ++ awayTeam match
            ++ " durch "
            ++ player
            ++ "."
        ]


worldCupFinal :: Logger Match
worldCupFinal =
    startMatch "Argentinien" "Frankreich"
        >>= scoreHome "Messi" 23
        >>= scoreHome "Di María" 36
        >>= scoreAway "Mbappé" 80
        >>= scoreAway "Mbappé" 81
        >>= scoreHome "Messi" 108
        >>= scoreAway "Mbappé" 118
        >>= endMatch