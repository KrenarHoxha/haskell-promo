import Control.Monad ((<=<))
import Data.List (nub, sort)


-- Aufgabe 1

succ' :: Integer -> Integer
succ' x = x + 1

pred' :: Integer -> Integer
pred' x = x - 1

opp' :: Integer -> Integer
opp' x = -x


plus :: Integer -> Integer -> Integer
plus x 0 = x
plus x y
    | y > 0 = plus (succ' x) (pred' y)
    | y < 0 = plus (pred' x) (succ' y)


minus :: Integer -> Integer -> Integer
minus x y =
    plus x (opp' y)


mult :: Integer -> Integer -> Integer
mult _ 0 = 0
mult x y
    | y > 0 = plus x (mult x (pred' y))
    | y < 0 = opp' (mult x (opp' y))


fact :: Integer -> Integer
fact 0 = 1
fact n
    | n > 0 = mult n (fact (pred' n))
    | n < 0 = mult n (fact (succ' n))


-- Aufgabe 2

class Default a where
    def :: a


instance Default Integer where
    def = 0


instance Default Bool where
    def = False


instance Default (Maybe a) where
    def = Nothing


instance Default [a] where
    def = []


instance Default a => Default (Either a b) where
    def = Left def


instance Default () where
    def = ()


instance (Default a, Default b) => Default (a, b) where
    def = (def, def)


instance Default b => Default (a -> b) where
    def _ = def


failSafe :: Default a => Maybe a -> a
failSafe Nothing  = def
failSafe (Just x) = x


-- Aufgabe 3

functorReplace :: Functor f => a -> f b -> f a
functorReplace value =
    fmap (\_ -> value)


brokenLine :: IO ()
brokenLine =
    functorReplace () getLine


don't :: Monad m => m a -> m ()
don't _ =
    return ()


-- Aufgabe 4

data Team = Team
    { name :: String
    , nW   :: Int
    , nD   :: Int
    , nL   :: Int
    , nGF  :: Int
    , nGA  :: Int
    }
    deriving (Eq, Show)


points :: Team -> Int
points team =
    3 * nW team + nD team


goalDifference :: Team -> Int
goalDifference team =
    nGF team - nGA team


instance Ord Team where
    compare team1 team2 =
        compare (points team1) (points team2)
            `mappend`
        compare
            (goalDifference team1)
            (goalDifference team2)
            `mappend`
        compare (name team2) (name team1)


leverkusen, stuttgart, bayern, dortmund :: Team
leipzig, wolfsburg, berlin, darmstadt :: Team

leverkusen =
    Team "Bayer 04 Leverkusen" 28 6 0 90 20

stuttgart =
    Team "VfB Stuttgart" 24 4 6 80 40

bayern =
    Team "Bayern Muenchen" 24 4 6 45 45

dortmund =
    Team "Borussia Dortmund" 20 5 9 60 25

leipzig =
    Team "Corporation Leipzig" 20 5 9 60 25

wolfsburg =
    Team "Wolfsburg" 10 20 4 50 30

berlin =
    Team "Union Berlin" 10 20 4 35 30

darmstadt =
    Team "SV Darmstadt 98" 1 10 10 15 50


bundesliga :: [Team]
bundesliga =
    [ bayern
    , berlin
    , darmstadt
    , dortmund
    , leipzig
    , leverkusen
    , stuttgart
    , wolfsburg
    ]


ranking :: [Team]
ranking =
    reverse (sort bundesliga)


-- Aufgabe 5

data Chain a
    = End
    | Link a (Chain a)
    deriving (Eq, Show)


instance Functor Chain where
    fmap _ End =
        End

    fmap f (Link x xs) =
        Link (f x) (fmap f xs)


instance Foldable Chain where
    foldMap _ End =
        mempty

    foldMap f (Link x xs) =
        f x `mappend` foldMap f xs


instance Traversable Chain where
    traverse _ End =
        pure End

    traverse f (Link x xs) =
        Link <$> f x <*> traverse f xs


safeEven :: Integer -> Maybe Integer
safeEven n
    | even n    = Just n
    | otherwise = Nothing


allEven :: Chain Integer -> Maybe (Chain Integer)
allEven =
    traverse safeEven


-- Aufgabe 6

type KnightPos = (Int, Int)


onBoard :: KnightPos -> Bool
onBoard (column, row) =
    column >= 1
        && column <= 8
        && row >= 1
        && row <= 8


moveKnight :: KnightPos -> [KnightPos]
moveKnight (column, row) =
    filter onBoard
        [ (column + 2, row + 1)
        , (column + 2, row - 1)
        , (column - 2, row + 1)
        , (column - 2, row - 1)
        , (column + 1, row + 2)
        , (column + 1, row - 2)
        , (column - 1, row + 2)
        , (column - 1, row - 2)
        ]


in3Moves :: KnightPos -> [KnightPos]
in3Moves start = do
    firstPosition  <- moveKnight start
    secondPosition <- moveKnight firstPosition
    moveKnight secondPosition


reachIn3Moves :: KnightPos -> KnightPos -> Bool
reachIn3Moves start target =
    target `elem` in3Moves start


inXMoves :: Int -> KnightPos -> [KnightPos]
inXMoves numberOfMoves
    | numberOfMoves < 0 = const []
    | otherwise =
        foldr
            (<=<)
            return
            (replicate numberOfMoves moveKnight)


reachInXMoves :: Int -> KnightPos -> KnightPos -> Bool
reachInXMoves numberOfMoves start target =
    target `elem` inXMoves numberOfMoves start


knightDistance :: KnightPos -> KnightPos -> Int
knightDistance start target
    | not (onBoard start) =
        error "Invalid start position"

    | not (onBoard target) =
        error "Invalid target position"

    | otherwise =
        bfs 0 [start] [start]
  where
    bfs distance frontier visited
        | target `elem` frontier =
            distance

        | null frontier =
            error "Target is unreachable"

        | otherwise =
            let nextFrontier =
                    nub
                        [ next
                        | current <- frontier
                        , next <- moveKnight current
                        , next `notElem` visited
                        ]
            in bfs
                (distance + 1)
                nextFrontier
                (visited ++ nextFrontier)


fromChessNotation :: String -> Maybe KnightPos
fromChessNotation [file, rank] = do
    column <- lookup file (zip ['a' .. 'h'] [1 .. 8])
    row    <- lookup rank (zip ['1' .. '8'] [1 .. 8])
    return (column, row)

fromChessNotation _ =
    Nothing


toChessNotation :: KnightPos -> Maybe String
toChessNotation (column, row) = do
    file <- lookup column (zip [1 .. 8] ['a' .. 'h'])
    rank <- lookup row (zip [1 .. 8] ['1' .. '8'])
    return [file, rank]


-- Aufgabe 7

data Pokemon = Pokemon
    { pID     :: Int
    , pName   :: String
    , pHealth :: Integer
    , pAttack :: Integer
    , pType   :: String
    }


instance Show Pokemon where
    show poke =
        show (pID poke)
            ++ ": "
            ++ pName poke
            ++ " / HP "
            ++ show (pHealth poke)
            ++ " / AP "
            ++ show (pAttack poke)
            ++ " / TYPE "
            ++ pType poke


pokeParse :: String -> Pokemon
pokeParse line =
    case words line of
        [idText, nameText, healthText, attackText, typeText] ->
            Pokemon
                { pID     = read idText
                , pName   = nameText
                , pHealth = read healthText
                , pAttack = read attackText
                , pType   = typeText
                }

        _ ->
            error "Invalid Pokemon data"


pokeLoad :: FilePath -> IO [Pokemon]
pokeLoad path = do
    fileContents <- readFile path
    return (map pokeParse (lines fileContents))


pokeBattle :: [Pokemon] -> IO ()
pokeBattle pokemons = do
    putStrLn "> Player1, choose a Pokemon (by ID)"
    player1Input <- getLine

    let player1Pokemon =
            pokemons !! read player1Input

    putStrLn $ "(P1) " ++ show player1Pokemon

    putStrLn "> Player2, choose a Pokemon (by ID)"
    player2Input <- getLine

    let player2Pokemon =
            pokemons !! read player2Input

    putStrLn $ "(P2) " ++ show player2Pokemon

    putStrLn $
        "Battle! "
            ++ pName player1Pokemon
            ++ " vs "
            ++ pName player2Pokemon

    battleLoop player1Pokemon player2Pokemon 1


battleLoop :: Pokemon -> Pokemon -> Int -> IO ()
battleLoop player1 player2 turn
    | pHealth player1 <= 0 =
        putStrLn $
            pName player1
                ++ " fainted! "
                ++ pName player2
                ++ " wins!"

    | pHealth player2 <= 0 =
        putStrLn $
            pName player2
                ++ " fainted! "
                ++ pName player1
                ++ " wins!"

    | otherwise = do
        putStrLn $ "----- Turn " ++ show turn

        putStrLn $
            pName player1
                ++ " / HP "
                ++ show (pHealth player1)

        putStrLn $
            pName player2
                ++ " / HP "
                ++ show (pHealth player2)

        if odd turn
            then do
                putStrLn $
                    "Turn for P1 ("
                        ++ pName player1
                        ++ " attacks):"

                putStrLn $
                    pName player1
                        ++ " attacks for "
                        ++ show (pAttack player1)
                        ++ " damage!"

                let damagedPlayer2 =
                        player2
                            { pHealth =
                                pHealth player2
                                    - pAttack player1
                            }

                battleLoop
                    player1
                    damagedPlayer2
                    (turn + 1)

            else do
                putStrLn $
                    "Turn for P2 ("
                        ++ pName player2
                        ++ " attacks):"

                putStrLn $
                    pName player2
                        ++ " attacks for "
                        ++ show (pAttack player2)
                        ++ " damage!"

                let damagedPlayer1 =
                        player1
                            { pHealth =
                                pHealth player1
                                    - pAttack player2
                            }

                battleLoop
                    damagedPlayer1
                    player2
                    (turn + 1)


-- Aufgabe 8

reciprocalsSquared :: [Double]
reciprocalsSquared =
    [1 / n^2 | n <- [1 ..]]


piApproxRZ2 :: Int -> Double
piApproxRZ2 numberOfTerms =
    sqrt
        (6 * sum (take numberOfTerms reciprocalsSquared))


reciprocalsFourth :: [Double]
reciprocalsFourth =
    [1 / n^4 | n <- [1 ..]]


reciprocalsEighth :: [Double]
reciprocalsEighth =
    [1 / n^8 | n <- [1 ..]]


piApproxRZ4 :: Int -> Double
piApproxRZ4 =
    (** 0.25)
        . (* 90)
        . sum
        . flip take reciprocalsFourth


piApproxRZ8 :: Int -> Double
piApproxRZ8 =
    (** 0.125)
        . (* 9450)
        . sum
        . flip take reciprocalsEighth


lazyFib :: [Integer]
lazyFib =
    1 : 1 : zipWith (+) lazyFib (tail lazyFib)


firstNFibonacci :: Int -> [Integer]
firstNFibonacci =
    flip take lazyFib


nthFibonacci :: Int -> Integer
nthFibonacci =
    (lazyFib !!) . subtract 1


lazyLucas :: [Integer]
lazyLucas =
    2 : 1 : zipWith (+) lazyLucas (tail lazyLucas)


lazyCheck :: [Bool]
lazyCheck =
    zipWith3
        (\lucasValue previousFib nextFib ->
            lucasValue == previousFib + nextFib)
        (drop 1 lazyLucas)
        (0 : lazyFib)
        (drop 2 (0 : lazyFib))


lazyPhiApprox :: [Integer] -> Int -> Double
lazyPhiApprox sequenceValues index =
    zipWith
        (/)
        (map fromInteger (drop 1 sequenceValues))
        (map fromInteger sequenceValues)
        !! index


goldenRatioFib :: Int -> Double
goldenRatioFib =
    lazyPhiApprox lazyFib


goldenRatioLucas :: Int -> Double
goldenRatioLucas =
    lazyPhiApprox lazyLucas


sqrt2CF, sqrt3CF, sqrt5CF :: [Integer]
sqrt10CF, sqrt17CF, sqrt23CF :: [Integer]
sqrt30CF, phiCF, eCF :: [Integer]

sqrt2CF =
    1 : repeat 2

sqrt3CF =
    1 : cycle [1, 2]

sqrt5CF =
    2 : repeat 4

sqrt10CF =
    3 : repeat 6

sqrt17CF =
    4 : repeat 8

sqrt23CF =
    4 : cycle [1, 3, 1, 8]

sqrt30CF =
    5 : repeat 10

phiCF =
    repeat 1

eCF =
    2 : concat
        [ [1, 2 * k, 1]
        | k <- [1 ..]
        ]


evalCF :: Int -> [Integer] -> Double
evalCF numberOfTerms
    | numberOfTerms <= 0 =
        error "evalCF requires at least one term"

    | otherwise =
        foldr1
            (\current rest ->
                current + 1 / rest)
            . map fromInteger
            . take numberOfTerms