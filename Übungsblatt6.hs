data Rolle = Crewmate | Impostor

data Zustand = Tot | Lebendig

data Farbe = Blau | Rot | Lila | Gelb | Rosa

data Astronaut = Astronaut
  { benutzername :: String
  , rolle        :: Rolle
  , zustand      :: Zustand
  , aufgaben     :: [String]
  , farbe        :: Farbe
  }

data Rolle = Crewmate | Impostor
  deriving (Show, Eq, Enum, Bounded)

data Zustand = Tot | Lebendig
  deriving (Show, Eq, Enum, Bounded)

data Farbe = Blau | Rot | Lila | Gelb | Rosa
  deriving (Show, Eq, Enum, Bounded)

data Astronaut = Astronaut
  { benutzername :: String
  , rolle        :: Rolle
  , zustand      :: Zustand
  , aufgaben     :: [String]
  , farbe        :: Farbe
  }

  instance Show Astronaut where
  show astronaut =
    benutzername astronaut ++ ":" ++ show (farbe astronaut)

instance Eq Astronaut where
  astronaut1 == astronaut2 =
    benutzername astronaut1 == benutzername astronaut2
    && farbe astronaut1 == farbe astronaut2

istCrewmate :: Astronaut -> Bool
istCrewmate astronaut =
  rolle astronaut == Crewmate

istImpostor :: Astronaut -> Bool
istImpostor astronaut =
  rolle astronaut == Impostor

istLebendig :: Astronaut -> Bool
istLebendig astronaut =
  zustand astronaut == Lebendig

istTot :: Astronaut -> Bool
istTot astronaut =
  zustand astronaut == Tot


data ML a = E | L a (ML a) deriving Show


-- (a)
liste1234 :: ML Integer
liste1234 = L 1 (L 2 (L 3 (L 4 E)))


-- (b)
myHead :: ML a -> a
myHead E = error "empty list"
myHead (L x _) = x


-- (c)
myAppend :: ML a -> ML a -> ML a
myAppend E ys = ys
myAppend (L x xs) ys = L x (myAppend xs ys)


-- (d)
myAdd :: Num a => ML a -> ML a -> ML a
myAdd E _ = E
myAdd _ E = E
myAdd (L x xs) (L y ys) = L (x + y) (myAdd xs ys)


-- (e)
myString :: Show a => ML a -> String
myString E = ""
myString (L x E) = show x
myString (L x xs) = show x ++ ", " ++ myString xs


-- (f)
myLess :: Ord a => ML a -> ML a -> Bool
myLess E E = False
myLess E (L _ _) = True
myLess (L _ _) E = False
myLess (L x xs) (L y ys)
  | x < y     = True
  | x > y     = False
  | otherwise = myLess xs ys