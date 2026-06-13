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