-- Aufgabe 3

data Foot = LeftF | RightF
  deriving (Show, Eq)

data Position =
    Goalkeeper
  | Defender
  | Midfielder
  | Forward
  deriving (Show, Eq)

data Player = Player
  String
  String
  Int
  Foot
  Position
  deriving (Show, Eq)


-- Beispielspieler

messi = Player "Messi" "Inter Miami" 10 LeftF Forward
neuer = Player "Neuer" "Bayern" 1 RightF Goalkeeper
kimmich = Player "Kimmich" "Bayern" 6 RightF Midfielder
davies = Player "Davies" "Bayern" 19 LeftF Defender
kane = Player "Kane" "Bayern" 9 RightF Forward


-- Aufgabe 4a

sameTeam :: [Player] -> Bool
sameTeam [] = True
sameTeam [_] = True
sameTeam (Player _ t1 _ _ _ : Player _ t2 _ _ _ : xs) =
  t1 == t2 &&
  sameTeam (Player "" t2 0 RightF Goalkeeper : xs)


-- Aufgabe 4b

uniqueNumbers :: [Player] -> Bool
uniqueNumbers [] = True
uniqueNumbers (Player _ _ n _ _ : xs) =
  notElem n [m | Player _ _ m _ _ <- xs]
  && uniqueNumbers xs


-- Aufgabe 4c

coverPositions :: [Player] -> Bool
coverPositions xs =
  has Goalkeeper &&
  has Defender &&
  has Midfielder &&
  has Forward
  where
    positions = [p | Player _ _ _ _ p <- xs]
    has p = elem p positions