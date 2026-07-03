--------------------------------------------------
-- Aufgabe 1: Monoide
--------------------------------------------------

-- Aufgabe 1(a): Komplexe Zahlen

data ComplexNumber = C (Double, Double)

instance Show ComplexNumber where
  show (C (a, b))
    | b < 0     = show a ++ " - " ++ show (abs b) ++ "i"
    | otherwise = show a ++ " + " ++ show b ++ "i"

instance Semigroup ComplexNumber where
  C (a, b) <> C (c, d) = C (a * c - b * d, a * d + b * c)

instance Monoid ComplexNumber where
  mempty = C (1, 0)


-- Aufgabe 1(b): RGB-Farben

data RGB = RGB Int Int Int
  deriving Show

clamp255 :: Int -> Int
clamp255 x = min 255 x

instance Semigroup RGB where
  RGB r1 g1 b1 <> RGB r2 g2 b2 =
    RGB
      (clamp255 (r1 + r2))
      (clamp255 (g1 + g2))
      (clamp255 (b1 + b2))

instance Monoid RGB where
  mempty = RGB 0 0 0


--------------------------------------------------
-- Aufgabe 2: Monoid auf {0,1}
--------------------------------------------------

op :: Int -> Int -> Int
op 0 0 = 1
op 0 1 = 0
op 1 0 = 0
op 1 1 = 1
op _ _ = error "op ist nur für 0 und 1 definiert"

instance Semigroup Int where
  (<>) = op

instance Monoid Int where
  mempty = 1


--------------------------------------------------
-- Aufgabe 3: Funktoren
--------------------------------------------------

-- Aufgabe 3(a): Triple

data Triple a = Triple a a a
  deriving Eq

instance Show a => Show (Triple a) where
  show (Triple a b c) =
    "(" ++ show a ++ ", " ++ show b ++ ", " ++ show c ++ ")"


-- Aufgabe 3(b): Zugriffsfunktionen

tfst :: Triple a -> a
tfst (Triple a _ _) = a

tsnd :: Triple a -> a
tsnd (Triple _ b _) = b

ttrd :: Triple a -> a
ttrd (Triple _ _ c) = c


-- Aufgabe 3(c): Kreuzprodukt

x :: Num a => Triple a -> Triple a -> Triple a
x (Triple a b c) (Triple d e f) =
  Triple
    (b * f - c * e)
    (c * d - a * f)
    (a * e - b * d)


-- Aufgabe 3(d): Functor und Skalarmultiplikation

instance Functor Triple where
  fmap f (Triple a b c) =
    Triple (f a) (f b) (f c)

scalarMult :: Num a => a -> Triple a -> Triple a
scalarMult s =
  fmap (* s)