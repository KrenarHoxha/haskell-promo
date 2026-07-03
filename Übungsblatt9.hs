--------------------------------------------------
-- Aufgabe 1: Binäre Suchbäume
--------------------------------------------------

data BinTree a = Empty | Node a (BinTree a) (BinTree a)
  deriving Show

leaf :: a -> BinTree a
leaf x = Node x Empty Empty


-- Aufgabe 1(a)

-- Einfügen von links nach rechts:
-- [6, 8, 12, 1, 3, 9, 5]
treeInserted :: BinTree Int
treeInserted =
  Node 6
    (Node 1
      Empty
      (Node 3
        Empty
        (leaf 5)))
    (Node 8
      Empty
      (Node 12
        (leaf 9)
        Empty))


-- Ausgeglichener binärer Suchbaum für dieselbe Menge:
treeBalanced :: BinTree Int
treeBalanced =
  Node 6
    (Node 3
      (leaf 1)
      (leaf 5))
    (Node 9
      (leaf 8)
      (leaf 12))


-- Aufgabe 1(b)

nodes :: Num a => BinTree b -> a
nodes Empty = 0
nodes (Node _ left right) =
  1 + nodes left + nodes right


-- Aufgabe 1(c)

search :: Ord a => a -> BinTree a -> Bool
search _ Empty = False
search x (Node y left right)
  | x == y    = True
  | x < y     = search x left
  | otherwise = search x right


-- Aufgabe 1(d)

expand :: Ord a => a -> BinTree a -> BinTree a
expand x Empty = leaf x
expand x (Node y left right)
  | x < y     = Node y (expand x left) right
  | x > y     = Node y left (expand x right)
  | otherwise = Node y left right


-- Aufgabe 1(e)

findMin :: Ord a => BinTree a -> Maybe a
findMin Empty = Nothing
findMin (Node x Empty _) = Just x
findMin (Node _ left _) = findMin left


findMax :: Ord a => BinTree a -> Maybe a
findMax Empty = Nothing
findMax (Node x _ Empty) = Just x
findMax (Node _ _ right) = findMax right


-- Aufgabe 1(f)

isBST :: Ord a => BinTree a -> Bool
isBST = check Nothing Nothing
  where
    check _ _ Empty = True
    check lower upper (Node x left right) =
      lowerOK &&
      upperOK &&
      check lower (Just x) left &&
      check (Just x) upper right
      where
        lowerOK =
          case lower of
            Nothing -> True
            Just lo -> lo < x

        upperOK =
          case upper of
            Nothing -> True
            Just hi -> x < hi


--------------------------------------------------
-- Aufgabe 2: Mathematische Terme als Bäume
--------------------------------------------------

data BinOp
  = Plus
  | Minus
  | Times
  | Division
  deriving Show

data UnOp
  = Negate
  deriving Show

data Term a
  = C a
  | BinTerm BinOp (Term a) (Term a)
  | UnTerm UnOp (Term a)
  deriving Show


-- Beispiel aus Aufgabe 2(a):
-- (5 + 4) * (3 - 2)

exampleTerm1 :: Term Int
exampleTerm1 =
  BinTerm Times
    (BinTerm Plus (C 5) (C 4))
    (BinTerm Minus (C 3) (C 2))


-- Beispiel aus Aufgabe 2(c):
-- (5 + (-4)) * (3 - 2)

exampleTerm2 :: Term Int
exampleTerm2 =
  BinTerm Times
    (BinTerm Plus
      (C 5)
      (UnTerm Negate (C 4)))
    (BinTerm Minus (C 3) (C 2))


-- Aufgabe 2(b) und 2(c)

eval :: Integral a => Term a -> a
eval (C x) = x

eval (UnTerm Negate t) =
  negate (eval t)

eval (BinTerm op left right) =
  case op of
    Plus     -> eval left + eval right
    Minus    -> eval left - eval right
    Times    -> eval left * eval right
    Division -> eval left `div` eval right


-- Aufgabe 2(d)

simplify :: Term a -> Term a
simplify (C x) = C x

simplify (UnTerm Negate t) =
  case simplify t of
    UnTerm Negate inner -> inner
    t'                  -> UnTerm Negate t'

simplify (BinTerm Plus left right) =
  case (simplify left, simplify right) of
    (x, UnTerm Negate y) -> BinTerm Minus x y
    (x, y)               -> BinTerm Plus x y

simplify (BinTerm Minus left right) =
  case (simplify left, simplify right) of
    (x, UnTerm Negate y) -> BinTerm Plus x y
    (x, y)               -> BinTerm Minus x y

simplify (BinTerm Times left right) =
  BinTerm Times (simplify left) (simplify right)

simplify (BinTerm Division left right) =
  BinTerm Division (simplify left) (simplify right)


--------------------------------------------------
-- Aufgabe 3: sumBT
--------------------------------------------------

sumBT :: Num a => BinTree a -> a
sumBT Empty = 0
sumBT (Node x left right) =
  x + sumBT left + sumBT right