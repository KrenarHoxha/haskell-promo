--------------------------------------------------
-- Aufgabe 1: (Applikative) Funktoren
--------------------------------------------------

data Liste a
  = Nil
  | Cons a (Liste a)
  deriving (Show, Eq)


instance Functor Liste where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)


negateFunctor :: (Functor f, Num b) => f b -> f b
negateFunctor = fmap negate


instance Applicative Liste where
  pure x = Cons x (pure x)

  Nil <*> _ = Nil
  _ <*> Nil = Nil
  Cons f fs <*> Cons x xs = Cons (f x) (fs <*> xs)


zipWith' :: (a -> b -> c) -> Liste a -> Liste b -> Liste c
zipWith' f xs ys = f <$> xs <*> ys



--------------------------------------------------
-- Aufgabe 2: Monaden / Maybe-Konto
--------------------------------------------------

type Money = Int

-- Erste Komponente: Ausgaben / Schulden / Debit
-- Zweite Komponente: Einzahlungen / Guthaben / Credit
type Account = (Money, Money)

type Balance = Money


checkAccount :: Account -> Maybe Account
checkAccount acc@(debit, credit)
  | debit > credit = Nothing
  | otherwise      = Just acc


withdraw :: Money -> Account -> Maybe Account
withdraw amount (debit, credit) =
  checkAccount (debit + amount, credit)


deposit :: Money -> Account -> Maybe Account
deposit amount (debit, credit) =
  checkAccount (debit, credit + amount)


accountState :: Account -> Maybe Balance
accountState (debit, credit)
  | debit > credit = Nothing
  | otherwise      = Just (credit - debit)



-- Aufgabe 2(c): do-Notation

bayernDo :: Maybe Balance
bayernDo = do
  a1 <- deposit 150000000 (0, 0)
  a2 <- withdraw 100000000 a1
  a3 <- withdraw 40000000 a2
  a4 <- withdraw 15000000 a3
  a5 <- deposit 30000000 a4
  accountState a5


leverkusenDo :: Maybe Balance
leverkusenDo = do
  a1 <- deposit 20000000 (0, 0)
  a2 <- withdraw 15000000 a1
  a3 <- deposit 30000000 a2
  a4 <- deposit 20000000 a3
  a5 <- deposit 10000000 a4
  accountState a5



-- Aufgabe 2(d): mit >>=

bayernBind :: Maybe Balance
bayernBind =
  deposit 150000000 (0, 0)
    >>= withdraw 100000000
    >>= withdraw 40000000
    >>= withdraw 15000000
    >>= deposit 30000000
    >>= accountState


leverkusenBind :: Maybe Balance
leverkusenBind =
  deposit 20000000 (0, 0)
    >>= withdraw 15000000
    >>= deposit 30000000
    >>= deposit 20000000
    >>= deposit 10000000
    >>= accountState



--------------------------------------------------
-- Aufgabe 3: Box mit Notiz
--------------------------------------------------

data Box a
  = Full a
  | Empty String
  deriving (Show, Eq)


instance Functor Box where
  fmap f (Full x) = Full (f x)
  fmap _ (Empty note) = Empty note


instance Applicative Box where
  pure = Full

  Full f <*> Full x = Full (f x)
  Empty note <*> _ = Empty note
  _ <*> Empty note = Empty note


instance Monad Box where
  Full x >>= f = f x
  Empty note >>= _ = Empty note