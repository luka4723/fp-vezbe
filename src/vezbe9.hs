{-  newtype

    Kljucna rec "newtype" koristi se da se wrapp-uje postojeci tip u novi
        - Npr. ako zelimo da navedeni tip bude instanca neke klase na vise nacina
    
    Moze da se postigne isto sa "data" ali ovako radi brze
    
    Sme da ima samo jedan value konstruktor

-}

import qualified Control.Monad.Fail as Fail

-- primer newtype

data Piksel a = Color a a a | GScale a deriving (Show)

instance Functor Piksel where
    fmap f (GScale g) = GScale (f g)
    fmap f (Color r g b) = Color (f r) (f g) (f b)

newtype NewPiksel a = NewPiksel (Piksel a) deriving (Show)

instance Functor NewPiksel where
    fmap f (NewPiksel (GScale g)) = NewPiksel (GScale (f g))
    fmap f (NewPiksel (Color r g b)) = NewPiksel (Color (f b) (f r) (f g))

{-  Monad

    Type class
        - class Applicative m => Monad (m :: * -> *) where
              (>>=) :: m a -> (a -> m b) -> m b
              (>>) :: m a -> m b -> m b
              return :: a -> m a
              fail :: String -> m a  - U novijim verzijama jezika prebaceno u Control.Monad.Fail.MonadFail
              {-# MINIMAL (>>=) #-}
    
    Ideja je da se funkcija koja prima "cist" parametar i vraca vrednost u kontekstu
    primenjuje na podatak u kontekstu
    
    Funkcija ">>" se koristi kada nam ne treba vrednost levog parametra ali
    zelimo da sacuvamo kontekst
        - u "do" bloku se pise bez "<-"
    
    Funkcija "fail" se poziva automatski kada ne uspe pattern match u do bloku
    
    Ne navodi se parametar konkretnog tipa
    
    Ako type konstruktor prima 2 parametra tipa mora se parcijalno primeniti
    
    Kljucna rec "do" radi u kontekstu bilo kojeg Monada, ne samo IO i ponasa se isto
        - povezuje nekoliko operacija sa Monadima u blok
        - vrednost celog bloka je vrednost poslednjeg izraza u bloku
        - najcesce se koristi ako imamo ugnjezdene pozive ">>=" funkcije
        - takodje se moze koristiti operator "<-"
    
    Pravila
        - return x >>= f = f x
        - m >>= return = m
        - (m >>= f) >>= g = m >>= (\x -> f x >>= g)
    
    Primer
        - instance Monad Maybe where
              return x = Just x
              Nothing >>= f = Nothing
              Just x >>= f  = f x
              fail _ = Nothing
        
        - instance Monad [] where
              return x = [x]
              xs >>= f = concat (map f xs)
              fail _ = []

    Dodatna literatura:
        - https://wiki.haskell.org/All_About_Monads
	
-}

-- primer Monad

plus2 :: Int -> Maybe Int
plus2 x
    | rezultat > 10 = Nothing
    | otherwise = Just rezultat
    where rezultat = x + 2

minus1 :: Int -> Maybe Int
minus1 x
    | rezultat < 0 = Nothing
    | otherwise = Just rezultat
    where rezultat = x - 1

uslov :: Int -> Maybe Int
uslov x
    | even x = Nothing
    | otherwise = Just x

operacija :: Int -> Maybe Int
operacija x = do a <- plus2 x
                 b <- minus1 a
                 uslov b
                 c <- plus2 b
                 return c

vici :: String -> Maybe String
vici str = Just (str ++ "!")

test :: String -> Maybe String
test str = do (x:xs) <- vici str  -- y
              return xs

data Logger a = Logger { getLog :: (a, [String]) }

instance Functor Logger where
    fmap f (Logger (x, log)) = Logger (f x, log)

instance Applicative Logger where
    pure x = Logger (x, [])
    (Logger (f, log1)) <*> (Logger (x, log2)) = Logger (f x, log1 `mappend` log2)

instance Semigroup a => Semigroup (Logger a) where
    (Logger (x, log1)) <> (Logger (y, log2)) =  Logger (x <> y, log1 <> log2)

instance Monoid a => Monoid (Logger a) where
    mempty = Logger (mempty, [])

instance Monad Logger where
    return x = Logger (x, [])
    (Logger (x, log)) >>= f = Logger (res, log `mappend` newLog)
                                where (Logger (res, newLog)) = f x

instance Fail.MonadFail Logger where
    fail msg = error msg

funny :: String -> Logger String
funny str = Logger (str ++ "ha", ["inc"])

notFunny :: String -> Logger String
notFunny str
    | length str >= 2 = Logger (init . init $ str, ["dec"])
    | otherwise = fail "nema dovoljno"

smeh :: String -> Logger String
smeh str = do a <- funny str
              b <- notFunny a
              (x:y:xs) <- notFunny b
              return (y:x:xs)



              
-- doMove (Mancala turn pBig cBig pSmall cSmall) n
--         | turn == Player = if n `elem` (valid (Mancala Player pBig cBig pSmall cSmall)) then handle (Mancala turn pBig cBig pSmall cSmall) n else error "Los input"
--         | turn == Computer = if n `elem` valid (Mancala Computer pBig cBig pSmall cSmall) then handle (Mancala turn pBig cBig pSmall cSmall) n else error "Los input"
--         where
--             fromSmall (Small a b c d e f) = [a,b,c,d,e,f]
--             toSmall [a,b,c,d,e,f] = (Small a b c d e f)
--             pebbles xs val = map (+1) (take val xs) ++ drop val xs
--             steal idx xs = if xs !! idx == 1 && idx < 6 && xs !! (12-idx) /=0 
--                            then take idx xs ++ [0] ++ (take (5-idx) $ drop (idx+1) xs) ++ 
--                                 [xs !! 6 + xs !! (12-idx) + xs !! idx] ++ 
--                                 (take (5-idx) $ drop 7 xs) ++ [0] ++ (drop (13-idx) xs)
--                            else xs
                           
--             handle (Mancala Player pBig cBig pSmall cSmall) n = do let xs = fromSmall pSmall
--                                                                    let val = xs !! (n-1) `mod` 13
--                                                                    let val' = xs !! (n-1) `div` 13
--                                                                    let whole = (take (n-1) xs) ++ [0] ++ (drop n xs) ++ [pBig] ++ (reverse . fromSmall $ cSmall)
--                                                                    let newState = steal ((n + val - 1) `mod` 13) $ map (+val') $ shift (13-n) $ pebbles (shift n whole) val
--                                                                    let newTurn = if val + n == 7 then Player else Computer
                                                                 
--                                                                    (Mancala newTurn (head . drop 6 $ newState) cBig
--                                                                     (toSmall $ take 6 newState) (toSmall $ take 6 (reverse newState)))
                                                                   
--             handle (Mancala Computer pBig cBig pSmall cSmall) m = do let xs = reverse . fromSmall $ cSmall
--                                                                      let n = m-6
--                                                                      let val = xs !! (n-1) `mod` 13
--                                                                      let val' = xs !! (n-1) `div` 13
--                                                                      let whole = (take (n-1) xs) ++ [0] ++ (drop n xs) ++ [cBig] ++ fromSmall pSmall    
--                                                                      let newState = steal ((n + val - 1) `mod` 13) $ map (+val') $ shift (13-n) $ pebbles (shift n whole) val
--                                                                      let newTurn = if val + n == 7 then Computer else Player
                                                                     
--                                                                      (Mancala newTurn pBig  (head . drop 6 $ newState)
--                                                                       (toSmall . reverse . take 6 $ reverse newState) (toSmall . reverse . take 6 $ newState))