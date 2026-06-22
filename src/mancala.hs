import Text.Printf
import Rose.Rose


data Small = Small Int Int Int Int Int Int
data Turn =  Player | Computer deriving (Show, Eq)
data Mancala = Mancala {turn :: Turn, pBig :: Int, cBig :: Int, pSmall :: Small, cSmall :: Small}

instance Show Small where
    show (Small a b c d e f) = printf "   %2d %2d %2d %2d %2d %2d\n" a b c d e f

adjust :: Int -> String
adjust a
       | a>=10 = show a
       | otherwise = " " ++ show a 

instance Show Mancala where
    show (Mancala turn pBig cBig pSmall cSmall) = printf $ "\n" ++ show cSmall ++ adjust cBig ++ "                   " ++ show pBig ++ "\n"
                                                         ++ show pSmall

valid :: Mancala -> [Int]
valid (Mancala turn pBig cBig pSmall cSmall)
        | turn == Player = [i | i <- [1..6], let (Small a b c d e f) = pSmall, let x = case i of
                                                                                      1 -> a
                                                                                      2 -> b
                                                                                      3 -> c
                                                                                      4 -> d
                                                                                      5 -> e
                                                                                      6 -> f, x > 0]
        | turn == Computer = [i | i <- [12,11..7], let (Small a b c d e f) = cSmall, let x = case i of
                                                                                              12 -> a
                                                                                              11 -> b
                                                                                              10 -> c
                                                                                              9 -> d
                                                                                              8 -> e
                                                                                              7 -> f, x > 0]

shift :: Int -> [a] -> [a]
shift _ [] = []
shift n xs = drop (n `mod` length xs) xs ++ take (n `mod` length xs) xs

doMove :: Mancala -> Int -> Mancala
doMove (Mancala turn pBig cBig pSmall cSmall) n = if n `elem` (valid (Mancala turn pBig cBig pSmall cSmall))
                                                  then handle (Mancala turn pBig cBig pSmall cSmall) n else error "Los input"
        where
            fromSmall (Small a b c d e f) = [a,b,c,d,e,f]
            toSmall [a,b,c,d,e,f] = (Small a b c d e f)
            pebbles xs val = map (+1) (take val xs) ++ drop val xs
            steal idx xs = if xs !! idx == 1 && idx < 6 && xs !! (12-idx) /=0 
                           then take idx xs ++ [0] ++ (take (5-idx) $ drop (idx+1) xs) ++ 
                                [xs !! 6 + xs !! (12-idx) + xs !! idx] ++ 
                                (take (5-idx) $ drop 7 xs) ++ [0] ++ (drop (13-idx) xs)
                           else xs
            flip Player = Computer
            flip Computer = Player
            ps = fromSmall pSmall
            cs = reverse . fromSmall $ cSmall
            big = if turn == Player then pBig else cBig        
            handle (Mancala turn pBig cBig pSmall cSmall) n   = do let xs = if turn == Player then ps else cs
                                                                   let idx = if turn == Player then n else n-6
                                                                   let val = xs !! (idx-1) `mod` 13
                                                                   let val' = xs !! (idx-1) `div` 13
                                                                   let whole = (take (idx-1) xs) ++ [0] ++ (drop idx xs) ++ [big] ++ (if turn == Player then cs else ps)
                                                                   let newState = steal ((idx + val - 1) `mod` 13) $ map (+val') $ shift (13-idx) $ pebbles (shift idx whole) val
                                                                   let newTurn = if val + idx == 7 then turn else flip turn
                                                                 
                                                                   if turn == Player 
                                                                    then (Mancala newTurn (head . drop 6 $ newState) cBig
                                                                    (toSmall $ take 6 newState) (toSmall $ take 6 (reverse newState)))

                                                                    else (Mancala newTurn pBig  (head . drop 6 $ newState)
                                                                      (toSmall . reverse . take 6 $ reverse newState) (toSmall . reverse . take 6 $ newState))                                                                

isGameOver :: Mancala -> Bool
isGameOver (Mancala turn pBig cBig pSmall cSmall) = if ((==0) . sum . fromSmall $ pSmall) || ((==0) . sum . fromSmall $ cSmall)
                                                    then True else False
                                                    where fromSmall (Small a b c d e f) = [a,b,c,d,e,f]

getWinner :: Mancala -> Turn
getWinner (Mancala turn pBig cBig pSmall cSmall) = if ((==0) . sum . fromSmall $ pSmall)
                                                    then Player else Computer
                                                    where fromSmall (Small a b c d e f) = [a,b,c,d,e,f] 

genMoves :: Rose Mancala -> Int -> Rose Mancala
genMoves (Node (Mancala turn pBig cBig pSmall cSmall) xs) 0 = (Node (Mancala turn pBig cBig pSmall cSmall) xs)
genMoves (Node (Mancala turn pBig cBig pSmall cSmall) xs) n = genMoves (Node (Mancala turn pBig cBig pSmall cSmall) 
                                                              [(Node (doMove (Mancala turn pBig cBig pSmall cSmall) i) []) | i<-valid (Mancala turn pBig cBig pSmall cSmall)]) (n-1)
main = do
        let mancala = Mancala Player 2 2 (Small 4 4 4 4 4 0) (Small 4 4 4 4 4 0)
        putStrLn $ show $ doMove mancala 2
        --let rose = genMoves (Node mancala []) 3
        --putStrLn $ show $ elemsOnDepth rose 1

newtype GameStateOp a = GameStateOp { runGameStateOp :: Mancala -> (a, Mancala) }

instance Functor GameStateOp where
    fmap f (GameStateOp f') = GameStateOp (\tabla -> let (a, tabla') = f' tabla in (f a, tabla'))

-- f (a->b) -> f a -> f b // f ((mancala->(a,mancala)->(mancala->(a,mancala)) -> f (mancala->(a,mancala) -> f (mancala->(a,mancala))
instance Applicative GameStateOp where
    pure x = (GameStateOp (\tabla -> (x,tabla)))
    (GameStateOp f) <*> (GameStateOp g) = (GameStateOp (\tabla -> let (f', tabla') = f tabla;
                                                                                     (a, tabla'') = g tabla'
                                                                                  in (f' a, tabla'')))

instance Monad GameStateOp where
    --(>>=) :: m a -> (a -> m b) -> m b
    (GameStateOp f) >>= g = (GameStateOp (\tabla -> let (a, tabla') = f tabla;
                                                                       GameStateOp h = g a
                                                                    in (h tabla')))

newtype GameStateOpHistory a = GameStateOpHistory { runGameStateOpHistory :: Mancala -> (a, [Mancala]) }

instance Functor GameStateOpHistory where
    fmap f (GameStateOpHistory f') = GameStateOpHistory (\tabla -> let (a, tabla') = f' tabla in (f a, tabla'))

instance Applicative GameStateOpHistory where
    pure x = (GameStateOpHistory (\tabla -> (x,[tabla])))
    (GameStateOpHistory f) <*> (GameStateOpHistory g) = (GameStateOpHistory (\tabla -> let (f', tabla') = f tabla;
                                                                                                                 (a, tabla'') = g tabla
                                                                                                              in (f' a, tabla' ++ tabla'')))

instance Monad GameStateOpHistory where
    (GameStateOpHistory f) >>= g = (GameStateOpHistory (\tabla -> let (a, tabla') = f tabla;
                                                                                            GameStateOpHistory h = g a;
                                                                                            (b,tabla'') = h (head tabla')
                                                                                         in (b, tabla''++tabla')))

runGameState :: (Bool,Mancala)
runGameState = runGameStateOp applyMoves mancalaInitialState
                                              where 
                                                mancalaInitialState = Mancala Player 0 0 (Small 4 4 4 4 4 4) (Small 4 4 4 4 4 4)
                                                applyMoves = do
                                                             applyMove 1
                                                             applyMove 7

applyMove :: Int -> GameStateOp Bool
applyMove move = GameStateOp (\tabla -> let tabla' = doMove tabla move
                                                        in (isGameOver tabla', tabla'))


runGameStateH :: (Bool,[Mancala])
runGameStateH = runGameStateOpHistory applyMovesH mancalaInitialStateH
                                              where 
                                                mancalaInitialStateH = Mancala Player 0 0 (Small 4 4 4 4 4 4) (Small 4 4 4 4 4 4)
                                                applyMovesH = do
                                                                initialize
                                                                applyMoveH 3
                                                                applyMoveH 6
                                                                applyMoveH 8
                                                                applyMoveH 11
                                                                applyMoveH 2
                                                                applyMoveH 6
                                                                applyMoveH 4
                                                                applyMoveH 7
                                                                applyMoveH 9
                                                                applyMoveH 1
                                                                applyMoveH 3
                                                                applyMoveH 4
                                                                applyMoveH 6
                                                                applyMoveH 11
                                                                applyMoveH 12
                                                                applyMoveH 6
                                                                applyMoveH 1
                                                                applyMoveH 9
                                                                applyMoveH 2
                                                                applyMoveH 8
                                                                applyMoveH 4
                                                                applyMoveH 12
                                                                applyMoveH 9
                                                                applyMoveH 6
                                                                applyMoveH 3
                                                                applyMoveH 10
                                                                applyMoveH 6
                                                                applyMoveH 4
                                                                applyMoveH 11
                                                                applyMoveH 12
                                                                


initialize :: GameStateOpHistory Bool
initialize = GameStateOpHistory $ \s -> (False,[s])

applyMoveH :: Int -> GameStateOpHistory Bool
applyMoveH move = GameStateOpHistory (\tabla -> let tabla' = doMove tabla move
                                                            in (isGameOver tabla', [tabla']))

helper :: Mancala -> Int
helper (Mancala turn pBig cBig (Small q w e r t y) (Small a s d f g h)) = pBig + cBig + q + w + e +r +t+y+a+s+d+f+g+h

main2 = do
        let (res,xs) = runGameStateH 
        putStrLn $ show $  xs
        --putStrLn $ show $ map helper xs
        putStrLn $ show $ res
        putStrLn $ show $ valid $ head xs
