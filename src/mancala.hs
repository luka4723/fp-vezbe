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

doMove :: Mancala -> Int -> Mancala
doMove (Mancala turn pBig cBig pSmall cSmall) n
        | turn == Player = if n `elem` (valid (Mancala Player pBig cBig pSmall cSmall)) then handle (Mancala turn pBig cBig pSmall cSmall) n else error "Los input"
        | turn == Computer = if n `elem` valid (Mancala Computer pBig cBig pSmall cSmall) then handle (Mancala turn pBig cBig pSmall cSmall) n else error "Los input"
        where
            fromSmall (Small a b c d e f) = [a,b,c,d,e,f]
            toSmall [a,b,c,d,e,f] = (Small a b c d e f)
            handle (Mancala Player pBig cBig pSmall cSmall) n = do let  xs = fromSmall pSmall
                                                                   let  whole = xs ++ [pBig] ++ (reverse . fromSmall $ cSmall)
                                                                   let  val = xs !! (n-1) `mod` 13
                                                                   let  val' = xs !! (n-1) `div` 13
                                                                   let  newPSmall = [x+i| (x,j)<-zip ((take (n-1) whole ++ [0])) [1..], let i = if j<=val-13+n then 1 else 0]
                                                                                    ++ [x+i| (x,j)<-zip (drop n whole) [1..], let i = if j<=val then 1 else 0]
                                                                   let newPSmall' = map (+val') newPSmall
                                                                   let newTurn = if val + n == 7 then Player else Computer
                                                                   let newPSmall'' = if val' == 0 && val+n<7 && newPSmall' !! (val + n - 1) == 1 then [ i | (x,j)<- zip newPSmall' [1..], let i = if j==7 then 1+x+ (newPSmall' !! (13-n)) else x] 
                                                                                                                   else newPSmall'
                                                                   let newPSmall''' = if val' == 0 && val+n<7 && newPSmall'' !! (val + n - 1) == 1 then [ i | (x,j)<- zip newPSmall'' [1..], let i = if j==val+n || j==12-val+n then 0 else x] 
                                                                                                                   else newPSmall''
                                                                   (Mancala newTurn (head . take 1 . drop 6 $ newPSmall''') cBig
                                                                    (toSmall $ take 6 newPSmall''') (toSmall $ take 6 (reverse newPSmall''')))
                                                                   
            handle (Mancala Computer pBig cBig pSmall cSmall) n = do let  xs = reverse . fromSmall $ cSmall
                                                                     let  whole = xs ++ [cBig] ++ fromSmall pSmall
                                                                     let  val = (xs !! (n-7)) `mod` 13
                                                                     let  val' = (xs !! (n-7)) `div` 13
                                                                     let  newCSmall = [x+i| (x,j)<-zip ((take (n-7) whole ++ [0])) [1..], let i = if j<=val-19+n then 1 else 0]
                                                                                    ++ [x+i| (x,j)<-zip (drop (n-6) whole) [1..], let i = if j<=val then 1 else 0 ]
                                                                     let newCSmall' = map (+val') newCSmall 
                                                                     let newTurn = if val + (n-7) == 7 then Computer else Player
                                                                     let newCSmall'' = if val' == 0 && val+(n-6)<7 && newCSmall' !! (val + (n-6) - 1) == 1 then [ i | (x,j)<- zip newCSmall' [1..], let i = if j==7 then 1+x+ (newCSmall' !! (13-(n-6))) else x] 
                                                                                                                   else newCSmall'
                                                                     let newCSmall''' = if val' == 0 && val+(n-6)<7 && newCSmall'' !! (val + (n-6) - 1) == 1 then [ i | (x,j)<- zip newCSmall'' [1..], let i = if j==val+(n-6) || j==12-val+(n-6) then 0 else x] 
                                                                                                                   else newCSmall''
                                                                     (Mancala newTurn pBig  (head . take 1 . drop 6 $ newCSmall''')
                                                                      (toSmall . reverse . take 6 $ reverse newCSmall''') (toSmall . reverse . take 6 $ newCSmall'''))

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
        let mancala = Mancala Computer 0 0 (Small 4 4 4 4 4 4) (Small 4 4 4 4 4 4)
        --putStrLn $ show mancala
        let rose = genMoves (Node mancala []) 3
        putStrLn $ show $ elemsOnDepth rose 1

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
                                                                                            (b,tabla'') = h tabla
                                                                                         in (b, tabla' ++ tabla'')))

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
                                                             applyMoveH 3
                                                             applyMoveH 2
                                                             applyMoveH 2
                                                             applyMoveH 2

applyMoveH :: Int -> GameStateOpHistory Bool
applyMoveH move = GameStateOpHistory (\tabla -> let tabla' = doMove tabla move
                                                            in (isGameOver tabla', [tabla',tabla]))

main2 = do
        let (res,xs) = runGameStateH 
        putStrLn $ show $ head xs
        putStrLn $ show $ valid $ head xs

