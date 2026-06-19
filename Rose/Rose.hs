module Rose.Rose where

data Rose a = Node a [Rose a]

-- a) size - vraća broj čvorova stabla
-- height - računa visinu stabla, odnosno najdužu putanju (broj grana) od korena do lista
size :: Rose a -> Int
size (Node a xs) = 1 + foldl (\acc x -> acc + size x) 0 xs

height :: Rose a -> Int
height (Node a []) = 0
height (Node a xs) = maxn $ map (\x -> 1 + height x) xs
                     where
                        maxn = foldl (\acc x-> if acc > x then acc else x) 0 

-- b) leaves - vraća listu koja sadrži vrednosti svih listova stabla
leaves :: Rose a -> [a]
leaves (Node a []) = [a]
leaves (Node a xs) = concatMap leaves xs

-- c) elemsOnDepth - vraća vrednosti svih elemenata na određenoj dubini stabla
elemsOnDepth :: Rose a -> Int -> [a] -- Maybe??
elemsOnDepth (Node a xs) 0 = [a]
elemsOnDepth (Node a xs) n = concatMap (\x -> elemsOnDepth x (n-1)) xs

-- d) instancirati tipsku klasu Functor za tip podataka Rose
instance Functor Rose where
    fmap f (Node a xs) = Node (f a) (map (fmap f) xs) 

-- e) napisati funkciju foldRose koja izršava fold (levi ili desni) na svim čvorovima stabla tipa Rose (na primer
-- ako imamo stablo sa celim brojevima i prosledimo funkciju (+) i akumulator 0 kao rezultat se vraća zbir svih
-- čvorova)
foldRose :: (b -> a -> b) -> b -> Rose a -> b
foldRose f acc (Node a xs) = foldl f acc (elems (Node a xs))
                             where 
                                elems (Node a []) = [a]
                                elems (Node a xs)= a:(concatMap elems xs)

demo = do
        let tree = Node 1 [Node 2 [], Node 3 []]
        let tree = Node 1 [Node 2 [ Node 5 [], Node 6 [] ], Node 3 [], Node 4[ Node 7 []]]
        putStrLn $ show $ size tree
        putStrLn $ show $ height tree
        putStrLn $ show $ leaves tree
        putStrLn $ show $ elemsOnDepth tree 0
        putStrLn $ show $ elemsOnDepth tree 1
        putStrLn $ show $ elemsOnDepth tree 2
        let tree2 = fmap (+1) tree
        putStrLn $ show $ elemsOnDepth tree2 0
        putStrLn $ show $ elemsOnDepth tree2 1
        putStrLn $ show $ elemsOnDepth tree2 2
        putStrLn $ show $ foldRose (+) 0 tree