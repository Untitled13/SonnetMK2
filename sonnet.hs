

import Data.List
import Data.Char
import System.Random


mode :: Ord a => [a] -> a
mode xs = snd $ maximum dic
     where mtrx = group $ sort xs
           dic  = zip (map length mtrx) (map head mtrx)

rmode :: Int -> [a] -> a
rmode n xs = xs !! (round $ (num * (fromIntegral (len-1))))
     where len = length xs
           num = fst (random (mkStdGen n) :: (Double, StdGen))


match :: Ord a => [a] -> [a] -> [a]
match m xs
     | not $ gtLen len xs = []
     | take len xs == m   = (head $ apply len tail xs) : (match m $ tail xs)
     | otherwise          = match m $ tail xs
     where len = length m



apply :: (Integral b) => b -> (a -> a) -> a -> a
apply 1 f = f
apply n f = f . (apply (n-1) f)

gtLen :: Int -> [a] -> Bool
gtLen _ [] = False
gtLen 0 _  = True
gtLen n (x:xs) = gtLen (n-1) xs

main :: IO()
main = (do
     putStrLn "\nNew Poem\n"
     putStr "Author = "
     ans <- getLine
     src <- readFile $ {-here you need to write your directory name in quotes for example "/Users/admin/Downloads" also you'll need to delete these brackets with dashes inside arround this comment -} ++ (artist ans)
     putStr "First Words = "
     input <- getLine
     putStr "Number of Words = "
     i <- getLine
     putStrLn ""
     putStr (' ':input)
     putStrLn $ concat $ loop (words $ map toLower $ filter (\x -> (isLetter x || x == ' '))  input) (words $ map toLower $ filter (\x -> (isLetter x || x == ' ')) src) (read i)
     main)


loop :: [[Char]] -> [[Char]] -> Int -> [[Char]]
loop _ _ 0 = []
loop input str n
     | (n `mod` 5) == 0 = (' ':(nxtWord ++ "\n")) : (loop ((tail input) ++ [nxtWord]) str (n-1))
     | otherwise        = (' ':nxtWord)           : (loop ((tail input) ++ [nxtWord]) str (n-1))
     where nxtWord = rmode n $ match input str

artist :: [Char] -> [Char]
artist "Frost" = "Robert_Frost_Proccessed.txt"
artist "Shakespeare" = "Sonnet.txt"
artist "Shakespeare All" = "Complete_Works.txt"
artist n = "Sonnet.txt"




