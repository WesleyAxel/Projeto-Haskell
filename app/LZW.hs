module LZW where

import Data.Int
import Data.Bits 
import Data.List (elemIndex, tails)
import Data.Maybe (fromJust)
 
-- realiza a compressão utilizando utilizando LZW com base em um Index de elementos.
doLZW :: Eq a => [a] -> [a] -> [Int]                               
doLZW _ [] = []
doLZW as (x:xs) = lzw (return <$> as) [x] xs
  where
    lzw a w [] = [fromJust $ elemIndex w a]
    lzw a w (x:xs)
      | w_ `elem` a = lzw a w_ xs
      | otherwise = fromJust (elemIndex w a) : lzw (a ++ [w_]) [x] xs
      where
        w_ = w ++ [x]
 
undoLZW :: [a] -> [Int] -> [a]
undoLZW _ [] = []
undoLZW a cs =
  cs >>=
  (!!)
    (foldl
       ((.) <$> (++) <*>
        (\x xs -> return (((++) <$> head <*> take 1 . last) ((x !!) <$> xs))))
       (return <$> a)
       (take2 cs))
 
take2 :: [a] -> [[a]]
take2 xs = filter ((2 ==) . length) (take 2 <$> tails xs)

{- O Algoritmo desenvolve uma lista de combinação para cada caractere com base em um dicionario passado na função doLZW de tamanho ['\0' .. '\255']
   A lista tem como tipo "Int", que em Haskell, ocupam 64 Bytes na memoria por possuir alcance de tamanho  [-2^29 .. 2^29-1]                 
   Para melhor conversão, a lista criada é convertida para o tipo "Int16" de alcance [-32,768 to +32,767], que acaba ocupando cada combinação, 16 bytes. 
   Após isso é calculado o tamano com base no tamanho da lista -}

converteparaInt16 :: Int -> Int16
converteparaInt16 x = a
    where a = fromIntegral x :: Int16
  
converteLista :: [Int] -> [Int16]
converteLista = fmap converteparaInt16

calculaTamanho :: [Int] -> Int
calculaTamanho xs = length (converteLista xs) * 16