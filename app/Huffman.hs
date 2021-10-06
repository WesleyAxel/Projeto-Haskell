module Huffman where

import Data.List as List (sortBy, nub, foldl')
import qualified Data.Binary as Binary
import qualified Data.Map as Map

type Code            = String
type CharFrequency   = (Char, Int)
type CharFrequencies = [CharFrequency]

data HuffmanTree = EmptyTree | Node { root  :: CharFrequency,
                                      left  :: HuffmanTree,
                                      right :: HuffmanTree
                                    } deriving (Show, Eq)

-- Faz a arvore de Huffman ser uma instancia de Ord, para poder organizar a arvore com base na frequencia de chars
instance Ord HuffmanTree where
  compare (Node (_,x) _ _) (Node (_,y) _ _) = compare x y

-- Faz a arvore de Huffman ser uma instancia de Binary para poder serializada.
instance Binary.Binary HuffmanTree where
  put EmptyTree = do Binary.put (0 :: Binary.Word8)
  put (Node root left right) = do Binary.put (1 :: Binary.Word8)
                                  Binary.put root
                                  Binary.put left
                                  Binary.put right
  get = do t <- Binary.get :: Binary.Get Binary.Word8
           case t of
             0 -> do return EmptyTree
             1 -> do root <- Binary.get
                     left <- Binary.get
                     right <- Binary.get
                     return (Node root left right)

makeLeaf ::  CharFrequency -> HuffmanTree
makeLeaf x = (Node x EmptyTree EmptyTree)

leaf :: HuffmanTree -> Bool
leaf (Node _ EmptyTree EmptyTree) = True
leaf                           _  = False

-- Pega o char associado com a raiz
char :: HuffmanTree -> Char
char = fst . root

-- Pega a frequencia associada com a raiz
frequency :: HuffmanTree -> Int
frequency = snd . root

-- Pega a frequencia de Chars de uma String
contentsFrequencies :: String -> [CharFrequency]
contentsFrequencies contents = nub $ map (\x -> (x, (count x contents))) contents
  where count x              = foldl' (\acc y -> if y == x then acc + 1 else 0) 0 

-- Faz cada (char,frequencia) par de uma folha na arvore de huffman
makeLeaves :: CharFrequencies -> [HuffmanTree]
makeLeaves = map makeLeaf

-- Faz a junção de duas arvores somando suas frequencias

mergeTrees :: HuffmanTree -> HuffmanTree -> HuffmanTree

mergeTrees  leftTree rightTree = Node (' ', frequenciesSum) leftTree rightTree
  where frequenciesSum         = frequency leftTree + frequency rightTree

-- Constroi a Arvore de Huffman com base em seus chars e suas frequencias
constructHuffmanTree :: CharFrequencies -> HuffmanTree
constructHuffmanTree          = construct . makeLeaves
  where construct []          = EmptyTree
        construct [tree]      = tree
        construct tree        = construct updated
          where sorted        = sortBy compare tree 
                smallestTrees = take 2 sorted 
                rightTree     = head smallestTrees
                leftTree      = head $ tail smallestTrees
                merged        = mergeTrees rightTree leftTree
                removed       = drop 2 sorted
                updated       = merged : removed

-- Faz uma identificação binaria de cada char de uma arvore de Huffman
binaryCodes :: HuffmanTree -> [(Char, Code)]
binaryCodes = generateCodes ""
  where generateCodes path tree 
          | leaf tree   = [(char tree, path)]
          | otherwise   = generateCodes (path ++ "0") (left tree)
                          ++ generateCodes (path ++ "1") (right tree)


-- Comprimi uma String através do algoritmo de Huffman e devolve um código binario da String comprimida e Arvore de Huffman
encode :: String -> (Code, HuffmanTree)
encode content      = (encoded content, huffmanTree)
  where huffmanTree = constructHuffmanTree $ contentsFrequencies content
        encoded     = concat . binaryCode
          where getValue (Just x) = x
                codes             = Map.fromList $ binaryCodes $ huffmanTree
                binaryCode        = Prelude.map (\x -> getValue (Map.lookup x codes))

-- Descomprimi uma Arvore de Huffman baseado no código binario             
decode :: (Code, HuffmanTree) -> String
decode (binaryCode, huffmanTree)                     = decodeTree binaryCode huffmanTree
  where decodeTree "" EmptyTree                      = ""
        decodeTree "" tree                           = [char tree]
        decodeTree code (Node x EmptyTree EmptyTree) = fst x : decodeTree code huffmanTree
        decodeTree (x:xs) tree                       = if x == '0'
                                                       then decodeTree xs (left tree)
                                                       else decodeTree xs (right tree)

-- Obtem em um String o codigo binario de uma arvore de Huffman
imprimiCode :: (Code, HuffmanTree) -> String
imprimiCode (_,arvore) = show (binaryCodes arvore)

