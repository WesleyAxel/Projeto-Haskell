module LZO where

import qualified Data.ByteString as BS
import Data.ByteString.UTF8 as BSU 
import qualified Codec.Compression.Lzo as LZ

-- transforma uma String em ByteString
stringtoByteString :: String -> BS.ByteString       
stringtoByteString = BSU.fromString 

-- Comprimi uma ByteString utilizand LZO
comprimir :: BS.ByteString -> BS.ByteString         
comprimir x = LZ.compress x

-- transforma uma ByteString em String
byteStringtoString :: BS.ByteString -> String       
byteStringtoString = BSU.toString

-- Calcula tamanho bytes de uma ByteString
calculaTamanho = BS.length
calculaTamanho :: BS.ByteString -> Int                     


{- Algoritmo se resume em pegar uma String, converter para uma ByteString através do package Data.ByteString.UTF8, comprimir através do package Codec.Compression.LZO, e calcular da ByteString comprimida,
ou converter para String novamente para tentar imprimir na tela a ByteString comprimida(AVISO, não será possivel entender nada da String comprimida e convertida) -}