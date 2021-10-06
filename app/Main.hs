module Main where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import qualified Huffman as HS
import qualified LZW as LW 
import qualified LZO as LO
import Control.Concurrent 
import Control.Monad
import System.TimeIt
import Numeric

type TextoComprimido = String
type TamanhoComprimido = Int
type ArquivoComprimido = (TextoComprimido,TamanhoComprimido)

-- Realiza a abertura de conexão da pagina através das configurações padrões, apenas mexendo no parametro de arquivos externos, colocando os mesmos dentro da pasta static
main :: IO ()
main = startGUI defaultConfig { jsStatic = Just "static" } setup

setup :: Window -> UI ()
setup window = do
    UI.addStyleSheet window "style.css"
    
    return window # set UI.title "Compressão em Haskell"

    heading         <- UI.h1 # set text "Codificação de Huffman, LZW e LZO em Haskell"
    getBody window #+ [element heading]

    textoind        <- UI.paragraph # set text "Codificação de um texto simples utilizando algoritmos de Huffman, LZW e LZO"
    getBody window #+ [element textoind]

    textoInt        <- UI.paragraph # set text "Insira o texto para comprimir:"
    inputcompr      <- UI.textarea #. "compr-input"
    getBody window #+ [element textoInt, element inputcompr]
    el              <- UI.div   #. "compr-area"  #+ [ UI.span  #. "compr-label", element inputcompr]
    getBody window #+ [element el]

    buttonCom       <- UI.button # set UI.text "Comprimir"
    getBody window #+ [element buttonCom]

    texto1 <- UI.paragraph # set text "Tamanho do texto original em bytes: "
    getBody window #+ [element texto1]

    huft            <- UI.h2 # set text "Compressão em Huffman"
    textoBox        <- UI.textarea # set UI.text "" # set (attr "readonly") ""
    textocomprimido <- UI.div #. "texto-comprimido" #+ [element textoBox]
    getBody window #+ [element huft, element textocomprimido]

    
    texto2 <- UI.paragraph # set text "Tamanho do texto comprimido em bits: "
    texto3 <- UI.paragraph # set text "Taxa de compressão: "
    tempo1 <- UI.paragraph # set text "Tempo decorrido em segundos:"
    getBody window #+ [element texto2, element texto3, element tempo1]

    lzw              <- UI.h2 # set text "Compressão em LZW"
    textoBox2        <- UI.textarea # set UI.text "" # set (attr "readonly") ""
    textocomprimido2 <- UI.div #. "texto-comprimido" #+ [element textoBox2]
    getBody window #+ [element lzw, element textocomprimido2]


    texto4 <- UI.paragraph # set text "Tamanho do texto comprimido em bits: "
    texto5 <- UI.paragraph # set text "Taxa de compressão: "
    tempo2 <- UI.paragraph # set text "Tempo decorrido em segundos:"
    getBody window #+ [element texto4, element texto5, element tempo2]

    lzo              <- UI.h2 # set text "Compressão em LZO"
    textoBox3        <- UI.textarea # set UI.text "" # set (attr "readonly") ""
    textocomprimido3 <- UI.div #. "texto-comprimido" #+ [element textoBox3]
    getBody window #+ [element lzo, element textocomprimido3]

    texto6 <- UI.paragraph # set text "Tamanho do texto comprimido em bytes: "
    texto7 <- UI.paragraph # set text "Taxa de compressão: "
    tempo3 <- UI.paragraph # set text "Tempo decorrido em segundos:"
    getBody window #+ [element texto6, element texto7, element tempo3]
    
    texto10        <- UI.paragraph # set text "Cada caractere de um texto que é uma String, ocupa 1 byte, que são 8 bits."
    texto11        <- UI.paragraph # set text "A codificação de Huffman é um método de compressão que usa as probabilidades de ocorrência dos símbolos no conjunto de dados a ser comprimido para determinar códigos de tamanho variável para cada símbolo.Uma árvore binária completa, chamada de árvore de Huffman é construída recursivamente a partir da junção dos dois símbolos de menor probabilidade, que são então somados em símbolos auxiliares e estes símbolos auxiliares recolocados no conjunto de símbolos. O processo termina quando todos os símbolos forem unidos em símbolos auxiliares, formando uma árvore binária. A árvore é então percorrida, atribuindo-se valores binários de 1 ou 0 para cada aresta, e os códigos são gerados a partir desse percurso. O resultado do algoritmo de Huffman pode ser visto como uma tabela de códigos de tamanho variável para codificar um símbolo da fonte. Assim como em outros métodos de codificação, os símbolos mais comuns são geralmente representados usando-se menos dígitos que os símbolos que aparecem com menos frequência."
    texto12        <- UI.paragraph # set text "LZW (Lempel-Ziv-Welch) é um algoritmo de compressão de dados, derivado do algoritmo LZ78, baseado na localização e no registro das padronagens de uma estrutura.O algoritmo visa eliminar a necessidade de se emitir um caractere literal junto com o endereço de dicionário. Para isso, o dicionário é inicializado com todos os símbolos do alfabeto (ao se usar codificação ASCII são 256 símbolos, de 0 a 255). A entrada é lida e acumulada em uma cadeia de caracteres que chamaremos de I. Sempre que a seqüência contida em I não estiver presente no dicionário emitimos o código correspondente a versão anterior de I (ou seja, I sem o último caractere) e adicionamos I ao dicionário. I volta a ser inicializado com o último caractere lido (o seu último caractere) e o processo continua até que não haja mais caracteres na entrada."
    texto13        <- UI.paragraph # set text "O algoritmo LZO Lempel–Ziv–Oberhumer (LZO) é um algorimo de compressão sem percas focado na velocidade de descompressão. O LZO funciona comprimindo e descomprimindo blocos de data. Cada bloco deve ter o mesmo tamanho para compressão e descompressão. o LZO comprimimi um bloco de data verificando as ocorrencias dos dados através de um dicionario, e executa combinações para produzir bons resultados em datas muito redundates."
    areaExplicacao <- UI.div #+ [element texto10, element texto11, element texto12, element texto13] # set UI.id_ "explicacao"
    
    
    texto14     <- UI.paragraph # set text "Interface grafica feita utilizando Threepenny-gui."
    texto15     <- UI.a # set UI.href "https://github.com/TsHristov/Huffman-Compression" # set text "Algoritmo de Huffman utilizado"
    breakLine1  <- UI.paragraph # set text "\n"
    texto16     <- UI.a # set UI.href "https://titanwolf.org/Network/Articles/Article?AID=c40c23fe-8571-465c-a408-43369c50550c#gsc.tab=0" # set text "Algoritmo LZW utilizado"
    breakLine2  <- UI.paragraph # set text "\n"
    texto17     <- UI.a # set UI.href "https://hackage.haskell.org/package/lzo-0.1.1.3/docs/Codec-Compression-Lzo.html" # set text "Codec de algoritmos LZO utilizados"
    breakLine3  <- UI.paragraph # set text "\n"
    texto18     <- UI.paragraph # set text "Projeto final para a disciplina de Paradigmas da computação - UFABC 2021 Q2."
    texto19     <- UI.paragraph # set text "Wesley Axel de Barros, RA 11201722424."
    footer      <- UI.div #+ [element texto14, element texto15, element breakLine1, element texto16, element breakLine2, element texto17, element breakLine3, element texto18, element texto19] # set UI.class_ "footer"
    getBody window #+ [element areaExplicacao, element footer]

    -- Ação realizada quando o botão comprimir é executado
    on UI.click buttonCom $ const $ do
        texto <- get value inputcompr
        element texto1 # set UI.text ("Tamanho do texto original em bytes: " ++ show (length texto) ++ " = " ++ show (8 * length texto) ++ " bits")

        huffman     <- liftIO $ timeItT (threadHuffman texto)   -- realiza a thread de compressão em huffman e devolve uma tupla contendo o tempo decorrido em um tipo Double e o resultado da compressão
        let x       = calculoPorcentagem (snd(snd huffman)) (8 * length texto)

        element textoBox # set UI.text (fst(snd huffman))
        element tempo1 # set UI.text ("Tempo decorrido  em segundos: "++ show (fst huffman))
        element texto2 # set UI.text ("Tamanho do texto comprimido em bits: " ++ show (snd(snd huffman)))       -- o tamanho é a contagem de quantidade de bits do texto comprimido com base na codificação
        element texto3 # set UI.text ("Taxa de compressão: " ++ show x ++ "%")

        lzw         <- liftIO $ timeItT (threadLZW texto)       -- realiza a thread de compressão em lzw e devolve uma uma tupla contendo o tempo decorrido em um tipo Double e o resultado da compressão
        let y       = calculoPorcentagem (snd(snd lzw)) (length texto)

        element textoBox2 # set UI.text ("Código comprimido: " ++ fst(snd lzw))
        element tempo2 # set UI.text ("Tempo decorrido em segundos: "++ show (fst lzw))
        element texto4 # set UI.text ("Tamanho do texto comprimido em bytes: " ++ show (snd(snd lzw)))          -- o tamanho é a contagem de elementos de tipo Int16 na lista
        element texto5 # set UI.text ("Taxa de compressão: " ++ show y ++ "%")


        lzo         <- liftIO $ timeItT (threadLZO texto)       -- realiza a thread de compressão em lzo e devolve uma uma tupla contendo o tempo decorrido em um tipo Double e o resultado da compressão
        let z       = calculoPorcentagem (snd(snd lzo)) (length texto)

        element textoBox3 # set UI.text ("Código comprimido: " ++ fst(snd lzo))
        element tempo3 # set UI.text ("Tempo decorrido em segundos: "++ show (fst lzo))
        element texto6 # set UI.text ("Tamanho do texto comprimido em bytes: " ++ show (snd(snd lzo)))          -- o tamanho é o comprimento do ByteString comprimido
        element texto7 # set UI.text ("Taxa de compressão: " ++ show z ++ "%")


--realiza a compressão em huffman com base na criação de uma thread paralela para a execução        
threadHuffman :: String -> IO ArquivoComprimido
threadHuffman input = do
    m <- newEmptyMVar
    forkIO $ do putMVar m (HS.encode input)
    x <- takeMVar m
    return ((("Código comprimido: " ++ (fst x) ++ "\n" ++ "Codificação de Huffman: "  ++ 
                HS.imprimiCode (x) ++ "\n" ++ "Arvore de Huffman: " ++ (show (snd x))))
                ,(length (fst x)))

--realiza a compressão em LZW com base na criação de uma thread paralela para a execução                        
threadLZW :: String -> IO ArquivoComprimido
threadLZW input = do
    w <- newEmptyMVar
    forkIO $ do putMVar w (LW.doLZW ['\0' .. '\255'] input)
    x <- takeMVar w
    return ((show x),(LW.calculaTamanho x))

--realiza a compressão em LZO com base na criação de uma thread paralela para a execução            
threadLZO :: String -> IO ArquivoComprimido
threadLZO input = do
    p <- newEmptyMVar
    forkIO $ do putMVar p (LO.comprimir(LO.stringtoByteString input))
    x <- takeMVar p
    return ((LO.byteStringtoString x),(LO.calculaTamanho x))

-- calcula a porcentagem com base nos valores originais e comprimidos
calculoPorcentagem :: Int -> Int -> Float
calculoPorcentagem x y = (1 - (a/b)) * 100
    where a = fromIntegral x :: Float
          b = fromIntegral y :: Float
        
