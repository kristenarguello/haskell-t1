-- Definir uma função recursiva que recebe um número binário (interpretado como número inteiro sem sinal) e
-- retorna o valor equivalente em decimal. 𝑏𝑖𝑛2𝑑𝑒𝑐 ∷ [𝐼𝑛𝑡] → 𝐼𝑛𝑡
bin2dec :: [Int] -> Int
bin2dec [] = 0
bin2dec (0:xs) = 0 + bin2dec xs
bin2dec (1:xs) = 2 ^ length xs + bin2dec xs

--bin2dec [0,0,1,0,1,0]
--10
--bin2dec [0,0,1,1,1,1,1]
--31

-- Definir uma função recursiva que recebe um número decimal inteiro não-negativo, um número de bits
-- desejado e retorna o valor equivalente em binário (interpretado como número inteiro sem sinal) com o
-- número de bits informado. Por exemplo, 𝑑𝑒𝑐2𝑏𝑖𝑛 2 8 deve retornar [0,0,0,0,0,0,1,0]. 𝑑𝑒𝑐2𝑏𝑖𝑛 ∷ 𝐼𝑛𝑡 →
-- 𝐼𝑛𝑡 → [𝐼𝑛𝑡]
dec2bin :: Int -> Int -> [Int]
dec2bin 0 0 = []
dec2bin v t
  | v > (2^t) = [-1] --lista vazia para quando nao for possivel representar o numero desejado com a qtd de bits desejada
  | v - (2 ^ (t-1)) < 0 = 0 : dec2bin v (t-1)
  | otherwise = 1 : dec2bin (v-(2 ^ (t-1))) (t-1)

--dec2bin 7 5
--[0,0,1,1,1]
--dec2bin 10 2
--[-1]

-- Definir uma função recursiva que recebe um número binário na representação de complemento de dois e
-- retorna o valor equivalente em decimal inteiro. 𝑏𝑖𝑛𝑐𝑜𝑚𝑝𝑙2𝑑𝑒𝑐 ∷ [𝐼𝑛𝑡] → 𝐼𝑛𝑡
bincompl2dec :: [Int] -> Int
bincompl2dec [] = 0
bincompl2dec (0:xs) = bin2dec xs
bincompl2dec (1:xs) = - bin2dec (soma1 (invert (1:xs)))

invert :: [Int] -> [Int]
invert [] = []
invert (1:xs) = 0 : invert xs
invert (0:xs) = 1 : invert xs

soma1 :: [Int] -> [Int]
soma1 [] = []
soma1 b = dec2bin (somadec + 1) (length b)
            where somadec = bin2dec b

--bincompl2dec [0,1,0,1,0,0,0,1]
--81
--bincompl2dec [1,0,0,1,0,1]
-- -27

-- Definir uma função recursiva que recebe um número decimal inteiro, um número de bits desejado e retorna
-- o valor equivalente em binário na representação de complemento de dois com o número de bits informado.
-- Por exemplo, 𝑑𝑒𝑐2𝑏𝑖𝑛𝑐𝑜𝑚𝑝𝑙 (−2) 8 deve retornar [1,1,1,1,1,1,1,0] 𝑑𝑒𝑐2𝑏𝑖𝑛𝑐𝑜𝑚𝑝𝑙 ∷ 𝐼𝑛𝑡 → 𝐼𝑛𝑡 →
-- [𝐼𝑛𝑡]
dec2bincompl :: Int -> Int -> [Int]
dec2bincompl 0 0 = []
dec2bincompl v t
  | v<(-(2^(t-1))) || v>(2^(t-1)-1) = [-1] --lista vazia para quando nao for possivel representar o numero desejado com a qtd de bits desejada
  | v > 0 = dec2bin v t
  | otherwise = soma1 (invert (dec2bincompl (v * (-1)) t))

--dec2bincompl (-4) 6
--[1,1,1,1,0,0]
--dec2bincompl (-9) 3
--[-1]
--dec2bincompl 27 4
--[-1]
--dec2bincompl 13 6
--[0,0,1,1,0,1]

-- Definir uma função recursiva que recebe um número fracionário decimal por parâmetro e devolvrt e um
-- número binário de ponto fixo de 32 bits. O número binário de ponto fixo dever ser representado por uma
-- tupla com dois números binários tal que a parte inteira deve estar na representação de complemento de
-- dois com 16 bits e a parte fracionária deve estar na representação de binário fracionado com 16 bits. Você
-- deve definir uma forma adequada de representar o resultado caso o número decimal estoure a
-- representação. Por exemplo, 𝑓𝑟𝑎𝑐2𝑏𝑖𝑛 (−8.5) deve retornar
-- ([1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0],[1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]). 𝑓𝑟𝑎𝑐2𝑏𝑖𝑛 ∷ 𝐷𝑜𝑢𝑏𝑙𝑒 → ([𝐼𝑛𝑡],[𝐼𝑛𝑡])
frac2bin :: Double -> ([Int], [Int])
frac2bin 0 = ([],[])
frac2bin v = if snd (separainteiro v) >= 1-0.5**16
            then (dec2bincompl (fst (separainteiro (v+1))) 16, [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]) 
            --quando há estouro de representaçao da parte fracionária (maior ou igual a 0.9999847412109375), o valor é arredondado para cima 
            else (dec2bincompl (fst (separainteiro v)) 16, fracionario (snd (separainteiro v)) 16)

separainteiro :: Double -> (Int, Double)
separainteiro 0.0 = (0, 0.0)
separainteiro v
    | v < 0 = let (intPart, fracPart) = separainteiro (-v)
              in (-intPart, fracPart)
    | otherwise = let intPart = floor v
                      fracPart = v - fromIntegral intPart
                  in (intPart, fracPart)

fracionario :: Double -> Int -> [Int]
fracionario _ 0 = []
fracionario x n = let newX = x * 2
                      intPart = truncate newX
                      fracPart = fracionario (newX - fromIntegral intPart) (n - 1)
                  in intPart : fracPart

--frac2bin 13.9999847412109389 (caso de estouro de representacao da parte fracionaria)
--([0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0],[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]) 
--frac2bin 9.78
--([0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,1],[1,1,0,0,0,1,1,1,1,0,1,0,1,1,1,0])
--frac2bin (-10.25)
--([1,1,1,1,1,1,1,1,1,1,1,1,0,1,1,0],[0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0])

-- Definir uma função recursiva que recebe uma tupla com dois números binários representando,
-- respectivamente, a parte inteira (na representação de complemento de dois com 16 bits) e a parte
-- fracionária (na representação de binário fracionado com 16 bits) de um número binário de ponto fixo com 32
-- bits, e retorna o correspondente valor fracionário decimal. Por exemplo,
-- 𝑏𝑖𝑛2𝑓𝑟𝑎𝑐 ([0,1,0,0,0,0,0,0,0,0,0,0,1,0,0,0],[1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0]) deve retornar
-- 16392.625. 𝑏𝑖𝑛2𝑓𝑟𝑎𝑐 ∷ ([𝐼𝑛𝑡],[𝐼𝑛𝑡]) → 𝐷𝑜𝑢𝑏𝑙�
bin2frac :: ([Int], [Int]) -> Double
bin2frac ([], []) = 0
bin2frac (v, t) = if bincompl2dec v < 0
                    then fromIntegral (bincompl2dec v) - decimal t (-1.0)
                    else fromIntegral (bincompl2dec v) + decimal t (-1.0)

decimal :: [Int] -> Double -> Double
decimal [] _ = 0.0
decimal (0:xs) i = 0 + decimal xs (i-1.0)
decimal (1:xs) i = 2.0 ** i + decimal xs (i-1.0)

--bin2frac ([0,0,1,1,0,0,0,0,0,0,0,1,0,1,0,0],[0,1,1,1,0,1,0,0,0,1,0,1,0,0,1,0])
--12308.454376220703
--bin2frac ([1,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0],[0,1,1,1,0,1,0,0,0,1,0,0,0,0,0,0])
-- -24560.4541015625
--bin2frac ([0,1,0,0,0,0,0,0,0,0,0,0,1,0,0,0],[1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0])
--16392.625
