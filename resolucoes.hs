-- Definir uma função recursiva que recebe um número binário (interpretado como número inteiro sem sinal) e
-- retorna o valor equivalente em decimal. 𝑏𝑖𝑛2𝑑𝑒𝑐 ∷ [𝐼𝑛𝑡] → 𝐼𝑛𝑡
bin2dec :: [Int] -> Int
bin2dec [] = 0
bin2dec (0:xs) = 0 + bin2dec xs
bin2dec (1:xs) = 2 ^ length xs + bin2dec xs


-- Definir uma função recursiva que recebe um número decimal inteiro não-negativo, um número de bits
-- desejado e retorna o valor equivalente em binário (interpretado como número inteiro sem sinal) com o
-- número de bits informado. Por exemplo, 𝑑𝑒𝑐2𝑏𝑖𝑛 2 8 deve retornar [0,0,0,0,0,0,1,0]. 𝑑𝑒𝑐2𝑏𝑖𝑛 ∷ 𝐼𝑛𝑡 →
-- 𝐼𝑛𝑡 → [𝐼𝑛𝑡]
dec2bin :: Int -> Int -> [Int]
dec2bin 0 0 = []
dec2bin v t = if v - (2 ^ (t-1)) < 0
                then 0 : dec2bin v (t-1)
                else 1 : dec2bin (v-(2 ^ (t-1))) (t-1)

-- Definir uma função recursiva que recebe um número binário na representação de complemento de dois e
-- retorna o valor equivalente em decimal inteiro. 𝑏𝑖𝑛𝑐𝑜𝑚𝑝𝑙2𝑑𝑒𝑐 ∷ [𝐼𝑛𝑡] → 𝐼𝑛𝑡
bincompl2dec :: [Int] -> Int
bincompl2dec [] = 0


-- Definir uma função recursiva que recebe um número decimal inteiro, um número de bits desejado e retorna
-- o valor equivalente em binário na representação de complemento de dois com o número de bits informado.
-- Por exemplo, 𝑑𝑒𝑐2𝑏𝑖𝑛𝑐𝑜𝑚𝑝𝑙 (−2) 8 deve retornar [1,1,1,1,1,1,1,0] 𝑑𝑒𝑐2𝑏𝑖𝑛𝑐𝑜𝑚𝑝𝑙 ∷ 𝐼𝑛𝑡 → 𝐼𝑛𝑡 →
-- [𝐼𝑛𝑡]


-- Definir uma função recursiva que recebe um número fracionário decimal por parâmetro e devolve um
-- número binário de ponto fixo de 32 bits. O número binário de ponto fixo dever ser representado por uma
-- tupla com dois números binários tal que a parte inteira deve estar na representação de complemento de
-- dois com 16 bits e a parte fracionária deve estar na representação de binário fracionado com 16 bits. Você
-- deve definir uma forma adequada de representar o resultado caso o número decimal estoure a
-- representação. Por exemplo, 𝑓𝑟𝑎𝑐2𝑏𝑖𝑛 (−8.5) deve retornar
-- ([1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0],[1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]). 𝑓𝑟𝑎𝑐2𝑏𝑖𝑛 ∷ 𝐷𝑜𝑢𝑏𝑙𝑒 →
-- ([𝐼𝑛𝑡],[𝐼𝑛𝑡])


-- Definir uma função recursiva que recebe uma tupla com dois números binários representando,
-- respectivamente, a parte inteira (na representação de complemento de dois com 16 bits) e a parte
-- fracionária (na representação de binário fracionado com 16 bits) de um número binário de ponto fixo com 32
-- bits, e retorna o correspondente valor fracionário decimal. Por exemplo,
-- 𝑏𝑖𝑛2𝑓𝑟𝑎𝑐 ([0,1,0,0,0,0,0,0,0,0,0,0,1,0,0,0],[1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0]) deve retornar
-- 16392.625. 𝑏𝑖𝑛2𝑓𝑟𝑎𝑐 ∷ ([𝐼𝑛𝑡],[𝐼𝑛𝑡]) → 𝐷𝑜𝑢𝑏𝑙�