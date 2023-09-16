-- Definir uma função recursiva que recebe um número binário (interpretado como número inteiro sem sinal) e
-- retorna o valor equivalente em decimal. 𝑏𝑖𝑛2𝑑𝑒𝑐 ∷ [𝐼𝑛𝑡] → 𝐼𝑛𝑡

bin2dec :: [Int] -> Int
bin2dec [] = 0
bin2dec (0:xs) = 0 + bin2dec xs
bin2dec (1:xs) = 2 ^ length xs + bin2dec xs
