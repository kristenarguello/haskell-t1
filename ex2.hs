-- Definir uma função recursiva que recebe um número decimal inteiro não-negativo, um número de bits
-- desejado e retorna o valor equivalente em binário (interpretado como número inteiro sem sinal) com o
-- número de bits informado. Por exemplo, 𝑑𝑒𝑐2𝑏𝑖𝑛 2 8 deve retornar [0,0,0,0,0,0,1,0]. 𝑑𝑒𝑐2𝑏𝑖𝑛 ∷ 𝐼𝑛𝑡 →
-- 𝐼𝑛𝑡 → [𝐼𝑛𝑡]

dec2bin :: Int -> Int -> [Int]
dec2bin 0 0 = []
dec2bin v t = if v - (2 ^ (t-1)) < 0
                then 0 : dec2bin v (t-1)
                else 1 : dec2bin (v-(2 ^ (t-1))) (t-1)
