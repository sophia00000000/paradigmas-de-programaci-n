hanoi :: Integer -> Char -> Char -> Char -> [(Char, Char)]
hanoi 0 _ _ _ = []
hanoi n from to aux =
    hanoi (n - 1) from aux to ++  -- Mueve n-1 discos de 'from' a 'aux'
    [(from, to)] ++               -- Mueve el disco más grande de 'from' a 'to'
    hanoi (n - 1) aux to from     -- Mueve los n-1 discos de 'aux' a 'to'

main :: IO ()
main = do
    let numDiscos = 3
    let movimientos = hanoi numDiscos 'A' 'C' 'B'
    print movimientos
