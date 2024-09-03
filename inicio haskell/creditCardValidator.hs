-- CreditCardValidator.hs

-- Ejercicio 1: toDigits y toDigitsRev
toDigits :: Integer -> [Integer]
toDigits n
    | n <= 0    = []
    | otherwise = map (read . (:[])) (show n)

toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse (toDigits n)

-- Ejercicio 2: doubleEveryOther
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther xs = reverse (zipWith ($) (cycle [id, (*2)]) (reverse xs))

-- Ejercicio 3: sumDigits
sumDigits :: [Integer] -> Integer
sumDigits xs = sum (concatMap toDigits xs)

-- Ejercicio 4: validate
validate :: Integer -> Bool
validate n = (sumDigits (doubleEveryOther (toDigits n))) `mod` 10 == 0

-- Función main para probar todos los ejercicios
main :: IO ()
main = do
    putStrLn "Ingrese un número: "
    input <- getLine
    let number = read input :: Integer
    
    putStrLn "Lista de dígitos:"
    print $ toDigits number
    
    putStrLn "Lista de dígitos en orden inverso:"
    print $ toDigitsRev number
    
    putStrLn "Duplica cada segundo dígito comenzando desde la derecha:"
    print $ doubleEveryOther (toDigits number)
    
    putStrLn "Suma de los dígitos:"
    print $ sumDigits (doubleEveryOther (toDigits number))
    
    putStrLn "¿Es un número de tarjeta válido?"
    print $ validate number