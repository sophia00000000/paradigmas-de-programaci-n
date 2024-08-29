-- Agregar un nuevo producto al inventario
addProduct :: [(String, Double, Int)] -> String -> Double -> Int -> [(String, Double, Int)]
addProduct inventory name price quantity = inventory ++ [(name, price, quantity)]

-- Actualizar la cantidad de un producto existente
updateQuantity :: [(String, Double, Int)] -> String -> Int -> [(String, Double, Int)]
updateQuantity [] _ _ = []
updateQuantity ((n, p, q):xs) name newQuantity
    | n == name = (n, p, newQuantity) : xs
    | otherwise = (n, p, q) : updateQuantity xs name newQuantity

-- Eliminar un producto del inventario
removeProduct :: [(String, Double, Int)] -> String -> [(String, Double, Int)]
removeProduct inventory name = filter (\(n, _, _) -> n /= name) inventory

-- Resumen del inventario: total de productos y valor total
inventorySummary :: [(String, Double, Int)] -> (Int, Double)
inventorySummary inventory = (totalQuantity, totalValue)
  where
    totalQuantity = sum [q | (_, _, q) <- inventory]
    totalValue = sum [p * fromIntegral q | (_, p, q) <- inventory]

--Buscar un poroducto en el inventario
searchProduct :: [(String, Double, Int)] -> String -> Maybe (Double, Int)
searchProduct [] _ = Nothing
searchProduct ((n, p, q):xs) name
    | n == name = Just (p, q)
    | otherwise = searchProduct xs name

--Descuento
applyDiscount :: [(String, Double, Int)] -> Double -> [(String, Double, Int)]
applyDiscount inventory discount = map aplicarDescuentoProducto inventory
  where
    aplicarDescuentoProducto (name, price, quantity) = (name, (price*discount / 100), quantity)


main :: IO ()
main = do
    let inventory = []
    let inventory1 = addProduct inventory "Manzanas" 20000 100
    let inventory2 = addProduct inventory1 "Platanos" 0.3 150
    let inventory3 = updateQuantity inventory2 "Manzanas" 120
    let inventory4 = removeProduct inventory3 "Platanos"
    let inventory5 = applyDiscount inventory4 50
    let (totalQty, totalValue) = inventorySummary inventory5

    case searchProduct inventory5 "Manzanas" of
        Just (p, q)  -> putStrLn $ "Manzanas: Precio = " ++ show p ++ ", Cantidad = " ++ show q
        Nothing -> putStrLn "Manzanas no encontrado en el inventario"
    
    --putStrLn $ "Inventario Final: " ++ show inventory4
    putStrLn $ "Producto con descuento: " ++ show inventory5
    putStrLn $ "Total de productos en stock: " ++ show totalQty
    putStrLn $ "Valor total del inventario: " ++ show totalValue