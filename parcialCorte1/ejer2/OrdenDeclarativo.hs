import Data.List (sortBy)
import Data.Ord (comparing)

estudiantes :: [(String, Double)]
estudiantes = [("Laura", 1), ("Angie", 4.5), ("Belen", 3.4), ("Carlos", 4), ("Nicolas", 5), ("Esteban", 1.2)]

ordenarEstudiantes :: [(String, Double)] -> [(String, Double)]
ordenarEstudiantes = sortBy (comparing (\(nombre, nota) -> (-nota, nombre)))

main :: IO ()
main = do
    let estudiantesOrdenada = ordenarEstudiantes estudiantes
    putStrLn "Lista de estudiantes ordenada:"
    mapM_ (putStrLn . show) estudiantesOrdenada
