import Text.Printf
import Text.Read (readMaybe)
import Data.List (intercalate)

-- ===============================
-- RCS formulas for basic shapes
-- ===============================

sphereRCS :: Double -> Double -> Double
sphereRCS r lambda = (pi * r * r) / (lambda * lambda)

plateRCS :: Double -> Double -> Double
plateRCS area lambda = (4 * pi * area * area) / (lambda * lambda)

cubeRCS :: Double -> Double -> Double
cubeRCS side lambda = (6 * side * side) / (lambda * lambda)

-- ===============================
-- Radar angle reflection model
-- ===============================

angleRCS :: Double -> Double -> Double
angleRCS base angle =
    base * (cos (angle * pi / 180))^2

-- ===============================
-- Heatmap model
-- ===============================

customRCS :: Double -> Double -> Double -> Double
customRCS k x y =
    (k * 1000) / (x*x + y*y + 1)

-- ===============================
-- Heatmap symbols
-- ===============================

symbol :: Double -> Char
symbol v
    | v > 50    = '#'
    | v > 20    = '*'
    | v > 5     = '+'
    | v > 1     = '.'
    | otherwise = ' '

-- ===============================
-- Generate heatmap
-- ===============================

heatmap :: Int -> Double -> [String]
heatmap size k =
    [ [ symbol (customRCS k (fromIntegral x) (fromIntegral y))
        | x <- [-size .. size] ]
        | y <- reverse [-size .. size] ]

printMap :: [String] -> IO ()
printMap rows = mapM_ putStrLn rows

-- ===============================
-- Input helpers
-- ===============================

promptDouble :: String -> IO Double
promptDouble msg = do
    putStr msg
    input <- getLine
    case readMaybe input of
        Just val -> return val
        Nothing  -> do
            putStrLn "Invalid number. Try again."
            promptDouble msg

promptInt :: String -> IO Int
promptInt msg = do
    putStr msg
    input <- getLine
    case readMaybe input of
        Just val -> return val
        Nothing  -> do
            putStrLn "Invalid integer. Try again."
            promptInt msg

-- ===============================
-- Angle simulation table
-- ===============================

printAngleSimulation :: Double -> IO ()
printAngleSimulation base = do
    putStrLn "\nRadar Angle Simulation:"
    putStrLn "Angle (deg)   RCS"
    mapM_ (\a -> printf "%3d°        %.6f m²\n" a (angleRCS base (fromIntegral a)))
          ([0,10..90] :: [Int])

-- ===============================
-- MAIN
-- ===============================

main :: IO ()
main = do
    putStrLn "====================================="
    putStrLn "   RADAR CROSS SECTION SIMULATOR"
    putStrLn "====================================="

    putStrLn "Select object type:"
    putStrLn " 1. Sphere (formula)"
    putStrLn " 2. Flat Plate (formula)"
    putStrLn " 3. Cube (formula)"
    putStrLn " 4. Custom object"
    putStrLn " 5. F-22 Raptor (~0.0001 m²)"
    putStrLn " 6. B-2 Spirit (~0.001 m²)"
    putStrLn " 7. Bomb (~1 m²)"
    putStrLn " 8. Man (~1 m²)"
    putStrLn " 9. Bird (~0.01 m²)"
    putStrLn "10. Commercial plane (~100 m²)"
    putStrLn "11. Insect (~0.00001 m²)"

    choice <- promptInt "Enter your choice (1-11): "

    rcsValue <- case choice of
        1 -> do
            r <- promptDouble "Enter radius: "
            l <- promptDouble "Enter wavelength: "
            return (sphereRCS r l)

        2 -> do
            a <- promptDouble "Enter plate area: "
            l <- promptDouble "Enter wavelength: "
            return (plateRCS a l)

        3 -> do
            s <- promptDouble "Enter cube side: "
            l <- promptDouble "Enter wavelength: "
            return (cubeRCS s l)

        4 -> promptDouble "Enter reflectivity constant: "

        5 -> return 0.0001
        6 -> return 0.001
        7 -> return 1.0
        8 -> return 1.0
        9 -> return 0.01
        10 -> return 100.0
        11 -> return 0.00001

        _ -> do
            putStrLn "Invalid option. Using default value."
            return 10.0

    printf "\nBase RCS value: %.6f m²\n" rcsValue

    -- Angle simulation
    printAngleSimulation rcsValue

    -- Heatmap
    size <- promptInt "\nEnter radar grid size (e.g., 5): "

    putStrLn "\nRadar Heatmap (top view):"

    let mapData = heatmap size rcsValue
    printMap mapData
