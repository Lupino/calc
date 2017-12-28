module Main (main) where

toRPN :: [String] -> [String]
toRPN []             = []
toRPN ("(":xs)       = toGroupRPN "(" $ splitGroup 0 xs []
toRPN ("+":"(":xs)   = toGroupRPN "+" $ splitGroup 0 xs []
toRPN ("-":"(":xs)   = toGroupRPN "-" $ splitGroup 0 xs []
toRPN ("*":"(":xs)   = toGroupRPN "*" $ splitGroup 0 xs []
toRPN ("/":"(":xs)   = toGroupRPN "/" $ splitGroup 0 xs []
toRPN ("+":x:"*":xs) = (x:toRPN ("*":xs)) ++ ["+"]
toRPN ("+":x:"/":xs) = (x:toRPN ("/":xs)) ++ ["+"]
toRPN ("-":x:"*":xs) = (x:toRPN ("*":xs)) ++ ["-"]
toRPN ("-":x:"/":xs) = (x:toRPN ("/":xs)) ++ ["-"]
toRPN ("+":x:xs)     = x : "+" : toRPN xs
toRPN ("-":x:xs)     = x : "-" : toRPN xs
toRPN ("*":x:xs)     = x : "*" : toRPN xs
toRPN ("/":x:xs)     = x : "/" : toRPN xs
toRPN (x:xs)         = x : toRPN xs

splitGroup :: Int -> [String] -> [String] -> ([String], [String])
splitGroup idx ("(":xs) ys = splitGroup (idx + 1) xs (ys ++ ["("])
splitGroup idx (")":xs) ys | idx == 0  = (ys, xs)
                           | otherwise = splitGroup (idx - 1) xs (ys ++ [")"])

splitGroup idx (x:xs) ys = splitGroup idx xs (ys ++ [x])
splitGroup idx [] ys     = (ys, [])

toGroupRPN :: String -> ([String], [String]) -> [String]
toGroupRPN "+" (ys, "*":xs) = toRPN ys ++ toRPN ("*":xs) ++ ["+"]
toGroupRPN "+" (ys, "/":xs) = toRPN ys ++ toRPN ("/":xs) ++ ["+"]
toGroupRPN "-" (ys, "*":xs) = toRPN ys ++ toRPN ("*":xs) ++ ["-"]
toGroupRPN "-" (ys, "/":xs) = toRPN ys ++ toRPN ("/":xs) ++ ["-"]
toGroupRPN "(" (ys, xs)     = toRPN ys ++ toRPN xs
toGroupRPN z (ys, xs)       = toRPN ys ++ [z] ++ toRPN xs

split :: String -> String -> [String]
split [] ys       = [ys]
split [' '] ys    = [ys]
split [')'] ys    = [ys, ")"]
split [x]   ys    = [ys ++ [x]]
split (' ':xs) ys = split xs ys
split ('+':xs) ys = ys : "+" : split xs ""
split ('-':xs) ys = ys : "-" : split xs ""
split ('*':xs) ys = ys : "*" : split xs ""
split ('/':xs) ys = ys : "/" : split xs ""
split ('(':xs) ys = ys : "(" : split xs ""
split (')':xs) ys = ys : ")" : split xs ""
split (x:xs) ys   = split xs (ys ++ [x])

solveRPN :: [String] -> Float
solveRPN = head . foldl foldingFunction []
  where foldingFunction (x:y:ys) "*"    = (x * y):ys
        foldingFunction (x:y:ys) "+"    = (x + y):ys
        foldingFunction (x:y:ys) "-"    = (y - x):ys
        foldingFunction (x:y:ys) "/"    = (y / x):ys
        foldingFunction xs numberString = read numberString:xs

main :: IO ()
main = print $ solveRPN $ toRPN $ filter (not . null) $ split "10 - (4 + 3) * ((2 + 3) / 10)" ""
