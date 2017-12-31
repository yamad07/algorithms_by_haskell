insert x [] = [x]
insert x (y:ys)
    | x < y = x:y:ys
    | otherwise = y : insert x ys

isort [] = []
isort (x:xs) = insert x (isort xs)

main = do
    print $ isort [2, 3, 1, 3, 10, 11]
