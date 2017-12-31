qsort [] = []
qsort (x:xs) = qsort is ++ [x] ++ qsort ts
    where
        is = [i| i <- xs, i < x]
        ts = [t| t <- xs, t >= x]

main = do
    print $ qsort [1, 2, 3, 3, 4]
