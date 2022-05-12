merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) = if x<y then x: merge xs (y:ys) else y: merge (x:xs) ys

mSort [] =[]
mSort [x] = [x]
mSort xs = merge (mSort(take (length xs `div` 2)  xs)) (mSort(drop (length xs `div` 2) xs))
