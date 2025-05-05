sudoku can be specified by the following: 
2d array of values 1-9, in which each digit appears once in each row, column and box.
i denote xijn = true to denote that the number n exists in location (i,j) 

> writeF s = writeFile "output.z3" s

> doAll = writeF ((genVars 999) ++ rowFull ++ colFull ++ boxFull ++ squareFull) 

> genVars :: Int -> String
> genVars 110 = " \n"
> genVars i | (i `mod` 10) == 0  = genVars (i-1)
>           | (i `rem` 100) < 11 = genVars (i-1)
>           | otherwise          = "(declare-const x" ++ show i ++ " Bool) \n" ++ genVars (i-1)

> rowClause :: Int -> Int -> String
> rowClause i n = "(_ at-least 1) " ++ (foldr (++) "" [" x" ++ show i ++ show j ++ show n | j <- [1..9]])

> rowFull = foldr (++) "" [ "(assert (" ++ rowClause i n ++ "))\n" | i <- [1..9], n <- [1..9]]

> colClause :: Int -> Int -> String
> colClause j n = "(_ at-least 1) " ++ (foldr (++) "" [" x" ++ show i ++ show j ++ show n | i <- [1..9]])

> colFull = foldr (++) "" [ "(assert (" ++ colClause j n ++ "))\n" | j <- [1..9], n <- [1..9]]

> boxClause :: Int -> Int -> Int -> String
> boxClause k u v = "(_ at-least 1) " ++ (foldr (++) "" [" x" ++ show (3*u + i) ++ show (3*v + j) ++ show k | j <- [1..3], i <- [1..3]])

> boxFull = foldr (++) "" [ "(assert (" ++ boxClause k u v ++ "))\n" | k <- [1..9], u <- [0..2], v <- [0..2]]

> squareClause :: Int -> Int -> Int -> Int -> String
> squareClause i j k k' = " not(and x" ++ show i ++ show j ++ show k ++ " x" ++ show i ++ show j ++ show k' ++  ")"

> squareFull = foldr (++) "" [ "(assert (" ++ squareClause i j k k' ++  "))\n" | i <- [1..9], j <- [1..9], k <- [1..9], k' <- [1..9], k < k']
