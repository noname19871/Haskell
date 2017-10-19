{-
Разбить список на список пар
рекурсивно и с использованием стандартных функций
-}

zipRec :: [a] -> [(a, a)]
zipRec [] = []
zipRec (x:[]) = []
zipRec (x:y:xs) = (x, y) : zipRec (y:xs)

zipStandart :: [a] -> [(a, a)]
zipStandart (x:xs) = zip (x:xs) xs
