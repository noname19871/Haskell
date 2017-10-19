{-
рекурсивная функция, которая считает факториал целого числа
-}

factN :: Integer -> Integer
factN 1 = 1
factN n = n * factN (n - 1)
