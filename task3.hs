import System.Environment
import Data.List

type Filename = String
type Quarter = Int

data Params = P Filename Quarter
          deriving (Show, Ord, Eq)

--Парсим аргументы командной строки, чтобы получить имя файла и номер искомой четверти
parseArgs :: [String] -> Maybe Params
parseArgs [] = Nothing
parseArgs (name:quarter:args) = Just (P name (read quarter))

--Загружаем строки из файла
loadData :: String -> IO [String]
loadData fname = do
  fc <- readFile fname
  return $ lines fc

--Разбиваем строку файла на координаты
coordinates :: String -> (Int, Int)
coordinates s = (read (head (words s)), read (last (words s)))

--Определяем четверть
defineQuarter :: (Int, Int) -> Int
defineQuarter (x, y)
              | x >= 0, y >= 0 = 1
              | x <= 0, y >= 0 = 2
              | x <= 0, y <= 0 = 3
              | x >= 0, y <= 0 = 4

--Парсим координаты из файла: Если четверть совпадает с введенной с консоли
--добавляем к результату, если нет, то движемся дальше
parseCoordinates :: [String] -> Quarter -> [String]
parseCoordinates [] n = []
parseCoordinates (x:xs) n = if defineQuarter (coordinates x) == n then
                                    x : (parseCoordinates xs n) else parseCoordinates xs n

--Основная функция: Нужна для печати результата в консоль
parseFile :: Params -> [String] -> IO ()
parseFile (P name quarter) ss = print $ unlines (parseCoordinates ss quarter)

-- Вывод информации о порядке использования генератора
usage :: IO ()
usage = print "incorrect usage of function"

main :: IO()
main = do
      args <- getArgs
      ss <- loadData (head args)
      case parseArgs args of
        Just p -> parseFile p ss
        Nothing -> usage
