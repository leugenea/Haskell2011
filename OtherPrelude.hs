--{-# LANGUAGE MultyParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module OtherPrelude where
import Prelude hiding ((++), tail, init, head, last, take, drop, takeWhile,
                       dropWhile, span, (!!), reverse, repeat, foldl, scanl,
		       foldr, scanr, map, concat, concatMap, zip, zipWith, break)

map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x:l) = (f x):(map f l)

-- Склеить два списка за O(length a)
(++) :: [a] -> [a] -> [a]
[] ++ b = b
(a:c) ++ b = a:(c ++ b)

-- Список без первого элемента
tail :: [a] -> [a]
tail [] = error "!!: empty list"
tail (x:a) = a

-- Список без последнего элемента
init :: [a] -> [a]
init [] = error "!!: empty list"
init [x] = []
init (x:a) = x:(init a)

-- Первый элемент
head :: [a] -> a
head [] = error "!!: empty list"
head (x:a) = x

-- Последний элемент
last :: [a] -> a
last [] = error "!!: empty list"
last [x] = x
last (x:a) = last a

-- n первых элементов списка
take :: Integer -> [a] -> [a]
take 0 a = []
take n [] = error "!!: not enough elements in list"
take n (x:a) = x:(take (n-1) a)

-- Список без n первых элементов
drop :: Integer -> [a] -> [a]
drop 0 a = a
drop n [] = error "!!: not enough elements in list"
drop n (x:a) = drop (n-1) a

-- Копировать из списка в результат до первого нарушения предиката
-- takeWhile (< 3) [1,2,3,4,1,2,3,4] == [1,2]
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile p [] = []
takeWhile p (x:a) = if (p x) then (x:(takeWhile p a)) else []

-- Не копировать из списка в результат до первого нарушения предиката,
-- после чего скопировать все элементы, включая первый нарушивший
-- dropWhile (< 3) [1,2,3,4,1,2,3,4] == [3,4,1,2,3,4]
dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile p [] = []
dropWhile p (x:a) = if (p x) then (dropWhile p a) else (x:a)

-- Разбить список в пару (найбольший префикс удовлетворяющий p, всё остальное)
span :: (a -> Bool) -> [a] -> ([a], [a])
span p a = ((takeWhile p a), (dropWhile p a))

-- Разбить список по предикату на (takeWhile p xs, dropWhile p xs),
-- но эффективнее
break :: (a -> Bool) -> [a] -> ([a], [a])
break p [] = ([], [])
break p (x:a) = if (p x) then (x:first, second) else ([], x:a) where (first, second) = (span p a)

-- n-ый элемент списка (считая с нуля)
(!!) :: [a] -> Integer -> a
[] !! n = error "!!: empty list"
(x:l) !! 0 = x
(x:l) !! n = l !! (n-1)

-- Список задом на перёд
reverse :: [a] -> [a]
reverse l = r l [] where r [] l = l
                         r (x:a) l = r a (x:l)

-- (*) Все подсписки данного списка
append :: a -> [[a]] -> [[a]]
append x xs = (map (\l -> x:l) xs)

begins :: [a] -> [[a]]
begins [] = [[]]
begins (x:l) = []:(append x (begins l))

subsequences :: [a] -> [[a]]
subsequences [] = [[]]
subsequences (x:xs) = (subsequences xs) ++ (append x (begins xs))

-- (*) Все перестановки элементов данного списка
insertAll :: a -> [a] -> [[a]]
insertAll t [] = [[t]]
insertAll t (x:l) = (t:(x:l)):(append x (insertAll t l))

insertInAll :: a -> [[a]] -> [[a]]
insertInAll t [] = []
insertInAll t (l:ll) = (insertAll t l) ++ (insertInAll t ll)

permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations (x:l) = insertInAll x (permutations l)

-- Повторяет элемент бесконечное число раз
repeat :: a -> [a]
repeat a = a:(repeat a)


-- Левая свёртка
-- порождает такое дерево вычислений:
--         f
--        / \
--       f   ...
--      / \
--     f   l!!2
--    / \
--   f   l!!1
--  / \
-- z  l!!0
foldl :: (a -> b -> a) -> a -> [b] -> a
foldl f z [] = z
foldl f z (x:l) = foldl f (f z x) l

-- Тот же foldl, но в списке оказываются все промежуточные результаты
-- last (scanl f z xs) == foldl f z xs
scanl :: (a -> b -> a) -> a -> [b] -> [a]
scanl f z [] = [z]
scanl f z (x:l) = (f z x):(scanl f (f z x) l)

-- Правая свёртка
-- порождает такое дерево вычислений:
--    f
--   /  \
-- l!!0  f
--     /  \
--   l!!1  f
--       /  \
--    l!!2  ...
--           \
--            z
--            
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z [] = z
foldr f z (x:l) = f x (foldr f z l)

-- Аналогично
--  head (scanr f z xs) == foldr f z xs.
scanr :: (a -> b -> b) -> b -> [a] -> [b]
scanr f z [] = [z]
scanr f z (x:l) = (f x (head res)):res where res = scanr f z l

finiteTimeTest = take 10 $ foldr (:) [] $ repeat 1

-- Склеивает список списков в список
concat :: [[a]] -> [a]
concat [[]] = []
concat (l:ll) = l ++ (concat ll)

-- Эквивалент (concat . map), но эффективнее
concatMap :: (a -> [b]) -> [a] -> [b]
concatMap f [] = []
concatMap f (x:l) = (f x) ++ (concatMap f l)

-- Сплющить два списка в список пар длинны min (length a, length b)
zip :: [a] -> [b] -> [(a, b)]
zip [] b = []
zip a [] = []
zip (x:a) (y:b) = (x, y):(zip a b)

-- Аналогично, но плющить при помощи функции, а не конструктором (,)
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f [] b = []
zipWith f a [] = []
zipWith f (x:a) (y:b) = (f x y):(zipWith f a b)

-- Интересные классы типов
class Monoid a where
    mzero :: a
    mappend :: a -> a -> a

instance Monoid [a] where
    mzero = []
    mappend = (++)

instance Monoid Integer where
    mzero = 0
    mappend = (+)

data MulInteger = Mult Integer
data MulRational = RMult Rational

-- Реализуйте инстансы Monoid для Rational и MulRational
--instance Monoid Rational where
--    mzero = 0
--    mappend = (+)

--instance Monoid MulRational where
--    mzero = 1
--    (RMult a) `mappend` (RMult b) = RMult $ a * b

--instance Monoid MulInteger where
--    mzero = 1
--    (Mult a) `mappend` (Mult b) = Mult $ a * b

-- Фолдабл
--class MFoldable t where
--    mfold :: Monoid a => (t a -> a)

-- Альтернативный фолдабл
--class Monoid a => AMFoldable t a where
--    amfold :: t a -> a
-- Изучите раздницу между этими двумя определениями.

-- Смотрите какой чит. Можно построить дерево только из элементов моноида.
--data MTree a = Monoid a => MLeaf | MNode a (MTree a) (MTree a)

-- Выпишите тип этого выражения. Фигурирует ли в нём Monoid? Почему?
--mtfold MLeaf = mzero -- А то, что a - моноид нам будет даровано самой природой
--mtfold (MNode a l r) = a `mappend` (mtfold l) `mappend` (mtfold r)

-- Напишите терм с типом
-- (...) => MTree a -> x
-- где x -- тип на ваш вкус, (...) - какие-то констреинты (возможно пустые),
-- при этом этот терм внутри должен использовать то, что a -- моноид, но в
-- констреинтах Monoid a быть не должно.
-- Для широты фантазии в терме можно использовать классы типов, определённые в любом
-- месте этого файла.
--mterm = ?

-- (**) Разберитесь чем отличаются эти определения.
-- "Скомпилируйте" их в наш гипотетический язык программирования с
-- типом Dict.
--instance MFoldable MTree where
--    mfold = mtfold

--instance Monoid a => AMFoldable MTree a where
--    amfold = mtfold

--------- Тут переделаем немного
-- Группа
--class Group a where
--    gzero :: a
--    ginv  :: a -> a
--    gmult :: a -> a -> a
--
--class Group Integer where
--    gzero = 0
--    ginv a = -a
--    gmult = (+)
--
--class Group MulInteger where
--    ? это я погорячился, да

-- Хаскель слабоват для нормального определения всех этих штук.
-- Кольцо вообще непонятно как определить, потому что группы и полугруппы
-- должны быть по паре (тип, операция).
--class Monoid a => Group a where
--    ginv :: a -> a

-- Определите
--instance Group для Integer, Rational, MulRational

-- Группу и Абелеву группу в Хаскеле тоже не различить :(
--class Group a => Ring a where
    -- mappend из моноида это сложение
--    rmul :: a -> a -> a -- а это умножение

-- Определите
--instance Ring для Integer, Rational

-- На самом деле коммутативное кольцо, но что поделать
--class Ring a => Field a where
--    rinv :: a -> a

-- Определите
--instance Field для Rational

-- Реализуйте тип для матриц (через списки) и операции над ними
--data Matrix a = ?
-- Чем должно быть a? Моноидом? Группой? Ещё чем-нибудь?

--matsum = ?

--matscalarmul = ?

--matmul = ?

-- (**) Реализуйте классы типов для векторных и скалярных полей.
-- Перепишите в этих терминах что-нибудь из написанного выше.
-- Реализуйте оператор дифференцирования, взятия градиента.
--class ? ScalarField ? where
--    ?

--class ? VectorField ? where
--    ?
