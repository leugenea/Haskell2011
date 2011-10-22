module Lambda where

import Prelude hiding (iterate, (++), elem)

type Varible = String

-- Лямбда-терм
data Term = Var Varible
          | Abs Varible Term
          | App Term Term
          deriving (Show)

-- Тип [ a ] == Типу List a
-- значение [] == значению Nil
-- значение [ a ] == значению Cons a Nil
-- конструктор (:) == конструктору Cons

-- Конкатенация двух списков
(++) :: [a] -> [a] -> [a]
[] ++ b = b
(a:as) ++ b = a:(as ++ b)

-- Свободные переменные терма
free (Var v) = [v]
free (Abs v t) = filter (/= v) . free $ t -- /= это <<не равно>>
free (App t t') = (free t) ++ (free t')

-- Заменить все вхождения переменной var на what в терме term
subst var what term = case term of
    Var v    -> if v == var then what else term
    Abs v t  -> if v == var then term else Abs v (subst var what t)
    App t t' -> App (subst var what t) (subst var what t')

-- Содержит ли список элемент?
elem a [] = False
elem a (l:ls) = if a == l then True else elem a ls

-- Любопытная функция
iterate f x = (:) x $ iterate f (f x)

-- Генерирует список имён, производных от v, не входящих в fv
newname fv v = head . filter (\x -> not . elem x $ fv) . iterate ('_':) $ v

-- Обычная бета-редукция, хендлящая переименования переменных
betaReduct :: Varible -> Term -> Term -> Term
betaReduct var what term =
    case term of
        Var v     -> if v == var
	    then what
	    else term
	App t1 t2 -> App (betaReduct var what t1) (betaReduct var what t2)
	Abs v t   -> if v == var
	    then term
	    else Abs v (betaReduct var newwhat t) where
	        newwhat = subst v (Var (newvar)) what where
	            newvar = newname ((free what) ++ (free t)) v

-- Нормализация нормальным порядком терма term
normal' :: Term -> Term
normal' (Var var) = (Var var)
normal' (Abs var term) = Abs var (normal' term)
normal' (App term1 term2) = 
    case term1 of
        Var var      -> App term1 (normal' term2)
        Abs var term -> normal' (betaReduct var term2 term)
        App t1 t2    -> case newterm1 of
            Var nvar          -> App newterm1 (normal' term2)
            Abs nvar nterm    -> normal' (betaReduct nvar  term2 nterm)
            App nterm1 nterm2 -> App nterm1 (normal' term2)
          where newterm1 = normal' term1

-- Эти строчки после реализации стереть

-- Нормализация аппликативным порядком терма term
applicative' :: Term -> Term
applicative' (Var var) = Var var
applicative' (Abs var term) = Abs var (applicative' term)
applivative' (App term1 term2) =
    case term1 of
        Var var      -> App term1 (applicative' term2)
	Abs var term -> betaReduct var (applicative' term2) term
	App t1 t2    -> case newterm1 of
	    Var nvar          -> App newterm1 (applicative' term2)
	    Abs nvar nterm    -> betaReduct nvar (applicative' term2) nterm
	    App nterm1 nterm2 -> App nterm1 (applicative' term2)
	  where newterm1 = applicative' term1

-- Эти строчки после реализации стереть

-- Маркер конца ресурсов
data TooLoong = TooLoong deriving Show

strat :: (Term -> (Bool, Term)) -> Int -> Term -> Either TooLoong (Int, Term)
strat st n term = if n < 0 then error "n < 0 !!1"
                           else if r then if (n == 0) then Left TooLoong
			                              else strat st (n-1) t
				     else Right (n, t)
			     where (r, t) = st term

-- (*) Нормализация нормальным порядком терма term за неболее чем n шагов.
-- Результат: Или числа итераций недостаточно, чтобы достичь нормальной
-- формы. Или (число нерастраченных итераций, терм в нормальной форме).

normSt :: Term -> (Bool, Term)
normSt (Var v) = (False, Var v)
normSt (Abs v t) = (res, Abs v new)
                     where (res, new) = normSt t
normSt (App term1 term2) = case term1 of
                             (Abs v t) -> (True, betaReduct v term2 t)
                             (_)       -> if r1 then (True, App new1 term2)
		                                else if r2 then (True, App term1 new2)
					                   else (False, App term1 term2)
						       where (r2, new2) = normSt term2
					    where (r1, new1) = normSt term1

normal :: Int -> Term -> Either TooLoong (Int, Term)
normal = strat normSt

-- Эту строчку после реализации стереть

-- (*) Аналогичная нормализация аппликативным порядком.

applSt :: Term -> (Bool, Term)
applSt (Var v) = (False, Var v)
applSt (Abs v t) = (res, Abs v new)
                     where (res, new) = applSt t
applSt (App term1 term2) = let (r2, new2) = applSt term2 in
                               if r2 then (True, App term1 new2)
                                   else if r1 then (True, App new1 term2)
				              else case term1 of
					          (Abs v t) -> (True, betaReduct v term2 t)
						  (_)       -> (False, App term1 term2)
				          where (r1, new1) = applSt term1

applicative :: Int -> Term -> Either TooLoong (Int, Term)
applicative = strat applSt

-- Эту строчку после реализации стереть

-- (***) Придумайте и реализуйте обобщённую функцию, выражающую некоторое
-- семейство стратегий редуцирования. В том смысле, что номальная, нормальная
-- головная, нормальная слабо-головная и аппликативная стратегии
-- при помощи этой функции будут выражаться некоторым элементарным образом.
-- Аргумент n можно отбросить, а можно оставить.
--
-- strategy = ?
--
-- normal = strategy ?
-- hnf = strategy ?
-- whnf = strategy ?
-- applicative = strategy ?
--
-- Какие ещё стратегии редуцирования вы знаете? Можно ли их выразить
-- при помощи этой стратегии? Если да, то как?
-- Если нет, то можно ли реализовать аналогичную функцию для _всех_
-- возможных стратегий редуцирования, а не только для такого семейства?
-- Если да, то как? Если нет, то почему?

--------------------------------------------------------

-- Область тестирования

loop' = Abs "x" $ App (Var "x") (Var "x")
loop = App loop' loop'

u = Abs "a" $ Abs "b" $ App (Var "a") $ App (Var "b") (Var "_b")
v = Abs "a" $ Abs "b" $ App (App (Var "a") (Var "b")) (Var "_b")
w = Abs "a" $ Abs "b" $ Abs "c" $ Abs "d" $ App (App (Var "a") (Var "b")) (App (Var "c") (Var "d"))

main = test 100
    [ ("no", normal)
    , ("ap", applicative) ]
    [ Var "a"
    , u
    , v
    , loop'
    , u `App` Var "a"
    , v `App` Var "a"
    , u `App` Var "b"
    , v `App` Var "b"
    , u `App` Var "_b"
    , v `App` Var "_b"
    , (u `App` Var "_b") `App` Var "_b"
    , (v `App` Var "_b") `App` Var "_b"
    , w
    , w `App` (Abs "a" (Var "a") `App` (Abs "b" $ Var "b"))
    , (w `App` Abs "a" (Var "b")) `App` loop
    , loop
    ]

-- Если вы не понимаете как это работает, то пока и не надо
pall n term  = mapM_ (\(desc, reduce) -> putStr (desc ++ ": ") >> print (reduce n term))

test :: Show a => Int -> [(String, Int -> Term -> a)] -> [Term] -> IO ()
test n funcs = mapM_ (\term -> print term >> pall n term funcs)
