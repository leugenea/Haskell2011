module S where

import Data.Set

data Logic a =
	   And (Logic a) (Logic a)
	|  Or (Logic a) (Logic a)
	|  Not (Logic a)
	|  Var a
    deriving (Eq , Ord , Show)

type LS a = Set (Logic a)

insert2 :: Ord a => a -> a -> Set a -> Set a
insert2 x y s = insert x (insert y s)

simplify :: Ord a => ( LS a , LS a ) -> ( LS a , LS a ) -> Set (( LS a , LS a ))
simplify ( lst, lsf ) ( lvt, lvf ) = if (Data.Set.null lst)
	then if (Data.Set.null lsf)
		then singleton $ (lvt,lvf)
		else let min = findMin lsf in
			case min of
				(Var x)   -> singleton $ (lvt, union lsf lvf)
				(And x y) -> union (simplify (empty , insert x (deleteMin lsf)) (lvt , lvf)) (simplify (empty , insert y (deleteMin lsf)) (lvt , lvf))
				(Or x y)  -> simplify (empty , insert2 x y (deleteMin lsf)) (lvt , lvf)
				(Not x)   -> simplify (singleton $ x , deleteMin lsf) (lvt , lvf)
	else let min = findMin lst in
		case min of
			(Var x)   -> simplify (empty , lsf) (union lst lvt , lvf)
			(And x y) -> simplify (insert2 x y (deleteMin lst) , lsf) (lvt , lvf)
			(Or x y)  -> union (simplify (insert x (deleteMin lst) , lsf) (lvt , lvf)) (simplify (insert y (deleteMin lst) , lsf) (lvt , lvf))
			(Not x)   -> simplify (deleteMin lst , insert x lsf) (lvt , lvf)

checkSol :: Ord a => (LS a , LS a) -> Bool
checkSol (lvt , lvf) = Data.Set.null $ (intersection lvt lvf)

filterSols :: Ord a => Set (( LS a , LS a )) -> Set (( LS a , LS a))
filterSols s = Data.Set.filter checkSol s

printLS :: (Ord a, Show a) => LS a -> Bool -> [String]
printLS s b = let bstr = show $ b in
	if (Data.Set.null s)
	then []
	else let min = findMin s in
		case min of
			(Var v) -> [(show $ v) ++ " = " ++ bstr] ++ printLS (deleteMin s) b
			_       -> error $ show s

printSols :: (Ord a, Show a) => Set (( LS a , LS a )) -> Set (( [String] , [String] ))
printSols s = Data.Set.map (\(s1 , s2) -> (printLS s1 True , printLS s2 False)) s

solve :: (Ord a, Show a) => Logic a -> Set (( [String] , [String] ))
solve x = (printSols . filterSols . flip simplify (empty, empty)) (empty, singleton $ (Not x))
