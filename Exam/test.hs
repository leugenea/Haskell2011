import Test.HUnit
import S
import Data.Set

v1 = Var 1
v2 = Var 2

testPrim = "testPrim" ~: test $
	[ solve (v1) ~?= (singleton ((["1 = True"], [])))
	, solve (Not v1) ~?= (singleton ([] , ["1 = False"]))
	, solve (And v1 v2) ~?= (singleton (["1 = True" , "2 = True"] , []))
	, solve (Or v1 v2) ~?= fromList [(["1 = True"],[]),(["2 = True"],[])]
	]
testPrim2 = "test2" ~: test $
	[ solve (Not (Or v1 v2)) ~?= (singleton ([], ["1 = False" , "2 = False"]))
	, solve (And v1 (Or v1 v2)) ~?= (union (singleton (["1 = True"] , [])) (singleton (["1 = True", "2 = True"] , [])))
	, solve (And v1 (Not (Or v1 v2))) ~?= empty
	]

main = runTestTT $ test [testPrim, testPrim2]
