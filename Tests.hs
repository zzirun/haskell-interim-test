module Tests where
import IC.TestSuite
import Radix


-- No autotests for size, but you can try the following manually:
-- *Main> size t1
-- 43
-- *Main> size' t2
-- 31
-- *Main> size' figure
-- 41


-- *Main> binary 0
-- [0]
-- *Main> binary 1
-- [1]
-- *Main> binary 13
-- [1,1,0,1]
binaryTestCases
  = [ 0 ==> [0],
      1 ==> [1],
      13 ==> [1,1,0,1]
    ]


-- *Main> insert [0] (Leaf False)
-- Node False (Leaf True) (Leaf False)
-- *Main> insert [1] (Leaf False)
-- Node False (Leaf False) (Leaf True)
-- *Main> insert [0] (insert [1] (Leaf False))
-- Node False (Leaf True) (Leaf True)
-- *Main> insert [0,1] (insert [1] (Leaf False))
-- Node False (Node False (Leaf False) (Leaf True)) (Leaf True)
insertTestCases
  = [ ([0],(Leaf False)) ==> Node False (Leaf True) (Leaf False),
      ([1],(Leaf False)) ==> Node False (Leaf False) (Leaf True),
      ([0],(insert [1] (Leaf False))) ==> Node False (Leaf True) (Leaf True),
      ([0,1],(insert [1] (Leaf False))) ==> Node False (Node False (Leaf False)
      (Leaf True)) (Leaf True)
    ]


-- *Main> buildRadixTree []
-- Leaf False
-- *Main> buildRadixTree [5,3,2]
-- Node False (Leaf False) (Node False (Node True (Leaf False) (Leaf True))
-- (Leaf True))
-- *Main> buildRadixTree [0,1,3,7,12] == figure
-- True
-- *Main> buildRadixTree [12,0,3,1,7] == figure
-- True
buildRadixTreeTestCases
  = [ [] ==> Leaf False,
      [5,3,2] ==> Node False (Leaf False) (Node False (Node True (Leaf False)
      (Leaf True)) (Leaf True)),
      ([0,1,3,7,12]) ==> figure,
      ([12,0,3,1,7]) ==> figure
    ]


-- *Main> member 4 (buildRadixTree [1,3,7])
-- False
-- *Main> member 7 (buildRadixTree [1,3,7])
-- True
-- *Main> member 2 (buildRadixTree [])
-- False
-- *Main> member 0 (buildRadixTree [0])
-- True
-- *Main> member 6 (insert [1,1,0] figure)
-- True
memberTestCases
  = [ (4,(buildRadixTree [1,3,7])) ==> False,
      (7,(buildRadixTree [1,3,7])) ==> True,
      (2,(buildRadixTree [])) ==> False,
      (0,(buildRadixTree [0])) ==> True,
      (6,(insert [1,1,0] figure)) ==> True
    ]


-- *Main> union (buildRadixTree [1,3,7]) (buildRadixTree [2,3,4,7])
-- Node False (Leaf False) (Node True (Node True (Leaf True) (Leaf False))
-- (Node True (Leaf False) (Leaf True)))
-- *Main> union figure (buildRadixTree []) == figure
-- True
unionTestCases
  = [ ((buildRadixTree [1,3,7]),(buildRadixTree [2,3,4,7])) ==> Node False
      (Leaf False) (Node True (Node True (Leaf True) (Leaf False)) (Node True
      (Leaf False) (Leaf True))),
      (figure,(buildRadixTree [])) ==> figure
    ]


-- *Main> intersection (buildRadixTree [1,3,7]) (buildRadixTree [2,3,4,7])
-- Node False (Leaf False) (Node False (Leaf False) (Node True (Leaf False)
-- (Leaf True)))
-- *Main> intersection figure (buildRadixTree [])
-- Leaf False
intersectionTestCases
  = [ ((buildRadixTree [1,3,7]),(buildRadixTree [2,3,4,7])) ==> Node False
     (Leaf False) (Node False (Leaf False) (Node True (Leaf False) (Leaf True)))
     ,(figure,(buildRadixTree [])) ==> Leaf False
    ]


-- You can add your own test cases above

allTestCases
  = [
      TestCase "binary" (binary)
              binaryTestCases
    , TestCase "insert" (uncurry insert)
              insertTestCases
    , TestCase "buildRadixTree" (buildRadixTree)
              buildRadixTreeTestCases
    , TestCase "member" (uncurry member)
              memberTestCases
    , TestCase "union" (uncurry union)
              unionTestCases
    , TestCase "intersection" (uncurry intersection)
              intersectionTestCases
    ]


runTests = mapM_ goTest allTestCases

main = runTests
