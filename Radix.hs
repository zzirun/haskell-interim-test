module Radix where
  
data Tree a = Empty | Leaf a | Node a (Tree a) (Tree a)
            deriving (Eq, Show)

type IntTree = Tree Int

type RadixTree = Tree Bool

type BitString = [Int]

--------------------------------------------------------------------------

buildIntTree :: [Int] -> IntTree
buildIntTree
  = foldr add Empty
  where
    add x Empty
      = Leaf x
    add x (Leaf y)
      = add x (Node y Empty Empty)
    add x t@(Node y l r)
      | x == y    = t
      | x < y     = Node y (add x l) r
      | otherwise = Node y l (add x r)

rISize :: Int -> Int
rISize n
  = size (buildIntTree (take n rs))

rRSize :: Int -> Int
rRSize n
  = size' (buildRadixTree (take n rs))

rCompSize :: Int -> Int
rCompSize com
  | rRSize com == rISize com = com
  | rRSize com > rISize com = rCompSize (com + 1)
  | rRSize com < rISize com = rCompSize (com - 1)
--------------------------------------------------------------------------

a, m :: Integer
m = 1073741824
a = 16387

rand :: Integer -> [Double]
rand s
  = fromInteger s / fromInteger m : rand s' where s' = (s * a) `mod` m

randomInts :: Int -> Int -> Integer -> [Int]
randomInts m n s
  = take m (map (round . (+1) . (* (fromIntegral n))) (rand s))

rs :: [Int]
rs = randomInts 1000 500 765539

--------------------------------------------------------------------------
-- Pre (universal): all integers are non-negative

size :: IntTree -> Int
size Empty
  = 1
size (Leaf a)
  = 5
size (Node a (tree1) (tree2))
  = 13 + size (tree1) + size (tree2)

size' :: RadixTree -> Int
size' (Leaf a)
  = 1
size' (Node a (tree1) (tree2))
  = 9 + size' (tree1) + size' (tree2)

binary :: Int -> BitString
binary 0 = [0]
binary 1 = [1]
binary num
  = (binary quot) ++ [rem]
  where
    (quot, rem) = quotRem num 2

insert :: BitString -> RadixTree -> RadixTree
insert [] (Leaf a)
  = (Leaf True)
insert [] (Node a (tree1) (tree2))
  = (Node True (tree1) (tree2))

insert bitstr (Node a (tree1) (tree2))
  | head bitstr == 0 = (Node a (insert (tail bitstr) tree1) (tree2))
  | otherwise = (Node a (tree1) (insert (tail bitstr) tree2))
insert bitstr (Leaf a)
  | head bitstr == 0 = (Node a (insert (tail bitstr) (Leaf False)) (Leaf False))
  | otherwise = (Node a (Leaf False) (insert (tail bitstr) (Leaf False)))

buildRadixTree :: [Int] -> RadixTree
buildRadixTree [] 
  = (Leaf False)
buildRadixTree (x : xs)
  = insert (binary x) (buildRadixTree xs)

fromBinary :: BitString -> Int
fromBinary [0] = 0
fromBinary [1] = 1
fromBinary bstr
  = (fromBinary bs) * 2 + b
  where
    bs = take ((length bstr) - 1) bstr
    [b] = drop ((length bstr) - 1) bstr

member :: Int -> RadixTree -> Bool
member num tree
  = memberb (binary num) tree

memberb :: BitString -> RadixTree -> Bool
memberb bnum (Leaf a)
  = False

memberb bnum (Node a (tree1) (tree2))
  | bs == [] = member' b (Node a (tree1) (tree2))
  | b == 1 = memberb (bs) tree2
  | b == 0 = memberb (bs) tree1
  where
    (b : bs) = bnum

    member' :: Int -> RadixTree -> Bool
    member' b (Node a (tree1) (tree2)) 
      | b == 0 = isTrue tree1
      | b == 1 = isTrue tree2

    isTrue :: RadixTree -> Bool
    isTrue (Node a (tree1) (tree2))
      = a
    isTrue (Leaf a)
      = a

union :: RadixTree -> RadixTree -> RadixTree
union tree1 tree2
  = buildRadixTree (union' 100 tree1 tree2)

union' :: Int -> RadixTree -> RadixTree -> [Int]
union' 0 tree1 tree2
  | member 0 tree1 || member 0 tree2 = [0]
  | otherwise = []
union' x tree1 tree2
  | member x tree1 || member x tree2 = x : (union' (x-1) tree1 tree2)
  | otherwise = union' (x-1) tree1 tree2

intersection :: RadixTree -> RadixTree -> RadixTree
intersection tree1 tree2
  = buildRadixTree (intersection' 100 tree1 tree2)

intersection' :: Int -> RadixTree -> RadixTree -> [Int]
intersection' 0 tree1 tree2
  | member 0 tree1 && member 0 tree2 = [0]
  | otherwise = []
intersection' x tree1 tree2
  | member x tree1 && member x tree2 = x : (intersection' (x-1) tree1 tree2)
  | otherwise = intersection' (x-1) tree1 tree2

-- CONCLUSION: The break-even point is 205.

-----------------------------------------------------------------------------
-- Some test trees...

figure :: RadixTree
figure
  = Node False (Leaf True)
               (Node True (Leaf False)
                          (Node True (Node False (Leaf True)
                                                 (Leaf False))
                                     (Leaf True)))

t1 :: IntTree
t1 = Node 20 (Node 8 Empty
                     (Node 12 Empty
                              Empty))
             Empty

t2 :: RadixTree
t2 = Node False (Node False (Leaf True)
                            (Node True (Leaf False) (Leaf True)))
                (Leaf True)

