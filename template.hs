type BinHeap a = [BinTree a]

data BinTree a = Node a Int (BinHeap a)
               deriving (Eq, Ord, Show)

--------------------------------------------------------------
-- PART I

-- Gets the key of a binomial tree
key :: BinTree a -> a
key (Node key _ _) = key

-- Gets the rank of a binomial tree
rank :: BinTree a -> Int
rank (Node _ rank _) = rank

-- Gets the children of a binomial tree
children :: BinTree a -> [BinTree a]
children (Node _ _ children) = children

-- Combines two binomial trees
combineTrees :: Ord a => BinTree a -> BinTree a -> BinTree a
combineTrees tree1@(Node key1 rank1 children1) tree2@(Node key2 rank2 children2)
    | key1 < key2 = Node key1 (rank2 + 1) (tree2 : children1)
    | otherwise = Node key2 (rank2 + 1) (tree1 : children2)

--------------------------------------------------------------
-- PART II

-- Extracts the min value
extractMin :: Ord a => BinHeap a -> a
extractMin = key . fst . removeMin

-- Merges two heaps
mergeHeaps :: Ord a => BinHeap a -> BinHeap a -> BinHeap a
mergeHeaps [] h = h
mergeHeaps h [] = h
mergeHeaps h@(t:ts) h'@(t':ts')
    | r < r' = t : mergeHeaps ts h'
    | r' < r = t' : mergeHeaps ts' h
    | otherwise = mergeHeaps [combineTrees t t'] $ mergeHeaps ts ts'
        where
            r = rank t
            r' = rank t'

-- Inserts a singleton tree with the value
insert :: Ord a => a -> BinHeap a -> BinHeap a
insert value = mergeHeaps [Node value 0 []]

-- Deletes the minimum from a binomial heap
deleteMin :: Ord a => BinHeap a -> BinHeap a
deleteMin heap = mergeHeaps (reverse $ children minTree) rest
    where
        (minTree, rest) = removeMin heap

-- Returns minimum tree and leftover heap with tree removed
removeMin :: Ord a => BinHeap a -> (BinTree a, BinHeap a)
removeMin [t] = (t, [])
removeMin (t:ts)
    | key t <= key t' = (t, ts)
    | otherwise = (t', t : ts')
        where (t', ts') = removeMin ts

-- Sorts a list using binomial heaps
binSort :: Ord a => [a] -> [a]
binSort items = sort heap
    where
        heap = foldl' (flip insert) [] items
        sort :: Ord a => BinHeap a -> [a]
        sort [] = []
        sort heap = extractMin heap : sort (deleteMin heap)

--------------------------------------------------------------
-- PART III

-- Takes in a heap and if contans rank r then 1 else 0
-- Heap is order ascending order by rank left to right
toBinary :: BinHeap a -> [Int]
toBinary [] = [0]
toBinary heap = toBinary' heap (-1)
    where
        toBinary' :: BinHeap a -> Int -> [Int]
        toBinary' [] _ = []
        toBinary' (t:ts) prevr = toBinary' ts r ++ (1 : replicate (r - prevr - 1) 0)
            where r = rank t

-- Sums two binary numbers
binarySum :: [Int] -> [Int] -> [Int]
binarySum bin1 bin2 = reverse $ binSummy (reverse bin1) (reverse bin2) 0
    where
        fullAdder :: Int -> Int -> Int -> (Int, Int)
        fullAdder b1 b2 cIn = 
            case (b1 + b2 + cIn) of
                0 -> (0, 0)
                1 -> (1, 0)
                2 -> (0, 1)
                3 -> (1, 1)

        binSummy :: [Int] -> [Int] -> Int -> [Int]
        binSummy [] [] 0 = []
        binSummy [] [] cIn = [cIn]
        binSummy [] bin cIn = binSummy bin [0] cIn
        binSummy bin [] cIn = binSummy bin [0] cIn
        binSummy (b:bs) (b':bs') cIn = sum : (binSummy bs bs' cOut)
            where 
                (sum, cOut) = fullAdder b b' cIn

------------------------------------------------------
-- Some sample trees...

t1, t2, t3, t4, t5, t6, t7, t8 :: BinTree Int
-- Note: t7 is the result of merging t5 and t6

-- t1 to t4 appear in Figure 1...
t1 = Node 4 0 []
t2 = Node 1 1 [Node 5 0 []]
t3 = Node 2 2 [Node 8 1 [Node 9 0 []], 
               Node 7 0 []]
t4 = Node 2 3 [Node 3 2 [Node 6 1 [Node 8 0 []], 
                         Node 10 0 []],
               Node 8 1 [Node 9 0 []],
               Node 7 0 []]

-- t5 and t6 are on the left of Figure 2; t7 is on the
-- right
t5 = Node 4 2 [Node 6 1 [Node 8 0 []], 
                         Node 10 0 []]
t6 = Node 2 2 [Node 8 1 [Node 9 0 []], Node 7 0 []]
t7 = Node 2 3 [Node 4 2 [Node 6 1 [Node 8 0 []], Node 10 0 []],
               Node 8 1 [Node 9 0 []], 
               Node 7 0 []]

-- An additional tree...
t8 = Node 12 1 [Node 16 0 []]

------------------------------------------------------
-- Some sample heaps...

h1, h2, h3, h4, h5, h6, h7 :: BinHeap Int
-- Two arbitrary heaps for testing...
h1 = [t2, t7]
h2 = [Node 1 2 [Node 12 1 [Node 16 0 []],
                Node 5 0 []],
      Node 2 3 [Node 4 2 [Node 6 1 [Node 8 0 []],
                          Node 10 0 []],
                Node 8 1 [Node 9 0 []],
                Node 7 0 []]]

-- h3 is shown in Figure 3...
h3 = [t1, t2, t4]

-- Two additional heaps, used below. They are shown
-- in Figure 4(a)...

h4 = [t2, t5]
h5 = [t1, t8]

-- h6 is the result of merging h4 and h5, shown in Figure 4(b)...
h6 = [Node 4 0 [],
      Node 1 3 [Node 4 2 [Node 6 1 [Node 8 0 []],
                          Node 10 0 []],
                Node 12 1 [Node 16 0 []],
                Node 5 0 []]]

-- h7 is shown in Figure 5...
h7 = [Node 4 3 [Node 4 2 [Node 12 1 [Node 16 0 []],
                          Node 5 0 []],
                Node 6 1 [Node 8 0 []],
                Node 10 0 []]]
