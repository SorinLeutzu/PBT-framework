module Lab where


--  Replace the  undefined subs with your implementation and 
--  run LabTests.hs to see your grade and code report.


-- Exercise 1: meanEven
--
-- Compute the arithmetic mean of the even elements of a list.

meanEven :: [Int] -> Double
-- meanEven = undefined



-- Examples:
--   meanEven [2,4,6]    ==  4.0
--   meanEven [1,2,3,4]  ==  3.0
--   meanEven [1,3,5]    ==  0.0
--   meanEven []         ==  0.0
--
-- Good solution:

-- meanEven xs = case filter even xs of
--   []    -> 0.0
--   evens -> fromIntegral (sum evens) / fromIntegral (length evens)
--

-- Bad solution: allows divison by 0 to take place:

meanEven xs = fromIntegral (sum evens) / fromIntegral (length evens)
    where evens = filter even xs







-- Exercise 2: unique
--
-- Eliminate all duplicates and preserve element order.
--
-- Examples:
--   unique [1,2,1,3,2]  ==  [1,2,3]
--   unique "abcabc"     ==  "abc"
--   unique [7,7,7]      ==  [7]
--   unique []            ==  []
--


unique :: Eq a => [a] -> [a]
-- unique = undefined

-- Good solution:

-- unique [] = []
-- unique (x:xs) = x : unique (filter (/= x) xs)
--
-- Bad solution that only removes adjacent duplicates:

unique [] = []
unique [x] = [x]
unique (x:y:xs)
  | x == y    = unique (y:xs)
  | otherwise = x : unique (y:xs)


--bad solution 2: alway returns the empty list
-- unique _ = []





-- Exercise 3: interleave3
--
-- Interleave 3 lists, element by element.
--

-- Examples:
--   interleave3 [1,2] [3,4] [5,6]   ==  [1,3,5,2,4,6]
--   interleave3 [1,2] [3] [4,5]     ==  [1,3,4,2,5]
--   interleave3 [] [] [1,2]          ==  [1,2]
--   interleave3 [1] [] []            ==  [1]
--

interleave3 :: [a] -> [a] -> [a] -> [a]
-- interleave3 = undefined

-- -- good solution:

-- interleave3 [] [] [] = []
-- interleave3 xs ys zs =
--   take 1 xs ++ take 1 ys ++ take 1 zs
--     ++ interleave3 (drop 1 xs) (drop 1 ys) (drop 1 zs)

-- bad solution:

interleave3 (x:xs) (y:ys) (z:zs) = x : y : z : interleave3 xs ys zs
interleave3 _ _ _ = []

-- interleave3 [1,2] [3] [4,5] returns [1,3,4] instead of [1,3,4,2,5]




-- Exercise 4: balanced
--
-- Checks if a string of brackets is a valid bracket sequence. 
-- Consider the 3 pairs of brackets: () , [] , {}
--
--
-- Examples:
--   balanced "([]){}"  ==  True
--   balanced "([)]"    ==  False
--   balanced ""        ==  True
--   balanced "((("     ==  False
--
-- Good solution, keeps a stack of the currently open brackets:

balanced s = checkStack [] s
  where
    checkStack [] [] = True
    checkStack _  [] = False
    checkStack stack (c:cs)
      | c `elem` "([{" = checkStack (c:stack) cs
      | c == ')' = case stack of { '(':r -> checkStack r cs; _ -> False }
      | c == ']' = case stack of { '[':r -> checkStack r cs; _ -> False }
      | c == '}' = case stack of { '{':r -> checkStack r cs; _ -> False }
      | otherwise = checkStack stack cs
--
-- Bad solution, counts the nesting depth instead of keeping a stack,
-- so it cannot detect bracket type mismatches:

-- balanced s = checkDepth 0 s
--   where
--     checkDepth 0 [] = True
--     checkDepth _ [] = False
--     checkDepth n (c:cs)
--       | c `elem` "([{" = checkDepth (n+1) cs
--       | c `elem` ")]}" = if n > 0 then checkDepth (n-1) cs else False
--       | otherwise = checkDepth n cs
--
-- balanced "([)]" gives True instead of False

balanced :: String -> Bool
-- balanced = undefined


-- Exercise 5: splitOn
--
-- Split a given list into sublists using a given delimiter.
--  Adjacent delimiters should produce empty lists

-- Examples:
--   splitOn ',' "a,b,c"      ==  ["a","b","c"]
--   splitOn ',' "hello"      ==  ["hello"]
--   splitOn ',' ",,"         ==  ["","",""]
--   splitOn ',' ""           ==  [""]
--   splitOn 0 [1,2,0,3,4]   ==  [[1,2],[3,4]]
--
splitOn :: Eq a => a -> [a] -> [[a]]
-- splitOn = undefined



-- good solution:

-- splitOn _ [] = [[]]
-- splitOn d xs = case break (== d) xs of
--  (chunk, [])     -> [chunk]
--  (chunk, _:rest) -> chunk : splitOn d rest
--

-- bad solution: enters infinite loop

splitOn _ [] = [[]]
splitOn d xs = case break (== d) xs of
 (chunk, [])   -> [chunk]
 (chunk, rest) -> chunk : splitOn d rest