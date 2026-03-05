module DeMoDNote.Util (
    safeIndex,
    safeTail,
    safeInit,
    clamp,
    chunksOf,
    safeMinimumBy,
    safeMaximumBy,
    padCenter,
    padLeft,
    padRight
) where

safeIndex :: [a] -> Int -> a -> a
safeIndex [] _ def = def
safeIndex _ n def | n < 0 = def
safeIndex (x:_) 0 _ = x
safeIndex (_:xs) n def = safeIndex xs (n - 1) def

safeTail :: [a] -> [a]
safeTail [] = []
safeTail (_:xs) = xs

safeInit :: [a] -> [a]
safeInit [] = []
safeInit [_] = []
safeInit xs = init xs

clamp :: Ord a => a -> a -> a -> a
clamp lo hi = max lo . min hi

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

safeMinimumBy :: (a -> a -> Ordering) -> [a] -> Maybe a
safeMinimumBy _ [] = Nothing
safeMinimumBy cmp xs = Just $ foldl1 (\x y -> if cmp x y == GT then y else x) xs

safeMaximumBy :: (a -> a -> Ordering) -> [a] -> Maybe a
safeMaximumBy _ [] = Nothing
safeMaximumBy cmp xs = Just $ foldl1 (\x y -> if cmp x y == GT then x else y) xs

padCenter :: Int -> Char -> String -> String
padCenter width pad str
    | len >= width = str
    | otherwise = replicate left pad ++ str ++ replicate right pad
  where
    len = length str
    left = (width - len) `div` 2
    right = width - len - left

padLeft :: Int -> Char -> String -> String
padLeft width pad str
    | len >= width = str
    | otherwise = replicate (width - len) pad ++ str
  where
    len = length str

padRight :: Int -> Char -> String -> String
padRight width pad str
    | len >= width = str
    | otherwise = str ++ replicate (width - len) pad
  where
    len = length str
