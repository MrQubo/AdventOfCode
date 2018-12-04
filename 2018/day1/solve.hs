{-# LANGUAGE OverloadedStrings #-}
import Data.List
import qualified Data.Set as Set


strip_plus_sign ('+':s) = s
strip_plus_sign s = s

parse input = map (read . strip_plus_sign) (words input)


part1 numbers = foldl (+) 0 numbers


part2 numbers = f (Set.singleton 0) 0 0 (cycle numbers)
    where
        f set acc i (n:ns)
            | Set.member next set = next
            | otherwise = f (Set.insert next set) next (i + 1) ns
            where
                next = acc + n


solve input = do
    let numbers = parse input
    print $ part1 numbers
    print $ part2 numbers


main = do
    input <- readFile "input"
    solve input
