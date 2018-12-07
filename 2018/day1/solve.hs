{-# LANGUAGE OverloadedStrings #-}
import Data.List
import Text.Printf (printf)
import qualified Data.Set as Set


strip_plus_sign ('+':s) = s
strip_plus_sign s = s

parse :: String -> [Integer]
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
    printf "part1: %d\npart2: %d\n" (part1 numbers) (part2 numbers)


main = do
    input <- readFile "input"
    solve input
