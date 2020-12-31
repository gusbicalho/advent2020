{-# LANGUAGE ImportQualifiedPost #-}

module Main (main) where

import Data.List qualified as List
import Data.Word (Word64)

main :: IO ()
main = do
  putStrLn . ("example: " <>) . show . solve1 $ example
  putStrLn . ("for real: " <>) . show . solve1 $ input

solve1 :: (Word64, Word64) -> Word64
solve1 (pk1, pk2) = transforms pk1 !! findLoopSize pk2 handshakeSubject

handshakeSubject :: Word64
handshakeSubject = 7

findLoopSize :: Word64 -> Word64 -> Int
findLoopSize expected =
  fst . head . filter ((expected ==) . snd) . zip [0 ..] . transforms

transforms :: Word64 -> [Word64]
transforms subject = List.iterate' loop 1
 where
  loop v = (v * subject) `mod` 20201227

example :: (Word64, Word64)
example = (5764801, 17807724)

input :: (Word64, Word64)
input = (8421034, 15993936)
