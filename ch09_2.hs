import System.IO
import Data.Char

-- data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode
readFile' = do
     h <- openFile "baabaa.txt" ReadMode
     contents <- hGetContents h
     putStr contents
     hClose h

readFile'' = do
  withFile "baabaa.txt" ReadMode $ \h -> do
    contents <- hGetContents h
    putStr contents

readFile_ = do
  contents <- readFile "baabaa.txt"
  putStr contents

readFile__ = do
  contents <- readFile "baabaa.txt"
  writeFile "baabaacaps.txt" (map toUpper contents)
