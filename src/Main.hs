{-# LANGUAGE GADTs #-}

module Main where

import Prelude
import Control.Applicative (Const(..))
import Control.Arrow ((&&&))
import Data.Functor.Product (Product(..))
import FreeApplicative

-- Algebra

data FileIOF a where
  FileRead :: FilePath -> FileIOF String
  FileWrite :: FilePath -> String -> FileIOF ()

-- Free'd + smart constructors

type FileIO = FreeApplicative FileIOF

fileRead :: FilePath -> FileIO String
fileRead fp = lift $ FileRead fp

fileWrite :: FilePath -> String -> FileIO ()
fileWrite fp s = lift $ FileWrite fp s

-- Monoidal analysis

data CountOps = CountOps { readOps :: Int, writeOps :: Int } deriving (Show)

instance Monoid CountOps where
  mempty = CountOps 0 0
  cx `mappend` cy = CountOps (readOps cx + readOps cy) (writeOps cx + writeOps cy)

countOps :: FileIOF a -> CountOps
countOps (FileRead _) = CountOps 1 0
countOps (FileWrite _ _) = CountOps 0 1


-- Actual action

doRead :: FileIOF a -> IO a
doRead (FileRead fp) = readFile fp
doRead (FileWrite fp s) = writeFile fp s

-- Interpreter

beFree :: FileIO a -> (CountOps, IO a)
beFree fileIO = twiddle $ interpret fileIO go
  where go = productToTuple . (Const . countOps &&& doRead)

        twiddle (Pair c io) = (getConst c, io)

        productToTuple (fa, ga) = Pair fa ga


main :: IO ()
main = go result
  where result = beFree $ fileRead "hello.txt" *> fileWrite "hello.txt" "hello!"
        go (count, io) = putStr "Count: " *> print count *> io
