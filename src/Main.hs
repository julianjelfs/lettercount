module Main where

import qualified Data.Map as M 
import Data.Map (Map)
import System.Directory (listDirectory) 
import System.FilePath ((</>))
import System.FilePath.Posix (takeExtension)
import System.Posix.Files (getFileStatus, isDirectory)
import Control.Monad (filterM, join, foldM)
import Control.Exception.Safe

type Letters = Map Char Int

main :: IO ()
main = putStrLn "hello"

walk :: Letters -> FilePath -> IO Letters
walk l d = do 
  things <- listDirectory d 
  let paths = fmap (d </>) things
  foldM go l paths
  where 
    go accum filePath = do 
      stat <- tryIO $ getFileStatus filePath
      case stat of 
        Left _ -> pure accum
        Right stat' -> 
          if isDirectory stat'
          then walk accum filePath
          else 
            if validFile filePath 
            then groupLetters accum <$> fileSymbols filePath
            else pure accum

validFile :: FilePath -> Bool 
validFile = flip elem exts . takeExtension

exts :: [String]
exts = [".ts", ".tsx", ".go"]
      
symbols :: String 
symbols = "!\"£$%^&*()`[]{}@#'=+-_|/"

groupLetters :: Letters -> String -> Letters
groupLetters  = 
  foldr 
    (\c m -> 
      case M.member c m of 
        False -> M.insert c 1 m
        True -> M.update (\n -> Just (n+1)) c m 
    ) 


fileSymbols :: String -> IO String
fileSymbols path = 
  filter (flip elem symbols) . concat . lines <$> readFile path
