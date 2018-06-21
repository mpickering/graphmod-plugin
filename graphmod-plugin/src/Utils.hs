module Utils
  (
  Qualifier
  , Import(..)
  , ImpType(..)
  , splitQualifier
  , ModName
  , splitModName
  , joinModName
  , relPaths
  , modToFile
  , suffixes
  ) where

import Control.Monad(mplus)
import Control.Exception(evaluate)
import Data.Maybe(catMaybes)
import Data.List(intersperse,isPrefixOf)
import System.Directory(doesFileExist)
import System.FilePath

data Import = Import { impMod :: ModName, impType :: ImpType }
                deriving Show

data ImpType = NormalImp | SourceImp
                deriving (Show,Eq,Ord)


-- | A hierarchical module name.
type Qualifier      = [String]
type ModName        = (Qualifier,String)


-- | Convert a string name into a hierarchical name qualifier.
splitQualifier     :: String -> Qualifier
splitQualifier cs   = case break ('.'==) cs of
                        (xs,_:ys)  -> xs : splitQualifier ys
                        _          -> [cs]

-- | Convert a string name into a hierarchical name.
splitModName       :: String -> ModName
splitModName cs     = case break ('.'==) cs of
                        (xs,_:ys)  -> let (as,bs) = splitModName ys
                                   in (xs:as,bs)
                        _ -> ([],cs)

joinModName        :: ModName -> String
joinModName (xs,y)  = concat $ intersperse "." (xs ++ [y])

-- | The files in which a module might reside.
relPaths           :: ModName -> [FilePath]
relPaths (xs,y)     = [ prefix ++ suffix | suffix <- suffixes ]
  where prefix      = foldr (</>) y xs

suffixes           :: [String]
suffixes            = [".hs",".lhs", ".imports"]

-- | The files in which a module might reside.
-- We report only files that exist.
modToFile          :: [FilePath] -> ModName -> IO [FilePath]
modToFile dirs m    = catMaybes `fmap` mapM check paths
  where
  paths             = [ d </> r | d <- dirs, r <- relPaths m ]
  check p           = do x <- doesFileExist p
                         return (if x then Just p else Nothing)


delit :: String -> String
delit txt = unlines $ bird $ lines txt
  where
  bird (('>' : cs) : ls)  = (' ' : cs) : bird ls
  bird (l : ls)
    | "\\begin{code}" `isPrefixOf` l  = in_code ls
    | otherwise                       = bird ls
  bird []                             = []

  in_code (l : ls)
    | "\\end{code}" `isPrefixOf` l    = bird ls
    | otherwise                       = l : in_code ls
  in_code []                          = []    -- unterminated code...



