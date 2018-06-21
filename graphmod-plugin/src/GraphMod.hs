{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module GraphMod where

import GhcPlugins
import TcRnTypes
import HsExtension
import HsImpExp
import Binary

import Data.Maybe
import Data.List

import Utils as GraphMod
import Dot as GraphMod
import Args
import qualified Trie

import qualified Data.Map as Map

import System.FilePath
import System.Directory
import System.Console.GetOpt
import System.Environment(getArgs)

initBinMemSize :: Int
initBinMemSize = 1024 * 1024


-- Installing the plugin
plugin :: Plugin
plugin = defaultPlugin  {
  typeCheckResultAction = install
  , pluginRecompile = impurePlugin
  }

-- The main plugin function.
install :: [CommandLineOption] -> ModSummary -> TcGblEnv -> TcM TcGblEnv
install opts ms tc_gbl = do
    let imps = tcg_rn_imports tc_gbl
        gm_imps = map (convertImport . unLoc) imps
        outdir = mkOutdir opts
        path = mkPath outdir (ms_mod ms)
        gm_modname = getModName ms
    liftIO $ do
      createDirectoryIfMissing False outdir
      writeBinary path (gm_modname, gm_imps)
    return tc_gbl

mkOutdir :: [CommandLineOption] -> FilePath
mkOutdir [] = defaultLocation
mkOutdir (x:_)  = x
-- Reading and writing the files

writeBinary :: Binary a => FilePath -> a -> IO ()
writeBinary path payload = do
  bh <- openBinMem initBinMemSize
  put_ bh payload
  writeBinMem bh path

mkPath :: FilePath -> Module -> FilePath
mkPath fp m
  = fp </> (moduleNameString (moduleName m) ++ (show (moduleUnitId m)))




-- The type we are going to serialise
type Payload = (GraphMod.ModName, [GraphMod.Import])

getModName :: ModSummary -> GraphMod.ModName
getModName ms = GraphMod.splitModName . moduleNameString . moduleName . ms_mod $ ms


convertImport :: ImportDecl GhcRn -> GraphMod.Import
convertImport (ImportDecl{..}) =
  GraphMod.Import { impMod = convertModName (ideclName)
                  , impType = if ideclSource
                                then GraphMod.SourceImp
                                else GraphMod.NormalImp
                  }
convertImport _ = error "Unreachable"

convertModName :: Located ModuleName -> GraphMod.ModName
convertModName (L _ mn) = GraphMod.splitModName (moduleNameString mn)

--
-- Finalisation logic



readImports :: FilePath -> FilePath -> IO Payload
readImports outdir fp = do
  readBinMem (outdir </> fp) >>= get


collectImports :: IO ()
collectImports = do
  raw_opts <- getArgs
  let (fs, _ms, _errs) = getOpt Permute options raw_opts
      opts = foldr ($) default_opts fs

      outdir = inputDir opts
  files <- listDirectory outdir
  usages <- mapM (readImports outdir) files
  let graph = maybePrune opts $ buildGraph usages
  putStr (GraphMod.make_dot opts graph)

-- G
modGraph :: [Payload] -> [GraphMod.ModName]
modGraph = nub . foldMap do_one
  where
    do_one (mn, is) = mn : map do_import is

    do_import (GraphMod.Import n _) = n

buildGraph :: [Payload] -> (GraphMod.AllEdges, GraphMod.Nodes)
buildGraph payloads = (aes, nodes)
  where
    nodeMapList = zip (modGraph payloads) [0..]

    nodeMap = Map.fromList nodeMapList

    nodes = foldr insertMod Trie.empty nodeMapList

    aes = foldr (makeEdges nodeMap) GraphMod.noEdges
            (concatMap (\(p, is) -> map (p,) is) payloads)

    insertMod (n, k) t = GraphMod.insMod n k t

makeEdges :: Map.Map GraphMod.ModName Int
          -> (GraphMod.ModName, GraphMod.Import)
          -> GraphMod.AllEdges
          -> GraphMod.AllEdges
makeEdges nodeMap (m_from, m_to) aes = fromMaybe (error "makeEdges") $ do
  from_i <- Map.lookup m_from nodeMap
  to_i   <- Map.lookup (GraphMod.impMod m_to) nodeMap
  return $ case GraphMod.impType m_to of
              GraphMod.SourceImp ->
                aes { GraphMod.sourceEdges
                    = GraphMod.insSet from_i to_i (GraphMod.sourceEdges aes) }
              GraphMod.NormalImp ->
                aes { GraphMod.normalEdges = GraphMod.insSet from_i to_i (GraphMod.normalEdges aes) }



--
-- Serialisation logic for GraphMod types

instance Binary GraphMod.Import where
  put_ bh (GraphMod.Import mn ip) = put_ bh mn >> put_ bh ip
  get bh = GraphMod.Import <$> get bh <*> get bh
instance Binary GraphMod.ImpType where
  put_ bh c =
    case c of
      GraphMod.NormalImp -> putByte bh 0
      GraphMod.SourceImp -> putByte bh 1
  get bh = getByte bh  >>= return . \case
                      0 -> GraphMod.NormalImp
                      1 -> GraphMod.SourceImp
                      _ -> error "Binary:GraphMod"



