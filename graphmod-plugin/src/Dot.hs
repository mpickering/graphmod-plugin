module Dot(make_dot
          , AllEdges
          , Nodes
          , noEdges
          , insMod
          , insSet
          , sourceEdges
          , normalEdges
          , maybePrune
          ) where
import Utils
import qualified Trie
import Args
import Text.Dot

import Control.Monad(forM_,msum,unless)
import Data.List(intersperse,transpose)
import Data.Maybe(isJust,fromMaybe,listToMaybe)
import qualified Data.IntMap as IMap
import qualified Data.Map    as Map
import qualified Data.IntSet as ISet
import System.IO(hPutStrLn,stderr)
import System.Console.GetOpt
import Numeric(showHex)

--import Paths_graphmod (version)
-- import Data.Version (showVersion)

version = "0"

type Nodes   = Trie.Trie String [((NodeT,String),Int)]
                    -- Maps a path to:   ((node, label), nodeId)

type Edges    = IMap.IntMap ISet.IntSet

data NodeT    = ModuleNode

              | ModuleInItsCluster
                -- ^ A module that has been relocated to its cluster

              | Redirect
                -- ^ This is not rendered. It is there to support replacing
                -- one node with another (e.g., when collapsing)

              | Deleted
                -- ^ This is not rendered, and edges to/from it are also
                -- not rendered.

              | CollapsedNode Bool
                -- ^ indicates if it contains module too.
                deriving (Show,Eq,Ord)

data AllEdges = AllEdges
  { normalEdges   :: Edges
  , sourceEdges   :: Edges
  }

noEdges :: AllEdges
noEdges = AllEdges { normalEdges    = IMap.empty
                   , sourceEdges    = IMap.empty
                   }


insMod :: ModName -> Int -> Nodes -> Nodes
insMod (q,m) n t  = Trie.insert q ins t
  where
  ins xs = case xs of
             Nothing -> [ ((ModuleNode,m),n) ]
             Just ys -> ((ModuleNode,m),n) : ys

insSet :: Int -> Int -> Edges -> Edges
insSet x y m = IMap.insertWith ISet.union x (ISet.singleton y) m


maybePrune :: Opts -> (AllEdges, Nodes) -> (AllEdges, Nodes)
maybePrune opts (es,ns)
    | prune_edges opts  = (es { normalEdges = pruneEdges (normalEdges es) }, ns)
    | otherwise         = (es,ns)

pruneEdges :: Edges -> Edges
pruneEdges es = foldr checkEdges es (IMap.toList es)
  where
  reachIn _ _ _ [] = False
  reachIn g tgt visited (x : xs)
    | x `ISet.member` visited = reachIn g tgt visited xs
    | x == tgt                = True
    | otherwise = let vs = neighbours g x
                  in reachIn g tgt (ISet.insert x visited) (vs ++ xs)

  neighbours g x = ISet.toList (IMap.findWithDefault ISet.empty x g)

  reachableIn g x y = reachIn g y ISet.empty [x]

  rmEdge x y g = IMap.adjust (ISet.delete y) x g

  checkEdge x y g = let g1 = rmEdge x y g
                    in if reachableIn g1 x y then g1 else g

  checkEdges (x,vs) g = foldr (checkEdge x) g (ISet.toList vs)


isIgnored :: IgnoreSet -> ModName -> Bool
isIgnored (Trie.Sub _ (Just IgnoreAll))       _        = True
isIgnored (Trie.Sub _ (Just (IgnoreSome ms))) ([],m)   = elem m ms
isIgnored (Trie.Sub _ Nothing)                ([],_)   = False
isIgnored (Trie.Sub ts _)                     (q:qs,m) =
  case Map.lookup q ts of
    Nothing -> False
    Just t  -> isIgnored t (qs,m)


-- XXX: We could combine collapseAll and collapse into a single pass
-- to avoid traversing form the root each time.
collapseAll :: Opts -> Nodes -> Trie.Trie String Bool -> Nodes
collapseAll opts t0 =
  foldr (\q t -> fromMaybe t (collapse opts t q)) t0 . toList
  where
  toList (Trie.Sub _ (Just x))  = return ([], x)
  toList (Trie.Sub as Nothing)  = do (q,t)  <- Map.toList as
                                     (qs,x) <- toList t
                                     return (q:qs, x)

-- NOTE: We use the Maybe type to indicate when things changed.
collapse :: Opts -> Nodes -> (Qualifier,Bool) -> Maybe Nodes
collapse _ _ ([],_) = return Trie.empty      -- Probably not terribly useful.

collapse opts (Trie.Sub ts mb) ([q],alsoMod) =
  do t   <- Map.lookup q ts
     let will_move = mod_in_cluster opts && Map.member q ts
         (thisMod,otherMods)
            | alsoMod || will_move = case findThisMod =<< mb of
                                       Nothing         -> (Nothing, [])
                                       Just (nid,rest) -> (Just nid, rest)
            | otherwise = (Nothing, fromMaybe [] mb)

     -- use this node-id to represent the collapsed cluster
     rep <- msum [ thisMod, getFirst t ]

     let close ((_,nm),_) = ((if will_move then Deleted else Redirect,nm),rep)
         ts'              = Map.insert q (fmap (map close) t) ts
         newT | alsoMod || not will_move = CollapsedNode (isJust thisMod)
              | otherwise                = ModuleNode

     return (Trie.Sub ts' (Just (((newT,q),rep) : otherMods)))
  where
  findThisMod (((_,nm),nid) : more) | nm == q = Just (nid,more)
  findThisMod (x : more) = do (yes,more') <- findThisMod more
                              return (yes, x:more')
  findThisMod []         = Nothing

  getFirst (Trie.Sub ts1 ms) =
    msum (fmap snd (listToMaybe =<< ms) : map getFirst (Map.elems ts1))

collapse opts (Trie.Sub ts ms) (q : qs,x) =
  do t <- Map.lookup q ts
     t1 <- collapse opts t (qs,x)
     return (Trie.Sub (Map.insert q t1 ts) ms)



-- | If inside cluster A.B we have a module M,
-- and there is a cluster A.B.M, then move M into that cluster as a special node
moveModulesInCluster :: Nodes -> Nodes
moveModulesInCluster (Trie.Sub su0 ms0) =
  goMb (fmap moveModulesInCluster su0) ms0
  where
  goMb su mb =
    case mb of
      Nothing -> Trie.Sub su Nothing
      Just xs -> go [] su xs

  go ns su xs =
    case xs of
      [] -> Trie.Sub su $ if null ns then Nothing else Just ns
      y : ys ->
        case check y su of
          Left it   -> go (it : ns) su ys
          Right su1 -> go ns su1 ys

  check it@((nt,s),i) mps =
    case nt of
      ModuleNode ->
        case Map.lookup s mps of
          Nothing -> Left it
          Just t  -> Right (Map.insert s (Trie.insert [] add t) mps)
            where
            newM   = ((ModuleInItsCluster,s),i)
            add xs = [newM] ++ fromMaybe [] xs


      ModuleInItsCluster    -> Left it
      CollapsedNode _       -> Left it
      Redirect              -> Left it
      Deleted               -> Left it


-- We use tries to group modules by directory.
--------------------------------------------------------------------------------



-- Render edges and a trie into the dot language
--------------------------------------------------------------------------------
make_dot :: Opts -> (AllEdges,Nodes) -> String
make_dot opts (es,t) =
  showDot $
  do attribute ("size", graph_size opts)
     attribute ("ratio", "fill")
     let cols = colors (color_scheme opts)
     if use_clusters opts
        then make_clustered_dot cols $
               if mod_in_cluster opts then moveModulesInCluster t else t
        else make_unclustered_dot cols "" t >> return ()
     genEdges normalAttr (normalEdges es)
     genEdges sourceAttr (sourceEdges es)
  where
  normalAttr _x _y  = []
  sourceAttr _x _y  = [("style","dashed")]

  genEdges attr edges =
    forM_ (IMap.toList edges) $ \(x,ys) ->
      forM_ (ISet.toList ys) $ \y ->
        edge (userNodeId x) (userNodeId y) (attr x y)






make_clustered_dot :: [Color] -> Nodes -> Dot ()
make_clustered_dot cs0 su = go (0,0,0) cs0 su >> return ()
  where
  clusterC = "#0000000F"

  go outer_col ~(this_col:more) (Trie.Sub xs ys) =
    do let outerC = renderColor outer_col
           thisC  = renderColor this_col

       forM_ (fromMaybe [] ys) $ \((t,ls),n) ->
         unless (t == Redirect || t == Deleted) $
         userNode (userNodeId n) $
         [ ("label",ls) ] ++
         case t of
           CollapsedNode False ->   [ ("shape", "box")
                                    , ("style","filled")
                                    , ("color", clusterC)
                                    ]
           CollapsedNode True    -> [ ("style","filled")
                                    , ("fillcolor", clusterC)
                                    ]
           ModuleInItsCluster    -> [ ("style","filled,bold")
                                    , ("fillcolor", outerC)
                                    ]

           ModuleNode            -> [ ("style", "filled")
                                    , ("fillcolor", thisC)
                                    , ("penwidth","0")
                                    ]
           Redirect              -> []
           Deleted               -> []
       goSub this_col more (Map.toList xs)

  goSub _ cs [] = return cs
  goSub outer_col cs ((name,sub) : more) =
    do (_,cs1) <- cluster $ do attribute ("label", name)
                               attribute ("color" , clusterC)
                               attribute ("style", "filled")
                               go outer_col cs sub

       goSub outer_col cs1 more


make_unclustered_dot :: [Color] -> String -> Nodes -> Dot [Color]
make_unclustered_dot c pre (Trie.Sub xs ys') =
  do let col = renderColor (head c)
     let ys = fromMaybe [] ys'
     forM_ ys $ \((t,ls),n) ->
       userNode (userNodeId n) $
           [ ("fillcolor", col)
           , ("style", "filled")
           , ("label", pre ++ ls)
           ] ++
         case t of
           CollapsedNode False   -> [ ("shape", "box"), ("color", col) ]
           CollapsedNode True    -> [ ("shape", "box") ]
           Redirect              -> []
           ModuleInItsCluster    -> []
           ModuleNode            -> []
           Deleted               -> []

     let c1 = if null ys then c else tail c
     c1 `seq` loop (Map.toList xs) c1
  where
  loop ((name,sub):ms) c1 =
    do let pre1 = pre ++ name ++ "."
       c2 <- make_unclustered_dot c1 pre1 sub
       loop ms c2
  loop [] c2 = return c2


type Color = (Int,Int,Int)

colors :: Int -> [Color]
colors n = cycle $ mix_colors $ drop n $ palettes

renderColor :: Color -> String
renderColor (x,y,z) = '#' : showHex (mk x) (showHex (mk y) (showHex (mk z) ""))
  where mk n = 0xFF - n * 0x44


mix_colors :: [[a]] -> [a]
mix_colors css = mk set1 ++ mk set2
  where
  (set1,set2) = unzip $ map (splitAt 3) css
  mk = concat . transpose


palettes :: [[Color]]
palettes = [green, yellow, blue, red, cyan, magenta ]
  where
  red :: [Color]
  red   = [ (0,1,1), (0,2,2), (0,3,3), (1,2,3), (1,3,3), (2,3,3) ]
  green = map rotR red
  blue  = map rotR green
  [cyan,magenta,yellow] = map (map compl . reverse) [red, green, blue]

  rotR (x,y,z)  = (z,x,y)
  compl (x,y,z) = (3-x,3-y,3-z)



