module AStar (aStar,aStarM) where
-- Modified from Cabal AStar to handle the display of the name of the steps of the path as well as the path

import qualified Data.Set as Set
import Data.Set (Set, (\\))
import qualified Data.Map as Map
import Data.Map (Map, (!))
import qualified Data.PSQueue as PSQ
import Data.PSQueue (PSQ, Binding(..), minView)
import Data.List (foldl', foldl, deleteBy)
import Control.Monad (foldM)

removeDups :: (Eq b) => [(a, b)] -> [(a, b)] -> [(a, b)]
removeDups = foldl (flip (deleteBy (\x y -> (snd x) == (snd y))))


data AStar a c = AStar { visited  :: !(Set a),
                         waiting  :: !(PSQ a c),
                         score    :: !(Map a c),
                         memoHeur :: !(Map a c),
                         cameFrom :: !(Map a a),
                         end      :: !(Maybe a) }
    deriving Show
    
aStarInit start = AStar { visited  = Set.empty,
                          waiting  = PSQ.singleton start 0,
                          score    = Map.singleton ("", snd start) 0,
                          memoHeur = Map.empty,
                          cameFrom = Map.empty,
                          end      = Nothing }

runAStar :: (Show a, Ord a, Show c, Ord c, Num c) =>
         ((String, a) -> Set (String, a))     -- adjacencies in graph
         -> ((String, a) -> (String, a) -> c) -- distance function
         -> ((String, a) -> c)      -- heuristic distance to goal
         -> ((String, a) -> Bool)   -- goal
         -> (String, a)             -- starting vertex
         -> AStar (String, a) c     -- final state

runAStar graph dist heur goal start = aStar' (aStarInit start)
  where aStar' s
          = case minView (waiting s) of
              Nothing            -> s
              Just ((str, x) :-> _, w') ->
                if goal (str,x)
                  then s { end = Just (str, x) }
                  else aStar' $ foldl' (expand (str, x))
                                       (s { waiting = w',
                                            visited = Set.insert ("", x) (visited s)})
                                       (removeDups (Set.toList $ graph (str, x)) (Set.toList $ visited s))
        expand x s y
          = let v = (score s) ! ("", (snd x)) + dist x y
            in case PSQ.lookup y (waiting s) of
                 Nothing -> link x y v
                              (s { memoHeur
                                     = Map.insert y (heur y) (memoHeur s) })
                 Just _  -> if v < ((score s) ! ("", snd y))
                              then link x y v s
                              else s
        link x y v s
           = s { cameFrom = Map.insert y x (cameFrom s),
                 score    = Map.insert ("", snd y) v (score s),
                 waiting  = PSQ.insert y (v + memoHeur s ! y) (waiting s) }

-- | This function computes an optimal (minimal distance) path through a graph in a best-first fashion,
-- starting from a given starting point.
aStar :: (Show a, Show c, Ord a, Ord c, Num c) =>
         ((String, a) -> Set (String, a))     -- ^ The graph we are searching through, given as a function from vertices
                          -- to their neighbours.
         -> ((String, a) -> (String, a) -> c) -- ^ Distance function between neighbouring vertices of the graph. This will
                          -- never be applied to vertices that are not neighbours, so may be undefined
                          -- on pairs that are not neighbours in the graph.
         -> ((String, a) -> c)      -- ^ Heuristic distance to the (nearest) goal. This should never overestimate the
                          -- distance, or else the path found may not be minimal.
         -> ((String, a) -> Bool)   -- ^ The goal, specified as a boolean predicate on vertices.
         -> (String, a)             -- ^ The vertex to start searching from.
         -> Maybe [(String, a)]     -- ^ An optimal path, if any path exists. This excludes the starting vertex.
aStar graph dist heur goal start
    = let s = runAStar graph dist heur goal start
      in case end s of
            Nothing -> Nothing
            Just e  -> Just (reverse . takeWhile (not . (== start)) . iterate ((cameFrom s) !) $ e)

runAStarM :: (Monad m, Ord a, Ord c, Num c) =>
          ((String, a) -> m (Set (String, a)))   -- adjacencies in graph
          -> ((String, a) -> (String, a) -> m c) -- distance function
          -> ((String, a) -> m c)      -- heuristic distance to goal
          -> ((String, a) -> m Bool)   -- goal
          -> (String, a)               -- starting vertex
          -> m (AStar (String, a) c)   -- final state

runAStarM graph dist heur goal start = aStar' (aStarInit start)
  where aStar' s
          = case minView (waiting s) of
              Nothing            -> return s
              Just (x :-> _, w') ->
                do g <- goal x
                   if g then return (s { end = Just x })
                        else do ns <- graph x
                                u <- foldM (expand x)
                                           (s { waiting = w',
                                                visited = Set.insert x (visited s)})
                                           (Set.toList (ns \\ visited s))
                                aStar' u
        expand x s y
          = do d <- dist x y
               let v = score s ! x + d
               case PSQ.lookup y (waiting s) of
                 Nothing -> do h <- heur y
                               return (link x y v (s { memoHeur = Map.insert y h (memoHeur s) }))
                 Just _  -> return $ if v < score s ! y
                                        then link x y v s
                                        else s
        link x y v s
           = s { cameFrom = Map.insert y x (cameFrom s),
                 score    = Map.insert y v (score s),
                 waiting  = PSQ.insert y (v + memoHeur s ! y) (waiting s) }

-- | This function computes an optimal (minimal distance) path through a graph in a best-first fashion,
-- starting from a given starting point.
aStarM :: (Monad m, Ord a, Ord c, Num c) =>
         ((String, a) -> m (Set (String, a)))   -- ^ The graph we are searching through, given as a function from vertices
                            -- to their neighbours.
         -> ((String, a) -> (String, a) -> m c) -- ^ Distance function between neighbouring vertices of the graph. This will
                            -- never be applied to vertices that are not neighbours, so may be undefined
                            -- on pairs that are not neighbours in the graph.
         -> ((String, a) -> m c)      -- ^ Heuristic distance to the (nearest) goal. This should never overestimate the
                            -- distance, or else the path found may not be minimal.
         -> ((String, a) -> m Bool)   -- ^ The goal, specified as a boolean predicate on vertices.
         -> m (String, a)             -- ^ The vertex to start searching from.
         -> m (Maybe [(String, a)])   -- ^ An optimal path, if any path exists. This excludes the starting vertex.
aStarM graph dist heur goal start
    = do sv <- start
         s <- runAStarM graph dist heur goal sv
         return $ case end s of
                    Nothing -> Nothing
                    Just e  -> Just (reverse . takeWhile (not . (== sv)) . iterate (cameFrom s !) $ e)




plane :: (Integer, Integer) -> Set (Integer, Integer)
plane (x,y) = Set.fromList [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]

planeHole :: (Integer, Integer) -> Set (Integer, Integer)
planeHole (x,y) = Set.filter (\(u,v) -> planeDist (u,v) (0,0) > 10) (plane (x,y))

planeDist :: (Integer, Integer) -> (Integer, Integer) -> Double
planeDist (x1,y1) (x2,y2) = sqrt ((x1'-x2')^2 + (y1'-y2')^2)
    where [x1',y1',x2',y2'] = map fromInteger [x1,y1,x2,y2]

