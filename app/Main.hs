{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE MonadComprehensions       #-}
module Main where

import Algorithm.Petri
import Data.Set.Monad as S
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

processLog :: [[String]]
processLog = [["1", "2", "3"], ["2", "1", "3"]]

type Node a = Either (Place a) (Transition a)
type Layer a = (Set (Node a), Set (Edge a))

 --(Set (Place a), Set (Transition a), Set (Edge a))

-- Generates:
--[(fromList [Left I],fromList [(I,t"1"),(I,t"2")]),
--(fromList [Right t"1",Right t"2"],fromList [(t"1",p(["1"],["3"])),(t"2",p(["2"],["3"]))]),
--(fromList [Left p(["1"],["3"]),Left p(["2"],["3"])],fromList [(p(["1"],["3"]),t"3"),(p(["2"],["3"]),t"3")]),
--(fromList [Right t"3"],fromList [(t"3",O)]),(fromList [Left O],fromList [])]
intoLayers :: (Ord a) => Petri a -> [Layer a]
intoLayers (places, transitions, edges) = go ((S.map Left places) `S.union` (S.map Right transitions)) edges (fromList [Left I])
  where
    go :: (Ord a) => Set(Node a) -> Set (Edge a) -> Set (Node a) -> [Layer a]
    go remaining edges curLayer
      | S.null curLayer = []
      | S.null edges = [(curLayer, relevantEdges)]
      | (curLayer == singleton (Left O)) = [(curLayer, relevantEdges)]
      | S.null remaining = [(curLayer, relevantEdges)]
      | otherwise = (curLayer, relevantEdges) : (go nextRemaining edges nextCurLayer)
        where
            nextRemaining = (remaining `S.difference` curLayer)

            startsAtPlace :: (Ord a, Eq a) => Place a -> Edge a -> Bool
            startsAtPlace place (ToTransition p t) = place == p
            startsAtPlace place (ToPlace t p) = False

            startsAtTransition :: (Ord a, Eq a) => Transition a -> Edge a -> Bool
            startsAtTransition trans (ToTransition p t) = False
            startsAtTransition trans (ToPlace t p) = trans == t

            edgesFrom :: (Ord a, Eq a) => Set (Edge a) -> Node a ->  Set (Edge a)
            edgesFrom edges (Left place) = S.filter (startsAtPlace place) edges
            edgesFrom edges (Right trans) = S.filter (startsAtTransition trans) edges

            extractEndPoint :: Edge a -> Node a
            extractEndPoint (ToTransition p t) = Right t
            extractEndPoint (ToPlace t p) = Left p

            endPoints :: (Ord a) => Set (Edge a) -> Set (Node a)
            endPoints edges = S.map extractEndPoint edges

            relevantEdges = S.foldr S.union S.empty $ S.map (edgesFrom edges) $ curLayer
            nextCurLayer = (endPoints relevantEdges) `S.intersection` nextRemaining


node :: Int -> Diagram B
node n = text (show n) # fontSizeL 0.2 # fc white
     <> circle 0.2 # fc green # named n

arrowOpts = with & gaps       .~ small
                  & headLength .~ local 0.15

tournament :: Int -> Diagram B
tournament n = atPoints (trailVertices $ regPoly n 1) (Prelude.map node [1..n])
  # applyAll [connectOutside' arrowOpts j k | j <- [1 .. n-1], k <- [j+1 .. n]]

example = tournament 6

main :: IO ()
main = do
  --print $ alphaAlgorithm $ processLog
  print $ intoLayers $ alphaAlgorithm $ processLog
