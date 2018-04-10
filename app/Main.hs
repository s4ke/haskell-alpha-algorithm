{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE MonadComprehensions       #-}
module Main where

import Algorithm.Petri
import Data.Set.Monad as S
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

processLog1 = [["A", "B", "C", "D"], ["A", "C", "B", "D"], ["A", "E", "D"]]

processLog2 =
  [
    ["1", "2", "3"],
    ["2", "1", "3"],
    ["1", "2", "3", "5"],
    ["1", "2", "3", "4"],
    ["1", "4", "3"]
  ]

processLog :: [[String]]
processLog = processLog1

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

            isNextLayerLast = nextRemaining == singleton (Left O)

            nextCurLayer =
              (endPoints relevantEdges)
              `S.intersection`
              (if isNextLayerLast then nextRemaining else S.filter (\x -> x /= (Left O)) nextRemaining)

nodeToDiagram :: (Ord a, Show a) => Node a -> Diagram B
nodeToDiagram (Left p)
    | (Place a b) <- p = plt ""
    | otherwise = plt $ show p
        where plt txt = text txt # fontSizeL 0.2 # fc white <> circle 0.2 # fc green # named (show p)
-- TODO: fix sizes in nodes
nodeToDiagram (Right t@(Transition a)) = text (show a) # fontSizeL 0.2 # fc white
     <> rect 0.4 0.4 # fc green # named (show t)

layerToDiagram :: (Ord a, Show a) => Layer a -> Diagram B
layerToDiagram layer = mconcat $ zipWith (\d num -> d # translate (r2 (num * 2, 0))) (Prelude.map nodeToDiagram $ toList $ fst $ layer) [1..]

arrowOpts = with & gaps       .~ small
                  & headLength .~ local 0.15

plotLayers :: (Ord a, Show a) => [Layer a] -> Diagram B
plotLayers layers =
  nodes # applyEdges
    where
      nodes = mconcat $ zipWith (\d num -> d # translate (r2 (0, num * 2))) (Prelude.map layerToDiagram layers) [1..]
      allEdgesAsStringTuples = concat $ Prelude.map (edgesAsStringTuples . snd) layers
      applyEdges = applyAll $ Prelude.map (uncurry (connectOutside' arrowOpts)) $ allEdgesAsStringTuples

      edgesAsStringTuples :: (Ord a, Show a) => Set (Edge a) -> [(String, String)]
      edgesAsStringTuples edges = Prelude.map edgeAsStringTuple $ toList edges

      edgeAsStringTuple :: (Ord a, Show a) => Edge a -> (String, String)
      edgeAsStringTuple (ToPlace a b) = (show a, show b)
      edgeAsStringTuple (ToTransition a b) = (show a, show b)

main :: IO ()
main = do
  let petriInLayers = intoLayers $ alphaAlgorithm $ processLog
  -- print $ petriInLayers
  mainWith $ plotLayers $ petriInLayers
