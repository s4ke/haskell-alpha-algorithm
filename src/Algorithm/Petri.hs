{-# LANGUAGE MonadComprehensions #-}
module Algorithm.Petri
    (
      Place(..),
      Transition(..),
      Edge(..),
      Petri(..),
      alphaAlgorithm
    ) where

import Data.List
import Data.Set.Monad as S
import Data.Maybe
import Data.Semigroup

import Debug.Trace

data Prec a = Immediate a a | Causal a a | Choice a a | Parallel a a deriving (Show, Eq, Ord)

dom :: (Ord a) => [[a]] -> Set a
dom = S.fromList . concat

cartesianProduct :: Set a -> Set b -> Set (a, b)
cartesianProduct as bs = [(a, b) | a <- as, b <- bs]

powerset s
    | s == empty = singleton empty
    | otherwise = S.map (S.insert x) pxs `S.union` pxs
        where (x, xs) = deleteFindMin s
              pxs = powerset xs

-- actual code

isImmediate :: Prec a -> Bool
isImmediate (Immediate _ _) = True
isImmediate _ = False

immediate :: (Ord a) => [[a]] -> Set (Prec a)
immediate logs = S.unions $ (Prelude.map ((goOver S.empty)) logs)
  where
    goOver :: (Ord a) => Set (Prec a) -> [a] -> Set (Prec a)
    goOver curSet (x:y:rest) = goOver (S.insert (Immediate x y) curSet) (y:rest)
    goOver curSet [_] = curSet
    goOver curSet [] = curSet

causal :: (Ord a) => Set (Prec a) -> Set (Prec a)
causal precs = [Causal a b | (Immediate a b) <- S.filter (isImmediate) precs, member (Immediate a b) precs, not (member (Immediate b a) precs)]

parallel :: (Ord a) => Set (Prec a) -> Set (Prec a)
parallel precs = [Parallel a b | (Immediate a b) <- S.filter (isImmediate) precs, member (Immediate a b) precs, member (Immediate b a) precs]

choice :: (Ord a) => Set a -> Set (Prec a) -> Set (Prec a)
choice domain precs = [Choice a b | a <- domain, b <- domain, noRelation a b]
                        where
                          noRelation a b =
                              (not $ member (Immediate a b) precs)
                              && (not $ member (Immediate b a) precs)

data Place a = Place (Set a) (Set a) | I | O deriving (Eq, Ord)
instance (Ord a, Show a) => Show (Place a) where
  show I = "I"
  show O = "O"
  show (Place a b) = "p(" ++ show (toList a) ++ "," ++ show (toList b) ++ ")"

data Transition a = Transition a deriving (Eq, Ord)
instance (Show a) => Show (Transition a) where
  show (Transition a) = "t" ++ (show a)

data Edge a = ToPlace (Transition a) (Place a) | ToTransition (Place a) (Transition a) deriving (Eq, Ord)
instance (Ord a, Show a) => Show (Edge a) where
  show (ToPlace a b) = "(" ++ show a ++ "," ++ show b ++ ")"
  show (ToTransition a b) = "(" ++ show a ++ "," ++ show b ++ ")"

type Petri a = (Set (Place a), Set (Transition a), Set (Edge a))

alphaAlgorithm :: (Show a, Ord a) => [[a]] -> Petri a
alphaAlgorithm logs = let
                          immediates = immediate logs
                          causals = causal immediates
                          parallels = parallel immediates
                          choices = choice (dom logs) immediates

                          t_l = dom logs
                          t_i = S.fromList $ Prelude.map head logs
                          t_o = S.fromList $ Prelude.map last logs

                          checkCausal as bs = all (\(a, b) -> S.member (Causal a b) causals) $ cartesianProduct as bs
                          checkChoice as = all (\(a1, a2) -> S.member (Choice a1 a2) choices) $ cartesianProduct as as
                          x_w = [(as, bs) | as <- powerset t_l, bs <- powerset t_l,
                                            not (S.null as), not (S.null bs),
                                            checkCausal as bs, checkChoice as, checkChoice bs]

                          maxSetFilter (as, bs) = all ((\(as', bs') -> not (as `S.isSubsetOf` as' && bs `S.isSubsetOf` bs') || (as == as' && bs == bs'))) x_w
                          y_l = S.filter maxSetFilter x_w

                          p_l = [Place as bs | (as, bs) <- y_l] `S.union` fromList [I, O]

                          f_l = [ToPlace (Transition a) (Place as bs) | (as, bs) <- y_l, a <- as]
                                `S.union` [ToTransition (Place as bs) (Transition b) | (as, bs) <- y_l, b <- bs]
                                `S.union` [ToPlace (Transition t) O | t <- t_o]
                                `S.union` [ToTransition I (Transition t) | t <- t_i]
                      in (p_l, S.map Transition t_l, f_l)
