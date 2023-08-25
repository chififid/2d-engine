{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}

module Collisions where

import Base
import Control.Lens
import Data.Bool
import Data.List
import Data.Maybe
import Data.Tuple
import Vec

broadCollisionPhase :: SimObj -> SimObj -> Bool
broadCollisionPhase x y = checkAABBCollision x' y'
    where
        x' = computeAndTranslateAABB x
        y' = computeAndTranslateAABB y

narrowCollisionPhase :: SimObj -> SimObj -> Bool
narrowCollisionPhase x y =
    case (x.shape, y.shape) of
        (ShapeAABB _, ShapeAABB _) -> True
        _ -> fastDistanceCheck (computeDistanceVec x y) dist
    where 
        dist = computeCollisionDist x.shape + computeCollisionDist y.shape

computeDistanceVec :: SimObj -> SimObj -> Vec
computeDistanceVec x y =
    case (x.shape, y.shape) of
        (ShapeCircle _, ShapeAABB _) -> computeDistanceVecAABBCicle y x |* (-1)
        (ShapeAABB _, ShapeCircle _) -> computeDistanceVecAABBCicle x y
        _ -> x.transform.pos |-| y.transform.pos

computeDistanceVecAABBCicle :: SimObj -> SimObj -> Vec
computeDistanceVecAABBCicle x y =
    let
        x' = computeAndTranslateAABB x
        closestPoint = clamp x'.minP x'.maxP y.transform.pos
     in
        closestPoint |-| y.transform.pos

computeCollisions :: [SimObj] -> [SimObj]
computeCollisions objs =
    let collisionPairs = pairs' (\x y -> broadCollisionPhase x y && narrowCollisionPhase x y) objs
    -- let collisionPairs = pairs' narrowCollisionPhase objs
     in foldl
            ( \res (x, y) ->
                let x' = chooseFromList res x
                    y' = chooseFromList res y
                    (x'', y'') = computeCollision x' y'
                 in replace' res [x'', y'']
            )
            objs
            collisionPairs

computeCollision :: SimObj -> SimObj -> (SimObj, SimObj)
computeCollision a b =
    let
        rp = computeDistanceVec b a
        n = normolize rp

        rv = b.velocity |-| a.velocity
        velAlongNorm = dotProduct rv n
        e = min a.material.restitution b.material.restitution

        invam = getInvMass a.mass
        invbm = getInvMass b.mass

        j = (-(1 + e)) * velAlongNorm
        j' = j / (invam + invbm)

        imp = n |* j'

        av = a.velocity |-| (imp |* invam)
        bv = b.velocity |+| (imp |* invbm)
     in
        if velAlongNorm > 0 || isNaN j'
            then (a, b)
            else (a & #velocity .~ av, b & #velocity .~ bv)

chooseFromList :: (Eq a) => [a] -> a -> a
chooseFromList xs x =
    fromMaybe x (find (== x) xs)

replace' :: (Eq a) => [a] -> [a] -> [a]
replace' xs repXs = map (chooseFromList repXs) xs
