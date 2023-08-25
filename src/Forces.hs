{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedLabels #-}

module Forces where

import Base
import Vec
import Control.Lens
import Data.Bool
import Collisions
import Data.List

computeVelocity :: Float -> SimObj -> SimObj
computeVelocity dt SimObj{..} =
    let
        velocity' = velocity |+| (force |* (getInvMass mass * dt))
     in
        SimObj{velocity = velocity', ..}

gravity :: SimObj -> SimObj
gravity SimObj{..} =
    let
        force' = force |+| Vec 0 ((-9.8) * mass)
     in
        SimObj{force = force', ..}

wind :: [SimField] -> SimObj -> SimObj
wind fields obj = computeFields fields obj Wind $ \square -> Vec (20 * square * obj.material.density) 0

air :: [SimField] -> SimObj -> SimObj
air fields obj = computeFields fields obj Air $ \square -> resistanceForce obj 1

water :: [SimField] -> SimObj -> SimObj
water fields obj = computeFields fields obj Water $ \square -> Vec 0 (square * 9.8) |+| resistanceForce obj 100

resistanceForce :: SimObj -> Float -> Vec
resistanceForce obj k =
    let len = getDiagonal obj.shape
    in (normolize obj.velocity) |* ((-1) * (length' obj.velocity ** 1.5 * 0.5 * len * k) / 8)

getDiagonal :: Shape -> Float
getDiagonal (ShapeAABB (AABB {..})) = length' (maxP |-| minP)
getDiagonal (ShapeCircle (Circle {..})) = r * 2

computeFields :: [SimField] -> SimObj -> Effect -> (Float -> Vec) -> SimObj
computeFields fields obj eff forceF =
    let
        windFilds = filter (\x -> x.effect == eff) fields
     in
        foldl (
            \o f ->
                let
                    f' = computeAndTranslateAABB f
                    o' = computeAndTranslateAABB o
                    clamp' = clamp f'.minP f'.maxP

                    realSquare = computeSquare o.shape
                    aabbSquare = computeSquareAABB o'
                    aabbSquareIner = computeSquareAABB (o' & #minP %~ clamp' & #maxP %~ clamp')
                    square = aabbSquareIner / aabbSquare * realSquare
                    force' = bool (Vec 0 0) (forceF square) (aabbSquareIner /= 0)
                in o & #force %~ (|+| force')
        ) obj windFilds
