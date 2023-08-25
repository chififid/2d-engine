{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Logic where

import Base
import Collisions
import Control.Lens
import Data.Generics.Labels
import Forces
import Graphics.Gloss.Data.ViewPort
import Vec
import Levels
import Links

tick :: ViewPort -> Float -> World -> World
tick v dt w =
    w
        & #tick %~ (+ 1)
        & everyNTick nTick w.tick (tick' v dt w.tick nTick)
    where
        nTick = round $ max 1 ((1 / dt) / currentFps)
        currentFps = 10000000 / fromIntegral (length w.objs ^ 2)

tick' :: ViewPort -> Float -> Int -> Int -> World -> World
tick' _ dt globalTick nTick w =
    w
        & #debug .~ show (length w.objs) ++ "  " ++ show nTick
        & #objs %~ map (initForce . initPos)
        & #objs %~ map (gravity . wind w.fields . air w.fields . water w.fields)
        & #objs %~ computeCollisions
        & #objs %~ map (computePos localDt . computeVelocity localDt) -- only after all forces
        & #objs %~ computeLinks w.links
        & #objs %~ map (correctVelocity dt) 
        & #objs %~ everyNTick localNTick localTick (addSimObj (circleGenerate (localTick `div` localNTick)))
  where
    localDt = dt
    localTick = globalTick `div` nTick
    localNTick = 200

initForce :: SimObj -> SimObj
initForce obj = obj & #force .~ Vec 0 0

initPos :: SimObj -> SimObj
initPos obj = obj & #transform %~ (\t -> Transform t.pos t.pos)

computePos :: Float -> SimObj -> SimObj
computePos dt obj = obj & #transform %~ move (obj.velocity |* dt)

correctVelocity :: Float -> SimObj -> SimObj
correctVelocity dt obj = obj & #velocity .~ (obj.transform.pos |-| obj.transform.lastPos) |/ dt

everyNTick :: Int -> Int -> (a -> a) -> (a -> a)
everyNTick n t f =
    if t `mod` n == 0
        then f
        else Prelude.id

circleGenerate :: Int -> (Int -> SimObj)
circleGenerate t =
    let
        r = fromIntegral (t `mod` 4 + 1) / 8
        a = (-pi / 2) 
     in
        createCircle r (Vec 0 100) (vecSc a 30) 1

initWorld :: World
initWorld = linkWorld
