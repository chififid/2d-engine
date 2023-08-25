{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedLabels #-}

module Links where

import Base
import Collisions
import Vec
import Control.Lens

computeLinks :: [SimLink] -> [SimObj] -> [SimObj]
computeLinks links objs =
    foldl
        (\res link ->
            let
                [x, y] = filter (\o -> o.id `elem` [link.aId, link.bId]) res
                (x', y') = computeLink x y link.dist
            in replace' res [x', y']
        )
        objs
        links

computeLink :: SimObj -> SimObj -> Float -> (SimObj, SimObj)
computeLink x y dist =
    let
        rp = y.transform.pos |-| x.transform.pos
        lenRp = length' rp
        rpN = normolize rp
        perpN = swap' rpN

        delta = dist - lenRp
        (xK, yK) =
            case (x.mass, y.mass) of
            (0, 0) -> (0, 0)
            (0, _) -> (0, 1)
            (_, 0) -> (1, 0)
            (_, _) -> (0.5, 0.5)

        x' = x
            & #transform %~ move (rpN |* (xK * delta * (-1)))
        y' = y
            & #transform %~ move (rpN |* (yK * delta))

     in (x', y')