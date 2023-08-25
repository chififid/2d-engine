{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Gui where

import Base
import Graphics.Gloss
import Vec
import GHC.Records (HasField)

draw :: World -> Picture
draw (World objs fields links t debug) =
    scale 5 5 $ Pictures
        [ Pictures $
            map (drawLink objs red) links
            ++
            map (drawObj yellow) objs
            ++ 
            map (\field -> drawObj (effectColor field.effect) field) fields
        , translate (-200) 600 $ color white $ text debug
        ]

effectColor :: Effect -> Color
effectColor Wind = makeColorI 100 100 150 100
effectColor Water = makeColorI 58 120 255 150
effectColor Air = makeColorI 200 200 250 150

drawLink :: [SimObj] -> Color -> SimLink -> Picture
drawLink objs c link =
    let points = 
            map (\x -> let p = x.transform.pos in (p.x, p.y)) $
                filter (\x -> x.id `elem` [link.aId, link.bId]) objs
    in color c $ line points

drawObj :: (HasField "shape" a Shape, HasField "transform" a Transform) => Color -> a -> Picture
drawObj c obj = translate' obj.transform.pos $ color c $ renderShape obj.shape

renderShape :: Shape -> Picture
renderShape (ShapeAABB x) = polygon $ rectanglePath w h
  where
    Vec w h = sizeAABB x
renderShape (ShapeCircle (Base.Circle{..})) = circleSolid r

translate' :: Vec -> Picture -> Picture
translate' Vec{..} = translate x y
