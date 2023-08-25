{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedLabels #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Base where

import Control.Lens
import Data.Generics.Labels ()
import GHC.Generics
import Graphics.Gloss (Picture)
import Vec
import GHC.Records (HasField)
import Data.List

data World = World
    { objs :: [SimObj]
    , fields :: [SimField]
    , links ::  [SimLink]
    , tick :: Int
    , debug :: String
    }
    deriving (Generic, Show)

data SimField = SimField
    { shape :: Shape
    , transform :: Transform
    , effect :: Effect
    }
    deriving (Generic, Show)

data SimLink = SimLink
    { aId :: Int
    , bId :: Int
    , dist :: Float
    }
    deriving (Generic, Show)

data SimObj = SimObj
    { id :: Int
    , shape :: Shape
    , transform :: Transform
    , material :: Material
    , mass :: Float
    , velocity :: Vec
    , force :: Vec
    }
    deriving (Generic, Show)

data Effect = Water | Wind | Air deriving (Show, Eq)

instance Eq SimObj where
    (==) :: SimObj -> SimObj -> Bool
    (==) x y = x.id == y.id

data AABB = AABB
    { minP :: Vec
    , maxP :: Vec
    }
    deriving (Generic, Show)

data Circle = Circle
    { r :: Float
    }
    deriving (Generic, Show)

data Material = Material
    { density :: Float
    , restitution :: Float
    }
    deriving (Generic, Show)

data Transform = Transform
    { pos :: Vec
    , lastPos :: Vec
    }
    deriving (Generic, Show)

data Shape = ShapeCircle Circle | ShapeAABB AABB deriving (Show)

sizeAABB :: AABB -> Vec
sizeAABB x = (x.maxP |-| x.minP) |% 0 $ (\x _ -> abs x)

translateAABB :: AABB -> Vec -> AABB
translateAABB x t = AABB (x.minP |+| t) (x.maxP |+| t)

computeAndTranslateAABB :: (HasField "shape" a Shape, HasField "transform" a Transform) => a -> AABB
computeAndTranslateAABB obj = translateAABB (computeAABB $ obj.shape) obj.transform.pos

computeAABB :: Shape -> AABB
computeAABB (ShapeAABB x) = x
computeAABB (ShapeCircle (Circle{..})) = AABB (Vec (-r) (-r)) (Vec r r)

computeSquare :: Shape -> Float
computeSquare (ShapeAABB x) = computeSquareAABB x
computeSquare (ShapeCircle (Circle{..})) = r ** 2 * pi

computeSquareAABB :: AABB -> Float
computeSquareAABB x = w * h
  where
    Vec w h = sizeAABB x

computeMass :: SimObj -> SimObj
computeMass SimObj{..} =
    let mass' = material.density * computeSquare shape
     in SimObj{mass = mass', ..}

computeCollisionDist :: Shape -> Float
computeCollisionDist (ShapeAABB x) = 0
computeCollisionDist (ShapeCircle (Circle{..})) = r

getInvMass :: Float -> Float
getInvMass 0 = 0
getInvMass m = 1 / m

createPos :: Vec -> Transform
createPos p = Transform p p

move :: Vec -> Transform -> Transform
move v Transform{..} =
    let pos' = pos |+| v
    in Transform{pos = pos', ..} 

createCircle :: Float -> Vec -> Vec -> Float -> (Int -> SimObj)
createCircle r p v d id = SimObj id (ShapeCircle (Circle r)) (createPos p) (Material d 0.6) 0 v (Vec 0 0)

createAABB :: Vec -> Vec -> Vec -> Vec -> (Int -> SimObj)
createAABB minB maxB p v id = SimObj id (ShapeAABB (AABB minB maxB)) (createPos p) (Material 0 1) 0 v (Vec 0 0)

createField :: Vec -> Vec -> Vec -> Effect -> SimField
createField minB maxB p = SimField (ShapeAABB (AABB minB maxB)) (createPos p)

addSimObj :: (Int -> SimObj) -> [SimObj] -> [SimObj]
addSimObj f xs = computeMass (f (length xs + 1)) : xs

addSimObjs :: [Int -> SimObj] -> [SimObj] -> [SimObj]
addSimObjs fs xs = reverse (zipWith (\f id -> computeMass $ f id) fs [length xs + 1 .. length xs + length fs]) ++ xs

checkAABBCollision :: AABB -> AABB -> Bool
checkAABBCollision x y = not $ (x.maxP.x < y.minP.x || x.minP.x > y.maxP.x) || (x.maxP.y < y.minP.y || x.minP.y > y.maxP.y)

pairs' :: (a -> a -> Bool) -> [a] -> [(a, a)]
pairs' f l = [(x, y) | (x : ys) <- tails l, y <- ys, f x y]

createChain :: World -> (Int -> SimObj) -> Vec -> Int -> World
createChain w obj direction count = w
    & #objs %~ addSimObjs (map
        (\steps id -> obj id & #transform %~ (\t -> createPos $ t.pos |+| (direction |* fromIntegral steps))
        ) [0..count - 1])
    & (\w ->
        let
            lastId = (head w.objs).id
         in w & #links %~ (map (\(aId, bId, dist) -> SimLink {..}) ([(x, x-1, length' direction) | x <- [lastId-count+2..lastId]]) ++))