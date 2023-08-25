{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Vec where

import GHC.Generics

data Vec = Vec
    { x :: Float
    , y :: Float
    }
    deriving (Generic, Show)

(|%|) :: Vec -> Vec -> (Float -> Float -> Float) -> Vec
(|%|) a b f = Vec (f a.x b.x) (f a.y b.y)

(|+|) :: Vec -> Vec -> Vec
(|+|) a b = a |%| b $ (+)

(|-|) :: Vec -> Vec -> Vec
(|-|) a b = a |%| b $ (-)

(|/|) :: Vec -> Vec -> Vec
(|/|) a b = a |%| b $ (/)

(|%) :: Vec -> Float -> (Float -> Float -> Float) -> Vec
(|%) a b f = Vec (f a.x b) (f a.y b)

(|*) :: Vec -> Float -> Vec
(|*) a b = a |% b $ (*)

(|**) :: Vec -> Float -> Vec
(|**) a b = a |% b $ (**)

(|/) :: Vec -> Float -> Vec
(|/) a b = a |% b $ (/)

clamp :: Vec -> Vec -> Vec -> Vec
clamp minB maxB dist = (dist |%| minB $ max) |%| maxB $ min

fastDistanceCheck :: Vec -> Float -> Bool
fastDistanceCheck v r = dotProduct v v < r ** 2

vecSc :: Float -> Float -> Vec
vecSc ang sc = Vec (cos ang * sc) (sin ang * sc)

normolize :: Vec -> Vec
normolize a = 
    case length' a of
        0 -> a
        len -> a |/ len

length' :: Vec -> Float 
length' a = sqrt (dotProduct a a)

swap' :: Vec -> Vec
swap' v = Vec (-v.y) v.x

dotProduct :: Vec -> Vec -> Float
dotProduct a b = a.x * b.x + a.y * b.y
