{-# LANGUAGE GADTs #-}

module Core where

data SDF where
  SDFShape :: Shape -> SDF
  SDFOp :: Op -> SDF
  SDFNone :: SDF

data Shape = Circle Vec3 Double

data Op = Union SDF SDF | Subtraction SDF SDF | Intersection SDF SDF

newtype Vec3 = Vec3 {vec3 :: (Double, Double, Double) }

instance Semigroup SDF where
  a <> b = SDFOp $ Union a b

instance Monoid SDF where
  mempty = SDFNone
  
instance Num Vec3 where
  Vec3 (x, y, z) +  Vec3 (x', y', z') = Vec3 (x + x', y + y', z + z')
  Vec3 (x, y, z) * Vec3 (x', y', z') = Vec3 (x * x', y * y', z * z')
  abs (Vec3 (x, y, z)) = Vec3 (abs x, abs y, abs z)
  signum = id
  fromInteger i = Vec3 (fromInteger i, fromInteger i, fromInteger i)
  negate (Vec3 (x, y, z)) = Vec3 (x * (-1), y * (-1), z * (-1))

    
shape :: Shape -> SDF
shape = SDFShape

circle :: Vec3 -> Double -> SDF
circle p r = shape $ Circle p r

(+->) :: SDF -> SDF -> SDF
a +-> b = a <> b

(-->) :: SDF -> SDF -> Op
a --> b = Subtraction a b

(|->) :: SDF -> SDF -> Op
a |-> b = Intersection a b

origin :: SDF -> Vec3
origin SDFNone = Vec3 (0, 0, 0)
origin (SDFShape s) = 
  case s of
    Circle p _ -> p
origin (SDFOp op) = 
  case op of
    Union a _ -> origin a
    Subtraction a _ -> origin a
    Intersection a _ -> origin a
