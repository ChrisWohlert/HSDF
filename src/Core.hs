{-# LANGUAGE GADTs #-}

module Core where

data SDF where
  SDFShape :: Shape -> SDF
  SDFOp :: Op -> SDF
  SDFNone :: SDF

data Shape = Circle Vec3 Double

data Op = Union SDF SDF | Subtraction SDF SDF | Intersection SDF SDF

type Vec3 = (Double, Double, Double)

instance Semigroup SDF where
  a <> b = SDFOp $ Union a b

instance Monoid SDF where
  mempty = SDFNone
    
shape :: Shape -> SDF
shape = SDFShape

circle :: Vec3 -> Double -> SDF
circle p r = shape $ Circle p r

(+->) :: Semigroup a => a -> a -> a
a +-> b = a <> b

(-->) :: SDF -> SDF -> Op
a --> b = Subtraction a b

(|->) :: SDF -> SDF -> Op
a |-> b = Intersection a b