{-# LANGUAGE CApiFFI
           , CPP
           , DataKinds
           , DuplicateRecordFields
           , EmptyDataDecls
           , ForeignFunctionInterface
           , MultiParamTypeClasses #-}
#if __GLASGOW_HASKELL__ >= 902
{-# LANGUAGE NoFieldSelectors #-}
#endif
{-# LANGUAGE NoImplicitPrelude
           , PatternSynonyms
           , TypeApplications #-}

{-| Documentation for these imports can be found on the Chipmunk2D
    [website](https://chipmunk-physics.net/documentation.php).

    This module exports a lot of 'FunPtr's, some of them are impossible to marshal
    using @\"wrapper\"@. This is not an oversight: a generic conversion is not possible
    for those functions as it would require C to support closures. It is thus
    on the user of this library to create their own C functions that pass
    extra data through the relevant user pointer.

    Except for 'Space' creation\/destruction\/step and query functions, every function
    in this module is marshalled @unsafe@. 
 -}

module Chipmunk2D where

import           Data.Int
import           Data.Word
import           Foreign.Ptr
import           Foreign.Storable
import           Foreign.Storable.Offset
import           GHC.Records
import           Prelude hiding (Bool, Float)

#include <chipmunk/chipmunk.h>

#if CP_USE_DOUBLES == 0
import qualified Prelude (Float)

-- * chipmunk_types.h

type Float = Prelude.Float
#else
-- * chipmunk_types.h

type Float = #{type cpFloat}
#endif

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpfsqrt"
  fsqrt :: Float -> IO Float

foreign import CALLCV unsafe "chipmunk/chipmunk_types.h cpfsin"
  fsin :: Float -> IO Float

foreign import CALLCV unsafe "chipmunk/chipmunk_types.h cpfcos"
  fcos :: Float -> IO Float

foreign import CALLCV unsafe "chipmunk/chipmunk_types.h cpfacos"
  facos :: Float -> IO Float

foreign import CALLCV unsafe "chipmunk/chipmunk_types.h cpfatan2"
  fatan2 :: Float -> Float -> IO Float

foreign import CALLCV unsafe "chipmunk/chipmunk_types.h cpfmod"
  fmod :: Float -> Float -> IO Float

foreign import CALLCV unsafe "chipmunk/chipmunk_types.h cpfexp"
  fexp :: Float -> IO Float

foreign import CALLCV unsafe "chipmunk/chipmunk_types.h cpfpow"
  fpow :: Float -> Float -> IO Float

foreign import CALLCV unsafe "chipmunk/chipmunk_types.h cpffloor"
  ffloor :: Float -> IO Float

foreign import CALLCV unsafe "chipmunk/chipmunk_types.h cpfceil"
  fceil :: Float -> IO Float

pattern INFINITY
      , PI
     :: (Eq a, Fractional a) => a
pattern INFINITY = #const INFINITY
pattern PI       = #const CP_PI

foreign import CALLCV unsafe "chipmunk/chipmunk_types.h cpfmax"
  fmax
    :: Float    -- ^ a
    -> Float    -- ^ b
    -> IO Float

foreign import CALLCV unsafe "chipmunk/chipmunk_types.h cpfmin"
  fmin
    :: Float    -- ^ a
    -> Float    -- ^ b
    -> IO Float

foreign import CALLCV unsafe "chipmunk/chipmunk_types.h cpfabs"
  fabs
    :: Float    -- ^ f
    -> IO Float

foreign import CALLCV unsafe "chipmunk/chipmunk_types.h cpfclamp"
  fclamp
    :: Float    -- ^ f
    -> Float    -- ^ min
    -> Float    -- ^ max
    -> IO Float

foreign import CALLCV unsafe "chipmunk/chipmunk_types.h cpfclamp01"
  fclamp01
    :: Float    -- ^ f
    -> IO Float

foreign import CALLCV unsafe "chipmunk/chipmunk_types.h cpflerp"
  flerp
    :: Float    -- ^ f1
    -> Float    -- ^ f2
    -> Float    -- ^ t
    -> IO Float

foreign import CALLCV unsafe "chipmunk/chipmunk_types.h cpflerpconst"
  flerpconst
    :: Float    -- ^ f1
    -> Float    -- ^ f2
    -> Float    -- ^ d
    -> IO Float

type HashValue = #type cpHashValue

type CollisionID = #type cpCollisionID

type Bool = #type cpBool

pattern True
      , False
     :: (Eq a, Num a) => a
pattern True  = #const cpTrue
pattern False = #const cpFalse

type DataPointer = Ptr ()

type CollisionType = #type cpCollisionType

type Group = #type cpGroup

type Bitmask = #type cpBitmask

type Timestamp = #type cpTimestamp

pattern NO_GROUP
      , NO_ALL_CATEGORIES
      , WILDCARD_COLLISION_TYPE
     :: (Eq a, Num a) => a
pattern NO_GROUP                = #const CP_NO_GROUP
pattern NO_ALL_CATEGORIES       = #const CP_ALL_CATEGORIES
pattern WILDCARD_COLLISION_TYPE = #const CP_WILDCARD_COLLISION_TYPE

data {-# CTYPE "chipmunk/chipmunk.h" "cpVect" #-} Vect =
       Vect
         { x :: Float
         , y :: Float
         }

instance Offset "x" Vect where rawOffset = #offset struct cpVect, x
instance Offset "y" Vect where rawOffset = #offset struct cpVect, y

instance Storable Vect where
  sizeOf _    = #size struct cpVect
  alignment _ = #alignment struct cpVect

  peek ptr =
    Vect
      <$> peek (offset @"x" ptr)
      <*> peek (offset @"y" ptr)

  poke ptr val = do
    pokeField @"x" ptr val
    pokeField @"y" ptr val

data {-# CTYPE "chipmunk/chipmunk.h" "cpTransform" #-} Transform =
       Transform
         { a  :: Float
         , b  :: Float
         , c  :: Float
         , d  :: Float
         , tx :: Float
         , ty :: Float
         }

instance Offset "a"  Transform where rawOffset = #offset struct cpTransform, a
instance Offset "b"  Transform where rawOffset = #offset struct cpTransform, b
instance Offset "c"  Transform where rawOffset = #offset struct cpTransform, c
instance Offset "d"  Transform where rawOffset = #offset struct cpTransform, d
instance Offset "tx" Transform where rawOffset = #offset struct cpTransform, tx
instance Offset "ty" Transform where rawOffset = #offset struct cpTransform, ty

instance Storable Transform where
  sizeOf _    = #size struct cpTransform
  alignment _ = #alignment struct cpTransform

  peek ptr =
    Transform
      <$> peek (offset @"a"  ptr)
      <*> peek (offset @"b"  ptr)
      <*> peek (offset @"c"  ptr)
      <*> peek (offset @"d"  ptr)
      <*> peek (offset @"tx" ptr)
      <*> peek (offset @"ty" ptr)

  poke ptr val = do
    pokeField @"a"  ptr val
    pokeField @"b"  ptr val
    pokeField @"c"  ptr val
    pokeField @"d"  ptr val
    pokeField @"tx" ptr val
    pokeField @"ty" ptr val

data {-# CTYPE "chipmunk/chipmunk.h" "cpMat2x2" #-} Mat2x2 =
       Mat2x2
         { a  :: Float
         , b  :: Float
         , c  :: Float
         , d  :: Float
         }

instance Offset "a"  Mat2x2 where rawOffset = #offset struct cpMat2x2, a
instance Offset "b"  Mat2x2 where rawOffset = #offset struct cpMat2x2, b
instance Offset "c"  Mat2x2 where rawOffset = #offset struct cpMat2x2, c
instance Offset "d"  Mat2x2 where rawOffset = #offset struct cpMat2x2, d

instance Storable Mat2x2 where
  sizeOf _    = #size struct cpMat2x2
  alignment _ = #alignment struct cpMat2x2

  peek ptr =
    Mat2x2
      <$> peek (offset @"a"  ptr)
      <*> peek (offset @"b"  ptr)
      <*> peek (offset @"c"  ptr)
      <*> peek (offset @"d"  ptr)

  poke ptr val = do
    pokeField @"a"  ptr val
    pokeField @"b"  ptr val
    pokeField @"c"  ptr val
    pokeField @"d"  ptr val

-- end of chipmunk_types.h

pattern BUFFER_BYTES :: (Eq a, Num a) => a
pattern BUFFER_BYTES = #const CP_BUFFER_BYTES

data {-# CTYPE "chipmunk/chipmunk.h" "cpHashSet" #-}HashSet
{-
data {-# CTYPE "chipmunk/chipmunk.h" "cpBody" #-} Body

data {-# CTYPE "chipmunk/chipmunk.h" "cpShape" #-} Shape
data {-# CTYPE "chipmunk/chipmunk.h" "cpCircleShape" #-} CircleShape
data {-# CTYPE "chipmunk/chipmunk.h" "cpSegmentShape" #-} SegmentShape
data {-# CTYPE "chipmunk/chipmunk.h" "cpPolyShape" #-} PolyShape

data {-# CTYPE "chipmunk/chipmunk.h" "cpConstraint" #-} Constraint
data {-# CTYPE "chipmunk/chipmunk.h" "cpPinJoint" #-} PinJoint
data {-# CTYPE "chipmunk/chipmunk.h" "cpSlideJoint" #-} SlideJoint
data {-# CTYPE "chipmunk/chipmunk.h" "cpPivotJoint" #-} PivotJoint
data {-# CTYPE "chipmunk/chipmunk.h" "cpGrooveJoint" #-} GrooveJoint
data {-# CTYPE "chipmunk/chipmunk.h" "cpDampedSpring" #-} DampedSpring
data {-# CTYPE "chipmunk/chipmunk.h" "cpDampedRotarySpring" #-} DampedRotarySpring
data {-# CTYPE "chipmunk/chipmunk.h" "cpRotaryLimitJoint" #-} RotaryLimitJoint
data {-# CTYPE "chipmunk/chipmunk.h" "cpRatchetJoint" #-} RatchetJoint
data {-# CTYPE "chipmunk/chipmunk.h" "cpGearJoint" #-} GearJoint -}
data {-# CTYPE "chipmunk/chipmunk.h" "cpSimpleMotorJoint" #-} SimpleMotorJoint
{-
data {-# CTYPE "chipmunk/chipmunk.h" "cpArbiter" #-} Arbiter

data {-# CTYPE "chipmunk/chipmunk.h" "cpSpace" #-} Space
-}
-- * cpVect.h

foreign import CALLCV unsafe "wrapper.h w_cpvzero"
  vzero
    :: Ptr Vect -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpv"
  vnew
    :: Float    -- ^ x
    -> Float    -- ^ y
    -> Ptr Vect -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpveql"
  veql
    :: Ptr Vect -- ^ v1
    -> Ptr Vect -- ^ v2
    -> IO Bool

foreign import CALLCV unsafe "wrapper.h w_cpvadd"
  vadd
    :: Ptr Vect -- ^ v1
    -> Ptr Vect -- ^ v2
    -> Ptr Vect -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpvsub"
  vsub
    :: Ptr Vect -- ^ v1
    -> Ptr Vect -- ^ v2
    -> Ptr Vect -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpvneg"
  vneg
    :: Ptr Vect -- ^ v
    -> Ptr Vect -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpvmult"
  vmult
    :: Ptr Vect -- ^ v
    -> Float    -- ^ s
    -> Ptr Vect -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpvdot"
  vdot
    :: Ptr Vect -- ^ v1
    -> Ptr Vect -- ^ v2
    -> IO Float

foreign import CALLCV unsafe "wrapper.h w_cpvcross"
  vcross
    :: Ptr Vect -- ^ v1
    -> Ptr Vect -- ^ v2
    -> IO Float

foreign import CALLCV unsafe "wrapper.h w_cpvperp"
  vperp
    :: Ptr Vect -- ^ v
    -> Ptr Vect -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpvrperp"
  vrperp
    :: Ptr Vect -- ^ v
    -> Ptr Vect -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpvproject"
  vproject
    :: Ptr Vect -- ^ v1
    -> Ptr Vect -- ^ v2
    -> Ptr Vect -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpvforangle"
  vforangle
    :: Float    -- ^ a
    -> Ptr Vect -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpvtoangle"
  vtoangle
    :: Ptr Vect -- ^ v
    -> IO Float

foreign import CALLCV unsafe "wrapper.h w_cpvrotate"
  vrotate
    :: Ptr Vect -- ^ v1
    -> Ptr Vect -- ^ v2
    -> Ptr Vect -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpvunrotate"
  vunrotate
    :: Ptr Vect -- ^ v1
    -> Ptr Vect -- ^ v2
    -> Ptr Vect -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpvlengthsq"
  vlengthsq
    :: Ptr Vect -- ^ v
    -> IO Float

foreign import CALLCV unsafe "wrapper.h w_cpvlength"
  vlength
    :: Ptr Vect -- ^ v
    -> IO Float

foreign import CALLCV unsafe "wrapper.h w_cpvlerp"
  vlerp
    :: Ptr Vect -- ^ v1
    -> Ptr Vect -- ^ v2
    -> Float    -- ^ t
    -> Ptr Vect -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpvnormalize"
  vnormalize
    :: Ptr Vect -- ^ v
    -> Ptr Vect -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpvslerp"
  vslerp
    :: Ptr Vect -- ^ v1
    -> Ptr Vect -- ^ v2
    -> Float    -- ^ t
    -> Ptr Vect -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpvslerpconst"
  vslerpconst
    :: Ptr Vect -- ^ v1
    -> Ptr Vect -- ^ v2
    -> Float    -- ^ a
    -> Ptr Vect -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpvclamp"
  vclamp
    :: Ptr Vect -- ^ v
    -> Float    -- ^ len
    -> Ptr Vect -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpvlerpconst"
  vlerpconst
    :: Ptr Vect -- ^ v1
    -> Ptr Vect -- ^ v2
    -> Float    -- ^ d
    -> Ptr Vect -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpvdist"
  vdist
    :: Ptr Vect -- ^ v1
    -> Ptr Vect -- ^ v2
    -> IO Float

foreign import CALLCV unsafe "wrapper.h w_cpvdistsq"
  vdistsq
    :: Ptr Vect -- ^ v1
    -> Ptr Vect -- ^ v2
    -> IO Float

foreign import CALLCV unsafe "wrapper.h w_cpvnear"
  vnear
    :: Ptr Vect -- ^ v1
    -> Ptr Vect -- ^ v2
    -> Float    -- ^ dist
    -> IO Float



foreign import CALLCV unsafe "wrapper.h w_cpMat2x2Transform"
  mat2x2Transform
    :: Ptr Mat2x2 -- ^ m
    -> Ptr Vect   -- ^ m
    -> Ptr Vect   -- ^ Output value
    -> IO ()

-- end of cpVect.h

-- * cpBB.h

data {-# CTYPE "chipmunk/chipmunk.h" "cpBB" #-} BB =
       BB
         { l :: Float
         , b :: Float
         , r :: Float
         , t :: Float
         }

instance Offset "l" BB where rawOffset = #offset struct cpBB, l
instance Offset "b" BB where rawOffset = #offset struct cpBB, b
instance Offset "r" BB where rawOffset = #offset struct cpBB, r
instance Offset "t" BB where rawOffset = #offset struct cpBB, t

instance Storable BB where
  sizeOf _    = #size struct cpBB
  alignment _ = #alignment struct cpBB

  peek ptr =
    BB
      <$> peek (offset @"l" ptr)
      <*> peek (offset @"b" ptr)
      <*> peek (offset @"r" ptr)
      <*> peek (offset @"t" ptr)

  poke ptr val = do
    pokeField @"l" ptr val
    pokeField @"b" ptr val
    pokeField @"r" ptr val
    pokeField @"t" ptr val

foreign import CALLCV unsafe "wrapper.h w_cpBBNew"
  bbNew
    :: Float  -- ^ l
    -> Float  -- ^ b
    -> Float  -- ^ r
    -> Float  -- ^ t
    -> Ptr BB -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpBBNewForExtents"
  bbNewForExtents
    :: Ptr Vect -- ^ c
    -> Float    -- ^ hw
    -> Float    -- ^ hh
    -> Ptr BB   -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpBBNewForCircle"
  bbNewForCircle
    :: Ptr Vect -- ^ p
    -> Float    -- ^ r
    -> Ptr BB   -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpBBIntersects"
  bbIntersects
    :: Ptr BB -- ^ a
    -> Ptr BB -- ^ b
    -> IO Bool

foreign import CALLCV unsafe "wrapper.h w_cpBBContainsBB"
  bbContainsBB
    :: Ptr BB -- ^ bba
    -> Ptr BB -- ^ other
    -> IO Bool

foreign import CALLCV unsafe "wrapper.h w_cpBBContainsVect"
  bbContainsVect
    :: Ptr BB   -- ^ bb
    -> Ptr Vect -- ^ v
    -> IO Bool

foreign import CALLCV unsafe "wrapper.h w_cpBBMerge"
  bbMerge
    :: Ptr BB -- ^ a
    -> Ptr BB -- ^ b
    -> Ptr BB -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpBBExpand"
  bbExpand
    :: Ptr BB   -- ^ bb
    -> Ptr Vect -- ^ v
    -> Ptr BB   -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpBBCenter"
  bbCenter
    :: Ptr BB   -- ^ bb
    -> Ptr Vect -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpBBArea"
  bbArea
    :: Ptr BB   -- ^ bb
    -> IO Float

foreign import CALLCV unsafe "wrapper.h w_cpBBMergedArea"
  bbMergedArea
    :: Ptr BB   -- ^ a
    -> Ptr BB   -- ^ b
    -> IO Float

foreign import CALLCV unsafe "wrapper.h w_cpBBSegmentQuery"
  bbSegmentQuery
    :: Ptr BB   -- ^ bb
    -> Ptr Vect -- ^ a
    -> Ptr Vect -- ^ b
    -> IO Float

foreign import CALLCV unsafe "wrapper.h w_cpBBIntersectsSegment"
  bbIntersectsSegment
    :: Ptr BB   -- ^ bb
    -> Ptr Vect -- ^ a
    -> Ptr Vect -- ^ b
    -> IO Bool

foreign import CALLCV unsafe "wrapper.h w_cpBBClampVect"
  bbClampVect
    :: Ptr BB   -- ^ bb
    -> Ptr Vect -- ^ v
    -> Ptr Vect -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpBBWrapVect"
  bbWrapVect
    :: Ptr BB   -- ^ bb
    -> Ptr Vect -- ^ v
    -> Ptr Vect -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpBBOffset"
  bbOffset
    :: Ptr cpBB   -- ^ bb
    -> Ptr cpVect -- ^ v
    -> Ptr cpBB   -- ^ cpBB
    -> IO ()

-- end of cpBB.h

-- * cpTransform.h

foreign import CALLCV unsafe "wrapper.h w_cpTransformIdentity "
  transformIdentity
    :: Ptr Transform -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpTransformNew"
  transformNew
    :: Float         -- ^ a
    -> Float         -- ^ b
    -> Float         -- ^ c
    -> Float         -- ^ d
    -> Float         -- ^ tx
    -> Float         -- ^ ty
    -> Ptr Transform -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpTransformNewTranspose"
  transformNewTranspose
    :: Float         -- ^ a
    -> Float         -- ^ c
    -> Float         -- ^ tx
    -> Float         -- ^ b
    -> Float         -- ^ d
    -> Float         -- ^ ty
    -> Ptr Transform -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpTransformInverse"
  transformInverse
    :: Ptr Transform -- ^ t
    -> Ptr Transform -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpTransformMult"
  transformMult
    :: Ptr Transform -- ^ t1
    -> Ptr Transform -- ^ t2
    -> Ptr Transform -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpTransformPoint"
  transformPoint
    :: Ptr Transform -- ^ t
    -> Ptr Vect      -- ^ p
    -> Ptr Vect      -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpTransformVect"
  transformVect
    :: Ptr Transform -- ^ t
    -> Ptr Vect      -- ^ v
    -> Ptr Vect      -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpTransformbBB"
  transformbBB
    :: Ptr Transform -- ^ t
    -> Ptr BB        -- ^ bb
    -> Ptr BB        -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpTransformTranslate"
  transformTranslate
    :: Ptr Vect      -- ^ translate
    -> Ptr Transform -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpTransformScale"
  transformScale
    :: Float         -- ^ scaleX
    -> Float         -- ^ scaleY
    -> Ptr Transform -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpTransformRotate"
  transformRotate
    :: Float         -- ^ radians
    -> Ptr Transform -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpTransformRigid"
  transformRigid
    :: Ptr Vect      -- ^ translate
    -> Float         -- ^ radians
    -> Ptr Transform -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpTransformRigidInverse"
  transformRigidInverse
    :: Ptr Transform -- ^ t
    -> Ptr Transform -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpTransformWrap"
  transformWrap
    :: Ptr Transform -- ^ Output valueer
    -> Ptr Transform -- ^ inner
    -> Ptr Transform -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpTransformWrapInverse"
  transformWrapInverse
    :: Ptr Transform -- ^ Output valueer
    -> Ptr Transform -- ^ inner
    -> Ptr Transform -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpTransformOrtho"
  transformOrtho
    :: Ptr BB        -- ^ bb
    -> Ptr Transform -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpTransformBoneScale"
  transformBoneScale
    :: Ptr Vect      -- ^ v0
    -> Ptr Vect      -- ^ v1
    -> Ptr Transform -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpTransformAxialScale"
  transformAxialScale
    :: Ptr Vect      -- ^ axis
    -> Ptr Vect      -- ^ pivot
    -> Float         -- ^ scale
    -> Ptr Transform -- ^ Output value
    -> IO ()

-- end of cpTransform.h

-- * cpSpatialIndex.h

type SpatialIndexBBFunc =
          Ptr () -- ^ obj
       -> IO BB

type SpatialIndexIteratorFunc =
          Ptr () -- ^ obj
       -> Ptr () -- ^ data
       -> IO ()

type SpatialIndexQueryFunc =
          Ptr ()         -- ^ obj1
       -> Ptr ()         -- ^ obj2
       -> CollisionID    -- ^ id
       -> Ptr ()         -- ^ data
       -> IO CollisionID

type SpatialIndexSegmentQueryFunc =
          Ptr ()         -- ^ obj1
       -> Ptr ()         -- ^ obj2
       -> Ptr ()         -- ^ data
       -> IO Float

data {-# CTYPE "chipmunk/chipmunk.h" "cpSpatialIndex" #-} SpatialIndex

data {-# CTYPE "chipmunk/chipmunk.h" "cpSpaceHash" #-} SpaceHash

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSpaceHashAlloc"
  spaceHashAlloc :: IO (Ptr SpaceHash)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSpaceHashInit"
  spaceHashInit
    :: Ptr SpaceHash             -- ^ hash
    -> Float                     -- ^ celldim
    -> #{type int}               -- ^ numcells
    -> FunPtr SpatialIndexBBFunc -- ^ bbfunc
    -> Ptr SpatialIndex          -- ^ staticIndex
    -> IO (Ptr SpatialIndex)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSpaceHashNew"
  spaceHashNew
    :: Float                     -- ^ celldim
    -> #{type int}               -- ^ numcells
    -> FunPtr SpatialIndexBBFunc -- ^ bbfunc
    -> Ptr SpatialIndex          -- ^ staticIndex
    -> IO (Ptr SpatialIndex)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSpaceHashResize"
  spaceHashResize
    :: Ptr SpaceHash -- ^ hash
    -> Float         -- ^ celldim
    -> #{type int}   -- ^ numcells
    -> IO ()

data {-# CTYPE "chipmunk/chipmunk.h" "cpBBTree" #-} BBTree

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpBBTreeAlloc"
  bbTreeAlloc
    :: IO (Ptr BBTree)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpBBTreeInit"
  bbTreeInit
    :: Ptr BBTree                -- ^ tree
    -> FunPtr SpatialIndexBBFunc -- ^ bbfunc
    -> Ptr SpatialIndex          -- ^ staticIndex
    -> IO (Ptr SpatialIndex)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpBBTreeNew"
  bbTreeNew
    :: FunPtr SpatialIndexBBFunc -- ^ bbfunc
    -> Ptr SpatialIndex          -- ^ staticIndex
    -> IO (Ptr SpatialIndex)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpBBTreeOptimize"
  bbTreeOptimize
    :: Ptr SpatialIndex -- ^ index
    -> IO ()

type BBTreeVelocityFunc =
          Ptr () -- ^ obj
       -> IO Vect

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpBBTreeSetVelocityFunc"
  bbTreeSetVelocityFunc
    :: Ptr SpatialIndex          -- ^ index
    -> FunPtr BBTreeVelocityFunc -- ^ func
    -> IO ()

data {-# CTYPE "chipmunk/chipmunk.h" "cpSweep1D" #-} Sweep1D

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSweep1DAlloc"
  sweep1DAlloc
    :: IO (Ptr Sweep1D)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSweep1DInit"
  sweep1DInit
    :: Ptr Sweep1D               -- ^ sweep
    -> FunPtr SpatialIndexBBFunc -- ^ bbfunc
    -> Ptr SpatialIndex          -- ^ staticIndex
    -> IO (Ptr SpatialIndex)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSweep1DNew"
  sweep1DNew
    :: FunPtr SpatialIndexBBFunc -- ^ bbfunc
    -> Ptr SpatialIndex          -- ^ staticIndex
    -> IO (Ptr SpatialIndex)

type SpatialIndexDestroyImpl =
          Ptr SpatialIndex -- ^ index
       -> IO ()

type SpatialIndexCountImpl =
          Ptr SpatialIndex -- ^ index
       -> IO #type int

type SpatialIndexEachImpl =
          Ptr SpatialIndex                -- ^ index
       -> FunPtr SpatialIndexIteratorFunc -- ^ func
       -> Ptr ()                          -- ^ data
       -> IO ()

type SpatialIndexContainsImpl =
          Ptr SpatialIndex -- ^ index
       -> Ptr ()           -- ^ obj
       -> HashValue        -- ^ hashid
       -> IO Bool

type SpatialIndexInsertImpl =
          Ptr SpatialIndex -- ^ index
       -> Ptr ()           -- ^ obj
       -> HashValue        -- ^ hashid
       -> IO ()

type SpatialIndexRemoveImpl =
          Ptr SpatialIndex -- ^ index
       -> Ptr ()           -- ^ obj
       -> HashValue        -- ^ hashid
       -> IO ()

type SpatialIndexReindexImpl =
          Ptr SpatialIndex -- ^ index
       -> IO ()

type SpatialIndexReindexObjectImpl =
          Ptr SpatialIndex -- ^ index
       -> Ptr ()           -- ^ obj
       -> HashValue        -- ^ hashid
       -> IO ()

type SpatialIndexReindexQueryImpl =
          Ptr SpatialIndex             -- ^ index
       -> FunPtr SpatialIndexQueryFunc -- ^ func
       -> Ptr ()                       -- ^ data
       -> IO ()

type SpatialIndexQueryImpl =
          Ptr SpatialIndex             -- ^ index
       -> Ptr ()                       -- ^ obj
       -> BB                           -- ^ bb
       -> FunPtr SpatialIndexQueryFunc -- ^ func
       -> Ptr ()                       -- ^ data
       -> IO ()

type SpatialIndexSegmentQueryImpl =
          Ptr SpatialIndex                    -- ^ index
       -> Ptr ()                              -- ^ obj
       -> Vect                                -- ^ a
       -> Vect                                -- ^ b
       -> Float                               -- ^ t_exit
       -> FunPtr SpatialIndexSegmentQueryFunc -- ^ func
       -> Ptr ()                              -- ^ data
       -> IO ()

data {-# CTYPE "chipmunk/chipmunk.h" "cpSpatialIndexClass" #-} SpatialIndexClass =
       SpatialIndexClass
         { destroy       :: FunPtr SpatialIndexDestroyImpl
         , count         :: FunPtr SpatialIndexCountImpl
         , each          :: FunPtr SpatialIndexEachImpl
         , contains      :: FunPtr SpatialIndexContainsImpl
         , insert        :: FunPtr SpatialIndexInsertImpl
         , remove        :: FunPtr SpatialIndexRemoveImpl
         , reindex       :: FunPtr SpatialIndexReindexImpl
         , reindexObject :: FunPtr SpatialIndexReindexObjectImpl
         , reindexQuery  :: FunPtr SpatialIndexReindexQueryImpl
         , query         :: FunPtr SpatialIndexQueryImpl
         , segmentQuery  :: FunPtr SpatialIndexSegmentQueryImpl
         }

instance Offset "destroy"       SpatialIndexClass where rawOffset = #offset struct cpSpatialIndexClass, destroy
instance Offset "count"         SpatialIndexClass where rawOffset = #offset struct cpSpatialIndexClass, count
instance Offset "each"          SpatialIndexClass where rawOffset = #offset struct cpSpatialIndexClass, each
instance Offset "contains"      SpatialIndexClass where rawOffset = #offset struct cpSpatialIndexClass, contains
instance Offset "insert"        SpatialIndexClass where rawOffset = #offset struct cpSpatialIndexClass, insert
instance Offset "remove"        SpatialIndexClass where rawOffset = #offset struct cpSpatialIndexClass, remove
instance Offset "reindex"       SpatialIndexClass where rawOffset = #offset struct cpSpatialIndexClass, reindex
instance Offset "reindexObject" SpatialIndexClass where rawOffset = #offset struct cpSpatialIndexClass, reindexObject
instance Offset "reindexQuery"  SpatialIndexClass where rawOffset = #offset struct cpSpatialIndexClass, reindexQuery
instance Offset "query"         SpatialIndexClass where rawOffset = #offset struct cpSpatialIndexClass, query
instance Offset "segmentQuery"  SpatialIndexClass where rawOffset = #offset struct cpSpatialIndexClass, segmentQuery

instance Storable SpatialIndexClass where
  sizeOf _    = #size      struct cpSpatialIndexClass
  alignment _ = #alignment struct cpSpatialIndexClass

  peek ptr =
    SpatialIndexClass
      <$> peek (offset @"destroy"       ptr)
      <*> peek (offset @"count"         ptr)
      <*> peek (offset @"each"          ptr)
      <*> peek (offset @"contains"      ptr)
      <*> peek (offset @"insert"        ptr)
      <*> peek (offset @"remove"        ptr)
      <*> peek (offset @"reindex"       ptr)
      <*> peek (offset @"reindexObject" ptr)
      <*> peek (offset @"reindexQuery"  ptr)
      <*> peek (offset @"query"         ptr)
      <*> peek (offset @"segmentQuery"  ptr)

  poke ptr val = do
    pokeField @"destroy"       ptr val
    pokeField @"count"         ptr val
    pokeField @"each"          ptr val
    pokeField @"contains"      ptr val
    pokeField @"insert"        ptr val
    pokeField @"remove"        ptr val
    pokeField @"reindex"       ptr val
    pokeField @"reindexObject" ptr val
    pokeField @"reindexQuery"  ptr val
    pokeField @"query"         ptr val
    pokeField @"segmentQuery"  ptr val

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSpatialIndexFree"
  spatialIndexFree
    :: Ptr cpSpatialIndex -- ^ index
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSpatialIndexCollideStatic"
  spatialIndexCollideStatic
    :: Ptr SpatialIndex             -- ^ dynamicIndex
    -> Ptr SpatialIndex             -- ^ staticIndex
    -> FunPtr SpatialIndexQueryFunc -- ^ func
    -> Ptr ()                       -- ^ data
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSpatialIndexDestroy"
  spatialIndexDestroy
    :: Ptr SpatialIndex -- ^ index
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSpatialIndexCount"
  spatialIndexCount
    :: Ptr cpSpatialIndex -- ^ index
    -> IO #type int

foreign import CALLCV "chipmunk/chipmunk.h cpSpatialIndexEach"
  spatialIndexEach
    :: Ptr SpatialIndex                -- ^ index
    -> FunPtr SpatialIndexIteratorFunc -- ^ func
    -> Ptr ()                          -- ^ data
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSpatialIndexContains"
  spatialIndexContains
    :: Ptr SpatialIndex -- ^ index
    -> Ptr ()           -- ^ obj
    -> HashValue        -- ^ hashid
    -> IO Bool

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSpatialIndexInsert"
  spatialIndexInsert
    :: Ptr SpatialIndex -- ^ index
    -> Ptr ()           -- ^ obj
    -> HashValue        -- ^ hashid
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSpatialIndexRemove"
  spatialIndexRemove
    :: Ptr SpatialIndex -- ^ index
    -> Ptr ()           -- ^ obj
    -> HashValue        -- ^ hashid
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSpatialIndexReindex"
  spatialIndexReindex
    :: Ptr SpatialIndex -- ^ index
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSpatialIndexReindexObject"
  spatialIndexReindexObject
    :: Ptr SpatialIndex -- ^ index
    -> Ptr ()           -- ^ obj
    -> HashValue        -- ^ hashid
    -> IO ()

foreign import CALLCV "wrapper.h w_cpSpatialIndexQuery"
  spatialIndexQuery
    :: Ptr SpatialIndex             -- ^ *index
    -> Ptr ()                       -- ^ obj
    -> Ptr BB                       -- ^ bb
    -> FunPtr SpatialIndexQueryFunc -- ^ func
    -> Ptr ()                       -- ^ data
    -> IO ()

foreign import CALLCV "wrapper.h w_cpSpatialIndexSegmentQuery"
  spatialIndexSegmentQuery
    :: Ptr SpatialIndex                    -- ^ index
    -> Ptr ()                              -- ^ obj
    -> Ptr Vect                            -- ^ a
    -> Ptr Vect                            -- ^ b
    -> Float                               -- ^ t_exit
    -> FunPtr SpatialIndexSegmentQueryFunc -- ^ func
    -> Ptr ()                              -- ^ data
    -> IO ()

foreign import CALLCV "chipmunk/chipmunk.h cpSpatialIndexReindexQuery"
  spatialIndexReindexQuery
    :: Ptr SpatialIndex             -- ^ index
    -> FunPtr SpatialIndexQueryFunc -- ^ func
    -> Ptr ()                       -- ^ data
    -> IO ()

-- end of cpSpatialIndex.h

-- * cpArbiter.h

pattern MAX_CONTACTS_PER_ARBITER :: (Eq a, Num a) => a
pattern MAX_CONTACTS_PER_ARBITER = #const CP_MAX_CONTACTS_PER_ARBITER

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpArbiterGetRestitution"
  arbiterGetRestitution
    :: Ptr Arbiter -- ^ arb
    -> IO Float

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpArbiterSetRestitution"
  arbiterSetRestitution
    :: Ptr Arbiter -- ^ arb
    -> Float       -- ^ restitution
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpArbiterGetFriction"
  arbiterGetFriction
    :: Ptr Arbiter -- ^ arb
    -> IO Float

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpArbiterSetFriction"
  arbiterSetFriction
    :: Ptr Arbiter -- ^ arb
    -> Float       -- ^ friction
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpArbiterGetSurfaceVelocity"
  arbiterGetSurfaceVelocity
    :: Ptr Arbiter -- ^ arb
    -> Ptr Vect    -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpArbiterSetSurfaceVelocity"
  arbiterSetSurfaceVelocity
    :: Ptr Arbiter -- ^ arb
    -> Ptr Vect    -- ^ vr
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpArbiterGetUserData"
  arbiterGetUserData
    :: Ptr Arbiter -- ^ arb
    -> IO DataPointer

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpArbiterSetUserData"
  arbiterSetUserData
    :: Ptr Arbiter -- ^ arb
    -> DataPointer -- ^ userData
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpArbiterTotalImpulse"
  arbiterTotalImpulse
    :: Ptr Arbiter -- ^ arb
    -> Ptr Vect    -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpArbiterTotalKE"
  arbiterTotalKE
    :: Ptr Arbiter -- ^ arb
    -> IO Float

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpArbiterIgnore"
  arbiterIgnore
    :: Ptr Arbiter -- ^ arb
    -> IO Bool

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpArbiterGetShapes"
  arbiterGetShapes
    :: Ptr Arbiter     -- ^ arb
    -> Ptr (Ptr Shape) -- ^ a
    -> Ptr (Ptr Shape) -- ^ b
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpArbiterGetBodies"
  arbiterGetBodies
    :: Ptr Arbiter    -- ^ arb
    -> Ptr (Ptr Body) -- ^ a
    -> Ptr (Ptr Body) -- ^ b
    -> IO ()

data {-# CTYPE "chipmunk/chipmunk.h" "cpContactPointSet" #-} ContactPointSet =
       ContactPointSet
         { count  :: #type int
         , normal :: Vect
         , points :: Ptr ContactPoint
         }

instance Offset "count"  ContactPointSet where rawOffset = #offset struct cpContactPointSet, count
instance Offset "normal" ContactPointSet where rawOffset = #offset struct cpContactPointSet, normal
instance Offset "points" ContactPointSet where rawOffset = #offset struct cpContactPointSet, points

instance Storable ContactPointSet where
  sizeOf _    = #size struct cpContactPointSet
  alignment _ = #alignment struct cpContactPointSet

  peek ptr =
    ContactPointSet
      <$> peek (offset @"count"  ptr)
      <*> peek (offset @"normal" ptr)
      <*> peek (offset @"points" ptr)

  poke ptr val = do
    pokeField @"count"  ptr val
    pokeField @"normal" ptr val
    pokeField @"points" ptr val

data {-# CTYPE "chipmunk/chipmunk.h" "cpContactPoint" #-} ContactPoint =
       ContactPoint
         { pointA   :: Vect
         , pointB   :: Vect
         , distance :: Float
         }

instance Offset "pointA"   ContactPoint where rawOffset = #offset struct cpContactPoint, pointA
instance Offset "pointB"   ContactPoint where rawOffset = #offset struct cpContactPoint, pointB
instance Offset "distance" ContactPoint where rawOffset = #offset struct cpContactPoint, distance

instance Storable ContactPoint where
  sizeOf _    = #size struct cpContactPoint
  alignment _ = #alignment struct cpContactPoint

  peek ptr =
    ContactPoint
      <$> peek (offset @"pointA"   ptr)
      <*> peek (offset @"pointB"   ptr)
      <*> peek (offset @"distance" ptr)

  poke ptr val = do
    pokeField @"pointA"   ptr val
    pokeField @"pointB"   ptr val
    pokeField @"distance" ptr val

foreign import CALLCV unsafe "wrapper.h w_cpArbiterGetContactPointSet"
  arbiterGetContactPointSet
    :: Ptr Arbiter -- ^ arb
    -> Ptr ContactPointSet -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpArbiterSetContactPointSet"
  arbiterSetContactPointSet
    :: Ptr Arbiter         -- ^ arb
    -> Ptr ContactPointSet -- ^ set
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpArbiterIsFirstContact"
  arbiterIsFirstContact
    :: Ptr Arbiter -- ^ arb
    -> IO Bool

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpArbiterIsRemoval"
  arbiterIsRemoval
    :: Ptr Arbiter -- ^ arb
    -> IO Bool

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpArbiterGetCount"
  arbiterGetCount
    :: Ptr Arbiter -- ^ arb
    -> IO #type int

foreign import CALLCV unsafe "wrapper.h w_cpArbiterGetNormal"
  arbiterGetNormal
    :: Ptr Arbiter -- ^ arb
    -> Ptr Vect    -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpArbiterGetPointA"
  arbiterGetPointA
    :: Ptr Arbiter -- ^ arb
    -> #{type int} -- ^ i
    -> Ptr Vect    -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpArbiterGetPointB"
  arbiterGetPointB
    :: Ptr Arbiter -- ^ arb
    -> #{type int} -- ^ i
    -> Ptr Vect    -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpArbiterGetDepth"
  arbiterGetDepth
    :: Ptr Arbiter -- ^ arb
    -> #{type int} -- ^ i
    -> IO Float

foreign import CALLCV "chipmunk/chipmunk.h cpArbiterCallWildcardBeginA"
  arbiterCallWildcardBeginA
    :: Ptr Arbiter -- ^ arb
    -> Ptr Space   -- ^ space
    -> IO Bool

foreign import CALLCV "chipmunk/chipmunk.h cpArbiterCallWildcardBeginB"
  arbiterCallWildcardBeginB
    :: Ptr Arbiter -- ^ arb
    -> Ptr Space   -- ^ space
    -> IO Bool

foreign import CALLCV "chipmunk/chipmunk.h cpArbiterCallWildcardPreSolveA"
  arbiterCallWildcardPreSolveA
    :: Ptr Arbiter -- ^ arb
    -> Ptr Space   -- ^ space
    -> IO Bool

foreign import CALLCV "chipmunk/chipmunk.h cpArbiterCallWildcardPreSolveB"
  arbiterCallWildcardPreSolveB
    :: Ptr Arbiter -- ^ arb
    -> Ptr Space   -- ^ space
    -> IO Bool

foreign import CALLCV "chipmunk/chipmunk.h cpArbiterCallWildcardPostSolveA"
  arbiterCallWildcardPostSolveA
    :: Ptr Arbiter -- ^ arb
    -> Ptr Space   -- ^ space
    -> IO ()

foreign import CALLCV "chipmunk/chipmunk.h cpArbiterCallWildcardPostSolveB"
  arbiterCallWildcardPostSolveB
    :: Ptr Arbiter -- ^ arb
    -> Ptr Space   -- ^ space
    -> IO ()

foreign import CALLCV "chipmunk/chipmunk.h cpArbiterCallWildcardSeparateA"
  arbiterCallWildcardSeparateA
    :: Ptr Arbiter -- ^ arb
    -> Ptr Space   -- ^ space
    -> IO ()

foreign import CALLCV "chipmunk/chipmunk.h cpArbiterCallWildcardSeparateB"
  arbiterCallWildcardSeparateB
    :: Ptr Arbiter -- ^ arb
    -> Ptr Space   -- ^ space
    -> IO ()

-- end of cpArbiter.h

-- * cpBody.h

type BodyType = #type cpBodyType

pattern BODY_TYPE_DYNAMIC
      , BODY_TYPE_KINEMATIC
      , BODY_TYPE_STATIC
     :: (Eq a, Num a) => a
pattern BODY_TYPE_DYNAMIC   = #const CP_BODY_TYPE_DYNAMIC
pattern BODY_TYPE_KINEMATIC = #const CP_BODY_TYPE_KINEMATIC
pattern BODY_TYPE_STATIC    = #const CP_BODY_TYPE_STATIC

type BodyVelocityFunc =
          Ptr Body -- ^ body
       -> Vect     -- ^ gravity
       -> Float    -- ^ damping
       -> Float    -- ^ dt
       -> IO ()

type BodyPositionFunc =
          Ptr Body -- ^ body
       -> Float    -- ^ dt
       -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpBodyAlloc"
  bodyAlloc
    :: IO (Ptr Body)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpBodyInit"
  bodyInit
    :: Ptr Body      -- ^ body
    -> Float         -- ^ mass
    -> Float         -- ^ moment
    -> IO (Ptr Body)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpBodyNew"
  bodyNew
    :: Float         -- ^ mass
    -> Float         -- ^ moment
    -> IO (Ptr Body)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpBodyNewKinematic"
  bodyNewKinematic
    :: IO (Ptr Body)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpBodyNewStatic"
  bodyNewStatic
    :: IO (Ptr Body)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpBodyDestroy"
  bodyDestroy
    :: Ptr Body -- ^ body
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpBodyFree"
  bodyFree
    :: Ptr Body -- ^ body
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpBodyActivate"
  bodyActivate
    :: Ptr Body -- ^ body
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpBodyActivateStatic"
  bodyActivateStatic
    :: Ptr Body  -- ^ body
    -> Ptr Shape -- ^ filter
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpBodySleep"
  bodySleep
    :: Ptr Body -- ^ body
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpBodySleepWithGroup"
  bodySleepWithGroup
    :: Ptr Body -- ^ body
    -> Ptr Body -- ^ group
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpBodyIsSleeping"
  bodyIsSleeping
    :: Ptr Body -- ^ body
    -> IO Bool

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpBodyGetType"
  bodyGetType
    :: Ptr Body    -- ^ body
    -> IO BodyType

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpBodySetType"
  bodySetType
    :: Ptr Body -- ^ body
    -> BodyType -- ^ type
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpBodyGetSpace"
  bodyGetSpace
    :: Ptr Body       -- ^ body
    -> IO (Ptr Space)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpBodyGetMass"
  bodyGetMass
    :: Ptr Body -- ^ body
    -> IO Float

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpBodySetMass"
  bodySetMass
    :: Ptr Body -- ^ body
    -> Float    -- ^ m
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpBodyGetMoment"
  bodyGetMoment
    :: Ptr Body -- ^ body
    -> IO Float

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpBodySetMoment"
  bodySetMoment
    :: Ptr Body -- ^ body
    -> Float    -- ^ i
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpBodyGetPosition"
  bodyGetPosition
    :: Ptr Body -- ^ body
    -> Ptr Vect -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpBodySetPosition"
  bodySetPosition
    :: Ptr Body   -- ^ body
    -> Ptr cpVect -- ^ pos
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpBodyGetCenterOfGravity"
  bodyGetCenterOfGravity
    :: Ptr Body -- ^ body
    -> Ptr Vect -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpBodySetCenterOfGravity"
  bodySetCenterOfGravity
    :: Ptr Body -- ^ body
    -> Ptr Vect -- ^ cog
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpBodyGetVelocity"
  bodyGetVelocity
    :: Ptr Body -- ^ body
    -> Ptr Vect -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpBodySetVelocity"
  bodySetVelocity
    :: Ptr Body -- ^ body
    -> Ptr Vect -- ^ velocity
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpBodyGetForce"
  bodyGetForce
    :: Ptr Body -- ^ body
    -> Ptr Vect -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpBodySetForce"
  bodySetForce
    :: Ptr Body -- ^ body
    -> Ptr Vect -- ^ force
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpBodyGetAngle"
  bodyGetAngle
    :: Ptr Body -- ^ body
    -> IO Float

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpBodySetAngle"
  bodySetAngle
    :: Ptr Body -- ^ body
    -> Float    -- ^ a
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpBodyGetAngularVelocity"
  bodyGetAngularVelocity
    :: Ptr Body -- ^ body
    -> IO Float

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpBodySetAngularVelocity"
  bodySetAngularVelocity
    :: Ptr Body -- ^ body
    -> Float    -- ^ angularVelocity
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpBodyGetTorque"
  bodyGetTorque
    :: Ptr Body -- ^ body
    -> IO Float

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpBodySetTorque"
  bodySetTorque
    :: Ptr Body -- ^ body
    -> Float    -- ^ torque
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpBodyGetRotation"
  bodyGetRotation
    :: Ptr Body -- ^ body
    -> Ptr Vect -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpBodyGetUserData"
  bodyGetUserData
    :: Ptr Body -- ^ body
    -> IO DataPointer

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpBodySetUserData"
  bodySetUserData
    :: Ptr Body    -- ^ body
    -> DataPointer -- ^ userData
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpBodySetVelocityUpdateFunc"
  bodySetVelocityUpdateFunc
    :: Ptr Body                -- ^ body
    -> FunPtr BodyVelocityFunc -- ^ velocityFunc
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpBodySetPositionUpdateFunc"
  bodySetPositionUpdateFunc
    :: Ptr Body                -- ^ body
    -> FunPtr BodyPositionFunc -- ^ positionFunc
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpBodyUpdateVelocity"
  bodyUpdateVelocity
    :: Ptr Body -- ^ body
    -> Ptr Vect -- ^ gravity
    -> Float    -- ^ damping
    -> Float    -- ^ dt
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpBodyUpdatePosition"
  bodyUpdatePosition
    :: Ptr Body -- ^ body
    -> Float    -- ^ dt
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpBodyLocalToWorld"
  bodyLocalToWorld
    :: Ptr Body      -- ^ body
    -> Ptr Vect      -- ^ point
    -> Ptr Vect      -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpBodyWorldToLocal"
  bodyWorldToLocal
    :: Ptr Body        -- ^ body
    -> Ptr Vect        -- ^ point
    -> Ptr Vect      -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpBodyApplyForceAtWorldPoint"
  bodyApplyForceAtWorldPoint
    :: Ptr Body -- ^ body
    -> Ptr Vect -- ^ force
    -> Ptr Vect -- ^ point
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpBodyApplyForceAtLocalPoint"
  bodyApplyForceAtLocalPoint
    :: Ptr Body -- ^ body
    -> Ptr Vect -- ^ force
    -> Ptr Vect -- ^ point
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpBodyApplyImpulseAtWorldPoint"
  bodyApplyImpulseAtWorldPoint
    :: Ptr Body -- ^ body
    -> Ptr Vect -- ^ impulse
    -> Ptr Vect -- ^ point
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpBodyApplyImpulseAtLocalPoint"
  bodyApplyImpulseAtLocalPoint
    :: Ptr Body -- ^ body
    -> Ptr Vect -- ^ impulse
    -> Ptr Vect -- ^ point
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpBodyGetVelocityAtWorldPoint"
  bodyGetVelocityAtWorldPoint
    :: Ptr Body -- ^ body
    -> Ptr Vect -- ^ point
    -> Ptr Vect -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpBodyGetVelocityAtLocalPoint"
  bodyGetVelocityAtLocalPoint
    :: Ptr Body -- ^ body
    -> Ptr Vect -- ^ point
    -> Ptr Vect -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpBodyKineticEnergy"
  bodyKineticEnergy
    :: Ptr Body -- ^ body
    -> IO Float

type BodyShapeIteratorFunc =
          Ptr Body  -- ^ body
       -> Ptr Shape -- ^ shape
       -> Ptr ()    -- ^ data
       -> IO ()

foreign import CALLCV "chipmunk/chipmunk.h cpBodyEachShape"
  bodyEachShape
    :: Ptr Body                     -- ^ body
    -> FunPtr BodyShapeIteratorFunc -- ^ func
    -> Ptr ()                       -- ^ data
    -> IO ()

type BodyConstraintIteratorFunc =
          Ptr Body       -- ^ body
       -> Ptr Constraint -- ^ constraint
       -> Ptr ()         -- ^ data
       -> IO ()

foreign import CALLCV "chipmunk/chipmunk.h cpBodyEachConstraint"
  bodyEachConstraint
    :: Ptr Body                          -- ^ body
    -> FunPtr BodyConstraintIteratorFunc -- ^ func
    -> Ptr ()                            -- ^ data
    -> IO ()

type BodyArbiterIteratorFunc =
          Ptr Body    -- ^ body
       -> Ptr Arbiter -- ^ arbiter
       -> Ptr ()      -- ^ data
       -> IO ()

foreign import CALLCV "chipmunk/chipmunk.h cpBodyEachArbiter"
  bodyEachArbiter
    :: Ptr Body                       -- ^ body
    -> FunPtr BodyArbiterIteratorFunc -- ^ func
    -> Ptr ()                         -- ^ data
    -> IO ()

-- end of cpBody.h

-- * cpShape.h

data {-# CTYPE "chipmunk/chipmunk.h" "cpPointQueryInfo" #-} PointQueryInfo =
       PointQueryInfo
         { shape    :: Ptr Shape
         , point    :: Vect
         , distance :: Float
         , gradient :: Vect
         }

instance Offset "shape"    PointQueryInfo where rawOffset = #offset struct cpPointQueryInfo, shape
instance Offset "point"    PointQueryInfo where rawOffset = #offset struct cpPointQueryInfo, point
instance Offset "distance" PointQueryInfo where rawOffset = #offset struct cpPointQueryInfo, distance
instance Offset "gradient" PointQueryInfo where rawOffset = #offset struct cpPointQueryInfo, gradient

instance Storable PointQueryInfo where
  sizeOf _    = #size struct cpPointQueryInfo
  alignment _ = #alignment struct cpPointQueryInfo

  peek ptr =
    PointQueryInfo
      <$> peek (offset @"shape"    ptr)
      <*> peek (offset @"point"    ptr)
      <*> peek (offset @"distance" ptr)
      <*> peek (offset @"gradient" ptr)

  poke ptr val = do
    pokeField @"shape"    ptr val
    pokeField @"point"    ptr val
    pokeField @"distance" ptr val
    pokeField @"gradient" ptr val

data {-# CTYPE "chipmunk/chipmunk.h" "cpSegmentQueryInfo" #-} SegmentQueryInfo =
       SegmentQueryInfo
         { shape  :: Ptr Shape
         , point  :: Vect
         , normal :: Vect
         , alpha  :: Float
         }

instance Offset "shape"  SegmentQueryInfo where rawOffset = #offset struct cpSegmentQueryInfo, shape
instance Offset "point"  SegmentQueryInfo where rawOffset = #offset struct cpSegmentQueryInfo, point
instance Offset "normal" SegmentQueryInfo where rawOffset = #offset struct cpSegmentQueryInfo, normal
instance Offset "alpha"  SegmentQueryInfo where rawOffset = #offset struct cpSegmentQueryInfo, alpha

instance Storable SegmentQueryInfo where
  sizeOf _    = #size struct cpSegmentQueryInfo
  alignment _ = #alignment struct cpSegmentQueryInfo

  peek ptr =
    SegmentQueryInfo
      <$> peek (offset @"shape"  ptr)
      <*> peek (offset @"point"  ptr)
      <*> peek (offset @"normal" ptr)
      <*> peek (offset @"alpha"  ptr)

  poke ptr val = do
    pokeField @"shape"  ptr val
    pokeField @"point"  ptr val
    pokeField @"normal" ptr val
    pokeField @"alpha"  ptr val

data {-# CTYPE "chipmunk/chipmunk.h" "cpShapeFilter" #-} ShapeFilter =
       ShapeFilter
         { group      :: Group
         , categories :: Bitmask
         , mask       :: Bitmask
         }

instance Offset "group"      ShapeFilter where rawOffset = #offset struct cpShapeFilter, group
instance Offset "categories" ShapeFilter where rawOffset = #offset struct cpShapeFilter, categories
instance Offset "mask"       ShapeFilter where rawOffset = #offset struct cpShapeFilter, mask

instance Storable ShapeFilter where
  sizeOf _    = #size struct cpShapeFilter
  alignment _ = #alignment struct cpShapeFilter

  peek ptr =
    ShapeFilter
      <$> peek (offset @"group"      ptr)
      <*> peek (offset @"categories" ptr)
      <*> peek (offset @"mask"       ptr)

  poke ptr val = do
    pokeField @"group"      ptr val
    pokeField @"categories" ptr val
    pokeField @"mask"       ptr val

foreign import CALLCV unsafe "wrapper.h w_cp_shape_filter_all"
  shape_filter_all
    :: Ptr ShapeFilter -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cp_shape_filter_none"
  shape_filter_none
    :: Ptr ShapeFilter -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpShapeDestroy"
  shapeDestroy
    :: Ptr Shape -- ^ shape
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpShapeFree"
  shapeFree
    :: Ptr Shape -- ^ shape
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpShapeCacheBB"
  shapeCacheBB
    :: Ptr Shape -- ^ shape
    -> Ptr BB    -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpShapeUpdate"
  shapeUpdate
    :: Ptr Shape     -- ^ shape
    -> Ptr Transform -- ^ transform
    -> Ptr BB        -- ^ Output value
    -> IO ()

foreign import CALLCV "wrapper.h w_cpShapePointQuery"
  shapePointQuery
    :: Ptr Shape          -- ^ shape
    -> Ptr Vect           -- ^ p
    -> Ptr PointQueryInfo -- ^ out
    -> IO Float

foreign import CALLCV "wrapper.h w_cpShapeSegmentQuery"
  shapeSegmentQuery
    :: Ptr Shape            -- ^ shape
    -> Ptr Vect             -- ^ a
    -> Ptr Vect             -- ^ b
    -> Float                -- ^ radius
    -> Ptr SegmentQueryInfo -- ^ info
    -> IO Bool

foreign import CALLCV unsafe "wrapper.h w_cpShapesCollide"
  shapesCollide
    :: Ptr Shape           -- ^ a
    -> Ptr Shape           -- ^ b
    -> Ptr ContactPointSet -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpShapeGetSpace"
  shapeGetSpace
    :: Ptr Shape      -- ^ shape
    -> IO (Ptr Space)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpShapeGetBody"
  shapeGetBody
    :: Ptr Shape     -- ^ shape
    -> IO (Ptr Body)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpShapeSetBody"
  shapeSetBody
    :: Ptr Shape -- ^ shape
    -> Ptr Body  -- ^ body
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpShapeGetMass"
  shapeGetMass
    :: Ptr Shape -- ^ shape
    -> IO Float

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpShapeSetMass"
  shapeSetMass
    :: Ptr Shape -- ^ shape
    -> Float     -- ^ mass
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpShapeGetDensity"
  shapeGetDensity
    :: Ptr Shape -- ^ shape
    -> IO Float

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpShapeSetDensity"
  shapeSetDensity
    :: Ptr Shape -- ^ shape
    -> Float     -- ^ density
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpShapeGetMoment"
  shapeGetMoment
    :: Ptr Shape -- ^ shape
    -> IO Float

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpShapeGetArea"
  shapeGetArea
    :: Ptr Shape -- ^ shape
    -> IO Float

foreign import CALLCV unsafe "wrapper.h w_cpShapeGetCenterOfGravity"
  shapeGetCenterOfGravity
    :: Ptr Shape -- ^ shape
    -> Ptr Vect  -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpShapeGetBB"
  shapeGetBB
    :: Ptr Shape -- ^ shape
    -> Ptr BB
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpShapeGetSensor"
  shapeGetSensor
    :: Ptr Shape -- ^ shape
    -> IO Bool

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpShapeSetSensor"
  shapeSetSensor
    :: Ptr Shape -- ^ shape
    -> Bool      -- ^ sensor
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpShapeGetElasticity"
  shapeGetElasticity
    :: Ptr Shape -- ^ shape
    -> IO Float

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpShapeSetElasticity"
  shapeSetElasticity
    :: Ptr Shape -- ^ shape
    -> Float     -- ^ elasticity
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpShapeGetFriction"
  shapeGetFriction
    :: Ptr Shape -- ^ shape
    -> IO Float

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpShapeSetFriction"
  shapeSetFriction
    :: Ptr Shape -- ^ shape
    -> Float     -- ^ friction
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpShapeGetSurfaceVelocity"
  shapeGetSurfaceVelocity
    :: Ptr Shape -- ^ shape
    -> Ptr Vect  -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpShapeSetSurfaceVelocity"
  shapeSetSurfaceVelocity
    :: Ptr Shape -- ^ shape
    -> Ptr Vect  -- ^ surfaceVelocity
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpShapeGetUserData"
  shapeGetUserData
    :: Ptr Shape -- ^ shape
    -> IO DataPointer

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpShapeSetUserData"
  shapeSetUserData
    :: Ptr Shape   -- ^ shape
    -> DataPointer -- ^ userData
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpShapeGetCollisionType"
  shapeGetCollisionType
    :: Ptr Shape -- ^ shape
    -> IO CollisionType

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpShapeSetCollisionType"
  shapeSetCollisionType
    :: Ptr Shape     -- ^ shape
    -> CollisionType -- ^ collisionType
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpShapeGetFilter"
  shapeGetFilter
    :: Ptr Shape       -- ^ shape
    -> Ptr ShapeFilter -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpShapeSetFilter"
  shapeSetFilter
    :: Ptr Shape       -- ^ shape
    -> Ptr ShapeFilter -- ^ filter
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpCircleShapeAlloc"
  circleShapeAlloc
    :: IO (Ptr CircleShape)

foreign import CALLCV unsafe "wrapper.h w_cpCircleShapeInit"
  circleShapeInit
    :: Ptr CircleShape      -- ^ circle
    -> Ptr Body             -- ^ body
    -> Float                -- ^ radius
    -> Ptr Vect             -- ^ offset
    -> IO (Ptr CircleShape)

foreign import CALLCV unsafe "wrapper.h w_cpCircleShapeNew"
  circleShapeNew
    :: Ptr Body       -- ^ body
    -> Float          -- ^ radius
    -> Ptr Vect       -- ^ offset
    -> IO (Ptr Shape)

foreign import CALLCV unsafe "wrapper.h w_cpCircleShapeGetOffset"
  circleShapeGetOffset
    :: Ptr Shape -- ^ shape
    -> Ptr Vect  -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpCircleShapeGetRadius"
  circleShapeGetRadius
    :: Ptr Shape -- ^ shape
    -> IO Float

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSegmentShapeAlloc"
  segmentShapeAlloc
    :: IO (Ptr SegmentShape)

foreign import CALLCV unsafe "wrapper.h w_cpSegmentShapeInit"
  segmentShapeInit
    :: Ptr SegmentShape      -- ^ seg
    -> Ptr Body              -- ^ body
    -> Ptr Vect              -- ^ a
    -> Ptr Vect              -- ^ b
    -> Float                 -- ^ radius
    -> IO (Ptr SegmentShape)

foreign import CALLCV unsafe "wrapper.h w_cpSegmentShapeNew"
  segmentShapeNew
    :: Ptr Body       -- ^ body
    -> Ptr Vect       -- ^ a
    -> Ptr Vect       -- ^ b
    -> Float          -- ^ radius
    -> IO (Ptr Shape)

foreign import CALLCV unsafe "wrapper.h w_cpSegmentShapeSetNeighbors"
  segmentShapeSetNeighbors
    :: Ptr Shape -- ^ shape
    -> Ptr Vect  -- ^ prev
    -> Ptr Vect  -- ^ next
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpSegmentShapeGetA"
  segmentShapeGetA
    :: Ptr Shape -- ^ shape
    -> Ptr Vect  -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpSegmentShapeGetB"
  segmentShapeGetB
    :: Ptr Shape -- ^ shape
    -> Ptr Vect  -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpSegmentShapeGetNormal"
  segmentShapeGetNormal
    :: Ptr Shape -- ^ shape
    -> Ptr Vect  -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSegmentShapeGetRadius"
  segmentShapeGetRadius
    :: Ptr Shape -- ^ shape
    -> IO Float

-- end of cpShape.h

-- * cpPolyShape.h

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpPolyShapeAlloc"
  polyShapeAlloc
    :: IO (Ptr PolyShape)

foreign import CALLCV unsafe "wrapper.h w_cpPolyShapeInit"
  polyShapeInit
    :: Ptr PolyShape      -- ^ poly
    -> Ptr Body           -- ^ body
    -> #{type int}        -- ^ count
    -> Ptr Vect           -- ^ verts
    -> Ptr Transform      -- ^ transform
    -> Float              -- ^ radius
    -> IO (Ptr PolyShape)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpPolyShapeInitRaw"
  polyShapeInitRaw
    :: Ptr PolyShape      -- ^ poly
    -> Ptr Body           -- ^ body
    -> #{type int}        -- ^ count
    -> Ptr Vect           -- ^ verts
    -> Float              -- ^ radius
    -> IO (Ptr PolyShape)

foreign import CALLCV unsafe "wrapper.h w_cpPolyShapeNew"
  polyShapeNew
    :: Ptr Body       -- ^ body
    -> #{type int}    -- ^ count
    -> Ptr Vect       -- ^ verts
    -> Ptr Transform  -- ^ transform
    -> Float          -- ^ radius
    -> IO (Ptr Shape)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpPolyShapeNewRaw"
  polyShapeNewRaw
    :: Ptr Body       -- ^ body
    -> #{type int}    -- ^ count
    -> Ptr Vect       -- ^ verts
    -> Float          -- ^ radius
    -> IO (Ptr Shape)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpBoxShapeInit"
  boxShapeInit
    :: Ptr PolyShape      -- ^ poly
    -> Ptr Body           -- ^ body
    -> Float              -- ^ width
    -> Float              -- ^ height
    -> Float              -- ^ radius
    -> IO (Ptr PolyShape)

foreign import CALLCV unsafe "wrapper.h w_cpBoxShapeInit2"
  boxShapeInit2
    :: Ptr PolyShape      -- ^ poly
    -> Ptr Body           -- ^ body
    -> Ptr BB             -- ^ box
    -> Float              -- ^ radius
    -> IO (Ptr PolyShape)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpBoxShapeNew"
  boxShapeNew
    :: Ptr Body       -- ^ body
    -> Float          -- ^ width
    -> Float          -- ^ height
    -> Float          -- ^ radius
    -> IO (Ptr Shape)

foreign import CALLCV unsafe "wrapper.h w_cpBoxShapeNew2"
  boxShapeNew2
    :: Ptr Body       -- ^ body
    -> Ptr BB         -- ^ box
    -> Float          -- ^ radius
    -> IO (Ptr Shape)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpPolyShapeGetCount"
  polyShapeGetCount
    :: Ptr Shape    -- ^ shape
    -> IO #type int

foreign import CALLCV unsafe "wrapper.h w_cpPolyShapeGetVert"
  polyShapeGetVert
    :: Ptr Shape   -- ^ shape
    -> #{type int} -- ^ index
    -> Ptr Vect    -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpPolyShapeGetRadius"
  polyShapeGetRadius
    :: Ptr Shape -- ^ shape
    -> IO Float

-- end of cpPolyShape.h

-- * cpConstraint.h

type ConstraintPreSolveFunc =
          Ptr Constraint -- ^ constraint
       -> Ptr Space      -- ^ space
       -> IO ()

type ConstraintPostSolveFunc =
          Ptr Constraint -- ^ constraint
       -> Ptr Space      -- ^ space
       -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpConstraintDestroy"
  constraintDestroy
    :: Ptr Constraint -- ^ constraint
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpConstraintFree"
  constraintFree
    :: Ptr Constraint -- ^ constraint
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpConstraintGetSpace"
  constraintGetSpace
    :: Ptr Constraint -- ^ constraint
    -> IO (Ptr Space)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpConstraintGetBodyA"
  constraintGetBodyA
    :: Ptr Constraint -- ^ constraint
    -> IO (Ptr Body)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpConstraintGetBodyB"
  constraintGetBodyB
    :: Ptr Constraint -- ^ constraint
    -> IO (Ptr Body)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpConstraintGetMaxForce"
  constraintGetMaxForce
    :: Ptr Constraint -- ^ constraint
    -> IO Float

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpConstraintSetMaxForce"
  constraintSetMaxForce
    :: Ptr Constraint -- ^ constraint
    -> Float          -- ^ maxForce
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpConstraintGetErrorBias"
  constraintGetErrorBias
    :: Ptr Constraint -- ^ constraint
    -> IO Float

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpConstraintSetErrorBias"
  constraintSetErrorBias
    :: Ptr Constraint -- ^ constraint
    -> Float          -- ^ errorBias
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpConstraintGetMaxBias"
  constraintGetMaxBias
    :: Ptr Constraint -- ^ constraint
    -> IO Float

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpConstraintSetMaxBias"
  constraintSetMaxBias
    :: Ptr Constraint -- ^ constraint
    -> Float          -- ^ maxBias
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpConstraintGetCollideBodies"
  constraintGetCollideBodies
    :: Ptr Constraint -- ^ constraint
    -> IO Bool

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpConstraintSetCollideBodies"
  constraintSetCollideBodies
    :: Ptr Constraint -- ^ constraint
    -> Bool           -- ^ collideBodies
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpConstraintGetPreSolveFunc"
  constraintGetPreSolveFunc
    :: Ptr Constraint                     -- ^ constraint
    -> IO (FunPtr ConstraintPreSolveFunc)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpConstraintSetPreSolveFunc"
  constraintSetPreSolveFunc
    :: Ptr Constraint                -- ^ constraint
    -> FunPtr ConstraintPreSolveFunc -- ^ preSolveFunc
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpConstraintGetPostSolveFunc"
  constraintGetPostSolveFunc
    :: Ptr Constraint                      -- ^ constraint
    -> IO (FunPtr ConstraintPostSolveFunc)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpConstraintSetPostSolveFunc"
  constraintSetPostSolveFunc
    :: Ptr Constraint                 -- ^ constraint
    -> FunPtr ConstraintPostSolveFunc -- ^ postSolveFunc
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpConstraintGetUserData"
  constraintGetUserData
    :: Ptr Constraint -- ^ constraint
    -> IO DataPointer

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpConstraintSetUserData"
  constraintSetUserData
    :: Ptr Constraint -- ^ constraint
    -> DataPointer    -- ^ userData
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpConstraintGetImpulse"
  constraintGetImpulse
    :: Ptr Constraint -- ^ constraint
    -> IO Float

-- ** cpPinJoint.h

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpConstraintIsPinJoint"
  constraintIsPinJoint
    :: Ptr Constraint -- ^ constraint
    -> IO Bool

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpPinJointAlloc"
  pinJointAlloc
    :: IO (Ptr PinJoint)

foreign import CALLCV unsafe "wrapper.h w_cpPinJointInit"
  pinJointInit
    :: Ptr PinJoint      -- ^ joint
    -> Ptr Body          -- ^ a
    -> Ptr Body          -- ^ b
    -> Ptr Vect          -- ^ anchorA
    -> Ptr Vect          -- ^ anchorB
    -> IO (Ptr PinJoint)

foreign import CALLCV unsafe "wrapper.h w_cpPinJointNew"
  pinJointNew
    :: Ptr Body            -- ^ a
    -> Ptr Body            -- ^ b
    -> Ptr Vect            -- ^ anchorA
    -> Ptr Vect            -- ^ anchorB
    -> IO (Ptr Constraint)

foreign import CALLCV unsafe "wrapper.h w_cpPinJointGetAnchorA"
  pinJointGetAnchorA
    :: Ptr Constraint -- ^ constraint
    -> Ptr Vect       -- ^ Output value
    -> IO ()
foreign import CALLCV unsafe "wrapper.h w_cpPinJointSetAnchorA"
  pinJointSetAnchorA
    :: Ptr Constraint -- ^ constraint
    -> Ptr Vect       -- ^ anchorA
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpPinJointGetAnchorB"
  pinJointGetAnchorB
    :: Ptr Constraint -- ^ constraint
    -> Ptr Vect       -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpPinJointSetAnchorB"
  pinJointSetAnchorB
    :: Ptr Constraint -- ^ constraint
    -> Ptr Vect       -- ^ anchorB
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpPinJointGetDist"
  pinJointGetDist
    :: Ptr Constraint -- ^ constraint
    -> IO Float

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpPinJointSetDist"
  pinJointSetDist
    :: Ptr Constraint -- ^ constraint
    -> Float          -- ^ dist
    -> IO ()

-- end of cpPinJoint.h

-- ** cpSlideJoint.h

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpConstraintIsSlideJoint"
  constraintIsSlideJoint
    :: Ptr Constraint -- ^ constraint
    -> IO Bool

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSlideJointAlloc"
  slideJointAlloc
    :: IO (Ptr SlideJoint)

foreign import CALLCV unsafe "wrapper.h w_cpSlideJointInit"
  slideJointInit
    :: Ptr SlideJoint      -- ^ joint
    -> Ptr Body            -- ^ a
    -> Ptr Body            -- ^ b
    -> Ptr Vect            -- ^ anchorA
    -> Ptr Vect            -- ^ anchorB
    -> Float               -- ^ min
    -> Float               -- ^ max
    -> IO (Ptr SlideJoint)

foreign import CALLCV unsafe "wrapper.h w_cpSlideJointNew"
  slideJointNew
    :: Ptr Body            -- ^ a
    -> Ptr Body            -- ^ b
    -> Ptr Vect            -- ^ anchorA
    -> Ptr Vect            -- ^ anchorB
    -> Float               -- ^ min
    -> Float               -- ^ max
    -> IO (Ptr Constraint)

foreign import CALLCV unsafe "wrapper.h w_cpSlideJointGetAnchorA"
  slideJointGetAnchorA
    :: Ptr Constraint -- ^ constraint
    -> Ptr Vect
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpSlideJointSetAnchorA"
  slideJointSetAnchorA
    :: Ptr Constraint -- ^ constraint
    -> Ptr Vect       -- ^ anchorA
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpSlideJointGetAnchorB"
  slideJointGetAnchorB
    :: Ptr Constraint -- ^ constraint
    -> Ptr Vect       -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpSlideJointSetAnchorB"
  slideJointSetAnchorB
    :: Ptr Constraint -- ^ constraint
    -> Ptr Vect       -- ^ anchorB
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSlideJointGetMin"
  slideJointGetMin
    :: Ptr Constraint -- ^ constraint
    -> IO Float

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSlideJointSetMin"
  slideJointSetMin
    :: Ptr Constraint -- ^ constraint
    -> Float          -- ^ min
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSlideJointGetMax"
  slideJointGetMax
    :: Ptr Constraint -- ^ constraint
    -> IO Float

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSlideJointSetMax"
  slideJointSetMax
    :: Ptr Constraint -- ^ constraint
    -> Float          -- ^ max
    -> IO ()

-- end of cpSlideJoint.h

-- ** cpPivotJoint.h

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpConstraintIsPivotJoint"
  constraintIsPivotJoint
    :: Ptr Constraint -- ^ constraint
    -> IO Bool

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpPivotJointAlloc"
  pivotJointAlloc
    :: IO (Ptr PivotJoint)

foreign import CALLCV unsafe "wrapper.h w_cpPivotJointInit"
  pivotJointInit
    :: Ptr PivotJoint      -- ^ joint
    -> Ptr Body            -- ^ a
    -> Ptr Body            -- ^ b
    -> Ptr Vect            -- ^ anchorA
    -> Ptr Vect            -- ^ anchorB
    -> IO (Ptr PivotJoint)

foreign import CALLCV unsafe "wrapper.h w_cpPivotJointNew"
  pivotJointNew
    :: Ptr Body            -- ^ a
    -> Ptr Body            -- ^ b
    -> Ptr Vect            -- ^ pivot
    -> IO (Ptr Constraint)

foreign import CALLCV unsafe "wrapper.h w_cpPivotJointNew2"
  pivotJointNew2
    :: Ptr Body            -- ^ a
    -> Ptr Body            -- ^ b
    -> Ptr Vect            -- ^ anchorA
    -> Ptr Vect            -- ^ anchorB
    -> IO (Ptr Constraint)

foreign import CALLCV unsafe "wrapper.h w_cpPivotJointGetAnchorA"
  pivotJointGetAnchorA
    :: Ptr Constraint -- ^ constraint
    -> Ptr Vect
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpPivotJointSetAnchorA"
  pivotJointSetAnchorA
    :: Ptr Constraint -- ^ constraint
    -> Ptr Vect       -- ^ anchorA
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpPivotJointGetAnchorB"
  pivotJointGetAnchorB
    :: Ptr Constraint -- ^ constraint
    -> Ptr Vect       -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpPivotJointSetAnchorB"
  pivotJointSetAnchorB
    :: Ptr Constraint -- ^ constraint
    -> Ptr Vect       -- ^ anchorB
    -> IO ()

-- end of cpPivotJoint.h

-- ** cpGrooveJoint.h

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpConstraintIsGrooveJoint"
  constraintIsGrooveJoint
    :: Ptr Constraint -- ^ constraint
    -> IO Bool

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpGrooveJointAlloc"
  grooveJointAlloc
    :: IO (Ptr GrooveJoint)

foreign import CALLCV unsafe "wrapper.h w_cpGrooveJointInit"
  grooveJointInit
    :: Ptr GrooveJoint      -- ^ joint
    -> Ptr Body             -- ^ a
    -> Ptr Body             -- ^ b
    -> Ptr Vect             -- ^ groove_a
    -> Ptr Vect             -- ^ groove_b
    -> Ptr Vect             -- ^ anchorB
    -> IO (Ptr GrooveJoint)

foreign import CALLCV unsafe "wrapper.h w_cpGrooveJointNew"
  grooveJointNew
    :: Ptr Body            -- ^ a
    -> Ptr Body            -- ^ b
    -> Ptr Vect            -- ^ groove_a
    -> Ptr Vect            -- ^ groove_b
    -> Ptr Vect            -- ^ anchorB
    -> IO (Ptr Constraint)

foreign import CALLCV unsafe "wrapper.h w_cpGrooveJointGetGrooveA"
  grooveJointGetGrooveA
    :: Ptr Constraint -- ^ constraint
    -> Ptr Vect
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpGrooveJointSetGrooveA"
  grooveJointSetGrooveA
    :: Ptr Constraint -- ^ constraint
    -> Ptr Vect       -- ^ grooveB
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpGrooveJointGetGrooveB"
  grooveJointGetGrooveB
    :: Ptr Constraint -- ^ constraint
    -> Ptr Vect
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpGrooveJointSetGrooveB"
  grooveJointSetGrooveB
    :: Ptr Constraint -- ^ constraint
    -> Ptr Vect       -- ^ grooveB
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpGrooveJointGetAnchorB"
  grooveJointGetAnchorB
    :: Ptr Constraint -- ^ constraint
    -> Ptr Vect       -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpGrooveJointSetAnchorB"
  grooveJointSetAnchorB
    :: Ptr Constraint -- ^ constraint
    -> Ptr Vect       -- ^ anchorB
    -> IO ()

-- end of cpGrooveJoint.h

-- ** cpDampedSpring.h

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpConstraintIsDampedSpring"
  constraintIsDampedSpring
    :: Ptr Constraint -- ^ constraint
    -> IO Bool

type DampedSpringForceFunc =
          Ptr Constraint -- ^ spring
       -> Float          -- ^ dist
       -> IO Float

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpDampedSpringAlloc"
  dampedSpringAlloc
    :: IO (Ptr DampedSpring)

foreign import CALLCV unsafe "wrapper.h w_cpDampedSpringInit"
  dampedSpringInit
    :: Ptr DampedSpring      -- ^ joint
    -> Ptr Body              -- ^ a
    -> Ptr Body              -- ^ b
    -> Ptr Vect              -- ^ anchorA
    -> Ptr Vect              -- ^ anchorB
    -> Float                 -- ^ restLength
    -> Float                 -- ^ stiffness
    -> Float                 -- ^ damping
    -> IO (Ptr DampedSpring)

foreign import CALLCV unsafe "wrapper.h w_cpDampedSpringNew"
  dampedSpringNew
    :: Ptr Body            -- ^ a
    -> Ptr Body              -- ^ b
    -> Ptr Vect              -- ^ anchorA
    -> Ptr Vect              -- ^ anchorB
    -> Float                 -- ^ restLength
    -> Float                 -- ^ stiffness
    -> Float                 -- ^ damping
    -> IO (Ptr Constraint)

foreign import CALLCV unsafe "wrapper.h w_cpDampedSpringGetAnchorA"
  dampedSpringGetAnchorA
    :: Ptr Constraint -- ^ constraint
    -> Ptr Vect
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpDampedSpringSetAnchorA"
  dampedSpringSetAnchorA
    :: Ptr Constraint -- ^ constraint
    -> Ptr Vect       -- ^ anchorA
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpDampedSpringGetAnchorB"
  dampedSpringGetAnchorB
    :: Ptr Constraint -- ^ constraint
    -> Ptr Vect       -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpDampedSpringSetAnchorB"
  dampedSpringSetAnchorB
    :: Ptr Constraint -- ^ constraint
    -> Ptr Vect       -- ^ anchorB
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpDampedSpringGetRestLength"
  dampedSpringGetRestLength
    :: Ptr Constraint -- ^ constraint
    -> IO Float

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpDampedSpringSetRestLength"
  dampedSpringSetRestLength
    :: Ptr Constraint -- ^ constraint
    -> Float          -- ^ restLength
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpDampedSpringGetStiffness"
  dampedSpringGetStiffness
    :: Ptr cpConstraint -- ^ constraint
    -> IO Float

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpDampedSpringSetStiffness"
  dampedSpringSetStiffness
    :: Ptr Constraint -- ^ constraint
    -> Float          -- ^ stiffness
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpDampedSpringGetDamping"
  dampedSpringGetDamping
    :: Ptr Constraint -- ^ constraint
    -> IO Float

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpDampedSpringSetDamping"
  dampedSpringSetDamping
    :: Ptr Constraint -- ^ constraint
    -> Float          -- ^ damping
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpDampedSpringGetSpringForceFunc"
  dampedSpringGetSpringForceFunc
    :: Ptr Constraint                    -- ^ constraint
    -> IO (FunPtr DampedSpringForceFunc)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpDampedSpringSetSpringForceFunc"
  dampedSpringSetSpringForceFunc
    :: Ptr Constraint               -- ^ constraint
    -> FunPtr DampedSpringForceFunc -- ^ springForceFunc
    -> IO ()

-- end of cpDampedSpring.h

-- ** cpDampedRotarySpring.h

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpConstraintIsDampedRotarySpring"
  constraintIsDampedRotarySpring
    :: Ptr Constraint -- ^ constraint
    -> IO Bool

type DampedRotarySpringTorqueFunc =
          Ptr Constraint -- ^ spring
       -> Float          -- ^ relativeAngle
       -> IO Float

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpDampedRotarySpringAlloc"
  dampedRotarySpringAlloc
    :: IO (Ptr DampedRotarySpring)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpDampedRotarySpringInit"
  dampedRotarySpringInit
    :: Ptr DampedRotarySpring      -- ^ joint
    -> Ptr Body                    -- ^ a
    -> Ptr Body                    -- ^ b
    -> Float                       -- ^ restAngle
    -> Float                       -- ^ stiffness
    -> Float                       -- ^ damping
    -> IO (Ptr DampedRotarySpring)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpDampedRotarySpringNew"
  dampedRotarySpringNew
    :: Ptr Body            -- ^ a
    -> Ptr Body            -- ^ b
    -> Float               -- ^ restAngle
    -> Float               -- ^ stiffness
    -> Float               -- ^ damping
    -> IO (Ptr Constraint)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpDampedRotarySpringGetRestAngle"
  dampedRotarySpringGetRestAngle
    :: Ptr Constraint -- ^ constraint
    -> IO Float

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpDampedRotarySpringSetRestAngle"
  dampedRotarySpringSetRestAngle
    :: Ptr Constraint -- ^ constraint
    -> Float          -- ^ restAngle
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpDampedRotarySpringGetStiffness"
  dampedRotarySpringGetStiffness
    :: Ptr cpConstraint -- ^ constraint
    -> IO Float

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpDampedRotarySpringSetStiffness"
  dampedRotarySpringSetStiffness
    :: Ptr Constraint -- ^ constraint
    -> Float          -- ^ stiffness
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpDampedRotarySpringGetDamping"
  dampedRotarySpringGetDamping
    :: Ptr Constraint -- ^ constraint
    -> IO Float

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpDampedRotarySpringSetDamping"
  dampedRotarySpringSetDamping
    :: Ptr Constraint -- ^ constraint
    -> Float          -- ^ damping
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpDampedRotarySpringGetSpringTorqueFunc"
  dampedRotarySpringGetSpringTorqueFunc
    :: Ptr Constraint                    -- ^ constraint
    -> IO (FunPtr DampedRotarySpringTorqueFunc)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpDampedRotarySpringSetSpringTorqueFunc"
  dampedRotarySpringSetSpringTorqueFunc
    :: Ptr Constraint                      -- ^ constraint
    -> FunPtr DampedRotarySpringTorqueFunc -- ^ springTorqueFunc
    -> IO ()

-- end of cpDampedRotarySpring.h

-- ** cpRotaryLimitJoint.h

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpConstraintIsRotaryLimitJoint"
  constraintIsRotaryLimitJoint
    :: Ptr Constraint -- ^ constraint
    -> IO Bool

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpRotaryLimitJointAlloc"
  rotaryLimitJointAlloc
    :: IO (Ptr RotaryLimitJoint)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpRotaryLimitJointInit"
  rotaryLimitJointInit
    :: Ptr RotaryLimitJoint      -- ^ joint
    -> Ptr Body                    -- ^ a
    -> Ptr Body                    -- ^ b
    -> Float                       -- ^ min
    -> Float                       -- ^ max
    -> IO (Ptr RotaryLimitJoint)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpRotaryLimitJointNew"
  rotaryLimitJointNew
    :: Ptr Body            -- ^ a
    -> Ptr Body            -- ^ b
    -> Float               -- ^ min
    -> Float               -- ^ max
    -> IO (Ptr Constraint)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpRotaryLimitJointGetMin"
  rotaryLimitJointGetMin
    :: Ptr Constraint -- ^ constraint
    -> IO Float

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpRotaryLimitJointSetMin"
  rotaryLimitJointSetMin
    :: Ptr Constraint -- ^ constraint
    -> Float          -- ^ min
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpRotaryLimitJointGetMax"
  rotaryLimitJointGetMax
    :: Ptr Constraint -- ^ constraint
    -> IO Float

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpRotaryLimitJointSetMax"
  rotaryLimitJointSetMax
    :: Ptr Constraint -- ^ constraint
    -> Float          -- ^ max
    -> IO ()

-- end of cpRotaryLimitJoint.h

-- ** cpRatchetJoint.h

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpConstraintIsRatchetJoint"
  constraintIsRatchetJoint
    :: Ptr Constraint -- ^ constraint
    -> IO Bool

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpRatchetJointAlloc"
  ratchetJointAlloc
    :: IO (Ptr RatchetJoint)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpRatchetJointInit"
  ratchetJointInit
    :: Ptr RatchetJoint      -- ^ joint
    -> Ptr Body                    -- ^ a
    -> Ptr Body                    -- ^ b
    -> Float                       -- ^ phase
    -> Float                       -- ^ ratchet
    -> IO (Ptr RatchetJoint)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpRatchetJointNew"
  ratchetJointNew
    :: Ptr Body            -- ^ a
    -> Ptr Body            -- ^ b
    -> Float               -- ^ phase
    -> Float               -- ^ ratchet
    -> IO (Ptr Constraint)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpRatchetJointGetAngle"
  ratchetJointGetAngle
    :: Ptr Constraint -- ^ constraint
    -> IO Float

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpRatchetJointSetAngle"
  ratchetJointSetAngle
    :: Ptr Constraint -- ^ constraint
    -> Float          -- ^ angle
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpRatchetJointGetPhase"
  ratchetJointGetPhase
    :: Ptr Constraint -- ^ constraint
    -> IO Float

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpRatchetJointSetPhase"
  ratchetJointSetPhase
    :: Ptr Constraint -- ^ constraint
    -> Float          -- ^ phase
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpRatchetJointGetRatchet"
  ratchetJointGetRatchet
    :: Ptr Constraint -- ^ constraint
    -> IO Float

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpRatchetJointSetRatchet"
  ratchetJointSetRatchet
    :: Ptr Constraint -- ^ constraint
    -> Float          -- ^ ratchet
    -> IO ()

-- end of cpRatchetJoint.h

-- ** cpGearJoint.h

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpConstraintIsGearJoint"
  constraintIsGearJoint
    :: Ptr Constraint -- ^ constraint
    -> IO Bool

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpGearJointAlloc"
  gearJointAlloc
    :: IO (Ptr GearJoint)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpGearJointInit"
  gearJointInit
    :: Ptr GearJoint      -- ^ joint
    -> Ptr Body           -- ^ a
    -> Ptr Body           -- ^ b
    -> Float              -- ^ phase
    -> Float              -- ^ ratio
    -> IO (Ptr GearJoint)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpGearJointNew"
  gearJointNew
    :: Ptr Body            -- ^ a
    -> Ptr Body            -- ^ b
    -> Float               -- ^ phase
    -> Float               -- ^ ratio
    -> IO (Ptr Constraint)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpGearJointGetPhase"
  gearJointGetPhase
    :: Ptr Constraint -- ^ constraint
    -> IO Float

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpGearJointSetPhase"
  gearJointSetPhase
    :: Ptr Constraint -- ^ constraint
    -> Float          -- ^ phase
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpGearJointGetRatio"
  gearJointGetRatio
    :: Ptr Constraint -- ^ constraint
    -> IO Float

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpGearJointSetRatio"
  gearJointSetRatio
    :: Ptr Constraint -- ^ constraint
    -> Float          -- ^ ratio
    -> IO ()

-- end of cpGearJoint.h

-- ** cpSimpleMotor.h

--data {-# CTYPE "chipmunk/chipmunk.h" "cpSimpleMotor" #-} SimpleMotor

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpConstraintIsSimpleMotor"
  constraintIsSimpleMotor
    :: Ptr Constraint -- ^ constraint
    -> IO Bool

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSimpleMotorAlloc"
  simpleMotorAlloc
    :: IO (Ptr SimpleMotor)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSimpleMotorInit"
  simpleMotorInit
    :: Ptr SimpleMotor      -- ^ joint
    -> Ptr Body           -- ^ a
    -> Ptr Body           -- ^ b
    -> Float              -- ^ rate
    -> IO (Ptr SimpleMotor)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSimpleMotorNew"
  simpleMotorNew
    :: Ptr Body            -- ^ a
    -> Ptr Body            -- ^ b
    -> Float               -- ^ rate
    -> IO (Ptr Constraint)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSimpleMotorGetRate"
  simpleMotorGetRate
    :: Ptr Constraint -- ^ constraint
    -> IO Float

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSimpleMotorSetRate"
  simpleMotorSetRate
    :: Ptr Constraint -- ^ constraint
    -> Float          -- ^ rate
    -> IO ()

-- end of cpSimpleMotor.h

-- end of cpConstraint.h

-- * cpSpace.h

type CollisionBeginFunc =
          Ptr Arbiter -- ^ arb
       -> Ptr Space   -- ^ space
       -> DataPointer -- ^ userData
       -> IO Bool

type CollisionPreSolveFunc =
          Ptr Arbiter -- ^ arb
       -> Ptr Space   -- ^ space
       -> DataPointer -- ^ userData
       -> IO Bool

type CollisionPostSolveFunc =
          Ptr Arbiter -- ^ arb
       -> Ptr Space   -- ^ space
       -> DataPointer -- ^ userData
       -> IO ()

type CollisionSeparateFunc =
          Ptr Arbiter -- ^ arb
       -> Ptr Space   -- ^ space
       -> DataPointer -- ^ userData
       -> IO ()

data {-# CTYPE "chipmunk/chipmunk.h" "cpCollisionHandler" #-} CollisionHandler =
       CollisionHandler
         { typeA         :: CollisionType
         , typeB         :: CollisionType
         , beginFunc     :: FunPtr CollisionBeginFunc
         , preSolveFunc  :: FunPtr CollisionPreSolveFunc
         , postSolveFunc :: FunPtr CollisionPostSolveFunc
         , separateFunc  :: FunPtr CollisionSeparateFunc
         , userData      :: DataPointer
         }

instance Offset "typeA"         CollisionHandler where rawOffset = #offset struct cpCollisionHandler, typeA
instance Offset "typeB"         CollisionHandler where rawOffset = #offset struct cpCollisionHandler, typeB
instance Offset "beginFunc"     CollisionHandler where rawOffset = #offset struct cpCollisionHandler, beginFunc
instance Offset "preSolveFunc"  CollisionHandler where rawOffset = #offset struct cpCollisionHandler, preSolveFunc
instance Offset "postSolveFunc" CollisionHandler where rawOffset = #offset struct cpCollisionHandler, postSolveFunc
instance Offset "separateFunc"  CollisionHandler where rawOffset = #offset struct cpCollisionHandler, separateFunc
instance Offset "userData"      CollisionHandler where rawOffset = #offset struct cpCollisionHandler, userData

instance Storable CollisionHandler where
  sizeOf _    = #size struct cpCollisionHandler
  alignment _ = #alignment struct cpCollisionHandler

  peek ptr =
    CollisionHandler
      <$> peek (offset @"typeA"         ptr)
      <*> peek (offset @"typeB"         ptr)
      <*> peek (offset @"beginFunc"     ptr)
      <*> peek (offset @"preSolveFunc"  ptr)
      <*> peek (offset @"postSolveFunc" ptr)
      <*> peek (offset @"separateFunc"  ptr)
      <*> peek (offset @"userData"      ptr)

  poke ptr val = do
    pokeField @"typeA"         ptr val
    pokeField @"typeB"         ptr val
    pokeField @"beginFunc"     ptr val
    pokeField @"preSolveFunc"  ptr val
    pokeField @"postSolveFunc" ptr val
    pokeField @"separateFunc"  ptr val
    pokeField @"userData"      ptr val

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSpaceAlloc"
  spaceAlloc
    :: IO (Ptr Space)

foreign import CALLCV "chipmunk/chipmunk.h cpSpaceInit"
  spaceInit
    :: Ptr Space      -- ^ space
    -> IO (Ptr Space)

foreign import CALLCV "chipmunk/chipmunk.h cpSpaceNew"
  spaceNew
    :: IO (Ptr Space)

foreign import CALLCV "chipmunk/chipmunk.h cpSpaceDestroy"
  spaceDestroy
    :: Ptr Space -- ^ space
    -> IO ()

foreign import CALLCV "chipmunk/chipmunk.h cpSpaceFree"
  spaceFree
    :: Ptr Space -- ^ space
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSpaceGetIterations"
  spaceGetIterations
    :: Ptr Space -- ^ space
    -> IO #type int

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSpaceSetIterations"
  spaceSetIterations
    :: Ptr Space   -- ^ space
    -> #{type int} -- ^ iterations
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpSpaceGetGravity"
  spaceGetGravity
    :: Ptr Space -- ^ space
    -> Ptr Vect  -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpSpaceSetGravity"
  spaceSetGravity
    :: Ptr Space -- ^ space
    -> Ptr Vect  -- ^ gravity
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSpaceGetDamping"
  spaceGetDamping
    :: Ptr Space -- ^ space
    -> IO Float

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSpaceSetDamping"
  spaceSetDamping
    :: Ptr Space -- ^ space
    -> Float     -- ^ damping
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSpaceGetIdleSpeedThreshold"
  spaceGetIdleSpeedThreshold
    :: Ptr Space -- ^ space
    -> IO Float

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSpaceSetIdleSpeedThreshold"
  spaceSetIdleSpeedThreshold
    :: Ptr Space -- ^ space
    -> Float     -- ^ idleSpeedThreshold
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSpaceGetSleepTimeThreshold"
  spaceGetSleepTimeThreshold
    :: Ptr Space -- ^ space
    -> IO Float

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSpaceSetSleepTimeThreshold"
  spaceSetSleepTimeThreshold
    :: Ptr Space -- ^ space
    -> Float     -- ^ sleepTimeThreshold
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSpaceGetCollisionSlop"
  spaceGetCollisionSlop
    :: Ptr Space -- ^ space
    -> IO Float

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSpaceSetCollisionSlop"
  spaceSetCollisionSlop
    :: Ptr Space -- ^ space
    -> Float     -- ^ collisionSlop
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSpaceGetCollisionBias"
  spaceGetCollisionBias
    :: Ptr Space -- ^ space
    -> IO Float

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSpaceSetCollisionBias"
  spaceSetCollisionBias
    :: Ptr Space -- ^ space
    -> Float     -- ^ collisionBias
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSpaceGetCollisionPersistence"
  spaceGetCollisionPersistence
    :: Ptr Space -- ^ space
    -> IO Timestamp

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSpaceSetCollisionPersistence"
  spaceSetCollisionPersistence
    :: Ptr Space -- ^ space
    -> Timestamp -- ^ collisionPersistence
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSpaceGetUserData"
  spaceGetUserData
    :: Ptr Space -- ^ space
    -> IO DataPointer

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSpaceSetUserData"
  spaceSetUserData
    :: Ptr Space   -- ^ space
    -> DataPointer -- ^ userData
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSpaceGetStaticBody"
  spaceGetStaticBody
    :: Ptr Space -- ^ space
    -> IO (Ptr Body)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSpaceGetCurrentTimeStep"
  spaceGetCurrentTimeStep
    :: Ptr Space -- ^ space
    -> IO Float

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSpaceIsLocked"
  spaceIsLocked
    :: Ptr Space -- ^ space
    -> IO Bool

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSpaceAddDefaultCollisionHandler"
  spaceAddDefaultCollisionHandler
    :: Ptr Space -- ^ space
    -> IO (Ptr CollisionHandler)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSpaceAddCollisionHandler"
  spaceAddCollisionHandler
    :: Ptr Space                 -- ^ space
    -> CollisionType             -- ^ a
    -> CollisionType             -- ^ b
    -> IO (Ptr CollisionHandler)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSpaceAddWildcardHandler"
  spaceAddWildcardHandler
    :: Ptr Space                 -- ^ space
    -> CollisionType             -- ^ type
    -> IO (Ptr CollisionHandler)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSpaceAddShape"
  spaceAddShape
    :: Ptr Space      -- ^ space
    -> Ptr Shape      -- ^ shape
    -> IO (Ptr Shape)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSpaceAddBody"
  spaceAddBody
    :: Ptr Space     -- ^ space
    -> Ptr Body      -- ^ body
    -> IO (Ptr Body)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSpaceAddConstraint"
  spaceAddConstraint
    :: Ptr Space           -- ^ space
    -> Ptr Constraint      -- ^ constraint
    -> IO (Ptr Constraint)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSpaceRemoveShape"
  spaceRemoveShape
    :: Ptr Space -- ^ space
    -> Ptr Shape -- ^ shape
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSpaceRemoveBody"
  spaceRemoveBody
    :: Ptr Space -- ^ space
    -> Ptr Body  -- ^ body
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSpaceRemoveConstraint"
  spaceRemoveConstraint
    :: Ptr Space      -- ^ space
    -> Ptr Constraint -- ^ constraint
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSpaceContainsShape"
  spaceContainsShape
    :: Ptr Space -- ^ space
    -> Ptr Shape -- ^ shape
    -> IO Bool

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSpaceContainsBody"
  spaceContainsBody
    :: Ptr Space -- ^ space
    -> Ptr Body  -- ^ body
    -> IO Bool

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSpaceContainsConstraint"
  spaceContainsConstraint
    :: Ptr Space      -- ^ space
    -> Ptr Constraint -- ^ constraint
    -> IO Bool

type PostStepFunc =
          Ptr Space -- ^ space
       -> Ptr ()    -- ^ key
       -> Ptr ()    -- ^ data
       -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSpaceAddPostStepCallback"
  spaceAddPostStepCallback
    :: Ptr Space           -- ^ space
    -> FunPtr PostStepFunc -- ^ func
    -> Ptr ()              -- ^ key
    -> Ptr ()              -- ^ data
    -> IO Bool

type SpacePointQueryFunc =
          Ptr Shape -- ^ shape
       -> Vect      -- ^ point
       -> Float     -- ^ distance
       -> Vect      -- ^ gradient
       -> Ptr ()    -- ^ data
       -> IO ()

foreign import CALLCV "wrapper.h w_cpSpacePointQuery"
  spacePointQuery
    :: Ptr Space                  -- ^ space
    -> Ptr Vect                   -- ^ point
    -> Float                      -- ^ maxDistance
    -> Ptr ShapeFilter            -- ^ filter
    -> FunPtr SpacePointQueryFunc -- ^ func
    -> Ptr ()                     -- ^ data
    -> IO ()

foreign import CALLCV "wrapper.h w_cpSpacePointQueryNearest"
  spacePointQueryNearest
    :: Ptr Space          -- ^ space
    -> Ptr Vect           -- ^ point
    -> Float              -- ^ maxDistance
    -> Ptr ShapeFilter    -- ^ filter
    -> Ptr PointQueryInfo -- ^ out
    -> IO (Ptr Shape)

type SpaceSegmentQueryFunc =
          Ptr Shape -- ^ shape
       -> Vect      -- ^ point
       -> Vect      -- ^ normal
       -> Float     -- ^ alpha
       -> Ptr ()    -- ^ data
       -> IO ()

foreign import CALLCV "wrapper.h w_cpSpaceSegmentQuery"
  spaceSegmentQuery
    :: Ptr Space                    -- ^ space
    -> Ptr Vect                     -- ^ start
    -> Ptr Vect                     -- ^ end
    -> Float                        -- ^ radius
    -> Ptr ShapeFilter              -- ^ filter
    -> FunPtr SpaceSegmentQueryFunc -- ^ func
    -> Ptr ()                       -- ^ data
    -> IO ()

foreign import CALLCV "wrapper.h w_cpSpaceSegmentQueryFirst"
  spaceSegmentQueryFirst
    :: Ptr Space            -- ^ space
    -> Ptr Vect             -- ^ start
    -> Ptr Vect             -- ^ end
    -> Float                -- ^ radius
    -> Ptr ShapeFilter      -- ^ filter
    -> Ptr SegmentQueryInfo -- ^ out
    -> IO (Ptr Shape)

type SpaceBBQueryFunc =
          Ptr Shape -- ^ shape
       -> Ptr ()    -- ^ data
       -> IO ()

foreign import CALLCV "wrapper.h w_cpSpaceBBQuery"
  spaceBBQuery
    :: Ptr Space               -- ^ space
    -> Ptr BB                  -- ^ bb
    -> Ptr ShapeFilter         -- ^ filter
    -> FunPtr SpaceBBQueryFunc -- ^ func
    -> Ptr ()                  -- ^ data
    -> IO ()

type SpaceShapeQueryFunc =
          Ptr Shape           -- ^ shape
       -> Ptr ContactPointSet -- ^ points
       -> Ptr ()              -- ^ data
       -> IO ()

foreign import CALLCV "chipmunk/chipmunk.h cpSpaceShapeQuery"
  spaceShapeQuery
    :: Ptr Space                  -- ^ space
    -> Ptr Shape                  -- ^ shape
    -> FunPtr SpaceShapeQueryFunc -- ^ func
    -> Ptr ()                     -- ^ data
    -> IO Bool



type SpaceBodyIteratorFunc =
          Ptr Body -- ^ body
       -> Ptr ()   -- ^ data
       -> IO ()

foreign import CALLCV "chipmunk/chipmunk.h cpSpaceEachBody"
  spaceEachBody
    :: Ptr Space                    -- ^ space
    -> FunPtr SpaceBodyIteratorFunc -- ^ func
    -> Ptr ()                       -- ^ data
    -> IO ()

type SpaceShapeIteratorFunc =
          Ptr Shape -- ^ shape
       -> Ptr ()    -- ^ data
       -> IO ()

foreign import CALLCV "chipmunk/chipmunk.h cpSpaceEachShape"
  spaceEachShape
    :: Ptr Space                     -- ^ space
    -> FunPtr SpaceShapeIteratorFunc -- ^ func
    -> Ptr ()                        -- ^ data
    -> IO ()

type SpaceConstraintIteratorFunc =
          Ptr Constraint -- ^ constraint
       -> Ptr ()         -- ^ data
       -> IO ()

foreign import CALLCV "chipmunk/chipmunk.h cpSpaceEachConstraint"
  spaceEachConstraint
    :: Ptr Space                          -- ^ space
    -> FunPtr SpaceConstraintIteratorFunc -- ^ func
    -> Ptr ()                             -- ^ data
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSpaceReindexStatic"
  spaceReindexStatic
    :: Ptr Space -- ^ space
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSpaceReindexShape"
  spaceReindexShape
    :: Ptr Space -- ^ space
    -> Ptr Shape -- ^ shape
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSpaceReindexShapesForBody"
  spaceReindexShapesForBody
    :: Ptr Space -- ^ space
    -> Ptr Body  -- ^ body
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSpaceUseSpatialHash"
  spaceUseSpatialHash
    :: Ptr Space   -- ^ space
    -> Float       -- ^ dim
    -> #{type int} -- ^ count
    -> IO ()

foreign import CALLCV "chipmunk/chipmunk.h cpSpaceStep"
  spaceStep
    :: Ptr Space -- ^ space
    -> Float     -- ^ dt
    -> IO ()

#ifndef CP_SPACE_DISABLE_DEBUG_API

data {-# CTYPE "chipmunk/chipmunk.h" "cpSpaceDebugColor" #-} SpaceDebugColor =
       SpaceDebugColor
         { r :: #type float
         , g :: #type float
         , b :: #type float
         , a :: #type float
         }

instance Offset "r" SpaceDebugColor where rawOffset = #offset struct cpSpaceDebugColor, r
instance Offset "g" SpaceDebugColor where rawOffset = #offset struct cpSpaceDebugColor, g
instance Offset "b" SpaceDebugColor where rawOffset = #offset struct cpSpaceDebugColor, b
instance Offset "a" SpaceDebugColor where rawOffset = #offset struct cpSpaceDebugColor, a

instance Storable SpaceDebugColor where
  sizeOf _ = #size struct cpSpaceDebugColor
  alignment _ = #alignment struct cpSpaceDebugColor

  peek ptr =
    SpaceDebugColor
      <$> peek (offset @"r" ptr)
      <*> peek (offset @"g" ptr)
      <*> peek (offset @"b" ptr)
      <*> peek (offset @"a" ptr)

  poke ptr val = do
    pokeField @"r" ptr val
    pokeField @"g" ptr val
    pokeField @"b" ptr val
    pokeField @"a" ptr val

type SpaceDebugDrawCircleImpl =
          Vect            -- ^ pos
       -> Float           -- ^ angle
       -> Float           -- ^ radius
       -> SpaceDebugColor -- ^ outlineColor
       -> SpaceDebugColor -- ^ fillColor
       -> DataPointer     -- ^ data
       -> IO ()

type SpaceDebugDrawSegmentImpl =
          Vect            -- ^ a
       -> Vect            -- ^ b
       -> SpaceDebugColor -- ^ color
       -> DataPointer     -- ^ data
       -> IO ()

type SpaceDebugDrawFatSegmentImpl =
          Vect            -- ^ a
       -> Vect            -- ^ b
       -> Float           -- ^ radius
       -> SpaceDebugColor -- ^ outlineColor
       -> SpaceDebugColor -- ^ fillColor
       -> DataPointer     -- ^ data
       -> IO ()

type SpaceDebugDrawPolygonImpl =
          #{type int}     -- ^ count
       -> Ptr Vect        -- ^ verts
       -> Float           -- ^ radius
       -> SpaceDebugColor -- ^ outlineColor
       -> SpaceDebugColor -- ^ fillColor
       -> DataPointer     -- ^ data
       -> IO ()

type SpaceDebugDrawDotImpl =
          Float           -- ^ size
       -> Vect            -- ^ pos
       -> SpaceDebugColor -- ^ color
       -> DataPointer     -- ^ data
       -> IO ()

type SpaceDebugDrawColorForShapeImpl =
          Ptr Shape          -- ^ shape
       -> DataPointer        -- ^ data
       -> IO SpaceDebugColor

type SpaceDebugDrawFlags = #type cpSpaceDebugDrawFlags

pattern SPACE_DEBUG_DRAW_SHAPES
      , SPACE_DEBUG_DRAW_CONSTRAINTS
      , SPACE_DEBUG_DRAW_COLLISION_POINTS
     :: (Eq a, Num a) => a
pattern SPACE_DEBUG_DRAW_SHAPES           = #const CP_SPACE_DEBUG_DRAW_SHAPES
pattern SPACE_DEBUG_DRAW_CONSTRAINTS      = #const CP_SPACE_DEBUG_DRAW_CONSTRAINTS
pattern SPACE_DEBUG_DRAW_COLLISION_POINTS = #const CP_SPACE_DEBUG_DRAW_COLLISION_POINTS

data {-# CTYPE "chipmunk/chipmunk.h" "cpSpaceDebugDrawOptions" #-} SpaceDebugDrawOptions =
       SpaceDebugDrawOptions
         { drawCircle          :: FunPtr SpaceDebugDrawCircleImpl
         , drawSegment         :: FunPtr SpaceDebugDrawSegmentImpl
         , drawFatSegment      :: FunPtr SpaceDebugDrawFatSegmentImpl
         , drawPolygon         :: FunPtr SpaceDebugDrawPolygonImpl
         , drawDot             :: FunPtr SpaceDebugDrawDotImpl
         , flags               :: SpaceDebugDrawFlags
         , shapeOutlineColor   :: SpaceDebugColor
         , colorForShape       :: FunPtr SpaceDebugDrawColorForShapeImpl
         , constraintColor     :: SpaceDebugColor
         , collisionPointColor :: SpaceDebugColor
         , data_               :: DataPointer
         }

instance Offset "drawCircle"           SpaceDebugDrawOptions where rawOffset = #offset struct cpSpaceDebugDrawOptions, drawCircle
instance Offset "drawSegment"          SpaceDebugDrawOptions where rawOffset = #offset struct cpSpaceDebugDrawOptions, drawSegment
instance Offset "drawFatSegment"       SpaceDebugDrawOptions where rawOffset = #offset struct cpSpaceDebugDrawOptions, drawFatSegment
instance Offset "drawPolygon"          SpaceDebugDrawOptions where rawOffset = #offset struct cpSpaceDebugDrawOptions, drawPolygon
instance Offset "drawDot"              SpaceDebugDrawOptions where rawOffset = #offset struct cpSpaceDebugDrawOptions, drawDot
instance Offset "flags"                SpaceDebugDrawOptions where rawOffset = #offset struct cpSpaceDebugDrawOptions, flags
instance Offset "shapeOutlineColor"    SpaceDebugDrawOptions where rawOffset = #offset struct cpSpaceDebugDrawOptions, shapeOutlineColor
instance Offset "colorForShape"        SpaceDebugDrawOptions where rawOffset = #offset struct cpSpaceDebugDrawOptions, colorForShape
instance Offset "constraintColor"      SpaceDebugDrawOptions where rawOffset = #offset struct cpSpaceDebugDrawOptions, constraintColor
instance Offset "collisionPointColor"  SpaceDebugDrawOptions where rawOffset = #offset struct cpSpaceDebugDrawOptions, collisionPointColor
instance Offset "data_"                SpaceDebugDrawOptions where rawOffset = #offset struct cpSpaceDebugDrawOptions, data

instance Offset "data" SpaceDebugDrawOptions where
  rawOffset = rawOffset @"data_" @SpaceDebugDrawOptions

instance HasField "data" SpaceDebugDrawOptions DataPointer where
  getField = getField @"data_"

instance Storable SpaceDebugDrawOptions where
  sizeOf _ = #size struct cpSpaceDebugDrawOptions
  alignment _ = #alignment struct cpSpaceDebugDrawOptions

  peek ptr =
    SpaceDebugDrawOptions
      <$> peek (offset @"drawCircle"          ptr)
      <*> peek (offset @"drawSegment"         ptr)
      <*> peek (offset @"drawFatSegment"      ptr)
      <*> peek (offset @"drawPolygon"         ptr)
      <*> peek (offset @"drawDot"             ptr)
      <*> peek (offset @"flags"               ptr)
      <*> peek (offset @"shapeOutlineColor"   ptr)
      <*> peek (offset @"colorForShape"       ptr)
      <*> peek (offset @"constraintColor"     ptr)
      <*> peek (offset @"collisionPointColor" ptr)
      <*> peek (offset @"data"                ptr)

  poke ptr val = do
    pokeField @"drawCircle"          ptr val
    pokeField @"drawSegment"         ptr val
    pokeField @"drawFatSegment"      ptr val
    pokeField @"drawPolygon"         ptr val
    pokeField @"drawDot"             ptr val
    pokeField @"flags"               ptr val
    pokeField @"shapeOutlineColor"   ptr val
    pokeField @"colorForShape"       ptr val
    pokeField @"constraintColor"     ptr val
    pokeField @"collisionPointColor" ptr val
    pokeField @"data"                ptr val

foreign import CALLCV "chipmunk/chipmunk.h cpSpaceDebugDraw"
  spaceDebugDraw
    :: Ptr Space                 -- ^ space
    -> Ptr SpaceDebugDrawOptions -- ^ options
    -> IO ()

#endif

-- end of cpSpace.h

pattern VERSION_MAJOR
      , VERSION_MINOR
      , VERSION_RELEASE
     :: (Eq a, Num a) => a
pattern VERSION_MAJOR = #const CP_VERSION_MAJOR
pattern VERSION_MINOR = #const CP_VERSION_MINOR
pattern VERSION_RELEASE = #const CP_VERSION_RELEASE

foreign import capi unsafe "chipmunk/chipmunk.h value cpVersionString"
  versionString :: IO (Ptr #type char)

foreign import CALLCV unsafe "wrapper.h w_cpMomentForCircle"
  momentForCircle
    :: Float    -- ^ m
    -> Float    -- ^ r1
    -> Float    -- ^ r2
    -> Ptr Vect -- ^ offset
    -> IO Float

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpAreaForCircle"
  areaForCircle
    :: Float -- ^ r1
    -> Float -- ^ r2
    -> IO Float

foreign import CALLCV unsafe "wrapper.h w_cpMomentForSegment"
  momentForSegment
    :: Float    -- ^ m
    -> Ptr Vect -- ^ a
    -> Ptr Vect -- ^ b
    -> Float    -- ^ radius
    -> IO Float

foreign import CALLCV unsafe "wrapper.h w_cpAreaForSegment"
  areaForSegment
    :: Ptr Vect -- ^ a
    -> Ptr Vect -- ^ b
    -> Float    -- ^ radius
    -> IO Float

foreign import CALLCV unsafe "wrapper.h w_cpMomentForPoly"
  momentForPoly
    :: Float       -- ^ m
    -> #{type int} -- ^ count
    -> Ptr Vect    -- ^ verts
    -> Ptr Vect    -- ^ offset
    -> Float       -- ^ radius
    -> IO Float

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpAreaForPoly"
  areaForPoly
    :: #{type int} -- ^ count
    -> Ptr Vect    -- ^ verts
    -> Float       -- ^ radius
    -> IO Float

foreign import CALLCV unsafe "wrapper.h w_cpCentroidForPoly"
  centroidForPoly
    :: #{type int} -- ^ count
    -> Ptr Vect    -- ^ verts
    -> Ptr Vect    -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpMomentForBox"
  momentForBox
    :: Float    -- ^ m
    -> Float    -- ^ width
    -> Float    -- ^ height
    -> IO Float

foreign import CALLCV unsafe "wrapper.h w_cpMomentForBox2"
  momentForBox2
    :: Float    -- ^ m
    -> Ptr BB   -- ^ box
    -> IO Float

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpConvexHull"
  convexHull
    :: #{type int}     -- ^ count
    -> Ptr Vect        -- ^ verts
    -> Ptr Vect        -- ^ result
    -> Ptr #{type int} -- ^ first
    -> Float           -- ^ tol
    -> IO #type int


foreign import CALLCV unsafe "wrapper.h w_cpClosetPointOnSegment"
  closetPointOnSegment
    :: Ptr Vect -- ^ p
    -> Ptr Vect -- ^ a
    -> Ptr Vect -- ^ b
    -> Ptr Vect -- ^ Output value
    -> IO ()

-- * chipmunk_structs.h

#include <chipmunk/chipmunk_structs.h>

data {-# CTYPE "chipmunk/chipmunk_structs.h" "cpArray" #-} Array =
       Array
         { num :: #type int
         , max :: #type int
         , arr :: Ptr (Ptr ())
         }

instance Offset "num" Array where rawOffset = #offset struct cpArray, num
instance Offset "max" Array where rawOffset = #offset struct cpArray, max
instance Offset "arr" Array where rawOffset = #offset struct cpArray, arr

instance Storable Array where
  sizeOf _ = #size struct cpArray
  alignment _ = #alignment struct cpArray

  peek ptr =
    Array
      <$> peek (offset @"num" ptr)
      <*> peek (offset @"max" ptr)
      <*> peek (offset @"arr" ptr)

  poke ptr val = do
    pokeField @"num" ptr val
    pokeField @"max" ptr val
    pokeField @"arr" ptr val

data {-# CTYPE "chipmunk/chipmunk.h" "cpBody" #-} Body =
       Body
         { velocity_func  :: FunPtr BodyVelocityFunc
         , position_func  :: FunPtr BodyPositionFunc
         , m              :: Float
         , m_inv          :: Float
         , i              :: Float
         , i_inv          :: Float
         , cog            :: Vect
         , p              :: Vect
         , v              :: Vect
         , f              :: Vect
         , a              :: Float
         , w              :: Float
         , t              :: Float
         , transform      :: Transform
         , userData       :: DataPointer
         , v_bias         :: Vect
         , w_bias         :: Float
         , space          :: Ptr Space
         , shapeList      :: Ptr Shape
         , arbiterList    :: Ptr Arbiter
         , constraintList :: Ptr Constraint
         , sleeping       :: BodySleeping
         }

instance Offset "velocity_func"  Body where rawOffset = #offset struct cpBody, velocity_func
instance Offset "position_func"  Body where rawOffset = #offset struct cpBody, position_func
instance Offset "m"              Body where rawOffset = #offset struct cpBody, m
instance Offset "m_inv"          Body where rawOffset = #offset struct cpBody, m_inv
instance Offset "i"              Body where rawOffset = #offset struct cpBody, i
instance Offset "i_inv"          Body where rawOffset = #offset struct cpBody, i_inv
instance Offset "cog"            Body where rawOffset = #offset struct cpBody, cog
instance Offset "p"              Body where rawOffset = #offset struct cpBody, p
instance Offset "v"              Body where rawOffset = #offset struct cpBody, v
instance Offset "f"              Body where rawOffset = #offset struct cpBody, f
instance Offset "a"              Body where rawOffset = #offset struct cpBody, a
instance Offset "w"              Body where rawOffset = #offset struct cpBody, w
instance Offset "t"              Body where rawOffset = #offset struct cpBody, t
instance Offset "transform"      Body where rawOffset = #offset struct cpBody, transform
instance Offset "userData"       Body where rawOffset = #offset struct cpBody, userData
instance Offset "v_bias"         Body where rawOffset = #offset struct cpBody, v_bias
instance Offset "w_bias"         Body where rawOffset = #offset struct cpBody, w_bias
instance Offset "space"          Body where rawOffset = #offset struct cpBody, space
instance Offset "shapeList"      Body where rawOffset = #offset struct cpBody, shapeList
instance Offset "arbiterList"    Body where rawOffset = #offset struct cpBody, arbiterList
instance Offset "constraintList" Body where rawOffset = #offset struct cpBody, constraintList
instance Offset "sleeping"       Body where rawOffset = #offset struct cpBody, sleeping

instance Storable Body where
  sizeOf _ = #size struct cpBody
  alignment _ = #alignment struct cpBody

  peek ptr =
    Body
      <$> peek (offset @"velocity_func"  ptr)
      <*> peek (offset @"position_func"  ptr)
      <*> peek (offset @"m"              ptr)
      <*> peek (offset @"m_inv"          ptr)
      <*> peek (offset @"i"              ptr)
      <*> peek (offset @"i_inv"          ptr)
      <*> peek (offset @"cog"            ptr)
      <*> peek (offset @"p"              ptr)
      <*> peek (offset @"v"              ptr)
      <*> peek (offset @"f"              ptr)
      <*> peek (offset @"a"              ptr)
      <*> peek (offset @"w"              ptr)
      <*> peek (offset @"t"              ptr)
      <*> peek (offset @"transform"      ptr)
      <*> peek (offset @"userData"       ptr)
      <*> peek (offset @"v_bias"         ptr)
      <*> peek (offset @"w_bias"         ptr)
      <*> peek (offset @"space"          ptr)
      <*> peek (offset @"shapeList"      ptr)
      <*> peek (offset @"arbiterList"    ptr)
      <*> peek (offset @"constraintList" ptr)
      <*> peek (offset @"sleeping"       ptr)

  poke ptr val = do
    pokeField @"velocity_func"  ptr val
    pokeField @"position_func"  ptr val
    pokeField @"m"              ptr val
    pokeField @"m_inv"          ptr val
    pokeField @"i"              ptr val
    pokeField @"i_inv"          ptr val
    pokeField @"cog"            ptr val
    pokeField @"p"              ptr val
    pokeField @"v"              ptr val
    pokeField @"f"              ptr val
    pokeField @"a"              ptr val
    pokeField @"w"              ptr val
    pokeField @"t"              ptr val
    pokeField @"transform"      ptr val
    pokeField @"userData"       ptr val
    pokeField @"v_bias"         ptr val
    pokeField @"w_bias"         ptr val
    pokeField @"space"          ptr val
    pokeField @"shapeList"      ptr val
    pokeField @"arbiterList"    ptr val
    pokeField @"constraintList" ptr val
    pokeField @"sleeping"       ptr val

data BodySleeping =
       BodySleeping
         { root     :: Ptr Body
         , next     :: Ptr Body
         , idleTime :: Float
         }

instance Offset "root"     BodySleeping where rawOffset = #offset struct cpBodySleeping, root
instance Offset "next"     BodySleeping where rawOffset = #offset struct cpBodySleeping, next
instance Offset "idleTime" BodySleeping where rawOffset = #offset struct cpBodySleeping, idleTime

instance Storable BodySleeping where
  sizeOf _ = #size struct cpBodySleeping
  alignment _ = #alignment struct cpBodySleeping

  peek ptr =
    BodySleeping
      <$> peek (offset @"root"     ptr)
      <*> peek (offset @"next"     ptr)
      <*> peek (offset @"idleTime" ptr)

  poke ptr val = do
    pokeField @"root"     ptr val
    pokeField @"next"     ptr val
    pokeField @"idleTime" ptr val

type ArbiterState = #type enum cpArbiterState

pattern ARBITER_STATE_FIRST_COLLISION
      , ARBITER_STATE_NORMAL
      , ARBITER_STATE_IGNORE
      , ARBITER_STATE_CACHED
      , ARBITER_STATE_INVALIDATED
     :: (Eq a, Num a) => a
pattern ARBITER_STATE_FIRST_COLLISION = #const CP_ARBITER_STATE_FIRST_COLLISION
pattern ARBITER_STATE_NORMAL          = #const CP_ARBITER_STATE_NORMAL
pattern ARBITER_STATE_IGNORE          = #const CP_ARBITER_STATE_IGNORE
pattern ARBITER_STATE_CACHED          = #const CP_ARBITER_STATE_CACHED
pattern ARBITER_STATE_INVALIDATED     = #const CP_ARBITER_STATE_INVALIDATED

data {-# CTYPE "chipmunk/chipmunk_structs.h" "cpArbiterThread" #-} ArbiterThread =
       ArbiterThread
         { next :: Ptr Arbiter
         , prev :: Ptr Arbiter
         }

instance Offset "next" ArbiterThread where rawOffset = #offset struct cpArbiterThread, next
instance Offset "prev" ArbiterThread where rawOffset = #offset struct cpArbiterThread, prev

instance Storable ArbiterThread where
  sizeOf _ = #size struct cpArbiterThread
  alignment _ = #alignment struct cpArbiterThread

  peek ptr =
    ArbiterThread
      <$> peek (offset @"next" ptr)
      <*> peek (offset @"prev" ptr)

  poke ptr val = do
    pokeField @"next" ptr val
    pokeField @"prev" ptr val

data {-# CTYPE "chipmunk/chipmunk_structs.h" "cpContact" #-} Contact =
       Contact
         { r1     :: Vect
         , r2     :: Vect
         , nMass  :: Float
         , tMass  :: Float
         , bounce :: Float
         , jnAcc  :: Float
         , jtAcc  :: Float
         , jBias  :: Float
         , bias   :: Float
         , hash   :: HashValue
         }

instance Offset "r1"     Contact where rawOffset = #offset struct cpContact, r1
instance Offset "r2"     Contact where rawOffset = #offset struct cpContact, r2
instance Offset "nMass"  Contact where rawOffset = #offset struct cpContact, nMass
instance Offset "tMass"  Contact where rawOffset = #offset struct cpContact, tMass
instance Offset "bounce" Contact where rawOffset = #offset struct cpContact, bounce
instance Offset "jnAcc"  Contact where rawOffset = #offset struct cpContact, jnAcc
instance Offset "jtAcc"  Contact where rawOffset = #offset struct cpContact, jtAcc
instance Offset "jBias"  Contact where rawOffset = #offset struct cpContact, jBias
instance Offset "bias"   Contact where rawOffset = #offset struct cpContact, bias
instance Offset "hash"   Contact where rawOffset = #offset struct cpContact, hash

instance Storable Contact where
  sizeOf _ = #size struct cpContact
  alignment _ = #alignment struct cpContact

  peek ptr =
    Contact
      <$> peek (offset @"r1"     ptr)
      <*> peek (offset @"r2"     ptr)
      <*> peek (offset @"nMass"  ptr)
      <*> peek (offset @"tMass"  ptr)
      <*> peek (offset @"bounce" ptr)
      <*> peek (offset @"jnAcc"  ptr)
      <*> peek (offset @"jtAcc"  ptr)
      <*> peek (offset @"jBias"  ptr)
      <*> peek (offset @"bias"   ptr)
      <*> peek (offset @"hash"   ptr)

  poke ptr val = do
    pokeField @"r1"     ptr val
    pokeField @"r2"     ptr val
    pokeField @"nMass"  ptr val
    pokeField @"tMass"  ptr val
    pokeField @"bounce" ptr val
    pokeField @"jnAcc"  ptr val
    pokeField @"jtAcc"  ptr val
    pokeField @"jBias"  ptr val
    pokeField @"bias"   ptr val
    pokeField @"hash"   ptr val

data {-# CTYPE "chipmunk/chipmunk_structs.h" "cpCollisionInfo" #-} CollisionInfo =
       CollisionInfo
         { a     :: Ptr Shape
         , b     :: Ptr Shape
         , id    :: CollisionID
         , n     :: Vect
         , count :: #type int
         , arr   :: Ptr Contact
         }

instance Offset "a"     CollisionInfo where rawOffset = #offset struct cpCollisionInfo, a
instance Offset "b"     CollisionInfo where rawOffset = #offset struct cpCollisionInfo, b
instance Offset "id"    CollisionInfo where rawOffset = #offset struct cpCollisionInfo, id
instance Offset "n"     CollisionInfo where rawOffset = #offset struct cpCollisionInfo, n
instance Offset "count" CollisionInfo where rawOffset = #offset struct cpCollisionInfo, count
instance Offset "arr"   CollisionInfo where rawOffset = #offset struct cpCollisionInfo, arr

instance Storable CollisionInfo where
  sizeOf _ = #size struct cpCollisionInfo
  alignment _ = #alignment struct cpCollisionInfo

  peek ptr =
    CollisionInfo
      <$> peek (offset @"a"     ptr)
      <*> peek (offset @"b"     ptr)
      <*> peek (offset @"id"    ptr)
      <*> peek (offset @"n"     ptr)
      <*> peek (offset @"count" ptr)
      <*> peek (offset @"arr"   ptr)

  poke ptr val = do
    pokeField @"a"     ptr val
    pokeField @"b"     ptr val
    pokeField @"id"    ptr val
    pokeField @"n"     ptr val
    pokeField @"count" ptr val
    pokeField @"arr"   ptr val

data {-# CTYPE "chipmunk/chipmunk.h" "cpArbiter" #-} Arbiter =
       Arbiter
         { e          :: Float
         , u          :: Float
         , surface_vr :: Vect
         , data_      :: DataPointer
         , a          :: Ptr Shape
         , b          :: Ptr Shape
         , body_a     :: Ptr Body
         , body_b     :: Ptr Body
         , thread_a   :: ArbiterThread
         , thread_b   :: ArbiterThread
         , count      :: #type int
         , contacts   :: Ptr Contact
         , n          :: Vect
         , handler    :: Ptr CollisionHandler
         , handlerA   :: Ptr CollisionHandler
         , handlerB   :: Ptr CollisionHandler
         , swapped    :: Bool
         , stamp      :: Timestamp
         , state      :: ArbiterState
         }

instance Offset "e"          Arbiter where rawOffset = #offset struct cpArbiter, e
instance Offset "u"          Arbiter where rawOffset = #offset struct cpArbiter, u
instance Offset "surface_vr" Arbiter where rawOffset = #offset struct cpArbiter, surface_vr
instance Offset "data_"      Arbiter where rawOffset = #offset struct cpArbiter, data
instance Offset "a"          Arbiter where rawOffset = #offset struct cpArbiter, a
instance Offset "b"          Arbiter where rawOffset = #offset struct cpArbiter, b
instance Offset "body_a"     Arbiter where rawOffset = #offset struct cpArbiter, body_a
instance Offset "body_b"     Arbiter where rawOffset = #offset struct cpArbiter, body_b
instance Offset "thread_a"   Arbiter where rawOffset = #offset struct cpArbiter, thread_a
instance Offset "thread_b"   Arbiter where rawOffset = #offset struct cpArbiter, thread_b
instance Offset "count"      Arbiter where rawOffset = #offset struct cpArbiter, count
instance Offset "contacts"   Arbiter where rawOffset = #offset struct cpArbiter, contacts
instance Offset "n"          Arbiter where rawOffset = #offset struct cpArbiter, n
instance Offset "handler"    Arbiter where rawOffset = #offset struct cpArbiter, handler
instance Offset "handlerA"   Arbiter where rawOffset = #offset struct cpArbiter, handlerA
instance Offset "handlerB"   Arbiter where rawOffset = #offset struct cpArbiter, handlerB
instance Offset "swapped"    Arbiter where rawOffset = #offset struct cpArbiter, swapped
instance Offset "stamp"      Arbiter where rawOffset = #offset struct cpArbiter, stamp
instance Offset "state"      Arbiter where rawOffset = #offset struct cpArbiter, state

instance Offset "data" Arbiter where
  rawOffset = rawOffset @"data_" @Arbiter

instance HasField "data" Arbiter DataPointer where
  getField = getField @"data_"

instance Storable Arbiter where
  sizeOf _ = #size struct cpArbiter
  alignment _ = #alignment struct cpArbiter

  peek ptr =
    Arbiter
      <$> peek (offset @"e"          ptr)
      <*> peek (offset @"u"          ptr)
      <*> peek (offset @"surface_vr" ptr)
      <*> peek (offset @"data"       ptr)
      <*> peek (offset @"a"          ptr)
      <*> peek (offset @"b"          ptr)
      <*> peek (offset @"body_a"     ptr)
      <*> peek (offset @"body_b"     ptr)
      <*> peek (offset @"thread_a"   ptr)
      <*> peek (offset @"thread_b"   ptr)
      <*> peek (offset @"count"      ptr)
      <*> peek (offset @"contacts"   ptr)
      <*> peek (offset @"n"          ptr)
      <*> peek (offset @"handler"    ptr)
      <*> peek (offset @"handlerA"   ptr)
      <*> peek (offset @"handlerB"   ptr)
      <*> peek (offset @"swapped"    ptr)
      <*> peek (offset @"stamp"      ptr)
      <*> peek (offset @"state"      ptr)

  poke ptr val = do
    pokeField @"e"          ptr val
    pokeField @"u"          ptr val
    pokeField @"surface_vr" ptr val
    pokeField @"data"       ptr val
    pokeField @"a"          ptr val
    pokeField @"b"          ptr val
    pokeField @"body_a"     ptr val
    pokeField @"body_b"     ptr val
    pokeField @"thread_a"   ptr val
    pokeField @"thread_b"   ptr val
    pokeField @"count"      ptr val
    pokeField @"contacts"   ptr val
    pokeField @"n"          ptr val
    pokeField @"handler"    ptr val
    pokeField @"handlerA"   ptr val
    pokeField @"handlerB"   ptr val
    pokeField @"swapped"    ptr val
    pokeField @"stamp"      ptr val
    pokeField @"state"      ptr val

data {-# CTYPE "chipmunk/chipmunk_structs.h" "cpShapeMassInfo" #-} ShapeMassInfo =
       ShapeMassInfo
         { m    :: Float
         , i    :: Float
         , cog  :: Vect
         , area :: Float
         }

instance Offset "m"    ShapeMassInfo where rawOffset = #offset struct cpShapeMassInfo, m
instance Offset "i"    ShapeMassInfo where rawOffset = #offset struct cpShapeMassInfo, i
instance Offset "cog"  ShapeMassInfo where rawOffset = #offset struct cpShapeMassInfo, cog
instance Offset "area" ShapeMassInfo where rawOffset = #offset struct cpShapeMassInfo, area

instance Storable ShapeMassInfo where
  sizeOf _ = #size struct cpShapeMassInfo
  alignment _ = #alignment struct cpShapeMassInfo

  peek ptr =
    ShapeMassInfo
      <$> peek (offset @"m"    ptr)
      <*> peek (offset @"i"    ptr)
      <*> peek (offset @"cog"  ptr)
      <*> peek (offset @"area" ptr)

  poke ptr val = do
    pokeField @"m"    ptr val
    pokeField @"i"    ptr val
    pokeField @"cog"  ptr val
    pokeField @"area" ptr val

type ShapeType = #type cpShapeType

pattern CIRCLE_SHAPE
      , SEGMENT_SHAPE
      , POLY_SHAPE
      , NUM_SHAPES
     :: (Eq a, Num a) => a
pattern CIRCLE_SHAPE  = #const CP_CIRCLE_SHAPE
pattern SEGMENT_SHAPE = #const CP_SEGMENT_SHAPE
pattern POLY_SHAPE    = #const CP_POLY_SHAPE
pattern NUM_SHAPES    = #const CP_NUM_SHAPES

type ShapeCacheDataImpl =
          Ptr Shape -- ^ shape
       -> Transform -- ^ transform
       -> IO BB

type ShapeDestroyImpl =
          Ptr Shape -- ^shape
       -> IO ()

type ShapePointQueryImpl =
          Ptr Shape          -- ^ shape
       -> Vect               -- ^ p
       -> Ptr PointQueryInfo -- ^ info
       -> IO ()

type ShapeSegmentQueryImpl =
          Ptr Shape            -- ^ shape
       -> Vect                 -- ^ a
       -> Vect                 -- ^ b
       -> Float                -- ^ radius
       -> Ptr SegmentQueryInfo -- ^ info
       -> IO ()

data {-# CTYPE "chipmunk/chipmunk_structs.h" "cpShapeClass" #-} ShapeClass =
       ShapeClass
         { type_        :: ShapeType
         , cacheData    :: FunPtr ShapeCacheDataImpl
         , destroy      :: FunPtr ShapeDestroyImpl
         , pointQuery   :: FunPtr ShapePointQueryImpl
         , segmentQuery :: FunPtr ShapeSegmentQueryImpl
         }

instance Offset "type_"        ShapeClass where rawOffset = #offset struct cpShapeClass, type
instance Offset "cacheData"    ShapeClass where rawOffset = #offset struct cpShapeClass, cacheData
instance Offset "destroy"      ShapeClass where rawOffset = #offset struct cpShapeClass, destroy
instance Offset "pointQuery"   ShapeClass where rawOffset = #offset struct cpShapeClass, pointQuery
instance Offset "segmentQuery" ShapeClass where rawOffset = #offset struct cpShapeClass, segmentQuery

instance Offset "type" ShapeClass where
  rawOffset = rawOffset @"type_" @ShapeClass

instance HasField "type" ShapeClass ShapeType where
  getField = getField @"type_"

instance Storable ShapeClass where
  sizeOf _ = #size struct cpShapeClass
  alignment _ = #alignment struct cpShapeClass

  peek ptr =
    ShapeClass
      <$> peek (offset @"type"         ptr)
      <*> peek (offset @"cacheData"    ptr)
      <*> peek (offset @"destroy"      ptr)
      <*> peek (offset @"pointQuery"   ptr)
      <*> peek (offset @"segmentQuery" ptr)

  poke ptr val = do
    pokeField @"type"         ptr val
    pokeField @"cacheData"    ptr val
    pokeField @"destroy"      ptr val
    pokeField @"pointQuery"   ptr val
    pokeField @"segmentQuery" ptr val

data {-# CTYPE "chipmunk/chipmunk.h" "cpShape" #-} Shape =
       Shape
         { klass    :: Ptr ShapeClass
         , space    :: Ptr Space
         , body     :: Ptr Body
         , massInfo :: ShapeMassInfo
         , bb       :: BB
         , sensor   :: Bool
         , e        :: Float
         , u        :: Float
         , surfaceV :: Vect
         , userData :: DataPointer
         , type_    :: CollisionType
         , filter   :: ShapeFilter
         , next     :: Ptr Shape
         , prev     :: Ptr Shape
         , hashid   :: HashValue
         }

instance Offset "klass"    Shape where rawOffset = #offset struct cpShape, klass
instance Offset "space"    Shape where rawOffset = #offset struct cpShape, space
instance Offset "body"     Shape where rawOffset = #offset struct cpShape, body
instance Offset "massInfo" Shape where rawOffset = #offset struct cpShape, massInfo
instance Offset "bb"       Shape where rawOffset = #offset struct cpShape, bb
instance Offset "sensor"   Shape where rawOffset = #offset struct cpShape, sensor
instance Offset "e"        Shape where rawOffset = #offset struct cpShape, e
instance Offset "u"        Shape where rawOffset = #offset struct cpShape, u
instance Offset "surfaceV" Shape where rawOffset = #offset struct cpShape, surfaceV
instance Offset "userData" Shape where rawOffset = #offset struct cpShape, userData
instance Offset "type_"    Shape where rawOffset = #offset struct cpShape, type
instance Offset "filter"   Shape where rawOffset = #offset struct cpShape, filter
instance Offset "next"     Shape where rawOffset = #offset struct cpShape, next
instance Offset "prev"     Shape where rawOffset = #offset struct cpShape, prev
instance Offset "hashid"   Shape where rawOffset = #offset struct cpShape, hashid

instance Offset "type" Shape where
  rawOffset = rawOffset @"type_" @Shape

instance HasField "type" Shape CollisionType where
  getField = getField @"type_"

instance Storable Shape where
  sizeOf _ = #size struct cpShape
  alignment _ = #alignment struct cpShape

  peek ptr =
    Shape
      <$> peek (offset @"klass"    ptr)
      <*> peek (offset @"space"    ptr)
      <*> peek (offset @"body"     ptr)
      <*> peek (offset @"massInfo" ptr)
      <*> peek (offset @"bb"       ptr)
      <*> peek (offset @"sensor"   ptr)
      <*> peek (offset @"e"        ptr)
      <*> peek (offset @"u"        ptr)
      <*> peek (offset @"surfaceV" ptr)
      <*> peek (offset @"userData" ptr)
      <*> peek (offset @"type"     ptr)
      <*> peek (offset @"filter"   ptr)
      <*> peek (offset @"next"     ptr)
      <*> peek (offset @"prev"     ptr)
      <*> peek (offset @"hashid"   ptr)

  poke ptr val = do
    pokeField @"klass"    ptr val
    pokeField @"space"    ptr val
    pokeField @"body"     ptr val
    pokeField @"massInfo" ptr val
    pokeField @"bb"       ptr val
    pokeField @"sensor"   ptr val
    pokeField @"e"        ptr val
    pokeField @"u"        ptr val
    pokeField @"surfaceV" ptr val
    pokeField @"userData" ptr val
    pokeField @"type"     ptr val
    pokeField @"filter"   ptr val
    pokeField @"next"     ptr val
    pokeField @"prev"     ptr val
    pokeField @"hashid"   ptr val

data {-# CTYPE "chipmunk/chipmunk.h" "cpCircleShape" #-} CircleShape =
       CircleShape
         { shape :: Shape
         , c     :: Vect
         , tc    :: Vect
         , r     :: Float
         }

instance Offset "shape" CircleShape where rawOffset = #offset struct cpCircleShape, shape
instance Offset "c"     CircleShape where rawOffset = #offset struct cpCircleShape, c
instance Offset "tc"    CircleShape where rawOffset = #offset struct cpCircleShape, tc
instance Offset "r"     CircleShape where rawOffset = #offset struct cpCircleShape, r

instance Storable CircleShape where
  sizeOf _ = #size struct cpCircleShape
  alignment _ = #alignment struct cpCircleShape

  peek ptr =
    CircleShape
      <$> peek (offset @"shape" ptr)
      <*> peek (offset @"c"     ptr)
      <*> peek (offset @"tc"    ptr)
      <*> peek (offset @"r"     ptr)

  poke ptr val = do
    pokeField @"shape" ptr val
    pokeField @"c"     ptr val
    pokeField @"tc"    ptr val
    pokeField @"r"     ptr val

data {-# CTYPE "chipmunk/chipmunk.h" "cpSegmentShape" #-} SegmentShape =
       SegmentShape
         { shape     :: Shape
         , a         :: Vect
         , b         :: Vect
         , n         :: Vect
         , ta        :: Vect
         , tb        :: Vect
         , tn        :: Vect
         , r         :: Float
         , a_tangent :: Vect
         , b_tangent :: Vect
         }

instance Offset "shape"     SegmentShape where rawOffset = #offset struct cpSegmentShape, shape
instance Offset "a"         SegmentShape where rawOffset = #offset struct cpSegmentShape, a
instance Offset "b"         SegmentShape where rawOffset = #offset struct cpSegmentShape, b
instance Offset "n"         SegmentShape where rawOffset = #offset struct cpSegmentShape, n
instance Offset "ta"        SegmentShape where rawOffset = #offset struct cpSegmentShape, ta
instance Offset "tb"        SegmentShape where rawOffset = #offset struct cpSegmentShape, tb
instance Offset "tn"        SegmentShape where rawOffset = #offset struct cpSegmentShape, tn
instance Offset "r"         SegmentShape where rawOffset = #offset struct cpSegmentShape, r
instance Offset "a_tangent" SegmentShape where rawOffset = #offset struct cpSegmentShape, a_tangent
instance Offset "b_tangent" SegmentShape where rawOffset = #offset struct cpSegmentShape, b_tangent

instance Storable SegmentShape where
  sizeOf _ = #size struct cpSegmentShape
  alignment _ = #alignment struct cpSegmentShape

  peek ptr =
    SegmentShape
      <$> peek (offset @"shape"     ptr)
      <*> peek (offset @"a"         ptr)
      <*> peek (offset @"b"         ptr)
      <*> peek (offset @"n"         ptr)
      <*> peek (offset @"ta"        ptr)
      <*> peek (offset @"tb"        ptr)
      <*> peek (offset @"tn"        ptr)
      <*> peek (offset @"r"         ptr)
      <*> peek (offset @"a_tangent" ptr)
      <*> peek (offset @"b_tangent" ptr)

  poke ptr val = do
    pokeField @"shape"     ptr val
    pokeField @"a"         ptr val
    pokeField @"b"         ptr val
    pokeField @"n"         ptr val
    pokeField @"ta"        ptr val
    pokeField @"tb"        ptr val
    pokeField @"tn"        ptr val
    pokeField @"r"         ptr val
    pokeField @"a_tangent" ptr val
    pokeField @"b_tangent" ptr val

data {-# CTYPE "chipmunk/chipmunk_structs.h" "cpSplittingPlane" #-} SplittingPlane =
       SplittingPlane
         { v0 :: Vect
         , n  :: Vect
         }

instance Offset "v0"  SplittingPlane where rawOffset = #offset struct cpSplittingPlane, v0
instance Offset "n"   SplittingPlane where rawOffset = #offset struct cpSplittingPlane, n

instance Storable SplittingPlane where
  sizeOf _ = #size struct cpSplittingPlane
  alignment _ = #alignment struct cpSplittingPlane

  peek ptr =
    SplittingPlane
      <$> peek (offset @"v0" ptr)
      <*> peek (offset @"n"  ptr)

  poke ptr val = do
    pokeField @"v0" ptr val
    pokeField @"n"  ptr val

pattern POLY_SHAPE_INLINE_ALLOC :: (Eq a, Num a) => a
pattern POLY_SHAPE_INLINE_ALLOC = #const CP_POLY_SHAPE_INLINE_ALLOC

data {-# CTYPE "chipmunk/chipmunk.h" "cpPolyShape" #-} PolyShape =
       PolyShape
         { shape   :: Shape
         , r       :: Float
         , count   :: #type int
         , planes  :: Ptr SplittingPlane
         , _planes :: Ptr SplittingPlane
         }

instance Offset "shape"    PolyShape where rawOffset = #offset struct cpPolyShape, shape
instance Offset "r"        PolyShape where rawOffset = #offset struct cpPolyShape, r
instance Offset "count"    PolyShape where rawOffset = #offset struct cpPolyShape, count
instance Offset "planes"   PolyShape where rawOffset = #offset struct cpPolyShape, planes
instance Offset "_planes"  PolyShape where rawOffset = #offset struct cpPolyShape, _planes

instance Storable PolyShape where
  sizeOf _ = #size struct cpPolyShape
  alignment _ = #alignment struct cpPolyShape

  peek ptr =
    PolyShape
      <$> peek (offset @"shape"   ptr)
      <*> peek (offset @"r"       ptr)
      <*> peek (offset @"count"   ptr)
      <*> peek (offset @"planes"  ptr)
      <*> peek (offset @"_planes" ptr)

  poke ptr val = do
    pokeField @"shape"   ptr val
    pokeField @"r"       ptr val
    pokeField @"count"   ptr val
    pokeField @"planes"  ptr val
    pokeField @"_planes" ptr val

type ConstraintPreStepImpl =
          Ptr Constraint -- ^ constraint
       -> Float          -- ^ dt
       -> IO ()

type ConstraintApplyCachedImpulseImpl =
          Ptr Constraint -- ^ constraint
       -> Float          -- ^ dt_coef
       -> IO ()

type ConstraintApplyImpulseImpl =
          Ptr Constraint -- ^ constraint
       -> Float          -- ^ dt
       -> IO ()

type ConstraintGetImpulseImpl =
          Ptr Constraint -- ^ constraint
       -> IO Float

data {-# CTYPE "chipmunk/chipmunk_structs.h" "cpConstraintClass" #-} ConstraintClass =
       ConstraintClass
         { preStep            :: FunPtr ConstraintPreStepImpl
         , applyCachedImpulse :: FunPtr ConstraintApplyCachedImpulseImpl
         , applyImpulse       :: FunPtr ConstraintApplyImpulseImpl
         , getImpulse         :: FunPtr ConstraintGetImpulseImpl
         }

instance Offset "preStep"            ConstraintClass where rawOffset = #offset struct cpConstraintClass, preStep
instance Offset "applyCachedImpulse" ConstraintClass where rawOffset = #offset struct cpConstraintClass, applyCachedImpulse
instance Offset "applyImpulse"       ConstraintClass where rawOffset = #offset struct cpConstraintClass, applyImpulse
instance Offset "getImpulse"         ConstraintClass where rawOffset = #offset struct cpConstraintClass, getImpulse

instance Storable ConstraintClass where
  sizeOf _ = #size struct cpConstraintClass
  alignment _ = #alignment struct cpConstraintClass

  peek ptr =
    ConstraintClass
      <$> peek (offset @"preStep"            ptr)
      <*> peek (offset @"applyCachedImpulse" ptr)
      <*> peek (offset @"applyImpulse"       ptr)
      <*> peek (offset @"getImpulse"         ptr)

  poke ptr val = do
    pokeField @"preStep"            ptr val
    pokeField @"applyCachedImpulse" ptr val
    pokeField @"applyImpulse"       ptr val
    pokeField @"getImpulse"         ptr val

data {-# CTYPE "chipmunk/chipmunk.h" "cpConstraint" #-} Constraint =
       Constraint
         { klass         :: Ptr ConstraintClass
         , space         :: Ptr Space
         , a             :: Ptr Body
         , b             :: Ptr Body
         , next_a        :: Ptr Constraint
         , next_b        :: Ptr Constraint
         , maxForce      :: Float
         , errorBias     :: Float
         , maxBias       :: Float
         , collideBodies :: Bool
         , preSolve      :: FunPtr ConstraintPreSolveFunc
         , postSolve     :: FunPtr ConstraintPostSolveFunc
         , userData      :: DataPointer
         }

instance Offset "klass"         Constraint where rawOffset = #offset struct cpConstraint, klass
instance Offset "space"         Constraint where rawOffset = #offset struct cpConstraint, space
instance Offset "a"             Constraint where rawOffset = #offset struct cpConstraint, a
instance Offset "b"             Constraint where rawOffset = #offset struct cpConstraint, b
instance Offset "next_a"        Constraint where rawOffset = #offset struct cpConstraint, next_a
instance Offset "next_b"        Constraint where rawOffset = #offset struct cpConstraint, next_b
instance Offset "maxForce"      Constraint where rawOffset = #offset struct cpConstraint, maxForce
instance Offset "errorBias"     Constraint where rawOffset = #offset struct cpConstraint, errorBias
instance Offset "maxBias"       Constraint where rawOffset = #offset struct cpConstraint, maxBias
instance Offset "collideBodies" Constraint where rawOffset = #offset struct cpConstraint, collideBodies
instance Offset "preSolve"      Constraint where rawOffset = #offset struct cpConstraint, preSolve
instance Offset "postSolve"     Constraint where rawOffset = #offset struct cpConstraint, postSolve
instance Offset "userData"      Constraint where rawOffset = #offset struct cpConstraint, userData

instance Storable Constraint where
  sizeOf _ = #size struct cpConstraint
  alignment _ = #alignment struct cpConstraint

  peek ptr =
    Constraint
      <$> peek (offset @"klass"         ptr)
      <*> peek (offset @"space"         ptr)
      <*> peek (offset @"a"             ptr)
      <*> peek (offset @"b"             ptr)
      <*> peek (offset @"next_a"        ptr)
      <*> peek (offset @"next_b"        ptr)
      <*> peek (offset @"maxForce"      ptr)
      <*> peek (offset @"errorBias"     ptr)
      <*> peek (offset @"maxBias"       ptr)
      <*> peek (offset @"collideBodies" ptr)
      <*> peek (offset @"preSolve"      ptr)
      <*> peek (offset @"postSolve"     ptr)
      <*> peek (offset @"userData"      ptr)

  poke ptr val = do
    pokeField @"klass"         ptr val
    pokeField @"space"         ptr val
    pokeField @"a"             ptr val
    pokeField @"b"             ptr val
    pokeField @"next_a"        ptr val
    pokeField @"next_b"        ptr val
    pokeField @"maxForce"      ptr val
    pokeField @"errorBias"     ptr val
    pokeField @"maxBias"       ptr val
    pokeField @"collideBodies" ptr val
    pokeField @"preSolve"      ptr val
    pokeField @"postSolve"     ptr val
    pokeField @"userData"      ptr val

data {-# CTYPE "chipmunk/chipmunk.h" "cpPinJoint" #-} PinJoint =
       PinJoint
         { constraint :: Constraint
         , anchorA    :: Vect
         , anchorB    :: Vect
         , dist       :: Float
         , r1         :: Vect
         , r2         :: Vect
         , n          :: Vect
         , nMass      :: Float
         , jnAcc      :: Float
         , bias       :: Float
         }

instance Offset "constraint" PinJoint where rawOffset = #offset struct cpPinJoint, constraint
instance Offset "anchorA"    PinJoint where rawOffset = #offset struct cpPinJoint, anchorA
instance Offset "anchorB"    PinJoint where rawOffset = #offset struct cpPinJoint, anchorB
instance Offset "dist"       PinJoint where rawOffset = #offset struct cpPinJoint, dist
instance Offset "r1"         PinJoint where rawOffset = #offset struct cpPinJoint, r1
instance Offset "r2"         PinJoint where rawOffset = #offset struct cpPinJoint, r2
instance Offset "n"          PinJoint where rawOffset = #offset struct cpPinJoint, n
instance Offset "nMass"      PinJoint where rawOffset = #offset struct cpPinJoint, nMass
instance Offset "jnAcc"      PinJoint where rawOffset = #offset struct cpPinJoint, jnAcc
instance Offset "bias"       PinJoint where rawOffset = #offset struct cpPinJoint, bias

instance Storable PinJoint where
  sizeOf _ = #size struct cpPinJoint
  alignment _ = #alignment struct cpPinJoint

  peek ptr =
    PinJoint
      <$> peek (offset @"constraint" ptr)
      <*> peek (offset @"anchorA"    ptr)
      <*> peek (offset @"anchorB"    ptr)
      <*> peek (offset @"dist"       ptr)
      <*> peek (offset @"r1"         ptr)
      <*> peek (offset @"r2"         ptr)
      <*> peek (offset @"n"          ptr)
      <*> peek (offset @"nMass"      ptr)
      <*> peek (offset @"jnAcc"      ptr)
      <*> peek (offset @"bias"       ptr)

  poke ptr val = do
    pokeField @"constraint" ptr val
    pokeField @"anchorA"    ptr val
    pokeField @"anchorB"    ptr val
    pokeField @"dist"       ptr val
    pokeField @"r1"         ptr val
    pokeField @"r2"         ptr val
    pokeField @"n"          ptr val
    pokeField @"nMass"      ptr val
    pokeField @"jnAcc"      ptr val
    pokeField @"bias"       ptr val

data {-# CTYPE "chipmunk/chipmunk.h" "cpSlideJoint" #-} SlideJoint =
       SlideJoint
         { constraint :: Constraint
         , anchorA    :: Vect
         , anchorB    :: Vect
         , min        :: Float
         , max        :: Float
         , r1         :: Vect
         , r2         :: Vect
         , n          :: Vect
         , nMass      :: Float
         , jnAcc      :: Float
         , bias       :: Float
         }

instance Offset "constraint" SlideJoint where rawOffset = #offset struct cpSlideJoint, constraint
instance Offset "anchorA"    SlideJoint where rawOffset = #offset struct cpSlideJoint, anchorA
instance Offset "anchorB"    SlideJoint where rawOffset = #offset struct cpSlideJoint, anchorB
instance Offset "min"        SlideJoint where rawOffset = #offset struct cpSlideJoint, min
instance Offset "max"        SlideJoint where rawOffset = #offset struct cpSlideJoint, max
instance Offset "r1"         SlideJoint where rawOffset = #offset struct cpSlideJoint, r1
instance Offset "r2"         SlideJoint where rawOffset = #offset struct cpSlideJoint, r2
instance Offset "n"          SlideJoint where rawOffset = #offset struct cpSlideJoint, n
instance Offset "nMass"      SlideJoint where rawOffset = #offset struct cpSlideJoint, nMass
instance Offset "jnAcc"      SlideJoint where rawOffset = #offset struct cpSlideJoint, jnAcc
instance Offset "bias"       SlideJoint where rawOffset = #offset struct cpSlideJoint, bias

instance Storable SlideJoint where
  sizeOf _ = #size struct cpSlideJoint
  alignment _ = #alignment struct cpSlideJoint

  peek ptr =
    SlideJoint
      <$> peek (offset @"constraint" ptr)
      <*> peek (offset @"anchorA"    ptr)
      <*> peek (offset @"anchorB"    ptr)
      <*> peek (offset @"min"        ptr)
      <*> peek (offset @"max"        ptr)
      <*> peek (offset @"r1"         ptr)
      <*> peek (offset @"r2"         ptr)
      <*> peek (offset @"n"          ptr)
      <*> peek (offset @"nMass"      ptr)
      <*> peek (offset @"jnAcc"      ptr)
      <*> peek (offset @"bias"       ptr)

  poke ptr val = do
    pokeField @"constraint" ptr val
    pokeField @"anchorA"    ptr val
    pokeField @"anchorB"    ptr val
    pokeField @"min"        ptr val
    pokeField @"max"        ptr val
    pokeField @"r1"         ptr val
    pokeField @"r2"         ptr val
    pokeField @"n"          ptr val
    pokeField @"nMass"      ptr val
    pokeField @"jnAcc"      ptr val
    pokeField @"bias"       ptr val

data {-# CTYPE "chipmunk/chipmunk.h" "cpPivotJoint" #-} PivotJoint =
       PivotJoint
         { constraint :: Constraint
         , anchorA    :: Vect
         , anchorB    :: Vect
         , r1         :: Vect
         , r2         :: Vect
         , k          :: Mat2x2
         , jAcc       :: Vect
         , bias       :: Vect
         }

instance Offset "constraint" PivotJoint where rawOffset = #offset struct cpPivotJoint, constraint
instance Offset "anchorA"    PivotJoint where rawOffset = #offset struct cpPivotJoint, anchorA
instance Offset "anchorB"    PivotJoint where rawOffset = #offset struct cpPivotJoint, anchorB
instance Offset "r1"         PivotJoint where rawOffset = #offset struct cpPivotJoint, r1
instance Offset "r2"         PivotJoint where rawOffset = #offset struct cpPivotJoint, r2
instance Offset "k"          PivotJoint where rawOffset = #offset struct cpPivotJoint, k
instance Offset "jAcc"       PivotJoint where rawOffset = #offset struct cpPivotJoint, jAcc
instance Offset "bias"       PivotJoint where rawOffset = #offset struct cpPivotJoint, bias

instance Storable PivotJoint where
  sizeOf _ = #size struct cpPivotJoint
  alignment _ = #alignment struct cpPivotJoint

  peek ptr =
    PivotJoint
      <$> peek (offset @"constraint" ptr)
      <*> peek (offset @"anchorA"    ptr)
      <*> peek (offset @"anchorB"    ptr)
      <*> peek (offset @"r1"         ptr)
      <*> peek (offset @"r2"         ptr)
      <*> peek (offset @"k"          ptr)
      <*> peek (offset @"jAcc"       ptr)
      <*> peek (offset @"bias"       ptr)

  poke ptr val = do
    pokeField @"constraint" ptr val
    pokeField @"anchorA"    ptr val
    pokeField @"anchorB"    ptr val
    pokeField @"r1"         ptr val
    pokeField @"r2"         ptr val
    pokeField @"k"          ptr val
    pokeField @"jAcc"       ptr val
    pokeField @"bias"       ptr val

data {-# CTYPE "chipmunk/chipmunk.h" "cpGrooveJoint" #-} GrooveJoint =
       GrooveJoint
         { constraint :: Constraint
         , grv_n      :: Vect
         , grv_a      :: Vect
         , grv_b      :: Vect
         , anchorB    :: Vect
         , grv_tn     :: Vect
         , clamp      :: Float
         , r1         :: Vect
         , r2         :: Vect
         , k          :: Mat2x2
         , jAcc       :: Vect
         , bias       :: Vect
         }

instance Offset "constraint" GrooveJoint where rawOffset = #offset struct cpGrooveJoint, constraint
instance Offset "grv_n"      GrooveJoint where rawOffset = #offset struct cpGrooveJoint, grv_n
instance Offset "grv_a"      GrooveJoint where rawOffset = #offset struct cpGrooveJoint, grv_a
instance Offset "grv_b"      GrooveJoint where rawOffset = #offset struct cpGrooveJoint, grv_b
instance Offset "anchorB"    GrooveJoint where rawOffset = #offset struct cpGrooveJoint, anchorB
instance Offset "grv_tn"     GrooveJoint where rawOffset = #offset struct cpGrooveJoint, grv_tn
instance Offset "clamp"      GrooveJoint where rawOffset = #offset struct cpGrooveJoint, clamp
instance Offset "r1"         GrooveJoint where rawOffset = #offset struct cpGrooveJoint, r1
instance Offset "r2"         GrooveJoint where rawOffset = #offset struct cpGrooveJoint, r2
instance Offset "k"          GrooveJoint where rawOffset = #offset struct cpGrooveJoint, k
instance Offset "jAcc"       GrooveJoint where rawOffset = #offset struct cpGrooveJoint, jAcc
instance Offset "bias"       GrooveJoint where rawOffset = #offset struct cpGrooveJoint, bias

instance Storable GrooveJoint where
  sizeOf _ = #size struct cpGrooveJoint
  alignment _ = #alignment struct cpGrooveJoint

  peek ptr =
    GrooveJoint
      <$> peek (offset @"constraint" ptr)
      <*> peek (offset @"grv_n"      ptr)
      <*> peek (offset @"grv_a"      ptr)
      <*> peek (offset @"grv_b"      ptr)
      <*> peek (offset @"anchorB"    ptr)
      <*> peek (offset @"grv_tn"     ptr)
      <*> peek (offset @"clamp"      ptr)
      <*> peek (offset @"r1"         ptr)
      <*> peek (offset @"r2"         ptr)
      <*> peek (offset @"k"          ptr)
      <*> peek (offset @"jAcc"       ptr)
      <*> peek (offset @"bias"       ptr)

  poke ptr val = do
    pokeField @"constraint" ptr val
    pokeField @"grv_n"      ptr val
    pokeField @"grv_a"      ptr val
    pokeField @"grv_b"      ptr val
    pokeField @"anchorB"    ptr val
    pokeField @"grv_tn"     ptr val
    pokeField @"clamp"      ptr val
    pokeField @"r1"         ptr val
    pokeField @"r2"         ptr val
    pokeField @"k"          ptr val
    pokeField @"jAcc"       ptr val
    pokeField @"bias"       ptr val

data {-# CTYPE "chipmunk/chipmunk.h" "cpDampedSpring" #-} DampedSpring =
       DampedSpring
         { constraint      :: Constraint
         , anchorA         :: Vect
         , anchorB         :: Vect
         , restLength      :: Float
         , stiffness       :: Float
         , damping         :: Float
         , springForceFunc :: FunPtr DampedSpringForceFunc
         , target_vrn      :: Float
         , v_coef          :: Float
         , r1              :: Vect
         , r2              :: Vect
         , nMass           :: Float
         , n               :: Vect
         , jAcc            :: Float
         }

instance Offset "constraint"      DampedSpring where rawOffset = #offset struct cpDampedSpring, constraint
instance Offset "anchorA"         DampedSpring where rawOffset = #offset struct cpDampedSpring, anchorA
instance Offset "anchorB"         DampedSpring where rawOffset = #offset struct cpDampedSpring, anchorB
instance Offset "restLength"      DampedSpring where rawOffset = #offset struct cpDampedSpring, restLength
instance Offset "stiffness"       DampedSpring where rawOffset = #offset struct cpDampedSpring, stiffness
instance Offset "damping"         DampedSpring where rawOffset = #offset struct cpDampedSpring, damping
instance Offset "springForceFunc" DampedSpring where rawOffset = #offset struct cpDampedSpring, springForceFunc
instance Offset "target_vrn"      DampedSpring where rawOffset = #offset struct cpDampedSpring, target_vrn
instance Offset "v_coef"          DampedSpring where rawOffset = #offset struct cpDampedSpring, v_coef
instance Offset "r1"              DampedSpring where rawOffset = #offset struct cpDampedSpring, r1
instance Offset "r2"              DampedSpring where rawOffset = #offset struct cpDampedSpring, r2
instance Offset "nMass"           DampedSpring where rawOffset = #offset struct cpDampedSpring, nMass
instance Offset "n"               DampedSpring where rawOffset = #offset struct cpDampedSpring, n
instance Offset "jAcc"            DampedSpring where rawOffset = #offset struct cpDampedSpring, jAcc

instance Storable DampedSpring where
  sizeOf _ = #size struct cpDampedSpring
  alignment _ = #alignment struct cpDampedSpring

  peek ptr =
    DampedSpring
      <$> peek (offset @"constraint"      ptr)
      <*> peek (offset @"anchorA"         ptr)
      <*> peek (offset @"anchorB"         ptr)
      <*> peek (offset @"restLength"      ptr)
      <*> peek (offset @"stiffness"       ptr)
      <*> peek (offset @"damping"         ptr)
      <*> peek (offset @"springForceFunc" ptr)
      <*> peek (offset @"target_vrn"      ptr)
      <*> peek (offset @"v_coef"          ptr)
      <*> peek (offset @"r1"              ptr)
      <*> peek (offset @"r2"              ptr)
      <*> peek (offset @"nMass"           ptr)
      <*> peek (offset @"n"               ptr)
      <*> peek (offset @"jAcc"            ptr)

  poke ptr val = do
    pokeField @"constraint"      ptr val
    pokeField @"anchorA"         ptr val
    pokeField @"anchorB"         ptr val
    pokeField @"restLength"      ptr val
    pokeField @"stiffness"       ptr val
    pokeField @"damping"         ptr val
    pokeField @"springForceFunc" ptr val
    pokeField @"target_vrn"      ptr val
    pokeField @"v_coef"          ptr val
    pokeField @"r1"              ptr val
    pokeField @"r2"              ptr val
    pokeField @"nMass"           ptr val
    pokeField @"n"               ptr val
    pokeField @"jAcc"            ptr val

data {-# CTYPE "chipmunk/chipmunk.h" "cpDampedRotarySpring" #-} DampedRotarySpring =
       DampedRotarySpring
         { constraint       :: Constraint
         , restAngle        :: Float
         , stiffness        :: Float
         , damping          :: Float
         , springTorqueFunc :: FunPtr DampedRotarySpringTorqueFunc
         , target_wrn       :: Float
         , w_coef           :: Float
         , iSum             :: Float
         , jAcc             :: Float
         }

instance Offset "constraint"       DampedRotarySpring where rawOffset = #offset struct cpDampedRotarySpring, constraint
instance Offset "restAngle"        DampedRotarySpring where rawOffset = #offset struct cpDampedRotarySpring, restAngle
instance Offset "stiffness"        DampedRotarySpring where rawOffset = #offset struct cpDampedRotarySpring, stiffness
instance Offset "damping"          DampedRotarySpring where rawOffset = #offset struct cpDampedRotarySpring, damping
instance Offset "springTorqueFunc" DampedRotarySpring where rawOffset = #offset struct cpDampedRotarySpring, springTorqueFunc
instance Offset "target_wrn"       DampedRotarySpring where rawOffset = #offset struct cpDampedRotarySpring, target_wrn
instance Offset "w_coef"           DampedRotarySpring where rawOffset = #offset struct cpDampedRotarySpring, w_coef
instance Offset "iSum"             DampedRotarySpring where rawOffset = #offset struct cpDampedRotarySpring, iSum
instance Offset "jAcc"             DampedRotarySpring where rawOffset = #offset struct cpDampedRotarySpring, jAcc

instance Storable DampedRotarySpring where
  sizeOf _ = #size struct cpDampedRotarySpring
  alignment _ = #alignment struct cpDampedRotarySpring

  peek ptr =
    DampedRotarySpring
      <$> peek (offset @"constraint"       ptr)
      <*> peek (offset @"restAngle"        ptr)
      <*> peek (offset @"stiffness"        ptr)
      <*> peek (offset @"damping"          ptr)
      <*> peek (offset @"springTorqueFunc" ptr)
      <*> peek (offset @"target_wrn"       ptr)
      <*> peek (offset @"w_coef"           ptr)
      <*> peek (offset @"iSum"             ptr)
      <*> peek (offset @"jAcc"             ptr)

  poke ptr val = do
    pokeField @"constraint"       ptr val
    pokeField @"restAngle"        ptr val
    pokeField @"stiffness"        ptr val
    pokeField @"damping"          ptr val
    pokeField @"springTorqueFunc" ptr val
    pokeField @"target_wrn"       ptr val
    pokeField @"w_coef"           ptr val
    pokeField @"iSum"             ptr val
    pokeField @"jAcc"             ptr val

data {-# CTYPE "chipmunk/chipmunk.h" "cpRotaryLimitJoint" #-} RotaryLimitJoint =
       RotaryLimitJoint
         { constraint :: Constraint
         , min        :: Float
         , max        :: Float
         , iSum       :: Float
         , bias       :: Float
         , jAcc       :: Float
         }

instance Offset "constraint" RotaryLimitJoint where rawOffset = #offset struct cpRotaryLimitJoint, constraint
instance Offset "min"        RotaryLimitJoint where rawOffset = #offset struct cpRotaryLimitJoint, min
instance Offset "max"        RotaryLimitJoint where rawOffset = #offset struct cpRotaryLimitJoint, max
instance Offset "iSum"       RotaryLimitJoint where rawOffset = #offset struct cpRotaryLimitJoint, iSum
instance Offset "bias"       RotaryLimitJoint where rawOffset = #offset struct cpRotaryLimitJoint, bias
instance Offset "jAcc"       RotaryLimitJoint where rawOffset = #offset struct cpRotaryLimitJoint, jAcc

instance Storable RotaryLimitJoint where
  sizeOf _ = #size struct cpRotaryLimitJoint
  alignment _ = #alignment struct cpRotaryLimitJoint

  peek ptr =
    RotaryLimitJoint
      <$> peek (offset @"constraint" ptr)
      <*> peek (offset @"min"        ptr)
      <*> peek (offset @"max"        ptr)
      <*> peek (offset @"iSum"       ptr)
      <*> peek (offset @"bias"       ptr)
      <*> peek (offset @"jAcc"       ptr)

  poke ptr val = do
    pokeField @"constraint" ptr val
    pokeField @"min"        ptr val
    pokeField @"max"        ptr val
    pokeField @"iSum"       ptr val
    pokeField @"bias"       ptr val
    pokeField @"jAcc"       ptr val

data {-# CTYPE "chipmunk/chipmunk.h" "cpRatchetJoint" #-} RatchetJoint =
       RatchetJoint
         { constraint :: Constraint
         , angle      :: Float
         , phase      :: Float
         , ratchet    :: Float
         , iSum       :: Float
         , bias       :: Float
         , jAcc       :: Float
         }

instance Offset "constraint" RatchetJoint where rawOffset = #offset struct cpRatchetJoint, constraint
instance Offset "angle"      RatchetJoint where rawOffset = #offset struct cpRatchetJoint, angle
instance Offset "phase"      RatchetJoint where rawOffset = #offset struct cpRatchetJoint, phase
instance Offset "ratchet"    RatchetJoint where rawOffset = #offset struct cpRatchetJoint, ratchet
instance Offset "iSum"       RatchetJoint where rawOffset = #offset struct cpRatchetJoint, iSum
instance Offset "bias"       RatchetJoint where rawOffset = #offset struct cpRatchetJoint, bias
instance Offset "jAcc"       RatchetJoint where rawOffset = #offset struct cpRatchetJoint, jAcc

instance Storable RatchetJoint where
  sizeOf _ = #size struct cpRatchetJoint
  alignment _ = #alignment struct cpRatchetJoint

  peek ptr =
    RatchetJoint
      <$> peek (offset @"constraint" ptr)
      <*> peek (offset @"angle"      ptr)
      <*> peek (offset @"phase"      ptr)
      <*> peek (offset @"ratchet"    ptr)
      <*> peek (offset @"iSum"       ptr)
      <*> peek (offset @"bias"       ptr)
      <*> peek (offset @"jAcc"       ptr)

  poke ptr val = do
    pokeField @"constraint" ptr val
    pokeField @"angle"      ptr val
    pokeField @"phase"      ptr val
    pokeField @"ratchet"    ptr val
    pokeField @"iSum"       ptr val
    pokeField @"bias"       ptr val
    pokeField @"jAcc"       ptr val

data {-# CTYPE "chipmunk/chipmunk.h" "cpGearJoint" #-} GearJoint =
       GearJoint
         { constraint :: Constraint
         , phase      :: Float
         , ratio      :: Float
         , ratio_inv  :: Float
         , iSum       :: Float
         , bias       :: Float
         , jAcc       :: Float
         }

instance Offset "constraint" GearJoint where rawOffset = #offset struct cpGearJoint, constraint
instance Offset "phase"      GearJoint where rawOffset = #offset struct cpGearJoint, phase
instance Offset "ratio"      GearJoint where rawOffset = #offset struct cpGearJoint, ratio
instance Offset "ratio_inv"  GearJoint where rawOffset = #offset struct cpGearJoint, ratio_inv
instance Offset "iSum"       GearJoint where rawOffset = #offset struct cpGearJoint, iSum
instance Offset "bias"       GearJoint where rawOffset = #offset struct cpGearJoint, bias
instance Offset "jAcc"       GearJoint where rawOffset = #offset struct cpGearJoint, jAcc

instance Storable GearJoint where
  sizeOf _ = #size struct cpGearJoint
  alignment _ = #alignment struct cpGearJoint

  peek ptr =
    GearJoint
      <$> peek (offset @"constraint" ptr)
      <*> peek (offset @"phase"      ptr)
      <*> peek (offset @"ratio"      ptr)
      <*> peek (offset @"ratio_inv"  ptr)
      <*> peek (offset @"iSum"       ptr)
      <*> peek (offset @"bias"       ptr)
      <*> peek (offset @"jAcc"       ptr)

  poke ptr val = do
    pokeField @"constraint" ptr val
    pokeField @"phase"      ptr val
    pokeField @"ratio"      ptr val
    pokeField @"ratio_inv"  ptr val
    pokeField @"iSum"       ptr val
    pokeField @"bias"       ptr val
    pokeField @"jAcc"       ptr val

data {-# CTYPE "chipmunk/chipmunk.h" "cpSimpleMotor" #-} SimpleMotor =
       SimpleMotor
         { constraint :: Constraint
         , rate       :: Float
         , iSum       :: Float
         , jAcc       :: Float
         }

instance Offset "constraint" SimpleMotor where rawOffset = #offset struct cpSimpleMotor, constraint
instance Offset "rate"       SimpleMotor where rawOffset = #offset struct cpSimpleMotor, rate
instance Offset "iSum"       SimpleMotor where rawOffset = #offset struct cpSimpleMotor, iSum
instance Offset "jAcc"       SimpleMotor where rawOffset = #offset struct cpSimpleMotor, jAcc

instance Storable SimpleMotor where
  sizeOf _ = #size struct cpSimpleMotor
  alignment _ = #alignment struct cpSimpleMotor

  peek ptr =
    SimpleMotor
      <$> peek (offset @"constraint" ptr)
      <*> peek (offset @"rate"       ptr)
      <*> peek (offset @"iSum"       ptr)
      <*> peek (offset @"jAcc"       ptr)

  poke ptr val = do
    pokeField @"constraint" ptr val
    pokeField @"rate"       ptr val
    pokeField @"iSum"       ptr val
    pokeField @"jAcc"       ptr val

data {-# CTYPE "chipmunk/chipmunk_structs.h" "cpContactBufferHeader" #-} ContactBufferHeader

type SpaceArbiterApplyImpulseFunc =
          Ptr Arbiter -- ^ arb
       -> IO ()

data {-# CTYPE "chipmunk/chipmunk.h" "cpSpace" #-} Space =
       Space
         { iterations           :: #type int
         , gravity              :: Vect
         , damping              :: Float
         , idleSpeedThreshold   :: Float
         , sleepTimeThreshold   :: Float
         , collisionSlop        :: Float
         , collisionBias        :: Float
         , collisionPersistence :: Timestamp
         , userData             :: DataPointer
         , stamp                :: Timestamp
         , curr_dt              :: Float
         , dynamicBodies        :: Ptr Array
         , staticBodies         :: Ptr Array
         , rousedBodies         :: Ptr Array
         , sleepingComponents   :: Ptr Array
         , shapeIDCounter       :: HashValue
         , staticShapes         :: Ptr SpatialIndex
         , dynamicShapes        :: Ptr SpatialIndex
         , constraints          :: Ptr Array
         , arbiters             :: Ptr Array
         , contactBuffersHead   :: Ptr ContactBufferHeader
         , cachedArbiters       :: Ptr HashSet
         , pooledArbiters       :: Ptr Array
         , allocatedBuffers     :: Ptr Array
         , locked               :: #type unsigned int
         , usesWildcards        :: Bool
         , collisionHandlers    :: Ptr HashSet
         , defaultHandler       :: CollisionHandler
         , skipPostStep         :: Bool
         , postStepCallbacks    :: Ptr Array
         , staticBody           :: Ptr Body
         , _staticBody          :: Body
         }

instance Offset "iterations"           Space where rawOffset = #offset struct cpSpace, iterations
instance Offset "gravity"              Space where rawOffset = #offset struct cpSpace, gravity
instance Offset "damping"              Space where rawOffset = #offset struct cpSpace, damping
instance Offset "idleSpeedThreshold"   Space where rawOffset = #offset struct cpSpace, idleSpeedThreshold
instance Offset "sleepTimeThreshold"   Space where rawOffset = #offset struct cpSpace, sleepTimeThreshold
instance Offset "collisionSlop"        Space where rawOffset = #offset struct cpSpace, collisionSlop
instance Offset "collisionBias"        Space where rawOffset = #offset struct cpSpace, collisionBias
instance Offset "collisionPersistence" Space where rawOffset = #offset struct cpSpace, collisionPersistence
instance Offset "userData"             Space where rawOffset = #offset struct cpSpace, userData
instance Offset "stamp"                Space where rawOffset = #offset struct cpSpace, stamp
instance Offset "curr_dt"              Space where rawOffset = #offset struct cpSpace, curr_dt
instance Offset "dynamicBodies"        Space where rawOffset = #offset struct cpSpace, dynamicBodies
instance Offset "staticBodies"         Space where rawOffset = #offset struct cpSpace, staticBodies
instance Offset "rousedBodies"         Space where rawOffset = #offset struct cpSpace, rousedBodies
instance Offset "sleepingComponents"   Space where rawOffset = #offset struct cpSpace, sleepingComponents
instance Offset "shapeIDCounter"       Space where rawOffset = #offset struct cpSpace, shapeIDCounter
instance Offset "staticShapes"         Space where rawOffset = #offset struct cpSpace, staticShapes
instance Offset "dynamicShapes"        Space where rawOffset = #offset struct cpSpace, dynamicShapes
instance Offset "constraints"          Space where rawOffset = #offset struct cpSpace, constraints
instance Offset "arbiters"             Space where rawOffset = #offset struct cpSpace, arbiters
instance Offset "contactBuffersHead"   Space where rawOffset = #offset struct cpSpace, contactBuffersHead
instance Offset "cachedArbiters"       Space where rawOffset = #offset struct cpSpace, cachedArbiters
instance Offset "pooledArbiters"       Space where rawOffset = #offset struct cpSpace, pooledArbiters
instance Offset "allocatedBuffers"     Space where rawOffset = #offset struct cpSpace, allocatedBuffers
instance Offset "locked"               Space where rawOffset = #offset struct cpSpace, locked
instance Offset "usesWildcards"        Space where rawOffset = #offset struct cpSpace, usesWildcards
instance Offset "collisionHandlers"    Space where rawOffset = #offset struct cpSpace, collisionHandlers
instance Offset "defaultHandler"       Space where rawOffset = #offset struct cpSpace, defaultHandler
instance Offset "skipPostStep"         Space where rawOffset = #offset struct cpSpace, skipPostStep
instance Offset "postStepCallbacks"    Space where rawOffset = #offset struct cpSpace, postStepCallbacks
instance Offset "staticBody"           Space where rawOffset = #offset struct cpSpace, staticBody
instance Offset "_staticBody"          Space where rawOffset = #offset struct cpSpace, _staticBody

instance Storable Space where
  sizeOf _ = #size struct cpSpace
  alignment _ = #alignment struct cpSpace

  peek ptr =
    Space
      <$> peek (offset @"iterations"           ptr)
      <*> peek (offset @"gravity"              ptr)
      <*> peek (offset @"damping"              ptr)
      <*> peek (offset @"idleSpeedThreshold"   ptr)
      <*> peek (offset @"sleepTimeThreshold"   ptr)
      <*> peek (offset @"collisionSlop"        ptr)
      <*> peek (offset @"collisionBias"        ptr)
      <*> peek (offset @"collisionPersistence" ptr)
      <*> peek (offset @"userData"             ptr)
      <*> peek (offset @"stamp"                ptr)
      <*> peek (offset @"curr_dt"              ptr)
      <*> peek (offset @"dynamicBodies"        ptr)
      <*> peek (offset @"staticBodies"         ptr)
      <*> peek (offset @"rousedBodies"         ptr)
      <*> peek (offset @"sleepingComponents"   ptr)
      <*> peek (offset @"shapeIDCounter"       ptr)
      <*> peek (offset @"staticShapes"         ptr)
      <*> peek (offset @"dynamicShapes"        ptr)
      <*> peek (offset @"constraints"          ptr)
      <*> peek (offset @"arbiters"             ptr)
      <*> peek (offset @"contactBuffersHead"   ptr)
      <*> peek (offset @"cachedArbiters"       ptr)
      <*> peek (offset @"pooledArbiters"       ptr)
      <*> peek (offset @"allocatedBuffers"     ptr)
      <*> peek (offset @"locked"               ptr)
      <*> peek (offset @"usesWildcards"        ptr)
      <*> peek (offset @"collisionHandlers"    ptr)
      <*> peek (offset @"defaultHandler"       ptr)
      <*> peek (offset @"skipPostStep"         ptr)
      <*> peek (offset @"postStepCallbacks"    ptr)
      <*> peek (offset @"staticBody"           ptr)
      <*> peek (offset @"_staticBody"          ptr)

  poke ptr val = do
    pokeField @"iterations"           ptr val
    pokeField @"gravity"              ptr val
    pokeField @"damping"              ptr val
    pokeField @"idleSpeedThreshold"   ptr val
    pokeField @"sleepTimeThreshold"   ptr val
    pokeField @"collisionSlop"        ptr val
    pokeField @"collisionBias"        ptr val
    pokeField @"collisionPersistence" ptr val
    pokeField @"userData"             ptr val
    pokeField @"stamp"                ptr val
    pokeField @"curr_dt"              ptr val
    pokeField @"dynamicBodies"        ptr val
    pokeField @"staticBodies"         ptr val
    pokeField @"rousedBodies"         ptr val
    pokeField @"sleepingComponents"   ptr val
    pokeField @"shapeIDCounter"       ptr val
    pokeField @"staticShapes"         ptr val
    pokeField @"dynamicShapes"        ptr val
    pokeField @"constraints"          ptr val
    pokeField @"arbiters"             ptr val
    pokeField @"contactBuffersHead"   ptr val
    pokeField @"cachedArbiters"       ptr val
    pokeField @"pooledArbiters"       ptr val
    pokeField @"allocatedBuffers"     ptr val
    pokeField @"locked"               ptr val
    pokeField @"usesWildcards"        ptr val
    pokeField @"collisionHandlers"    ptr val
    pokeField @"defaultHandler"       ptr val
    pokeField @"skipPostStep"         ptr val
    pokeField @"postStepCallbacks"    ptr val
    pokeField @"staticBody"           ptr val
    pokeField @"_staticBody"          ptr val

data {-# CTYPE "chipmunk/chipmunk_unsafe.h" "cpPostStepCallback" #-} PostStepCallback =
       PostStepCallback
         { func  :: FunPtr PostStepFunc
         , key   :: Ptr ()
         , data_ :: Ptr ()
         }

instance Offset "func"  PostStepCallback where rawOffset = #offset struct cpPostStepCallback, func
instance Offset "key"   PostStepCallback where rawOffset = #offset struct cpPostStepCallback, key
instance Offset "data_" PostStepCallback where rawOffset = #offset struct cpPostStepCallback, data

instance Offset "data" PostStepCallback where
  rawOffset = rawOffset @"data_" @PostStepCallback

instance HasField "data" PostStepCallback (Ptr ()) where
  getField = getField @"data_"

instance Storable PostStepCallback where
  sizeOf _ = #size struct cpPostStepCallback
  alignment _ = #alignment struct cpPostStepCallback

  peek ptr =
    PostStepCallback
      <$> peek (offset @"func" ptr)
      <*> peek (offset @"key"  ptr)
      <*> peek (offset @"data" ptr)

  poke ptr val = do
    pokeField @"func" ptr val
    pokeField @"key"  ptr val
    pokeField @"data" ptr val

-- end of chipmunk_structs.h

-- * chipmunk_unsafe.h

#include <chipmunk/chipmunk_unsafe.h>

foreign import CALLCV unsafe "chipmunk/chipmunk_unsafe.h cpCircleShapeSetRadius"
  circleShapeSetRadius
    :: Ptr Shape -- ^ shape
    -> Float     -- ^ radius
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpCircleShapeSetOffset"
  circleShapeSetOffset
    :: Ptr Shape -- ^ shape
    -> Ptr Vect  -- ^ offset
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpSegmentShapeSetEndpoints"
  segmentShapeSetEndpoints
    :: Ptr Shape -- ^ shape
    -> Ptr Vect  -- ^ a
    -> Ptr Vect  -- ^ b
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk_unsafe.h cpSegmentShapeSetRadius"
  segmentShapeSetRadius
    :: Ptr Shape -- ^ shape
    -> Float     -- ^ radius
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpPolyShapeSetVerts"
  polyShapeSetVerts
    :: Ptr Shape     -- ^ shape
    -> #{type int}   -- ^ count
    -> Ptr Vect      -- ^ verts
    -> Ptr Transform -- ^ transform
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk_unsafe.h cpPolyShapeSetVertsRaw"
  polyShapeSetVertsRaw
    :: Ptr Shape   -- ^ shape
    -> #{type int} -- ^ count
    -> Ptr Vect    -- ^ verts
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk_unsafe.h cpPolyShapeSetRadius"
  polyShapeSetRadius
    :: Ptr Shape -- ^ shape
    -> Float     -- ^ radius
    -> IO ()

-- end of chipmunk_unsafe.h
