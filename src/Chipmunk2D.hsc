{-# LANGUAGE CApiFFI
           , CPP
           , DataKinds
           , DuplicateRecordFields
           , EmptyDataDecls
           , FlexibleInstances
           , ForeignFunctionInterface
           , MultiParamTypeClasses #-}
#if __GLASGOW_HASKELL__ >= 902
{-# LANGUAGE NoFieldSelectors #-}
#endif
{-# LANGUAGE PatternSynonyms
           , TypeApplications #-}

{-| Documentation for these imports can be found on the Chipmunk2D
    [website](https://chipmunk-physics.net/documentation.php).

    This module exports a lot of 'FunPtr's, some of them are impossible to marshal
    using @\"wrapper\"@. This is not an oversight: a generic conversion is not possible
    for those functions as it would require C to support closures. It is thus
    on the user of this library to create their own C functions that pass
    extra data through the relevant user pointer.

    Except for 'CpSpace' creation\/destruction\/step and query functions, every function
    in this module is marshalled @unsafe@. 
 -}

module Chipmunk2D where

import           Data.Int
import           Data.Word
import           Foreign.Ptr
import           Foreign.Storable
import           Foreign.Storable.Offset
import           GHC.Records

#include <chipmunk/chipmunk.h>

-- * chipmunk_types.h

type CpFloat = #{type cpFloat}

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpfsqrt"
  cpfsqrt :: CpFloat -> IO CpFloat

foreign import CALLCV unsafe "chipmunk/chipmunk_types.h cpfsin"
  cpfsin :: CpFloat -> IO CpFloat

foreign import CALLCV unsafe "chipmunk/chipmunk_types.h cpfcos"
  cpfcos :: CpFloat -> IO CpFloat

foreign import CALLCV unsafe "chipmunk/chipmunk_types.h cpfacos"
  cpfacos :: CpFloat -> IO CpFloat

foreign import CALLCV unsafe "chipmunk/chipmunk_types.h cpfatan2"
  cpfatan2 :: CpFloat -> CpFloat -> IO CpFloat

foreign import CALLCV unsafe "chipmunk/chipmunk_types.h cpfmod"
  cpfmod :: CpFloat -> CpFloat -> IO CpFloat

foreign import CALLCV unsafe "chipmunk/chipmunk_types.h cpfexp"
  cpfexp :: CpFloat -> IO CpFloat

foreign import CALLCV unsafe "chipmunk/chipmunk_types.h cpfpow"
  cpfpow :: CpFloat -> CpFloat -> IO CpFloat

foreign import CALLCV unsafe "chipmunk/chipmunk_types.h cpffloor"
  cpffloor :: CpFloat -> IO CpFloat

foreign import CALLCV unsafe "chipmunk/chipmunk_types.h cpfceil"
  cpfceil :: CpFloat -> IO CpFloat

pattern INFINITY
      , CP_PI
     :: (Eq a, Fractional a) => a
pattern INFINITY = #const INFINITY
pattern CP_PI    = #const CP_PI

foreign import CALLCV unsafe "chipmunk/chipmunk_types.h cpfmax"
  cpfmax
    :: CpFloat    -- ^ a
    -> CpFloat    -- ^ b
    -> IO CpFloat

foreign import CALLCV unsafe "chipmunk/chipmunk_types.h cpfmin"
  cpfmin
    :: CpFloat    -- ^ a
    -> CpFloat    -- ^ b
    -> IO CpFloat

foreign import CALLCV unsafe "chipmunk/chipmunk_types.h cpfabs"
  cpfabs
    :: CpFloat    -- ^ f
    -> IO CpFloat

foreign import CALLCV unsafe "chipmunk/chipmunk_types.h cpfclamp"
  cpfclamp
    :: CpFloat    -- ^ f
    -> CpFloat    -- ^ min
    -> CpFloat    -- ^ max
    -> IO CpFloat

foreign import CALLCV unsafe "chipmunk/chipmunk_types.h cpfclamp01"
  cpfclamp01
    :: CpFloat    -- ^ f
    -> IO CpFloat

foreign import CALLCV unsafe "chipmunk/chipmunk_types.h cpflerp"
  cpflerp
    :: CpFloat    -- ^ f1
    -> CpFloat    -- ^ f2
    -> CpFloat    -- ^ t
    -> IO CpFloat

foreign import CALLCV unsafe "chipmunk/chipmunk_types.h cpflerpconst"
  cpflerpconst
    :: CpFloat    -- ^ f1
    -> CpFloat    -- ^ f2
    -> CpFloat    -- ^ d
    -> IO CpFloat

type CpHashValue = #type cpHashValue

type CpCollisionID = #type cpCollisionID

type CpBool = #type cpBool

pattern CpTrue
      , CpFalse
     :: (Eq a, Num a) => a
pattern CpTrue  = #const cpTrue
pattern CpFalse = #const cpFalse

type CpDataPointer = Ptr ()

type CpCollisionType = #type cpCollisionType

type CpGroup = #type cpGroup

type CpBitmask = #type cpBitmask

type CpTimestamp = #type cpTimestamp

pattern CP_NO_GROUP
      , CP_NO_ALL_CATEGORIES
      , CP_WILDCARD_COLLISION_TYPE
     :: (Eq a, Num a) => a
pattern CP_NO_GROUP                = #const CP_NO_GROUP
pattern CP_NO_ALL_CATEGORIES       = #const CP_ALL_CATEGORIES
pattern CP_WILDCARD_COLLISION_TYPE = #const CP_WILDCARD_COLLISION_TYPE

data {-# CTYPE "chipmunk/chipmunk.h" "cpVect" #-} CpVect =
       CpVect
         { x :: CpFloat
         , y :: CpFloat
         }

instance Offset "x" CpVect where rawOffset = #offset struct cpVect, x
instance Offset "y" CpVect where rawOffset = #offset struct cpVect, y

instance Storable CpVect where
  sizeOf _    = #size struct cpVect
  alignment _ = #alignment struct cpVect

  peek ptr =
    CpVect
      <$> peek (offset @"x" ptr)
      <*> peek (offset @"y" ptr)

  poke ptr val = do
    pokeField @"x" ptr val
    pokeField @"y" ptr val

data {-# CTYPE "chipmunk/chipmunk.h" "cpTransform" #-} CpTransform =
       CpTransform
         { a  :: CpFloat
         , b  :: CpFloat
         , c  :: CpFloat
         , d  :: CpFloat
         , tx :: CpFloat
         , ty :: CpFloat
         }

instance Offset "a"  CpTransform where rawOffset = #offset struct cpTransform, a
instance Offset "b"  CpTransform where rawOffset = #offset struct cpTransform, b
instance Offset "c"  CpTransform where rawOffset = #offset struct cpTransform, c
instance Offset "d"  CpTransform where rawOffset = #offset struct cpTransform, d
instance Offset "tx" CpTransform where rawOffset = #offset struct cpTransform, tx
instance Offset "ty" CpTransform where rawOffset = #offset struct cpTransform, ty

instance Storable CpTransform where
  sizeOf _    = #size struct cpTransform
  alignment _ = #alignment struct cpTransform

  peek ptr =
    CpTransform
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

data {-# CTYPE "chipmunk/chipmunk.h" "cpMat2x2" #-} CpMat2x2 =
       CpMat2x2
         { a  :: CpFloat
         , b  :: CpFloat
         , c  :: CpFloat
         , d  :: CpFloat
         }

instance Offset "a"  CpMat2x2 where rawOffset = #offset struct cpMat2x2, a
instance Offset "b"  CpMat2x2 where rawOffset = #offset struct cpMat2x2, b
instance Offset "c"  CpMat2x2 where rawOffset = #offset struct cpMat2x2, c
instance Offset "d"  CpMat2x2 where rawOffset = #offset struct cpMat2x2, d

instance Storable CpMat2x2 where
  sizeOf _    = #size struct cpMat2x2
  alignment _ = #alignment struct cpMat2x2

  peek ptr =
    CpMat2x2
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

pattern CP_BUFFER_BYTES :: (Eq a, Num a) => a
pattern CP_BUFFER_BYTES = #const CP_BUFFER_BYTES

data {-# CTYPE "chipmunk/chipmunk.h" "cpHashSet" #-} CpHashSet
{-
data {-# CTYPE "chipmunk/chipmunk.h" "cpBody" #-} CpBody

data {-# CTYPE "chipmunk/chipmunk.h" "cpShape" #-} CpShape
data {-# CTYPE "chipmunk/chipmunk.h" "cpCircleShape" #-} CpCircleShape
data {-# CTYPE "chipmunk/chipmunk.h" "cpSegmentShape" #-} CpSegmentShape
data {-# CTYPE "chipmunk/chipmunk.h" "cpPolyShape" #-} CpPolyShape

data {-# CTYPE "chipmunk/chipmunk.h" "cpConstraint" #-} CpConstraint
data {-# CTYPE "chipmunk/chipmunk.h" "cpPinJoint" #-} CpPinJoint
data {-# CTYPE "chipmunk/chipmunk.h" "cpSlideJoint" #-} CpSlideJoint
data {-# CTYPE "chipmunk/chipmunk.h" "cpPivotJoint" #-} CpPivotJoint
data {-# CTYPE "chipmunk/chipmunk.h" "cpGrooveJoint" #-} CpGrooveJoint
data {-# CTYPE "chipmunk/chipmunk.h" "cpDampedSpring" #-} CpDampedSpring
data {-# CTYPE "chipmunk/chipmunk.h" "cpDampedRotarySpring" #-} CpDampedRotarySpring
data {-# CTYPE "chipmunk/chipmunk.h" "cpRotaryLimitJoint" #-} CpRotaryLimitJoint
data {-# CTYPE "chipmunk/chipmunk.h" "cpRatchetJoint" #-} CpRatchetJoint
data {-# CTYPE "chipmunk/chipmunk.h" "cpGearJoint" #-} CpGearJoint -}
data {-# CTYPE "chipmunk/chipmunk.h" "cpSimpleMotorJoint" #-} CpSimpleMotorJoint
{-
data {-# CTYPE "chipmunk/chipmunk.h" "cpArbiter" #-} CpArbiter

data {-# CTYPE "chipmunk/chipmunk.h" "cpSpace" #-} CpSpace
-}
-- * cpVect.h

foreign import CALLCV unsafe "wrapper.h w_cpvzero"
  cpvzero
    :: Ptr CpVect -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpv"
  cpv
    :: CpFloat    -- ^ x
    -> CpFloat    -- ^ y
    -> Ptr CpVect -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpveql"
  cpveql
    :: Ptr CpVect -- ^ v1
    -> Ptr CpVect -- ^ v2
    -> IO CpBool

foreign import CALLCV unsafe "wrapper.h w_cpvadd"
  cpvadd
    :: Ptr CpVect -- ^ v1
    -> Ptr CpVect -- ^ v2
    -> Ptr CpVect -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpvsub"
  cpvsub
    :: Ptr CpVect -- ^ v1
    -> Ptr CpVect -- ^ v2
    -> Ptr CpVect -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpvneg"
  cpvneg
    :: Ptr CpVect -- ^ v
    -> Ptr CpVect -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpvmult"
  cpvmult
    :: Ptr CpVect -- ^ v
    -> CpFloat    -- ^ s
    -> Ptr CpVect -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpvdot"
  cpvdot
    :: Ptr CpVect -- ^ v1
    -> Ptr CpVect -- ^ v2
    -> IO CpFloat

foreign import CALLCV unsafe "wrapper.h w_cpvcross"
  cpvcross
    :: Ptr CpVect -- ^ v1
    -> Ptr CpVect -- ^ v2
    -> IO CpFloat

foreign import CALLCV unsafe "wrapper.h w_cpvperp"
  cpvperp
    :: Ptr CpVect -- ^ v
    -> Ptr CpVect -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpvrperp"
  cpvrperp
    :: Ptr CpVect -- ^ v
    -> Ptr CpVect -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpvproject"
  cpvproject
    :: Ptr CpVect -- ^ v1
    -> Ptr CpVect -- ^ v2
    -> Ptr CpVect -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpvforangle"
  cpvforangle
    :: CpFloat    -- ^ a
    -> Ptr CpVect -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpvtoangle"
  cpvtoangle
    :: Ptr CpVect -- ^ v
    -> IO CpFloat

foreign import CALLCV unsafe "wrapper.h w_cpvrotate"
  cpvrotate
    :: Ptr CpVect -- ^ v1
    -> Ptr CpVect -- ^ v2
    -> Ptr CpVect -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpvunrotate"
  cpvunrotate
    :: Ptr CpVect -- ^ v1
    -> Ptr CpVect -- ^ v2
    -> Ptr CpVect -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpvlengthsq"
  cpvlengthsq
    :: Ptr CpVect -- ^ v
    -> IO CpFloat

foreign import CALLCV unsafe "wrapper.h w_cpvlength"
  cpvlength
    :: Ptr CpVect -- ^ v
    -> IO CpFloat

foreign import CALLCV unsafe "wrapper.h w_cpvlerp"
  cpvlerp
    :: Ptr CpVect -- ^ v1
    -> Ptr CpVect -- ^ v2
    -> CpFloat    -- ^ t
    -> Ptr CpVect -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpvnormalize"
  cpvnormalize
    :: Ptr CpVect -- ^ v
    -> Ptr CpVect -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpvslerp"
  cpvslerp
    :: Ptr CpVect -- ^ v1
    -> Ptr CpVect -- ^ v2
    -> CpFloat    -- ^ t
    -> Ptr CpVect -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpvslerpconst"
  cpvslerpconst
    :: Ptr CpVect -- ^ v1
    -> Ptr CpVect -- ^ v2
    -> CpFloat    -- ^ a
    -> Ptr CpVect -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpvclamp"
  cpvclamp
    :: Ptr CpVect -- ^ v
    -> CpFloat    -- ^ len
    -> Ptr CpVect -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpvlerpconst"
  cpvlerpconst
    :: Ptr CpVect -- ^ v1
    -> Ptr CpVect -- ^ v2
    -> CpFloat    -- ^ d
    -> Ptr CpVect -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpvdist"
  cpvdist
    :: Ptr CpVect -- ^ v1
    -> Ptr CpVect -- ^ v2
    -> IO CpFloat

foreign import CALLCV unsafe "wrapper.h w_cpvdistsq"
  cpvdistsq
    :: Ptr CpVect -- ^ v1
    -> Ptr CpVect -- ^ v2
    -> IO CpFloat

foreign import CALLCV unsafe "wrapper.h w_cpvnear"
  cpvnear
    :: Ptr CpVect -- ^ v1
    -> Ptr CpVect -- ^ v2
    -> CpFloat    -- ^ dist
    -> IO CpFloat



foreign import CALLCV unsafe "wrapper.h w_cpMat2x2New"
  cpMat2x2New
    :: CpFloat      -- ^ a
    -> CpFloat      -- ^ b
    -> CpFloat      -- ^ c
    -> CpFloat      -- ^ d
    -> Ptr CpMat2x2 -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpMat2x2Transform"
  cpMat2x2Transform
    :: Ptr CpMat2x2 -- ^ m
    -> Ptr CpVect   -- ^ m
    -> Ptr CpVect   -- ^ Output value
    -> IO ()

-- end of cpVect.h

-- * cpBB.h

data {-# CTYPE "chipmunk/chipmunk.h" "cpBB" #-} CpBB =
       CpBB
         { l :: CpFloat
         , b :: CpFloat
         , r :: CpFloat
         , t :: CpFloat
         }

instance Offset "l" CpBB where rawOffset = #offset struct cpBB, l
instance Offset "b" CpBB where rawOffset = #offset struct cpBB, b
instance Offset "r" CpBB where rawOffset = #offset struct cpBB, r
instance Offset "t" CpBB where rawOffset = #offset struct cpBB, t

instance Storable CpBB where
  sizeOf _    = #size struct cpBB
  alignment _ = #alignment struct cpBB

  peek ptr =
    CpBB
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
  cpBBNew
    :: CpFloat  -- ^ l
    -> CpFloat  -- ^ b
    -> CpFloat  -- ^ r
    -> CpFloat  -- ^ t
    -> Ptr CpBB -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpBBNewForExtents"
  cpBBNewForExtents
    :: Ptr CpVect -- ^ c
    -> CpFloat    -- ^ hw
    -> CpFloat    -- ^ hh
    -> Ptr CpBB   -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpBBNewForCircle"
  cpBBNewForCircle
    :: Ptr CpVect -- ^ p
    -> CpFloat    -- ^ r
    -> Ptr CpBB   -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpBBIntersects"
  cpBBIntersects
    :: Ptr CpBB  -- ^ a
    -> Ptr CpBB  -- ^ b
    -> IO CpBool

foreign import CALLCV unsafe "wrapper.h w_cpBBContainsBB"
  cpBBContainsBB
    :: Ptr CpBB  -- ^ bba
    -> Ptr CpBB  -- ^ other
    -> IO CpBool

foreign import CALLCV unsafe "wrapper.h w_cpBBContainsVect"
  cpBBContainsVect
    :: Ptr CpBB   -- ^ bb
    -> Ptr CpVect -- ^ v
    -> IO CpBool

foreign import CALLCV unsafe "wrapper.h w_cpBBMerge"
  cpBBMerge
    :: Ptr CpBB -- ^ a
    -> Ptr CpBB -- ^ b
    -> Ptr CpBB -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpBBExpand"
  cpBBExpand
    :: Ptr CpBB   -- ^ bb
    -> Ptr CpVect -- ^ v
    -> Ptr CpBB   -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpBBCenter"
  cpBBCenter
    :: Ptr CpBB   -- ^ bb
    -> Ptr CpVect -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpBBArea"
  cpBBArea
    :: Ptr CpBB   -- ^ bb
    -> IO CpFloat

foreign import CALLCV unsafe "wrapper.h w_cpBBMergedArea"
  cpBBMergedArea
    :: Ptr CpBB   -- ^ a
    -> Ptr CpBB   -- ^ b
    -> IO CpFloat

foreign import CALLCV unsafe "wrapper.h w_cpBBSegmentQuery"
  cpBBSegmentQuery
    :: Ptr CpBB   -- ^ bb
    -> Ptr CpVect -- ^ a
    -> Ptr CpVect -- ^ b
    -> IO CpFloat

foreign import CALLCV unsafe "wrapper.h w_cpBBIntersectsSegment"
  cpBBIntersectsSegment
    :: Ptr CpBB   -- ^ bb
    -> Ptr CpVect -- ^ a
    -> Ptr CpVect -- ^ b
    -> IO CpBool

foreign import CALLCV unsafe "wrapper.h w_cpBBClampVect"
  cpBBClampVect
    :: Ptr CpBB   -- ^ bb
    -> Ptr CpVect -- ^ v
    -> Ptr CpVect -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpBBWrapVect"
  cpBBWrapVect
    :: Ptr CpBB   -- ^ bb
    -> Ptr CpVect -- ^ v
    -> Ptr CpVect -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpBBOffset"
  cpBBOffset
    :: Ptr cpBB   -- ^ bb
    -> Ptr cpVect -- ^ v
    -> Ptr cpBB   -- ^ cpBB
    -> IO ()

-- end of cpBB.h

-- * cpTransform.h

foreign import CALLCV unsafe "wrapper.h w_cpTransformIdentity"
  cpTransformIdentity
    :: Ptr CpTransform -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpTransformNew"
  cpTransformNew
    :: CpFloat         -- ^ a
    -> CpFloat         -- ^ b
    -> CpFloat         -- ^ c
    -> CpFloat         -- ^ d
    -> CpFloat         -- ^ tx
    -> CpFloat         -- ^ ty
    -> Ptr CpTransform -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpTransformNewTranspose"
  cpTransformNewTranspose
    :: CpFloat         -- ^ a
    -> CpFloat         -- ^ c
    -> CpFloat         -- ^ tx
    -> CpFloat         -- ^ b
    -> CpFloat         -- ^ d
    -> CpFloat         -- ^ ty
    -> Ptr CpTransform -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpTransformInverse"
  cpTransformInverse
    :: Ptr CpTransform -- ^ t
    -> Ptr CpTransform -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpTransformMult"
  cpTransformMult
    :: Ptr CpTransform -- ^ t1
    -> Ptr CpTransform -- ^ t2
    -> Ptr CpTransform -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpTransformPoint"
  cpTransformPoint
    :: Ptr CpTransform -- ^ t
    -> Ptr CpVect      -- ^ p
    -> Ptr CpVect      -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpTransformVect"
  cpTransformVect
    :: Ptr CpTransform -- ^ t
    -> Ptr CpVect      -- ^ v
    -> Ptr CpVect      -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpTransformbBB"
  cpTransformbBB
    :: Ptr CpTransform -- ^ t
    -> Ptr CpBB        -- ^ bb
    -> Ptr CpBB        -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpTransformTranslate"
  cpTransformTranslate
    :: Ptr CpVect      -- ^ translate
    -> Ptr CpTransform -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpTransformScale"
  cpTransformScale
    :: CpFloat         -- ^ scaleX
    -> CpFloat         -- ^ scaleY
    -> Ptr CpTransform -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpTransformRotate"
  cpTransformRotate
    :: CpFloat         -- ^ radians
    -> Ptr CpTransform -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpTransformRigid"
  cpTransformRigid
    :: Ptr CpVect      -- ^ translate
    -> CpFloat         -- ^ radians
    -> Ptr CpTransform -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpTransformRigidInverse"
  cpTransformRigidInverse
    :: Ptr CpTransform -- ^ t
    -> Ptr CpTransform -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpTransformWrap"
  cpTransformWrap
    :: Ptr CpTransform -- ^ Output valueer
    -> Ptr CpTransform -- ^ inner
    -> Ptr CpTransform -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpTransformWrapInverse"
  cpTransformWrapInverse
    :: Ptr CpTransform -- ^ Output valueer
    -> Ptr CpTransform -- ^ inner
    -> Ptr CpTransform -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpTransformOrtho"
  cpTransformOrtho
    :: Ptr CpBB        -- ^ bb
    -> Ptr CpTransform -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpTransformBoneScale"
  cpTransformBoneScale
    :: Ptr CpVect      -- ^ v0
    -> Ptr CpVect      -- ^ v1
    -> Ptr CpTransform -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpTransformAxialScale"
  cpTransformAxialScale
    :: Ptr CpVect      -- ^ axis
    -> Ptr CpVect      -- ^ pivot
    -> CpFloat         -- ^ scale
    -> Ptr CpTransform -- ^ Output value
    -> IO ()

-- end of cpTransform.h

-- * cpSpatialIndex.h

type CpSpatialIndexBBFunc =
          Ptr () -- ^ obj
       -> IO CpBB

type CpSpatialIndexIteratorFunc =
          Ptr () -- ^ obj
       -> Ptr () -- ^ data
       -> IO ()

type CpSpatialIndexQueryFunc =
          Ptr ()        -- ^ obj1
       -> Ptr ()        -- ^ obj2
       -> CpCollisionID -- ^ id
       -> Ptr ()        -- ^ data
       -> IO CpCollisionID

type CpSpatialIndexSegmentQueryFunc =
          Ptr ()         -- ^ obj1
       -> Ptr ()         -- ^ obj2
       -> Ptr ()         -- ^ data
       -> IO CpFloat

data {-# CTYPE "chipmunk/chipmunk.h" "cpSpatialIndex" #-} CpSpatialIndex

data {-# CTYPE "chipmunk/chipmunk.h" "cpSpaceHash" #-} CpSpaceHash

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSpaceHashAlloc"
  cpSpaceHashAlloc :: IO (Ptr CpSpaceHash)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSpaceHashInit"
  cpSpaceHashInit
    :: Ptr CpSpaceHash             -- ^ hash
    -> CpFloat                     -- ^ celldim
    -> #{type int}                 -- ^ numcells
    -> FunPtr CpSpatialIndexBBFunc -- ^ bbfunc
    -> Ptr CpSpatialIndex          -- ^ staticIndex
    -> IO (Ptr CpSpatialIndex)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSpaceHashNew"
  cpSpaceHashNew
    :: CpFloat                     -- ^ celldim
    -> #{type int}                 -- ^ numcells
    -> FunPtr CpSpatialIndexBBFunc -- ^ bbfunc
    -> Ptr CpSpatialIndex          -- ^ staticIndex
    -> IO (Ptr CpSpatialIndex)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSpaceHashResize"
  cpSpaceHashResize
    :: Ptr CpSpaceHash -- ^ hash
    -> CpFloat         -- ^ celldim
    -> #{type int}     -- ^ numcells
    -> IO ()

data {-# CTYPE "chipmunk/chipmunk.h" "cpBBTree" #-} CpBBTree

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpBBTreeAlloc"
  cpCpBBTreeAlloc
    :: IO (Ptr CpBBTree)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpBBTreeInit"
  cpCpBBTreeInit
    :: Ptr CpBBTree                -- ^ tree
    -> FunPtr CpSpatialIndexBBFunc -- ^ bbfunc
    -> Ptr CpSpatialIndex          -- ^ staticIndex
    -> IO (Ptr CpSpatialIndex)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpBBTreeNew"
  cpCpBBTreeNew
    :: FunPtr CpSpatialIndexBBFunc -- ^ bbfunc
    -> Ptr CpSpatialIndex          -- ^ staticIndex
    -> IO (Ptr CpSpatialIndex)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpBBTreeOptimize"
  cpCpBBTreeOptimize
    :: Ptr CpSpatialIndex -- ^ index
    -> IO ()

type CpBBTreeVelocityFunc =
          Ptr () -- ^ obj
       -> IO CpVect

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpBBTreeSetVelocityFunc"
  cpCpBBTreeSetVelocityFunc
    :: Ptr CpSpatialIndex          -- ^ index
    -> FunPtr CpBBTreeVelocityFunc -- ^ func
    -> IO ()

data {-# CTYPE "chipmunk/chipmunk.h" "cpSweep1D" #-} CpSweep1D

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSweep1DAlloc"
  cpSweep1DAlloc
    :: IO (Ptr CpSweep1D)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSweep1DInit"
  cpSweep1DInit
    :: Ptr CpSweep1D               -- ^ sweep
    -> FunPtr CpSpatialIndexBBFunc -- ^ bbfunc
    -> Ptr CpSpatialIndex          -- ^ staticIndex
    -> IO (Ptr CpSpatialIndex)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSweep1DNew"
  cpSweep1DNew
    :: FunPtr CpSpatialIndexBBFunc -- ^ bbfunc
    -> Ptr CpSpatialIndex          -- ^ staticIndex
    -> IO (Ptr CpSpatialIndex)

type CpSpatialIndexDestroyImpl =
          Ptr CpSpatialIndex -- ^ index
       -> IO ()

type CpSpatialIndexCountImpl =
          Ptr CpSpatialIndex -- ^ index
       -> IO #type int

type CpSpatialIndexEachImpl =
          Ptr CpSpatialIndex                -- ^ index
       -> FunPtr CpSpatialIndexIteratorFunc -- ^ func
       -> Ptr ()                          -- ^ data
       -> IO ()

type CpSpatialIndexContainsImpl =
          Ptr CpSpatialIndex -- ^ index
       -> Ptr ()           -- ^ obj
       -> CpHashValue        -- ^ hashid
       -> IO CpBool

type CpSpatialIndexInsertImpl =
          Ptr CpSpatialIndex -- ^ index
       -> Ptr ()           -- ^ obj
       -> CpHashValue        -- ^ hashid
       -> IO ()

type CpSpatialIndexRemoveImpl =
          Ptr CpSpatialIndex -- ^ index
       -> Ptr ()           -- ^ obj
       -> CpHashValue        -- ^ hashid
       -> IO ()

type CpSpatialIndexReindexImpl =
          Ptr CpSpatialIndex -- ^ index
       -> IO ()

type CpSpatialIndexReindexObjectImpl =
          Ptr CpSpatialIndex -- ^ index
       -> Ptr ()           -- ^ obj
       -> CpHashValue        -- ^ hashid
       -> IO ()

type CpSpatialIndexReindexQueryImpl =
          Ptr CpSpatialIndex             -- ^ index
       -> FunPtr CpSpatialIndexQueryFunc -- ^ func
       -> Ptr ()                       -- ^ data
       -> IO ()

type CpSpatialIndexQueryImpl =
          Ptr CpSpatialIndex             -- ^ index
       -> Ptr ()                       -- ^ obj
       -> CpBB                           -- ^ bb
       -> FunPtr CpSpatialIndexQueryFunc -- ^ func
       -> Ptr ()                       -- ^ data
       -> IO ()

type CpSpatialIndexSegmentQueryImpl =
          Ptr CpSpatialIndex                    -- ^ index
       -> Ptr ()                              -- ^ obj
       -> CpVect                                -- ^ a
       -> CpVect                                -- ^ b
       -> CpFloat                               -- ^ t_exit
       -> FunPtr CpSpatialIndexSegmentQueryFunc -- ^ func
       -> Ptr ()                              -- ^ data
       -> IO ()

data {-# CTYPE "chipmunk/chipmunk.h" "cpSpatialIndexClass" #-} CpSpatialIndexClass =
       CpSpatialIndexClass
         { destroy       :: FunPtr CpSpatialIndexDestroyImpl
         , count         :: FunPtr CpSpatialIndexCountImpl
         , each          :: FunPtr CpSpatialIndexEachImpl
         , contains      :: FunPtr CpSpatialIndexContainsImpl
         , insert        :: FunPtr CpSpatialIndexInsertImpl
         , remove        :: FunPtr CpSpatialIndexRemoveImpl
         , reindex       :: FunPtr CpSpatialIndexReindexImpl
         , reindexObject :: FunPtr CpSpatialIndexReindexObjectImpl
         , reindexQuery  :: FunPtr CpSpatialIndexReindexQueryImpl
         , query         :: FunPtr CpSpatialIndexQueryImpl
         , segmentQuery  :: FunPtr CpSpatialIndexSegmentQueryImpl
         }

instance Offset "destroy"       CpSpatialIndexClass where rawOffset = #offset struct cpSpatialIndexClass, destroy
instance Offset "count"         CpSpatialIndexClass where rawOffset = #offset struct cpSpatialIndexClass, count
instance Offset "each"          CpSpatialIndexClass where rawOffset = #offset struct cpSpatialIndexClass, each
instance Offset "contains"      CpSpatialIndexClass where rawOffset = #offset struct cpSpatialIndexClass, contains
instance Offset "insert"        CpSpatialIndexClass where rawOffset = #offset struct cpSpatialIndexClass, insert
instance Offset "remove"        CpSpatialIndexClass where rawOffset = #offset struct cpSpatialIndexClass, remove
instance Offset "reindex"       CpSpatialIndexClass where rawOffset = #offset struct cpSpatialIndexClass, reindex
instance Offset "reindexObject" CpSpatialIndexClass where rawOffset = #offset struct cpSpatialIndexClass, reindexObject
instance Offset "reindexQuery"  CpSpatialIndexClass where rawOffset = #offset struct cpSpatialIndexClass, reindexQuery
instance Offset "query"         CpSpatialIndexClass where rawOffset = #offset struct cpSpatialIndexClass, query
instance Offset "segmentQuery"  CpSpatialIndexClass where rawOffset = #offset struct cpSpatialIndexClass, segmentQuery

instance Storable CpSpatialIndexClass where
  sizeOf _    = #size      struct cpSpatialIndexClass
  alignment _ = #alignment struct cpSpatialIndexClass

  peek ptr =
    CpSpatialIndexClass
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
  cpSpatialIndexFree
    :: Ptr cpSpatialIndex -- ^ index
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSpatialIndexCollideStatic"
  cpSpatialIndexCollideStatic
    :: Ptr CpSpatialIndex             -- ^ dynamicIndex
    -> Ptr CpSpatialIndex             -- ^ staticIndex
    -> FunPtr CpSpatialIndexQueryFunc -- ^ func
    -> Ptr ()                         -- ^ data
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSpatialIndexDestroy"
  cpSpatialIndexDestroy
    :: Ptr CpSpatialIndex -- ^ index
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSpatialIndexCount"
  cpSpatialIndexCount
    :: Ptr cpSpatialIndex -- ^ index
    -> IO #type int

foreign import CALLCV "chipmunk/chipmunk.h cpSpatialIndexEach"
  cpSpatialIndexEach
    :: Ptr CpSpatialIndex                -- ^ index
    -> FunPtr CpSpatialIndexIteratorFunc -- ^ func
    -> Ptr ()                            -- ^ data
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSpatialIndexContains"
  cpSpatialIndexContains
    :: Ptr CpSpatialIndex -- ^ index
    -> Ptr ()             -- ^ obj
    -> CpHashValue        -- ^ hashid
    -> IO CpBool

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSpatialIndexInsert"
  cpSpatialIndexInsert
    :: Ptr CpSpatialIndex -- ^ index
    -> Ptr ()             -- ^ obj
    -> CpHashValue        -- ^ hashid
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSpatialIndexRemove"
  cpSpatialIndexRemove
    :: Ptr CpSpatialIndex -- ^ index
    -> Ptr ()             -- ^ obj
    -> CpHashValue        -- ^ hashid
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSpatialIndexReindex"
  cpSpatialIndexReindex
    :: Ptr CpSpatialIndex -- ^ index
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSpatialIndexReindexObject"
  cpSpatialIndexReindexObject
    :: Ptr CpSpatialIndex -- ^ index
    -> Ptr ()             -- ^ obj
    -> CpHashValue        -- ^ hashid
    -> IO ()

foreign import CALLCV "wrapper.h w_cpSpatialIndexQuery"
  cpSpatialIndexQuery
    :: Ptr CpSpatialIndex             -- ^ *index
    -> Ptr ()                         -- ^ obj
    -> Ptr CpBB                       -- ^ bb
    -> FunPtr CpSpatialIndexQueryFunc -- ^ func
    -> Ptr ()                         -- ^ data
    -> IO ()

foreign import CALLCV "wrapper.h w_cpSpatialIndexSegmentQuery"
  cpSpatialIndexSegmentQuery
    :: Ptr CpSpatialIndex                    -- ^ index
    -> Ptr ()                                -- ^ obj
    -> Ptr CpVect                            -- ^ a
    -> Ptr CpVect                            -- ^ b
    -> CpFloat                               -- ^ t_exit
    -> FunPtr CpSpatialIndexSegmentQueryFunc -- ^ func
    -> Ptr ()                                -- ^ data
    -> IO ()

foreign import CALLCV "chipmunk/chipmunk.h cpSpatialIndexReindexQuery"
  cpSpatialIndexReindexQuery
    :: Ptr CpSpatialIndex             -- ^ index
    -> FunPtr CpSpatialIndexQueryFunc -- ^ func
    -> Ptr ()                         -- ^ data
    -> IO ()

-- end of cpSpatialIndex.h

-- * cpArbiter.h

pattern CP_MAX_CONTACTS_PER_ARBITER :: (Eq a, Num a) => a
pattern CP_MAX_CONTACTS_PER_ARBITER = #const CP_MAX_CONTACTS_PER_ARBITER

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpArbiterGetRestitution"
  cpArbiterGetRestitution
    :: Ptr CpArbiter -- ^ arb
    -> IO CpFloat

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpArbiterSetRestitution"
  cpArbiterSetRestitution
    :: Ptr CpArbiter -- ^ arb
    -> CpFloat       -- ^ restitution
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpArbiterGetFriction"
  cpArbiterGetFriction
    :: Ptr CpArbiter -- ^ arb
    -> IO CpFloat

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpArbiterSetFriction"
  cpArbiterSetFriction
    :: Ptr CpArbiter -- ^ arb
    -> CpFloat       -- ^ friction
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpArbiterGetSurfaceVelocity"
  cpArbiterGetSurfaceVelocity
    :: Ptr CpArbiter -- ^ arb
    -> Ptr CpVect    -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpArbiterSetSurfaceVelocity"
  cpArbiterSetSurfaceVelocity
    :: Ptr CpArbiter -- ^ arb
    -> Ptr CpVect    -- ^ vr
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpArbiterGetUserData"
  cpArbiterGetUserData
    :: Ptr CpArbiter -- ^ arb
    -> IO CpDataPointer

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpArbiterSetUserData"
  cpArbiterSetUserData
    :: Ptr CpArbiter -- ^ arb
    -> CpDataPointer -- ^ userData
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpArbiterTotalImpulse"
  cpArbiterTotalImpulse
    :: Ptr CpArbiter -- ^ arb
    -> Ptr CpVect    -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpArbiterTotalKE"
  cpArbiterTotalKE
    :: Ptr CpArbiter -- ^ arb
    -> IO CpFloat

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpArbiterIgnore"
  cpArbiterIgnore
    :: Ptr CpArbiter -- ^ arb
    -> IO CpBool

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpArbiterGetShapes"
  cpArbiterGetShapes
    :: Ptr CpArbiter     -- ^ arb
    -> Ptr (Ptr CpShape) -- ^ a
    -> Ptr (Ptr CpShape) -- ^ b
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpArbiterGetBodies"
  cpArbiterGetBodies
    :: Ptr CpArbiter    -- ^ arb
    -> Ptr (Ptr CpBody) -- ^ a
    -> Ptr (Ptr CpBody) -- ^ b
    -> IO ()

data {-# CTYPE "chipmunk/chipmunk.h" "cpContactPointSet" #-} CpContactPointSet =
       CpContactPointSet
         { count  :: #type int
         , normal :: CpVect
         , points :: Ptr CpContactPoint
         }

instance Offset "count"  CpContactPointSet where rawOffset = #offset struct cpContactPointSet, count
instance Offset "normal" CpContactPointSet where rawOffset = #offset struct cpContactPointSet, normal
instance Offset "points" CpContactPointSet where rawOffset = #offset struct cpContactPointSet, points

instance Storable CpContactPointSet where
  sizeOf _    = #size struct cpContactPointSet
  alignment _ = #alignment struct cpContactPointSet

  peek ptr =
    CpContactPointSet
      <$> peek (offset @"count"  ptr)
      <*> peek (offset @"normal" ptr)
      <*> peek (offset @"points" ptr)

  poke ptr val = do
    pokeField @"count"  ptr val
    pokeField @"normal" ptr val
    pokeField @"points" ptr val

data CpContactPoint =
       CpContactPoint
         { pointA   :: CpVect
         , pointB   :: CpVect
         , distance :: CpFloat
         }

instance Offset "pointA"   CpContactPoint where rawOffset = #offset struct cpContactPoint, pointA
instance Offset "pointB"   CpContactPoint where rawOffset = #offset struct cpContactPoint, pointB
instance Offset "distance" CpContactPoint where rawOffset = #offset struct cpContactPoint, distance

instance Storable CpContactPoint where
  sizeOf _    = #size struct cpContactPoint
  alignment _ = #alignment struct cpContactPoint

  peek ptr =
    CpContactPoint
      <$> peek (offset @"pointA"   ptr)
      <*> peek (offset @"pointB"   ptr)
      <*> peek (offset @"distance" ptr)

  poke ptr val = do
    pokeField @"pointA"   ptr val
    pokeField @"pointB"   ptr val
    pokeField @"distance" ptr val

foreign import CALLCV unsafe "wrapper.h w_cpArbiterGetContactPointSet"
  cpArbiterGetContactPointSet
    :: Ptr CpArbiter         -- ^ arb
    -> Ptr CpContactPointSet -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpArbiterSetContactPointSet"
  cpArbiterSetContactPointSet
    :: Ptr CpArbiter         -- ^ arb
    -> Ptr CpContactPointSet -- ^ set
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpArbiterIsFirstContact"
  cpArbiterIsFirstContact
    :: Ptr CpArbiter -- ^ arb
    -> IO CpBool

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpArbiterIsRemoval"
  cpArbiterIsRemoval
    :: Ptr CpArbiter -- ^ arb
    -> IO CpBool

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpArbiterGetCount"
  cpArbiterGetCount
    :: Ptr CpArbiter -- ^ arb
    -> IO #type int

foreign import CALLCV unsafe "wrapper.h w_cpArbiterGetNormal"
  cpArbiterGetNormal
    :: Ptr CpArbiter -- ^ arb
    -> Ptr CpVect    -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpArbiterGetPointA"
  cpArbiterGetPointA
    :: Ptr CpArbiter -- ^ arb
    -> #{type int}   -- ^ i
    -> Ptr CpVect    -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpArbiterGetPointB"
  cpArbiterGetPointB
    :: Ptr CpArbiter -- ^ arb
    -> #{type int}   -- ^ i
    -> Ptr CpVect    -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpArbiterGetDepth"
  cpArbiterGetDepth
    :: Ptr CpArbiter -- ^ arb
    -> #{type int}   -- ^ i
    -> IO CpFloat

foreign import CALLCV "chipmunk/chipmunk.h cpArbiterCallWildcardBeginA"
  cpArbiterCallWildcardBeginA
    :: Ptr CpArbiter -- ^ arb
    -> Ptr CpSpace   -- ^ space
    -> IO CpBool

foreign import CALLCV "chipmunk/chipmunk.h cpArbiterCallWildcardBeginB"
  cpArbiterCallWildcardBeginB
    :: Ptr CpArbiter -- ^ arb
    -> Ptr CpSpace   -- ^ space
    -> IO CpBool

foreign import CALLCV "chipmunk/chipmunk.h cpArbiterCallWildcardPreSolveA"
  cpArbiterCallWildcardPreSolveA
    :: Ptr CpArbiter -- ^ arb
    -> Ptr CpSpace   -- ^ space
    -> IO CpBool

foreign import CALLCV "chipmunk/chipmunk.h cpArbiterCallWildcardPreSolveB"
  cpArbiterCallWildcardPreSolveB
    :: Ptr CpArbiter -- ^ arb
    -> Ptr CpSpace   -- ^ space
    -> IO CpBool

foreign import CALLCV "chipmunk/chipmunk.h cpArbiterCallWildcardPostSolveA"
  cpArbiterCallWildcardPostSolveA
    :: Ptr CpArbiter -- ^ arb
    -> Ptr CpSpace   -- ^ space
    -> IO ()

foreign import CALLCV "chipmunk/chipmunk.h cpArbiterCallWildcardPostSolveB"
  cpArbiterCallWildcardPostSolveB
    :: Ptr CpArbiter -- ^ arb
    -> Ptr CpSpace   -- ^ space
    -> IO ()

foreign import CALLCV "chipmunk/chipmunk.h cpArbiterCallWildcardSeparateA"
  cpArbiterCallWildcardSeparateA
    :: Ptr CpArbiter -- ^ arb
    -> Ptr CpSpace   -- ^ space
    -> IO ()

foreign import CALLCV "chipmunk/chipmunk.h cpArbiterCallWildcardSeparateB"
  cpArbiterCallWildcardSeparateB
    :: Ptr CpArbiter -- ^ arb
    -> Ptr CpSpace   -- ^ space
    -> IO ()

-- end of cpArbiter.h

-- * cpBody.h

type CpBodyType = #type cpBodyType

pattern CP_BODY_TYPE_DYNAMIC
      , CP_BODY_TYPE_KINEMATIC
      , CP_BODY_TYPE_STATIC
     :: (Eq a, Num a) => a
pattern CP_BODY_TYPE_DYNAMIC   = #const CP_BODY_TYPE_DYNAMIC
pattern CP_BODY_TYPE_KINEMATIC = #const CP_BODY_TYPE_KINEMATIC
pattern CP_BODY_TYPE_STATIC    = #const CP_BODY_TYPE_STATIC

type CpBodyVelocityFunc =
          Ptr CpBody -- ^ body
       -> CpVect     -- ^ gravity
       -> CpFloat    -- ^ damping
       -> CpFloat    -- ^ dt
       -> IO ()

type CpBodyPositionFunc =
          Ptr CpBody -- ^ body
       -> CpFloat    -- ^ dt
       -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpBodyAlloc"
  cpBodyAlloc
    :: IO (Ptr CpBody)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpBodyInit"
  cpBodyInit
    :: Ptr CpBody      -- ^ body
    -> CpFloat         -- ^ mass
    -> CpFloat         -- ^ moment
    -> IO (Ptr CpBody)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpBodyNew"
  cpBodyNew
    :: CpFloat         -- ^ mass
    -> CpFloat         -- ^ moment
    -> IO (Ptr CpBody)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpBodyNewKinematic"
  cpBodyNewKinematic
    :: IO (Ptr CpBody)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpBodyNewStatic"
  cpBodyNewStatic
    :: IO (Ptr CpBody)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpBodyDestroy"
  cpBodyDestroy
    :: Ptr CpBody -- ^ body
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpBodyFree"
  cpBodyFree
    :: Ptr CpBody -- ^ body
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpBodyActivate"
  cpBodyActivate
    :: Ptr CpBody -- ^ body
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpBodyActivateStatic"
  cpBodyActivateStatic
    :: Ptr CpBody  -- ^ body
    -> Ptr CpShape -- ^ filter
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpBodySleep"
  cpBodySleep
    :: Ptr CpBody -- ^ body
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpBodySleepWithGroup"
  cpBodySleepWithGroup
    :: Ptr CpBody -- ^ body
    -> Ptr CpBody -- ^ group
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpBodyIsSleeping"
  cpBodyIsSleeping
    :: Ptr CpBody -- ^ body
    -> IO CpBool

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpBodyGetType"
  cpBodyGetType
    :: Ptr CpBody    -- ^ body
    -> IO CpBodyType

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpBodySetType"
  cpBodySetType
    :: Ptr CpBody -- ^ body
    -> CpBodyType -- ^ type
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpBodyGetSpace"
  cpBodyGetSpace
    :: Ptr CpBody       -- ^ body
    -> IO (Ptr CpSpace)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpBodyGetMass"
  cpBodyGetMass
    :: Ptr CpBody -- ^ body
    -> IO CpFloat

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpBodySetMass"
  cpBodySetMass
    :: Ptr CpBody -- ^ body
    -> CpFloat    -- ^ m
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpBodyGetMoment"
  cpBodyGetMoment
    :: Ptr CpBody -- ^ body
    -> IO CpFloat

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpBodySetMoment"
  cpBodySetMoment
    :: Ptr CpBody -- ^ body
    -> CpFloat    -- ^ i
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpBodyGetPosition"
  cpBodyGetPosition
    :: Ptr CpBody -- ^ body
    -> Ptr CpVect -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpBodySetPosition"
  cpBodySetPosition
    :: Ptr CpBody -- ^ body
    -> Ptr cpVect -- ^ pos
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpBodyGetCenterOfGravity"
  cpBodyGetCenterOfGravity
    :: Ptr CpBody -- ^ body
    -> Ptr CpVect -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpBodySetCenterOfGravity"
  cpBodySetCenterOfGravity
    :: Ptr CpBody -- ^ body
    -> Ptr CpVect -- ^ cog
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpBodyGetVelocity"
  cpBodyGetVelocity
    :: Ptr CpBody -- ^ body
    -> Ptr CpVect -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpBodySetVelocity"
  cpBodySetVelocity
    :: Ptr CpBody -- ^ body
    -> Ptr CpVect -- ^ velocity
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpBodyGetForce"
  cpBodyGetForce
    :: Ptr CpBody -- ^ body
    -> Ptr CpVect -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpBodySetForce"
  cpBodySetForce
    :: Ptr CpBody -- ^ body
    -> Ptr CpVect -- ^ force
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpBodyGetAngle"
  cpBodyGetAngle
    :: Ptr CpBody -- ^ body
    -> IO CpFloat

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpBodySetAngle"
  cpBodySetAngle
    :: Ptr CpBody -- ^ body
    -> CpFloat    -- ^ a
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpBodyGetAngularVelocity"
  cpBodyGetAngularVelocity
    :: Ptr CpBody -- ^ body
    -> IO CpFloat

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpBodySetAngularVelocity"
  cpBodySetAngularVelocity
    :: Ptr CpBody -- ^ body
    -> CpFloat    -- ^ angularVelocity
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpBodyGetTorque"
  cpBodyGetTorque
    :: Ptr CpBody -- ^ body
    -> IO CpFloat

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpBodySetTorque"
  cpBodySetTorque
    :: Ptr CpBody -- ^ body
    -> CpFloat    -- ^ torque
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpBodyGetRotation"
  cpBodyGetRotation
    :: Ptr CpBody -- ^ body
    -> Ptr CpVect -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpBodyGetUserData"
  cpBodyGetUserData
    :: Ptr CpBody -- ^ body
    -> IO CpDataPointer

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpBodySetUserData"
  cpBodySetUserData
    :: Ptr CpBody    -- ^ body
    -> CpDataPointer -- ^ userData
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpBodySetVelocityUpdateFunc"
  cpBodySetVelocityUpdateFunc
    :: Ptr CpBody                -- ^ body
    -> FunPtr CpBodyVelocityFunc -- ^ velocityFunc
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpBodySetPositionUpdateFunc"
  cpBodySetPositionUpdateFunc
    :: Ptr CpBody                -- ^ body
    -> FunPtr CpBodyPositionFunc -- ^ positionFunc
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpBodyUpdateVelocity"
  cpBodyUpdateVelocity
    :: Ptr CpBody -- ^ body
    -> Ptr CpVect -- ^ gravity
    -> CpFloat    -- ^ damping
    -> CpFloat    -- ^ dt
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpBodyUpdatePosition"
  cpBodyUpdatePosition
    :: Ptr CpBody -- ^ body
    -> CpFloat    -- ^ dt
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpBodyLocalToWorld"
  cpBodyLocalToWorld
    :: Ptr CpBody -- ^ body
    -> Ptr CpVect -- ^ point
    -> Ptr CpVect -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpBodyWorldToLocal"
  cpBodyWorldToLocal
    :: Ptr CpBody -- ^ body
    -> Ptr CpVect -- ^ point
    -> Ptr CpVect -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpBodyApplyForceAtWorldPoint"
  cpBodyApplyForceAtWorldPoint
    :: Ptr CpBody -- ^ body
    -> Ptr CpVect -- ^ force
    -> Ptr CpVect -- ^ point
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpBodyApplyForceAtLocalPoint"
  cpBodyApplyForceAtLocalPoint
    :: Ptr CpBody -- ^ body
    -> Ptr CpVect -- ^ force
    -> Ptr CpVect -- ^ point
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpBodyApplyImpulseAtWorldPoint"
  cpBodyApplyImpulseAtWorldPoint
    :: Ptr CpBody -- ^ body
    -> Ptr CpVect -- ^ impulse
    -> Ptr CpVect -- ^ point
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpBodyApplyImpulseAtLocalPoint"
  cpBodyApplyImpulseAtLocalPoint
    :: Ptr CpBody -- ^ body
    -> Ptr CpVect -- ^ impulse
    -> Ptr CpVect -- ^ point
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpBodyGetVelocityAtWorldPoint"
  cpBodyGetVelocityAtWorldPoint
    :: Ptr CpBody -- ^ body
    -> Ptr CpVect -- ^ point
    -> Ptr CpVect -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpBodyGetVelocityAtLocalPoint"
  cpBodyGetVelocityAtLocalPoint
    :: Ptr CpBody -- ^ body
    -> Ptr CpVect -- ^ point
    -> Ptr CpVect -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpBodyKineticEnergy"
  cpBodyKineticEnergy
    :: Ptr CpBody -- ^ body
    -> IO CpFloat

type CpBodyShapeIteratorFunc =
          Ptr CpBody  -- ^ body
       -> Ptr CpShape -- ^ shape
       -> Ptr ()      -- ^ data
       -> IO ()

foreign import CALLCV "chipmunk/chipmunk.h cpBodyEachShape"
  cpBodyEachShape
    :: Ptr CpBody                     -- ^ body
    -> FunPtr CpBodyShapeIteratorFunc -- ^ func
    -> Ptr ()                         -- ^ data
    -> IO ()

type CpBodyConstraintIteratorFunc =
          Ptr CpBody       -- ^ body
       -> Ptr CpConstraint -- ^ constraint
       -> Ptr ()           -- ^ data
       -> IO ()

foreign import CALLCV "chipmunk/chipmunk.h cpBodyEachConstraint"
  cpBodyEachConstraint
    :: Ptr CpBody                          -- ^ body
    -> FunPtr CpBodyConstraintIteratorFunc -- ^ func
    -> Ptr ()                              -- ^ data
    -> IO ()

type CpBodyArbiterIteratorFunc =
          Ptr CpBody    -- ^ body
       -> Ptr CpArbiter -- ^ arbiter
       -> Ptr ()        -- ^ data
       -> IO ()

foreign import CALLCV "chipmunk/chipmunk.h cpBodyEachArbiter"
  cpBodyEachArbiter
    :: Ptr CpBody                       -- ^ body
    -> FunPtr CpBodyArbiterIteratorFunc -- ^ func
    -> Ptr ()                           -- ^ data
    -> IO ()

-- end of cpBody.h

-- * cpShape.h

data {-# CTYPE "chipmunk/chipmunk.h" "cpPointQueryInfo" #-} CpPointQueryInfo =
       CpPointQueryInfo
         { shape    :: Ptr CpShape
         , point    :: CpVect
         , distance :: CpFloat
         , gradient :: CpVect
         }

instance Offset "shape"    CpPointQueryInfo where rawOffset = #offset struct cpPointQueryInfo, shape
instance Offset "point"    CpPointQueryInfo where rawOffset = #offset struct cpPointQueryInfo, point
instance Offset "distance" CpPointQueryInfo where rawOffset = #offset struct cpPointQueryInfo, distance
instance Offset "gradient" CpPointQueryInfo where rawOffset = #offset struct cpPointQueryInfo, gradient

instance Storable CpPointQueryInfo where
  sizeOf _    = #size struct cpPointQueryInfo
  alignment _ = #alignment struct cpPointQueryInfo

  peek ptr =
    CpPointQueryInfo
      <$> peek (offset @"shape"    ptr)
      <*> peek (offset @"point"    ptr)
      <*> peek (offset @"distance" ptr)
      <*> peek (offset @"gradient" ptr)

  poke ptr val = do
    pokeField @"shape"    ptr val
    pokeField @"point"    ptr val
    pokeField @"distance" ptr val
    pokeField @"gradient" ptr val

data {-# CTYPE "chipmunk/chipmunk.h" "cpSegmentQueryInfo" #-} CpSegmentQueryInfo =
       CpSegmentQueryInfo
         { shape  :: Ptr CpShape
         , point  :: CpVect
         , normal :: CpVect
         , alpha  :: CpFloat
         }

instance Offset "shape"  CpSegmentQueryInfo where rawOffset = #offset struct cpSegmentQueryInfo, shape
instance Offset "point"  CpSegmentQueryInfo where rawOffset = #offset struct cpSegmentQueryInfo, point
instance Offset "normal" CpSegmentQueryInfo where rawOffset = #offset struct cpSegmentQueryInfo, normal
instance Offset "alpha"  CpSegmentQueryInfo where rawOffset = #offset struct cpSegmentQueryInfo, alpha

instance Storable CpSegmentQueryInfo where
  sizeOf _    = #size struct cpSegmentQueryInfo
  alignment _ = #alignment struct cpSegmentQueryInfo

  peek ptr =
    CpSegmentQueryInfo
      <$> peek (offset @"shape"  ptr)
      <*> peek (offset @"point"  ptr)
      <*> peek (offset @"normal" ptr)
      <*> peek (offset @"alpha"  ptr)

  poke ptr val = do
    pokeField @"shape"  ptr val
    pokeField @"point"  ptr val
    pokeField @"normal" ptr val
    pokeField @"alpha"  ptr val

data {-# CTYPE "chipmunk/chipmunk.h" "cpShapeFilter" #-} CpShapeFilter =
       CpShapeFilter
         { group      :: CpGroup
         , categories :: CpBitmask
         , mask       :: CpBitmask
         }

instance Offset "group"      CpShapeFilter where rawOffset = #offset struct cpShapeFilter, group
instance Offset "categories" CpShapeFilter where rawOffset = #offset struct cpShapeFilter, categories
instance Offset "mask"       CpShapeFilter where rawOffset = #offset struct cpShapeFilter, mask

instance Storable CpShapeFilter where
  sizeOf _    = #size struct cpShapeFilter
  alignment _ = #alignment struct cpShapeFilter

  peek ptr =
    CpShapeFilter
      <$> peek (offset @"group"      ptr)
      <*> peek (offset @"categories" ptr)
      <*> peek (offset @"mask"       ptr)

  poke ptr val = do
    pokeField @"group"      ptr val
    pokeField @"categories" ptr val
    pokeField @"mask"       ptr val

foreign import CALLCV unsafe "wrapper.h w_cp_shape_filter_all"
  cpShape_filter_all
    :: Ptr CpShapeFilter -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cp_shape_filter_none"
  cpShape_filter_none
    :: Ptr CpShapeFilter -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpShapeDestroy"
  cpShapeDestroy
    :: Ptr CpShape -- ^ shape
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpShapeFree"
  cpShapeFree
    :: Ptr CpShape -- ^ shape
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpShapeCacheBB"
  cpShapeCacheBB
    :: Ptr CpShape -- ^ shape
    -> Ptr CpBB    -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpShapeUpdate"
  cpShapeUpdate
    :: Ptr CpShape     -- ^ shape
    -> Ptr CpTransform -- ^ transform
    -> Ptr CpBB        -- ^ Output value
    -> IO ()

foreign import CALLCV "wrapper.h w_cpShapePointQuery"
  cpShapePointQuery
    :: Ptr CpShape          -- ^ shape
    -> Ptr CpVect           -- ^ p
    -> Ptr CpPointQueryInfo -- ^ out
    -> IO CpFloat

foreign import CALLCV "wrapper.h w_cpShapeSegmentQuery"
  cpShapeSegmentQuery
    :: Ptr CpShape            -- ^ shape
    -> Ptr CpVect             -- ^ a
    -> Ptr CpVect             -- ^ b
    -> CpFloat                -- ^ radius
    -> Ptr CpSegmentQueryInfo -- ^ info
    -> IO CpBool

foreign import CALLCV unsafe "wrapper.h w_cpShapesCollide"
  cpShapesCollide
    :: Ptr CpShape           -- ^ a
    -> Ptr CpShape           -- ^ b
    -> Ptr CpContactPointSet -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpShapeGetSpace"
  cpShapeGetSpace
    :: Ptr CpShape      -- ^ shape
    -> IO (Ptr CpSpace)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpShapeGetBody"
  cpShapeGetBody
    :: Ptr CpShape     -- ^ shape
    -> IO (Ptr CpBody)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpShapeSetBody"
  cpShapeSetBody
    :: Ptr CpShape -- ^ shape
    -> Ptr CpBody  -- ^ body
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpShapeGetMass"
  cpShapeGetMass
    :: Ptr CpShape -- ^ shape
    -> IO CpFloat

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpShapeSetMass"
  cpShapeSetMass
    :: Ptr CpShape -- ^ shape
    -> CpFloat     -- ^ mass
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpShapeGetDensity"
  cpShapeGetDensity
    :: Ptr CpShape -- ^ shape
    -> IO CpFloat

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpShapeSetDensity"
  cpShapeSetDensity
    :: Ptr CpShape -- ^ shape
    -> CpFloat     -- ^ density
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpShapeGetMoment"
  cpShapeGetMoment
    :: Ptr CpShape -- ^ shape
    -> IO CpFloat

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpShapeGetArea"
  cpShapeGetArea
    :: Ptr CpShape -- ^ shape
    -> IO CpFloat

foreign import CALLCV unsafe "wrapper.h w_cpShapeGetCenterOfGravity"
  cpShapeGetCenterOfGravity
    :: Ptr CpShape -- ^ shape
    -> Ptr CpVect  -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpShapeGetBB"
  cpShapeGetBB
    :: Ptr CpShape -- ^ shape
    -> Ptr CpBB
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpShapeGetSensor"
  cpShapeGetSensor
    :: Ptr CpShape -- ^ shape
    -> IO CpBool

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpShapeSetSensor"
  cpShapeSetSensor
    :: Ptr CpShape -- ^ shape
    -> CpBool      -- ^ sensor
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpShapeGetElasticity"
  cpShapeGetElasticity
    :: Ptr CpShape -- ^ shape
    -> IO CpFloat

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpShapeSetElasticity"
  cpShapeSetElasticity
    :: Ptr CpShape -- ^ shape
    -> CpFloat     -- ^ elasticity
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpShapeGetFriction"
  cpShapeGetFriction
    :: Ptr CpShape -- ^ shape
    -> IO CpFloat

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpShapeSetFriction"
  cpShapeSetFriction
    :: Ptr CpShape -- ^ shape
    -> CpFloat     -- ^ friction
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpShapeGetSurfaceVelocity"
  cpShapeGetSurfaceVelocity
    :: Ptr CpShape -- ^ shape
    -> Ptr CpVect  -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpShapeSetSurfaceVelocity"
  cpShapeSetSurfaceVelocity
    :: Ptr CpShape -- ^ shape
    -> Ptr CpVect  -- ^ surfaceVelocity
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpShapeGetUserData"
  cpShapeGetUserData
    :: Ptr CpShape -- ^ shape
    -> IO CpDataPointer

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpShapeSetUserData"
  cpShapeSetUserData
    :: Ptr CpShape   -- ^ shape
    -> CpDataPointer -- ^ userData
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpShapeGetCollisionType"
  cpShapeGetCollisionType
    :: Ptr CpShape -- ^ shape
    -> IO CpCollisionType

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpShapeSetCollisionType"
  cpShapeSetCollisionType
    :: Ptr CpShape     -- ^ shape
    -> CpCollisionType -- ^ collisionType
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpShapeGetFilter"
  cpShapeGetFilter
    :: Ptr CpShape       -- ^ shape
    -> Ptr CpShapeFilter -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpShapeSetFilter"
  cpShapeSetFilter
    :: Ptr CpShape       -- ^ shape
    -> Ptr CpShapeFilter -- ^ filter
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpCircleShapeAlloc"
  cpCircleShapeAlloc
    :: IO (Ptr CpCircleShape)

foreign import CALLCV unsafe "wrapper.h w_cpCircleShapeInit"
  cpCircleShapeInit
    :: Ptr CpCircleShape      -- ^ circle
    -> Ptr CpBody             -- ^ body
    -> CpFloat                -- ^ radius
    -> Ptr CpVect             -- ^ offset
    -> IO (Ptr CpCircleShape)

foreign import CALLCV unsafe "wrapper.h w_cpCircleShapeNew"
  cpCircleShapeNew
    :: Ptr CpBody       -- ^ body
    -> CpFloat          -- ^ radius
    -> Ptr CpVect       -- ^ offset
    -> IO (Ptr CpShape)

foreign import CALLCV unsafe "wrapper.h w_cpCircleShapeGetOffset"
  cpCircleShapeGetOffset
    :: Ptr CpShape -- ^ shape
    -> Ptr CpVect  -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpCircleShapeGetRadius"
  cpCircleShapeGetRadius
    :: Ptr CpShape -- ^ shape
    -> IO CpFloat

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSegmentShapeAlloc"
  cpSegmentShapeAlloc
    :: IO (Ptr CpSegmentShape)

foreign import CALLCV unsafe "wrapper.h w_cpSegmentShapeInit"
  cpSegmentShapeInit
    :: Ptr CpSegmentShape      -- ^ seg
    -> Ptr CpBody              -- ^ body
    -> Ptr CpVect              -- ^ a
    -> Ptr CpVect              -- ^ b
    -> CpFloat                 -- ^ radius
    -> IO (Ptr CpSegmentShape)

foreign import CALLCV unsafe "wrapper.h w_cpSegmentShapeNew"
  cpSegmentShapeNew
    :: Ptr CpBody       -- ^ body
    -> Ptr CpVect       -- ^ a
    -> Ptr CpVect       -- ^ b
    -> CpFloat          -- ^ radius
    -> IO (Ptr CpShape)

foreign import CALLCV unsafe "wrapper.h w_cpSegmentShapeSetNeighbors"
  cpSegmentShapeSetNeighbors
    :: Ptr CpShape -- ^ shape
    -> Ptr CpVect  -- ^ prev
    -> Ptr CpVect  -- ^ next
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpSegmentShapeGetA"
  cpSegmentShapeGetA
    :: Ptr CpShape -- ^ shape
    -> Ptr CpVect  -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpSegmentShapeGetB"
  cpSegmentShapeGetB
    :: Ptr CpShape -- ^ shape
    -> Ptr CpVect  -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpSegmentShapeGetNormal"
  cpSegmentShapeGetNormal
    :: Ptr CpShape -- ^ shape
    -> Ptr CpVect  -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSegmentShapeGetRadius"
  cpSegmentShapeGetRadius
    :: Ptr CpShape -- ^ shape
    -> IO CpFloat

-- end of cpShape.h

-- * cpPolyShape.h

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpPolyShapeAlloc"
  cpPolyShapeAlloc
    :: IO (Ptr CpPolyShape)

foreign import CALLCV unsafe "wrapper.h w_cpPolyShapeInit"
  cpPolyShapeInit
    :: Ptr CpPolyShape      -- ^ poly
    -> Ptr CpBody           -- ^ body
    -> #{type int}          -- ^ count
    -> Ptr CpVect           -- ^ verts
    -> Ptr CpTransform      -- ^ transform
    -> CpFloat              -- ^ radius
    -> IO (Ptr CpPolyShape)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpPolyShapeInitRaw"
  cpPolyShapeInitRaw
    :: Ptr CpPolyShape      -- ^ poly
    -> Ptr CpBody           -- ^ body
    -> #{type int}          -- ^ count
    -> Ptr CpVect           -- ^ verts
    -> CpFloat              -- ^ radius
    -> IO (Ptr CpPolyShape)

foreign import CALLCV unsafe "wrapper.h w_cpPolyShapeNew"
  cpPolyShapeNew
    :: Ptr CpBody       -- ^ body
    -> #{type int}      -- ^ count
    -> Ptr CpVect       -- ^ verts
    -> Ptr CpTransform  -- ^ transform
    -> CpFloat          -- ^ radius
    -> IO (Ptr CpShape)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpPolyShapeNewRaw"
  cpPolyShapeNewRaw
    :: Ptr CpBody       -- ^ body
    -> #{type int}      -- ^ count
    -> Ptr CpVect       -- ^ verts
    -> CpFloat          -- ^ radius
    -> IO (Ptr CpShape)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpBoxShapeInit"
  cpBoxShapeInit
    :: Ptr CpPolyShape      -- ^ poly
    -> Ptr CpBody           -- ^ body
    -> CpFloat              -- ^ width
    -> CpFloat              -- ^ height
    -> CpFloat              -- ^ radius
    -> IO (Ptr CpPolyShape)

foreign import CALLCV unsafe "wrapper.h w_cpBoxShapeInit2"
  cpBoxShapeInit2
    :: Ptr CpPolyShape      -- ^ poly
    -> Ptr CpBody           -- ^ body
    -> Ptr CpBB             -- ^ box
    -> CpFloat              -- ^ radius
    -> IO (Ptr CpPolyShape)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpBoxShapeNew"
  cpBoxShapeNew
    :: Ptr CpBody       -- ^ body
    -> CpFloat          -- ^ width
    -> CpFloat          -- ^ height
    -> CpFloat          -- ^ radius
    -> IO (Ptr CpShape)

foreign import CALLCV unsafe "wrapper.h w_cpBoxShapeNew2"
  cpBoxShapeNew2
    :: Ptr CpBody       -- ^ body
    -> Ptr CpBB         -- ^ box
    -> CpFloat          -- ^ radius
    -> IO (Ptr CpShape)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpPolyShapeGetCount"
  cpPolyShapeGetCount
    :: Ptr CpShape    -- ^ shape
    -> IO #type int

foreign import CALLCV unsafe "wrapper.h w_cpPolyShapeGetVert"
  cpPolyShapeGetVert
    :: Ptr CpShape -- ^ shape
    -> #{type int} -- ^ index
    -> Ptr CpVect  -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpPolyShapeGetRadius"
  cpPolyShapeGetRadius
    :: Ptr CpShape -- ^ shape
    -> IO CpFloat

-- end of cpPolyShape.h

-- * cpConstraint.h

type CpConstraintPreSolveFunc =
          Ptr CpConstraint -- ^ constraint
       -> Ptr CpSpace      -- ^ space
       -> IO ()

type CpConstraintPostSolveFunc =
          Ptr CpConstraint -- ^ constraint
       -> Ptr CpSpace      -- ^ space
       -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpConstraintDestroy"
  cpConstraintDestroy
    :: Ptr CpConstraint -- ^ constraint
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpConstraintFree"
  cpConstraintFree
    :: Ptr CpConstraint -- ^ constraint
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpConstraintGetSpace"
  cpConstraintGetSpace
    :: Ptr CpConstraint -- ^ constraint
    -> IO (Ptr CpSpace)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpConstraintGetBodyA"
  cpConstraintGetBodyA
    :: Ptr CpConstraint -- ^ constraint
    -> IO (Ptr CpBody)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpConstraintGetBodyB"
  cpConstraintGetBodyB
    :: Ptr CpConstraint -- ^ constraint
    -> IO (Ptr CpBody)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpConstraintGetMaxForce"
  cpConstraintGetMaxForce
    :: Ptr CpConstraint -- ^ constraint
    -> IO CpFloat

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpConstraintSetMaxForce"
  cpConstraintSetMaxForce
    :: Ptr CpConstraint -- ^ constraint
    -> CpFloat          -- ^ maxForce
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpConstraintGetErrorBias"
  cpConstraintGetErrorBias
    :: Ptr CpConstraint -- ^ constraint
    -> IO CpFloat

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpConstraintSetErrorBias"
  cpConstraintSetErrorBias
    :: Ptr CpConstraint -- ^ constraint
    -> CpFloat          -- ^ errorBias
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpConstraintGetMaxBias"
  cpConstraintGetMaxBias
    :: Ptr CpConstraint -- ^ constraint
    -> IO CpFloat

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpConstraintSetMaxBias"
  cpConstraintSetMaxBias
    :: Ptr CpConstraint -- ^ constraint
    -> CpFloat          -- ^ maxBias
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpConstraintGetCollideBodies"
  cpConstraintGetCollideBodies
    :: Ptr CpConstraint -- ^ constraint
    -> IO CpBool

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpConstraintSetCollideBodies"
  cpConstraintSetCollideBodies
    :: Ptr CpConstraint -- ^ constraint
    -> CpBool           -- ^ collideBodies
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpConstraintGetPreSolveFunc"
  cpConstraintGetPreSolveFunc
    :: Ptr CpConstraint                     -- ^ constraint
    -> IO (FunPtr CpConstraintPreSolveFunc)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpConstraintSetPreSolveFunc"
  cpConstraintSetPreSolveFunc
    :: Ptr CpConstraint                -- ^ constraint
    -> FunPtr CpConstraintPreSolveFunc -- ^ preSolveFunc
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpConstraintGetPostSolveFunc"
  cpConstraintGetPostSolveFunc
    :: Ptr CpConstraint                      -- ^ constraint
    -> IO (FunPtr CpConstraintPostSolveFunc)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpConstraintSetPostSolveFunc"
  cpConstraintSetPostSolveFunc
    :: Ptr CpConstraint                 -- ^ constraint
    -> FunPtr CpConstraintPostSolveFunc -- ^ postSolveFunc
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpConstraintGetUserData"
  cpConstraintGetUserData
    :: Ptr CpConstraint -- ^ constraint
    -> IO CpDataPointer

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpConstraintSetUserData"
  cpConstraintSetUserData
    :: Ptr CpConstraint -- ^ constraint
    -> CpDataPointer    -- ^ userData
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpConstraintGetImpulse"
  cpConstraintGetImpulse
    :: Ptr CpConstraint -- ^ constraint
    -> IO CpFloat

-- ** cpPinJoint.h

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpConstraintIsPinJoint"
  cpConstraintIsPinJoint
    :: Ptr CpConstraint -- ^ constraint
    -> IO CpBool

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpPinJointAlloc"
  cpPinJointAlloc
    :: IO (Ptr CpPinJoint)

foreign import CALLCV unsafe "wrapper.h w_cpPinJointInit"
  cpPinJointInit
    :: Ptr CpPinJoint      -- ^ joint
    -> Ptr CpBody          -- ^ a
    -> Ptr CpBody          -- ^ b
    -> Ptr CpVect          -- ^ anchorA
    -> Ptr CpVect          -- ^ anchorB
    -> IO (Ptr CpPinJoint)

foreign import CALLCV unsafe "wrapper.h w_cpPinJointNew"
  cpPinJointNew
    :: Ptr CpBody            -- ^ a
    -> Ptr CpBody            -- ^ b
    -> Ptr CpVect            -- ^ anchorA
    -> Ptr CpVect            -- ^ anchorB
    -> IO (Ptr CpConstraint)

foreign import CALLCV unsafe "wrapper.h w_cpPinJointGetAnchorA"
  cpPinJointGetAnchorA
    :: Ptr CpConstraint -- ^ constraint
    -> Ptr CpVect       -- ^ Output value
    -> IO ()
foreign import CALLCV unsafe "wrapper.h w_cpPinJointSetAnchorA"
  cpPinJointSetAnchorA
    :: Ptr CpConstraint -- ^ constraint
    -> Ptr CpVect       -- ^ anchorA
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpPinJointGetAnchorB"
  cpPinJointGetAnchorB
    :: Ptr CpConstraint -- ^ constraint
    -> Ptr CpVect       -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpPinJointSetAnchorB"
  cpPinJointSetAnchorB
    :: Ptr CpConstraint -- ^ constraint
    -> Ptr CpVect       -- ^ anchorB
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpPinJointGetDist"
  cpPinJointGetDist
    :: Ptr CpConstraint -- ^ constraint
    -> IO CpFloat

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpPinJointSetDist"
  cpPinJointSetDist
    :: Ptr CpConstraint -- ^ constraint
    -> CpFloat          -- ^ dist
    -> IO ()

-- end of cpPinJoint.h

-- ** cpSlideJoint.h

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpConstraintIsSlideJoint"
  cpConstraintIsSlideJoint
    :: Ptr CpConstraint -- ^ constraint
    -> IO CpBool

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSlideJointAlloc"
  cpSlideJointAlloc
    :: IO (Ptr CpSlideJoint)

foreign import CALLCV unsafe "wrapper.h w_cpSlideJointInit"
  cpSlideJointInit
    :: Ptr CpSlideJoint      -- ^ joint
    -> Ptr CpBody            -- ^ a
    -> Ptr CpBody            -- ^ b
    -> Ptr CpVect            -- ^ anchorA
    -> Ptr CpVect            -- ^ anchorB
    -> CpFloat               -- ^ min
    -> CpFloat               -- ^ max
    -> IO (Ptr CpSlideJoint)

foreign import CALLCV unsafe "wrapper.h w_cpSlideJointNew"
  cpSlideJointNew
    :: Ptr CpBody            -- ^ a
    -> Ptr CpBody            -- ^ b
    -> Ptr CpVect            -- ^ anchorA
    -> Ptr CpVect            -- ^ anchorB
    -> CpFloat               -- ^ min
    -> CpFloat               -- ^ max
    -> IO (Ptr CpConstraint)

foreign import CALLCV unsafe "wrapper.h w_cpSlideJointGetAnchorA"
  cpSlideJointGetAnchorA
    :: Ptr CpConstraint -- ^ constraint
    -> Ptr CpVect
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpSlideJointSetAnchorA"
  cpSlideJointSetAnchorA
    :: Ptr CpConstraint -- ^ constraint
    -> Ptr CpVect       -- ^ anchorA
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpSlideJointGetAnchorB"
  cpSlideJointGetAnchorB
    :: Ptr CpConstraint -- ^ constraint
    -> Ptr CpVect       -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpSlideJointSetAnchorB"
  cpSlideJointSetAnchorB
    :: Ptr CpConstraint -- ^ constraint
    -> Ptr CpVect       -- ^ anchorB
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSlideJointGetMin"
  cpSlideJointGetMin
    :: Ptr CpConstraint -- ^ constraint
    -> IO CpFloat

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSlideJointSetMin"
  cpSlideJointSetMin
    :: Ptr CpConstraint -- ^ constraint
    -> CpFloat          -- ^ min
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSlideJointGetMax"
  cpSlideJointGetMax
    :: Ptr CpConstraint -- ^ constraint
    -> IO CpFloat

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSlideJointSetMax"
  cpSlideJointSetMax
    :: Ptr CpConstraint -- ^ constraint
    -> CpFloat          -- ^ max
    -> IO ()

-- end of cpSlideJoint.h

-- ** cpPivotJoint.h

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpConstraintIsPivotJoint"
  cpConstraintIsPivotJoint
    :: Ptr CpConstraint -- ^ constraint
    -> IO CpBool

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpPivotJointAlloc"
  cpPivotJointAlloc
    :: IO (Ptr CpPivotJoint)

foreign import CALLCV unsafe "wrapper.h w_cpPivotJointInit"
  cpPivotJointInit
    :: Ptr CpPivotJoint      -- ^ joint
    -> Ptr CpBody            -- ^ a
    -> Ptr CpBody            -- ^ b
    -> Ptr CpVect            -- ^ anchorA
    -> Ptr CpVect            -- ^ anchorB
    -> IO (Ptr CpPivotJoint)

foreign import CALLCV unsafe "wrapper.h w_cpPivotJointNew"
  cpPivotJointNew
    :: Ptr CpBody            -- ^ a
    -> Ptr CpBody            -- ^ b
    -> Ptr CpVect            -- ^ pivot
    -> IO (Ptr CpConstraint)

foreign import CALLCV unsafe "wrapper.h w_cpPivotJointNew2"
  cpPivotJointNew2
    :: Ptr CpBody            -- ^ a
    -> Ptr CpBody            -- ^ b
    -> Ptr CpVect            -- ^ anchorA
    -> Ptr CpVect            -- ^ anchorB
    -> IO (Ptr CpConstraint)

foreign import CALLCV unsafe "wrapper.h w_cpPivotJointGetAnchorA"
  cpPivotJointGetAnchorA
    :: Ptr CpConstraint -- ^ constraint
    -> Ptr CpVect
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpPivotJointSetAnchorA"
  cpPivotJointSetAnchorA
    :: Ptr CpConstraint -- ^ constraint
    -> Ptr CpVect       -- ^ anchorA
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpPivotJointGetAnchorB"
  cpPivotJointGetAnchorB
    :: Ptr CpConstraint -- ^ constraint
    -> Ptr CpVect       -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpPivotJointSetAnchorB"
  cpPivotJointSetAnchorB
    :: Ptr CpConstraint -- ^ constraint
    -> Ptr CpVect       -- ^ anchorB
    -> IO ()

-- end of cpPivotJoint.h

-- ** cpGrooveJoint.h

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpConstraintIsGrooveJoint"
  cpConstraintIsGrooveJoint
    :: Ptr CpConstraint -- ^ constraint
    -> IO CpBool

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpGrooveJointAlloc"
  cpGrooveJointAlloc
    :: IO (Ptr CpGrooveJoint)

foreign import CALLCV unsafe "wrapper.h w_cpGrooveJointInit"
  cpGrooveJointInit
    :: Ptr CpGrooveJoint      -- ^ joint
    -> Ptr CpBody             -- ^ a
    -> Ptr CpBody             -- ^ b
    -> Ptr CpVect             -- ^ groove_a
    -> Ptr CpVect             -- ^ groove_b
    -> Ptr CpVect             -- ^ anchorB
    -> IO (Ptr CpGrooveJoint)

foreign import CALLCV unsafe "wrapper.h w_cpGrooveJointNew"
  cpGrooveJointNew
    :: Ptr CpBody            -- ^ a
    -> Ptr CpBody            -- ^ b
    -> Ptr CpVect            -- ^ groove_a
    -> Ptr CpVect            -- ^ groove_b
    -> Ptr CpVect            -- ^ anchorB
    -> IO (Ptr CpConstraint)

foreign import CALLCV unsafe "wrapper.h w_cpGrooveJointGetGrooveA"
  cpGrooveJointGetGrooveA
    :: Ptr CpConstraint -- ^ constraint
    -> Ptr CpVect
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpGrooveJointSetGrooveA"
  cpGrooveJointSetGrooveA
    :: Ptr CpConstraint -- ^ constraint
    -> Ptr CpVect       -- ^ grooveB
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpGrooveJointGetGrooveB"
  cpGrooveJointGetGrooveB
    :: Ptr CpConstraint -- ^ constraint
    -> Ptr CpVect
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpGrooveJointSetGrooveB"
  cpGrooveJointSetGrooveB
    :: Ptr CpConstraint -- ^ constraint
    -> Ptr CpVect       -- ^ grooveB
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpGrooveJointGetAnchorB"
  cpGrooveJointGetAnchorB
    :: Ptr CpConstraint -- ^ constraint
    -> Ptr CpVect       -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpGrooveJointSetAnchorB"
  cpGrooveJointSetAnchorB
    :: Ptr CpConstraint -- ^ constraint
    -> Ptr CpVect       -- ^ anchorB
    -> IO ()

-- end of cpGrooveJoint.h

-- ** cpDampedSpring.h

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpConstraintIsDampedSpring"
  cpConstraintIsDampedSpring
    :: Ptr CpConstraint -- ^ constraint
    -> IO CpBool

type CpDampedSpringForceFunc =
          Ptr CpConstraint -- ^ spring
       -> CpFloat          -- ^ dist
       -> IO CpFloat

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpDampedSpringAlloc"
  cpDampedSpringAlloc
    :: IO (Ptr CpDampedSpring)

foreign import CALLCV unsafe "wrapper.h w_cpDampedSpringInit"
  cpDampedSpringInit
    :: Ptr CpDampedSpring      -- ^ joint
    -> Ptr CpBody              -- ^ a
    -> Ptr CpBody              -- ^ b
    -> Ptr CpVect              -- ^ anchorA
    -> Ptr CpVect              -- ^ anchorB
    -> CpFloat                 -- ^ restLength
    -> CpFloat                 -- ^ stiffness
    -> CpFloat                 -- ^ damping
    -> IO (Ptr CpDampedSpring)

foreign import CALLCV unsafe "wrapper.h w_cpDampedSpringNew"
  cpDampedSpringNew
    :: Ptr CpBody            -- ^ a
    -> Ptr CpBody            -- ^ b
    -> Ptr CpVect            -- ^ anchorA
    -> Ptr CpVect            -- ^ anchorB
    -> CpFloat               -- ^ restLength
    -> CpFloat               -- ^ stiffness
    -> CpFloat               -- ^ damping
    -> IO (Ptr CpConstraint)

foreign import CALLCV unsafe "wrapper.h w_cpDampedSpringGetAnchorA"
  cpDampedSpringGetAnchorA
    :: Ptr CpConstraint -- ^ constraint
    -> Ptr CpVect
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpDampedSpringSetAnchorA"
  cpDampedSpringSetAnchorA
    :: Ptr CpConstraint -- ^ constraint
    -> Ptr CpVect       -- ^ anchorA
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpDampedSpringGetAnchorB"
  cpDampedSpringGetAnchorB
    :: Ptr CpConstraint -- ^ constraint
    -> Ptr CpVect       -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpDampedSpringSetAnchorB"
  cpDampedSpringSetAnchorB
    :: Ptr CpConstraint -- ^ constraint
    -> Ptr CpVect       -- ^ anchorB
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpDampedSpringGetRestLength"
  cpDampedSpringGetRestLength
    :: Ptr CpConstraint -- ^ constraint
    -> IO CpFloat

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpDampedSpringSetRestLength"
  cpDampedSpringSetRestLength
    :: Ptr CpConstraint -- ^ constraint
    -> CpFloat          -- ^ restLength
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpDampedSpringGetStiffness"
  cpDampedSpringGetStiffness
    :: Ptr cpConstraint -- ^ constraint
    -> IO CpFloat

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpDampedSpringSetStiffness"
  cpDampedSpringSetStiffness
    :: Ptr CpConstraint -- ^ constraint
    -> CpFloat          -- ^ stiffness
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpDampedSpringGetDamping"
  cpDampedSpringGetDamping
    :: Ptr CpConstraint -- ^ constraint
    -> IO CpFloat

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpDampedSpringSetDamping"
  cpDampedSpringSetDamping
    :: Ptr CpConstraint -- ^ constraint
    -> CpFloat          -- ^ damping
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpDampedSpringGetSpringForceFunc"
  cpDampedSpringGetSpringForceFunc
    :: Ptr CpConstraint                    -- ^ constraint
    -> IO (FunPtr CpDampedSpringForceFunc)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpDampedSpringSetSpringForceFunc"
  cpDampedSpringSetSpringForceFunc
    :: Ptr CpConstraint               -- ^ constraint
    -> FunPtr CpDampedSpringForceFunc -- ^ springForceFunc
    -> IO ()

-- end of cpDampedSpring.h

-- ** cpDampedRotarySpring.h

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpConstraintIsDampedRotarySpring"
  cpConstraintIsDampedRotarySpring
    :: Ptr CpConstraint -- ^ constraint
    -> IO CpBool

type CpDampedRotarySpringTorqueFunc =
          Ptr CpConstraint -- ^ spring
       -> CpFloat          -- ^ relativeAngle
       -> IO CpFloat

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpDampedRotarySpringAlloc"
  cpDampedRotarySpringAlloc
    :: IO (Ptr CpDampedRotarySpring)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpDampedRotarySpringInit"
  cpDampedRotarySpringInit
    :: Ptr CpDampedRotarySpring      -- ^ joint
    -> Ptr CpBody                    -- ^ a
    -> Ptr CpBody                    -- ^ b
    -> CpFloat                       -- ^ restAngle
    -> CpFloat                       -- ^ stiffness
    -> CpFloat                       -- ^ damping
    -> IO (Ptr CpDampedRotarySpring)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpDampedRotarySpringNew"
  cpDampedRotarySpringNew
    :: Ptr CpBody            -- ^ a
    -> Ptr CpBody            -- ^ b
    -> CpFloat               -- ^ restAngle
    -> CpFloat               -- ^ stiffness
    -> CpFloat               -- ^ damping
    -> IO (Ptr CpConstraint)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpDampedRotarySpringGetRestAngle"
  cpDampedRotarySpringGetRestAngle
    :: Ptr CpConstraint -- ^ constraint
    -> IO CpFloat

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpDampedRotarySpringSetRestAngle"
  cpDampedRotarySpringSetRestAngle
    :: Ptr CpConstraint -- ^ constraint
    -> CpFloat          -- ^ restAngle
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpDampedRotarySpringGetStiffness"
  cpDampedRotarySpringGetStiffness
    :: Ptr cpConstraint -- ^ constraint
    -> IO CpFloat

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpDampedRotarySpringSetStiffness"
  cpDampedRotarySpringSetStiffness
    :: Ptr CpConstraint -- ^ constraint
    -> CpFloat          -- ^ stiffness
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpDampedRotarySpringGetDamping"
  cpDampedRotarySpringGetDamping
    :: Ptr CpConstraint -- ^ constraint
    -> IO CpFloat

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpDampedRotarySpringSetDamping"
  cpDampedRotarySpringSetDamping
    :: Ptr CpConstraint -- ^ constraint
    -> CpFloat          -- ^ damping
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpDampedRotarySpringGetSpringTorqueFunc"
  cpDampedRotarySpringGetSpringTorqueFunc
    :: Ptr CpConstraint                    -- ^ constraint
    -> IO (FunPtr CpDampedRotarySpringTorqueFunc)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpDampedRotarySpringSetSpringTorqueFunc"
  cpDampedRotarySpringSetSpringTorqueFunc
    :: Ptr CpConstraint                      -- ^ constraint
    -> FunPtr CpDampedRotarySpringTorqueFunc -- ^ springTorqueFunc
    -> IO ()

-- end of cpDampedRotarySpring.h

-- ** cpRotaryLimitJoint.h

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpConstraintIsRotaryLimitJoint"
  cpConstraintIsRotaryLimitJoint
    :: Ptr CpConstraint -- ^ constraint
    -> IO CpBool

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpRotaryLimitJointAlloc"
  cpRotaryLimitJointAlloc
    :: IO (Ptr CpRotaryLimitJoint)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpRotaryLimitJointInit"
  cpRotaryLimitJointInit
    :: Ptr CpRotaryLimitJoint      -- ^ joint
    -> Ptr CpBody                  -- ^ a
    -> Ptr CpBody                  -- ^ b
    -> CpFloat                     -- ^ min
    -> CpFloat                     -- ^ max
    -> IO (Ptr CpRotaryLimitJoint)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpRotaryLimitJointNew"
  cpRotaryLimitJointNew
    :: Ptr CpBody            -- ^ a
    -> Ptr CpBody            -- ^ b
    -> CpFloat               -- ^ min
    -> CpFloat               -- ^ max
    -> IO (Ptr CpConstraint)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpRotaryLimitJointGetMin"
  cpRotaryLimitJointGetMin
    :: Ptr CpConstraint -- ^ constraint
    -> IO CpFloat

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpRotaryLimitJointSetMin"
  cpRotaryLimitJointSetMin
    :: Ptr CpConstraint -- ^ constraint
    -> CpFloat          -- ^ min
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpRotaryLimitJointGetMax"
  cpRotaryLimitJointGetMax
    :: Ptr CpConstraint -- ^ constraint
    -> IO CpFloat

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpRotaryLimitJointSetMax"
  cpRotaryLimitJointSetMax
    :: Ptr CpConstraint -- ^ constraint
    -> CpFloat          -- ^ max
    -> IO ()

-- end of cpRotaryLimitJoint.h

-- ** cpRatchetJoint.h

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpConstraintIsRatchetJoint"
  cpConstraintIsRatchetJoint
    :: Ptr CpConstraint -- ^ constraint
    -> IO CpBool

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpRatchetJointAlloc"
  cpRatchetJointAlloc
    :: IO (Ptr CpRatchetJoint)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpRatchetJointInit"
  cpRatchetJointInit
    :: Ptr CpRatchetJoint      -- ^ joint
    -> Ptr CpBody              -- ^ a
    -> Ptr CpBody              -- ^ b
    -> CpFloat                 -- ^ phase
    -> CpFloat                 -- ^ ratchet
    -> IO (Ptr CpRatchetJoint)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpRatchetJointNew"
  cpRatchetJointNew
    :: Ptr CpBody            -- ^ a
    -> Ptr CpBody            -- ^ b
    -> CpFloat               -- ^ phase
    -> CpFloat               -- ^ ratchet
    -> IO (Ptr CpConstraint)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpRatchetJointGetAngle"
  cpRatchetJointGetAngle
    :: Ptr CpConstraint -- ^ constraint
    -> IO CpFloat

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpRatchetJointSetAngle"
  cpRatchetJointSetAngle
    :: Ptr CpConstraint -- ^ constraint
    -> CpFloat          -- ^ angle
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpRatchetJointGetPhase"
  cpRatchetJointGetPhase
    :: Ptr CpConstraint -- ^ constraint
    -> IO CpFloat

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpRatchetJointSetPhase"
  cpRatchetJointSetPhase
    :: Ptr CpConstraint -- ^ constraint
    -> CpFloat          -- ^ phase
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpRatchetJointGetRatchet"
  cpRatchetJointGetRatchet
    :: Ptr CpConstraint -- ^ constraint
    -> IO CpFloat

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpRatchetJointSetRatchet"
  cpRatchetJointSetRatchet
    :: Ptr CpConstraint -- ^ constraint
    -> CpFloat          -- ^ ratchet
    -> IO ()

-- end of cpRatchetJoint.h

-- ** cpGearJoint.h

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpConstraintIsGearJoint"
  cpConstraintIsGearJoint
    :: Ptr CpConstraint -- ^ constraint
    -> IO CpBool

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpGearJointAlloc"
  cpGearJointAlloc
    :: IO (Ptr CpGearJoint)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpGearJointInit"
  cpGearJointInit
    :: Ptr CpGearJoint      -- ^ joint
    -> Ptr CpBody           -- ^ a
    -> Ptr CpBody           -- ^ b
    -> CpFloat              -- ^ phase
    -> CpFloat              -- ^ ratio
    -> IO (Ptr CpGearJoint)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpGearJointNew"
  cpGearJointNew
    :: Ptr CpBody            -- ^ a
    -> Ptr CpBody            -- ^ b
    -> CpFloat               -- ^ phase
    -> CpFloat               -- ^ ratio
    -> IO (Ptr CpConstraint)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpGearJointGetPhase"
  cpGearJointGetPhase
    :: Ptr CpConstraint -- ^ constraint
    -> IO CpFloat

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpGearJointSetPhase"
  cpGearJointSetPhase
    :: Ptr CpConstraint -- ^ constraint
    -> CpFloat          -- ^ phase
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpGearJointGetRatio"
  cpGearJointGetRatio
    :: Ptr CpConstraint -- ^ constraint
    -> IO CpFloat

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpGearJointSetRatio"
  cpGearJointSetRatio
    :: Ptr CpConstraint -- ^ constraint
    -> CpFloat          -- ^ ratio
    -> IO ()

-- end of cpGearJoint.h

-- ** cpSimpleMotor.h

--data {-# CTYPE "chipmunk/chipmunk.h" "cpSimpleMotor" #-} CpSimpleMotor

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpConstraintIsSimpleMotor"
  cpConstraintIsSimpleMotor
    :: Ptr CpConstraint -- ^ constraint
    -> IO CpBool

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSimpleMotorAlloc"
  cpSimpleMotorAlloc
    :: IO (Ptr CpSimpleMotor)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSimpleMotorInit"
  cpSimpleMotorInit
    :: Ptr CpSimpleMotor      -- ^ joint
    -> Ptr CpBody             -- ^ a
    -> Ptr CpBody             -- ^ b
    -> CpFloat                -- ^ rate
    -> IO (Ptr CpSimpleMotor)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSimpleMotorNew"
  cpSimpleMotorNew
    :: Ptr CpBody            -- ^ a
    -> Ptr CpBody            -- ^ b
    -> CpFloat               -- ^ rate
    -> IO (Ptr CpConstraint)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSimpleMotorGetRate"
  cpSimpleMotorGetRate
    :: Ptr CpConstraint -- ^ constraint
    -> IO CpFloat

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSimpleMotorSetRate"
  cpSimpleMotorSetRate
    :: Ptr CpConstraint -- ^ constraint
    -> CpFloat          -- ^ rate
    -> IO ()

-- end of cpSimpleMotor.h

-- end of cpConstraint.h

-- * cpSpace.h

type CpCollisionBeginFunc =
          Ptr CpArbiter -- ^ arb
       -> Ptr CpSpace   -- ^ space
       -> CpDataPointer -- ^ userData
       -> IO CpBool

type CpCollisionPreSolveFunc =
          Ptr CpArbiter -- ^ arb
       -> Ptr CpSpace   -- ^ space
       -> CpDataPointer -- ^ userData
       -> IO CpBool

type CpCollisionPostSolveFunc =
          Ptr CpArbiter -- ^ arb
       -> Ptr CpSpace   -- ^ space
       -> CpDataPointer -- ^ userData
       -> IO ()

type CpCollisionSeparateFunc =
          Ptr CpArbiter -- ^ arb
       -> Ptr CpSpace   -- ^ space
       -> CpDataPointer -- ^ userData
       -> IO ()

data {-# CTYPE "chipmunk/chipmunk.h" "cpCollisionHandler" #-} CpCollisionHandler =
       CpCollisionHandler
         { typeA         :: CpCollisionType
         , typeB         :: CpCollisionType
         , beginFunc     :: FunPtr CpCollisionBeginFunc
         , preSolveFunc  :: FunPtr CpCollisionPreSolveFunc
         , postSolveFunc :: FunPtr CpCollisionPostSolveFunc
         , separateFunc  :: FunPtr CpCollisionSeparateFunc
         , userData      :: CpDataPointer
         }

instance Offset "typeA"         CpCollisionHandler where rawOffset = #offset struct cpCollisionHandler, typeA
instance Offset "typeB"         CpCollisionHandler where rawOffset = #offset struct cpCollisionHandler, typeB
instance Offset "beginFunc"     CpCollisionHandler where rawOffset = #offset struct cpCollisionHandler, beginFunc
instance Offset "preSolveFunc"  CpCollisionHandler where rawOffset = #offset struct cpCollisionHandler, preSolveFunc
instance Offset "postSolveFunc" CpCollisionHandler where rawOffset = #offset struct cpCollisionHandler, postSolveFunc
instance Offset "separateFunc"  CpCollisionHandler where rawOffset = #offset struct cpCollisionHandler, separateFunc
instance Offset "userData"      CpCollisionHandler where rawOffset = #offset struct cpCollisionHandler, userData

instance Storable CpCollisionHandler where
  sizeOf _    = #size struct cpCollisionHandler
  alignment _ = #alignment struct cpCollisionHandler

  peek ptr =
    CpCollisionHandler
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
  cpSpaceAlloc
    :: IO (Ptr CpSpace)

foreign import CALLCV "chipmunk/chipmunk.h cpSpaceInit"
  cpSpaceInit
    :: Ptr CpSpace      -- ^ space
    -> IO (Ptr CpSpace)

foreign import CALLCV "chipmunk/chipmunk.h cpSpaceNew"
  cpSpaceNew
    :: IO (Ptr CpSpace)

foreign import CALLCV "chipmunk/chipmunk.h cpSpaceDestroy"
  cpSpaceDestroy
    :: Ptr CpSpace -- ^ space
    -> IO ()

foreign import CALLCV "chipmunk/chipmunk.h cpSpaceFree"
  cpSpaceFree
    :: Ptr CpSpace -- ^ space
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSpaceGetIterations"
  cpSpaceGetIterations
    :: Ptr CpSpace -- ^ space
    -> IO #type int

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSpaceSetIterations"
  cpSpaceSetIterations
    :: Ptr CpSpace -- ^ space
    -> #{type int} -- ^ iterations
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpSpaceGetGravity"
  cpSpaceGetGravity
    :: Ptr CpSpace -- ^ space
    -> Ptr CpVect  -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpSpaceSetGravity"
  cpSpaceSetGravity
    :: Ptr CpSpace -- ^ space
    -> Ptr CpVect  -- ^ gravity
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSpaceGetDamping"
  cpSpaceGetDamping
    :: Ptr CpSpace -- ^ space
    -> IO CpFloat

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSpaceSetDamping"
  cpSpaceSetDamping
    :: Ptr CpSpace -- ^ space
    -> CpFloat     -- ^ damping
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSpaceGetIdleSpeedThreshold"
  cpSpaceGetIdleSpeedThreshold
    :: Ptr CpSpace -- ^ space
    -> IO CpFloat

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSpaceSetIdleSpeedThreshold"
  cpSpaceSetIdleSpeedThreshold
    :: Ptr CpSpace -- ^ space
    -> CpFloat     -- ^ idleSpeedThreshold
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSpaceGetSleepTimeThreshold"
  cpSpaceGetSleepTimeThreshold
    :: Ptr CpSpace -- ^ space
    -> IO CpFloat

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSpaceSetSleepTimeThreshold"
  cpSpaceSetSleepTimeThreshold
    :: Ptr CpSpace -- ^ space
    -> CpFloat     -- ^ sleepTimeThreshold
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSpaceGetCollisionSlop"
  cpSpaceGetCollisionSlop
    :: Ptr CpSpace -- ^ space
    -> IO CpFloat

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSpaceSetCollisionSlop"
  cpSpaceSetCollisionSlop
    :: Ptr CpSpace -- ^ space
    -> CpFloat     -- ^ collisionSlop
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSpaceGetCollisionBias"
  cpSpaceGetCollisionBias
    :: Ptr CpSpace -- ^ space
    -> IO CpFloat

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSpaceSetCollisionBias"
  cpSpaceSetCollisionBias
    :: Ptr CpSpace -- ^ space
    -> CpFloat     -- ^ collisionBias
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSpaceGetCollisionPersistence"
  cpSpaceGetCollisionPersistence
    :: Ptr CpSpace -- ^ space
    -> IO CpTimestamp

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSpaceSetCollisionPersistence"
  cpSpaceSetCollisionPersistence
    :: Ptr CpSpace -- ^ space
    -> CpTimestamp -- ^ collisionPersistence
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSpaceGetUserData"
  cpSpaceGetUserData
    :: Ptr CpSpace -- ^ space
    -> IO CpDataPointer

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSpaceSetUserData"
  cpSpaceSetUserData
    :: Ptr CpSpace   -- ^ space
    -> CpDataPointer -- ^ userData
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSpaceGetStaticBody"
  cpSpaceGetStaticBody
    :: Ptr CpSpace -- ^ space
    -> IO (Ptr CpBody)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSpaceGetCurrentTimeStep"
  cpSpaceGetCurrentTimeStep
    :: Ptr CpSpace -- ^ space
    -> IO CpFloat

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSpaceIsLocked"
  cpSpaceIsLocked
    :: Ptr CpSpace -- ^ space
    -> IO CpBool

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSpaceAddDefaultCollisionHandler"
  cpSpaceAddDefaultCollisionHandler
    :: Ptr CpSpace                 -- ^ space
    -> IO (Ptr CpCollisionHandler)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSpaceAddCollisionHandler"
  cpSpaceAddCollisionHandler
    :: Ptr CpSpace                 -- ^ space
    -> CpCollisionType             -- ^ a
    -> CpCollisionType             -- ^ b
    -> IO (Ptr CpCollisionHandler)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSpaceAddWildcardHandler"
  cpSpaceAddWildcardHandler
    :: Ptr CpSpace                 -- ^ space
    -> CpCollisionType             -- ^ type
    -> IO (Ptr CpCollisionHandler)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSpaceAddShape"
  cpSpaceAddShape
    :: Ptr CpSpace      -- ^ space
    -> Ptr CpShape      -- ^ shape
    -> IO (Ptr CpShape)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSpaceAddBody"
  cpSpaceAddBody
    :: Ptr CpSpace     -- ^ space
    -> Ptr CpBody      -- ^ body
    -> IO (Ptr CpBody)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSpaceAddConstraint"
  cpSpaceAddConstraint
    :: Ptr CpSpace           -- ^ space
    -> Ptr CpConstraint      -- ^ constraint
    -> IO (Ptr CpConstraint)

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSpaceRemoveShape"
  cpSpaceRemoveShape
    :: Ptr CpSpace -- ^ space
    -> Ptr CpShape -- ^ shape
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSpaceRemoveBody"
  cpSpaceRemoveBody
    :: Ptr CpSpace -- ^ space
    -> Ptr CpBody  -- ^ body
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSpaceRemoveConstraint"
  cpSpaceRemoveConstraint
    :: Ptr CpSpace      -- ^ space
    -> Ptr CpConstraint -- ^ constraint
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSpaceContainsShape"
  cpSpaceContainsShape
    :: Ptr CpSpace -- ^ space
    -> Ptr CpShape -- ^ shape
    -> IO CpBool

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSpaceContainsBody"
  cpSpaceContainsBody
    :: Ptr CpSpace -- ^ space
    -> Ptr CpBody  -- ^ body
    -> IO CpBool

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSpaceContainsConstraint"
  cpSpaceContainsConstraint
    :: Ptr CpSpace      -- ^ space
    -> Ptr CpConstraint -- ^ constraint
    -> IO CpBool

type CpPostStepFunc =
          Ptr CpSpace -- ^ space
       -> Ptr ()      -- ^ key
       -> Ptr ()      -- ^ data
       -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSpaceAddPostStepCallback"
  cpSpaceAddPostStepCallback
    :: Ptr CpSpace           -- ^ space
    -> FunPtr CpPostStepFunc -- ^ func
    -> Ptr ()                -- ^ key
    -> Ptr ()                -- ^ data
    -> IO CpBool

type CpSpacePointQueryFunc =
          Ptr CpShape -- ^ shape
       -> CpVect      -- ^ point
       -> CpFloat     -- ^ distance
       -> CpVect      -- ^ gradient
       -> Ptr ()      -- ^ data
       -> IO ()

foreign import CALLCV "wrapper.h w_cpSpacePointQuery"
  cpSpacePointQuery
    :: Ptr CpSpace                  -- ^ space
    -> Ptr CpVect                   -- ^ point
    -> CpFloat                      -- ^ maxDistance
    -> Ptr CpShapeFilter            -- ^ filter
    -> FunPtr CpSpacePointQueryFunc -- ^ func
    -> Ptr ()                       -- ^ data
    -> IO ()

foreign import CALLCV "wrapper.h w_cpSpacePointQueryNearest"
  cpSpacePointQueryNearest
    :: Ptr CpSpace          -- ^ space
    -> Ptr CpVect           -- ^ point
    -> CpFloat              -- ^ maxDistance
    -> Ptr CpShapeFilter    -- ^ filter
    -> Ptr CpPointQueryInfo -- ^ out
    -> IO (Ptr CpShape)

type CpSpaceSegmentQueryFunc =
          Ptr CpShape -- ^ shape
       -> CpVect      -- ^ point
       -> CpVect      -- ^ normal
       -> CpFloat     -- ^ alpha
       -> Ptr ()      -- ^ data
       -> IO ()

foreign import CALLCV "wrapper.h w_cpSpaceSegmentQuery"
  cpSpaceSegmentQuery
    :: Ptr CpSpace                    -- ^ space
    -> Ptr CpVect                     -- ^ start
    -> Ptr CpVect                     -- ^ end
    -> CpFloat                        -- ^ radius
    -> Ptr CpShapeFilter              -- ^ filter
    -> FunPtr CpSpaceSegmentQueryFunc -- ^ func
    -> Ptr ()                         -- ^ data
    -> IO ()

foreign import CALLCV "wrapper.h w_cpSpaceSegmentQueryFirst"
  cpSpaceSegmentQueryFirst
    :: Ptr CpSpace            -- ^ space
    -> Ptr CpVect             -- ^ start
    -> Ptr CpVect             -- ^ end
    -> CpFloat                -- ^ radius
    -> Ptr CpShapeFilter      -- ^ filter
    -> Ptr CpSegmentQueryInfo -- ^ out
    -> IO (Ptr CpShape)

type CpSpaceBBQueryFunc =
          Ptr CpShape -- ^ shape
       -> Ptr ()      -- ^ data
       -> IO ()

foreign import CALLCV "wrapper.h w_cpSpaceBBQuery"
  cpSpaceBBQuery
    :: Ptr CpSpace               -- ^ space
    -> Ptr CpBB                  -- ^ bb
    -> Ptr CpShapeFilter         -- ^ filter
    -> FunPtr CpSpaceBBQueryFunc -- ^ func
    -> Ptr ()                    -- ^ data
    -> IO ()

type CpSpaceShapeQueryFunc =
          Ptr CpShape           -- ^ shape
       -> Ptr CpContactPointSet -- ^ points
       -> Ptr ()                -- ^ data
       -> IO ()

foreign import CALLCV "chipmunk/chipmunk.h cpSpaceShapeQuery"
  cpSpaceShapeQuery
    :: Ptr CpSpace                  -- ^ space
    -> Ptr CpShape                  -- ^ shape
    -> FunPtr CpSpaceShapeQueryFunc -- ^ func
    -> Ptr ()                       -- ^ data
    -> IO CpBool



type CpSpaceBodyIteratorFunc =
          Ptr CpBody -- ^ body
       -> Ptr ()   -- ^ data
       -> IO ()

foreign import CALLCV "chipmunk/chipmunk.h cpSpaceEachBody"
  cpSpaceEachBody
    :: Ptr CpSpace                    -- ^ space
    -> FunPtr CpSpaceBodyIteratorFunc -- ^ func
    -> Ptr ()                         -- ^ data
    -> IO ()

type CpSpaceShapeIteratorFunc =
          Ptr CpShape -- ^ shape
       -> Ptr ()    -- ^ data
       -> IO ()

foreign import CALLCV "chipmunk/chipmunk.h cpSpaceEachShape"
  cpSpaceEachShape
    :: Ptr CpSpace                     -- ^ space
    -> FunPtr CpSpaceShapeIteratorFunc -- ^ func
    -> Ptr ()                          -- ^ data
    -> IO ()

type CpSpaceConstraintIteratorFunc =
          Ptr CpConstraint -- ^ constraint
       -> Ptr ()         -- ^ data
       -> IO ()

foreign import CALLCV "chipmunk/chipmunk.h cpSpaceEachConstraint"
  cpSpaceEachConstraint
    :: Ptr CpSpace                          -- ^ space
    -> FunPtr CpSpaceConstraintIteratorFunc -- ^ func
    -> Ptr ()                               -- ^ data
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSpaceReindexStatic"
  cpSpaceReindexStatic
    :: Ptr CpSpace -- ^ space
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSpaceReindexShape"
  cpSpaceReindexShape
    :: Ptr CpSpace -- ^ space
    -> Ptr CpShape -- ^ shape
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSpaceReindexShapesForBody"
  cpSpaceReindexShapesForBody
    :: Ptr CpSpace -- ^ space
    -> Ptr CpBody  -- ^ body
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpSpaceUseSpatialHash"
  cpSpaceUseSpatialHash
    :: Ptr CpSpace -- ^ space
    -> CpFloat     -- ^ dim
    -> #{type int} -- ^ count
    -> IO ()

foreign import CALLCV "chipmunk/chipmunk.h cpSpaceStep"
  cpSpaceStep
    :: Ptr CpSpace -- ^ space
    -> CpFloat     -- ^ dt
    -> IO ()

#ifndef CP_SPACE_DISABLE_DEBUG_API

data {-# CTYPE "chipmunk/chipmunk.h" "cpSpaceDebugColor" #-} CpSpaceDebugColor =
       CpSpaceDebugColor
         { r :: #type float
         , g :: #type float
         , b :: #type float
         , a :: #type float
         }

instance Offset "r" CpSpaceDebugColor where rawOffset = #offset struct cpSpaceDebugColor, r
instance Offset "g" CpSpaceDebugColor where rawOffset = #offset struct cpSpaceDebugColor, g
instance Offset "b" CpSpaceDebugColor where rawOffset = #offset struct cpSpaceDebugColor, b
instance Offset "a" CpSpaceDebugColor where rawOffset = #offset struct cpSpaceDebugColor, a

instance Storable CpSpaceDebugColor where
  sizeOf _ = #size struct cpSpaceDebugColor
  alignment _ = #alignment struct cpSpaceDebugColor

  peek ptr =
    CpSpaceDebugColor
      <$> peek (offset @"r" ptr)
      <*> peek (offset @"g" ptr)
      <*> peek (offset @"b" ptr)
      <*> peek (offset @"a" ptr)

  poke ptr val = do
    pokeField @"r" ptr val
    pokeField @"g" ptr val
    pokeField @"b" ptr val
    pokeField @"a" ptr val

type CpSpaceDebugDrawCircleImpl =
          CpVect            -- ^ pos
       -> CpFloat           -- ^ angle
       -> CpFloat           -- ^ radius
       -> CpSpaceDebugColor -- ^ outlineColor
       -> CpSpaceDebugColor -- ^ fillColor
       -> CpDataPointer     -- ^ data
       -> IO ()

type CpSpaceDebugDrawSegmentImpl =
          CpVect            -- ^ a
       -> CpVect            -- ^ b
       -> CpSpaceDebugColor -- ^ color
       -> CpDataPointer     -- ^ data
       -> IO ()

type CpSpaceDebugDrawFatSegmentImpl =
          CpVect            -- ^ a
       -> CpVect            -- ^ b
       -> CpFloat           -- ^ radius
       -> CpSpaceDebugColor -- ^ outlineColor
       -> CpSpaceDebugColor -- ^ fillColor
       -> CpDataPointer     -- ^ data
       -> IO ()

type CpSpaceDebugDrawPolygonImpl =
          #{type int}       -- ^ count
       -> Ptr CpVect        -- ^ verts
       -> CpFloat           -- ^ radius
       -> CpSpaceDebugColor -- ^ outlineColor
       -> CpSpaceDebugColor -- ^ fillColor
       -> CpDataPointer     -- ^ data
       -> IO ()

type CpSpaceDebugDrawDotImpl =
          CpFloat           -- ^ size
       -> CpVect            -- ^ pos
       -> CpSpaceDebugColor -- ^ color
       -> CpDataPointer     -- ^ data
       -> IO ()

type CpSpaceDebugDrawColorForShapeImpl =
          Ptr CpShape          -- ^ shape
       -> CpDataPointer        -- ^ data
       -> IO CpSpaceDebugColor

type CpSpaceDebugDrawFlags = #type cpSpaceDebugDrawFlags

pattern CP_SPACE_DEBUG_DRAW_SHAPES
      , CP_SPACE_DEBUG_DRAW_CONSTRAINTS
      , CP_SPACE_DEBUG_DRAW_COLLISION_POINTS
     :: (Eq a, Num a) => a
pattern CP_SPACE_DEBUG_DRAW_SHAPES           = #const CP_SPACE_DEBUG_DRAW_SHAPES
pattern CP_SPACE_DEBUG_DRAW_CONSTRAINTS      = #const CP_SPACE_DEBUG_DRAW_CONSTRAINTS
pattern CP_SPACE_DEBUG_DRAW_COLLISION_POINTS = #const CP_SPACE_DEBUG_DRAW_COLLISION_POINTS

data {-# CTYPE "chipmunk/chipmunk.h" "cpSpaceDebugDrawOptions" #-} CpSpaceDebugDrawOptions =
       CpSpaceDebugDrawOptions
         { drawCircle          :: FunPtr CpSpaceDebugDrawCircleImpl
         , drawSegment         :: FunPtr CpSpaceDebugDrawSegmentImpl
         , drawFatSegment      :: FunPtr CpSpaceDebugDrawFatSegmentImpl
         , drawPolygon         :: FunPtr CpSpaceDebugDrawPolygonImpl
         , drawDot             :: FunPtr CpSpaceDebugDrawDotImpl
         , flags               :: CpSpaceDebugDrawFlags
         , shapeOutlineColor   :: CpSpaceDebugColor
         , colorForShape       :: FunPtr CpSpaceDebugDrawColorForShapeImpl
         , constraintColor     :: CpSpaceDebugColor
         , collisionPointColor :: CpSpaceDebugColor
         , data_               :: CpDataPointer
         }

instance Offset "drawCircle"           CpSpaceDebugDrawOptions where rawOffset = #offset struct cpSpaceDebugDrawOptions, drawCircle
instance Offset "drawSegment"          CpSpaceDebugDrawOptions where rawOffset = #offset struct cpSpaceDebugDrawOptions, drawSegment
instance Offset "drawFatSegment"       CpSpaceDebugDrawOptions where rawOffset = #offset struct cpSpaceDebugDrawOptions, drawFatSegment
instance Offset "drawPolygon"          CpSpaceDebugDrawOptions where rawOffset = #offset struct cpSpaceDebugDrawOptions, drawPolygon
instance Offset "drawDot"              CpSpaceDebugDrawOptions where rawOffset = #offset struct cpSpaceDebugDrawOptions, drawDot
instance Offset "flags"                CpSpaceDebugDrawOptions where rawOffset = #offset struct cpSpaceDebugDrawOptions, flags
instance Offset "shapeOutlineColor"    CpSpaceDebugDrawOptions where rawOffset = #offset struct cpSpaceDebugDrawOptions, shapeOutlineColor
instance Offset "colorForShape"        CpSpaceDebugDrawOptions where rawOffset = #offset struct cpSpaceDebugDrawOptions, colorForShape
instance Offset "constraintColor"      CpSpaceDebugDrawOptions where rawOffset = #offset struct cpSpaceDebugDrawOptions, constraintColor
instance Offset "collisionPointColor"  CpSpaceDebugDrawOptions where rawOffset = #offset struct cpSpaceDebugDrawOptions, collisionPointColor
instance Offset "data_"                CpSpaceDebugDrawOptions where rawOffset = #offset struct cpSpaceDebugDrawOptions, data

instance Offset "data" CpSpaceDebugDrawOptions where
  rawOffset = rawOffset @"data_" @CpSpaceDebugDrawOptions

instance HasField "data" CpSpaceDebugDrawOptions CpDataPointer where
  getField = getField @"data_"

instance Storable CpSpaceDebugDrawOptions where
  sizeOf _ = #size struct cpSpaceDebugDrawOptions
  alignment _ = #alignment struct cpSpaceDebugDrawOptions

  peek ptr =
    CpSpaceDebugDrawOptions
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
  cpSpaceDebugDraw
    :: Ptr CpSpace                 -- ^ space
    -> Ptr CpSpaceDebugDrawOptions -- ^ options
    -> IO ()

#endif

-- end of cpSpace.h

pattern CP_VERSION_MAJOR
      , CP_VERSION_MINOR
      , CP_VERSION_RELEASE
     :: (Eq a, Num a) => a
pattern CP_VERSION_MAJOR = #const CP_VERSION_MAJOR
pattern CP_VERSION_MINOR = #const CP_VERSION_MINOR
pattern CP_VERSION_RELEASE = #const CP_VERSION_RELEASE

foreign import capi unsafe "chipmunk/chipmunk.h value cpVersionString"
  cpVersionString :: IO (Ptr #type char)

foreign import CALLCV unsafe "wrapper.h w_cpMomentForCircle"
  cpMomentForCircle
    :: CpFloat    -- ^ m
    -> CpFloat    -- ^ r1
    -> CpFloat    -- ^ r2
    -> Ptr CpVect -- ^ offset
    -> IO CpFloat

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpAreaForCircle"
  cpAreaForCircle
    :: CpFloat -- ^ r1
    -> CpFloat -- ^ r2
    -> IO CpFloat

foreign import CALLCV unsafe "wrapper.h w_cpMomentForSegment"
  cpMomentForSegment
    :: CpFloat    -- ^ m
    -> Ptr CpVect -- ^ a
    -> Ptr CpVect -- ^ b
    -> CpFloat    -- ^ radius
    -> IO CpFloat

foreign import CALLCV unsafe "wrapper.h w_cpAreaForSegment"
  cpAreaForSegment
    :: Ptr CpVect -- ^ a
    -> Ptr CpVect -- ^ b
    -> CpFloat    -- ^ radius
    -> IO CpFloat

foreign import CALLCV unsafe "wrapper.h w_cpMomentForPoly"
  cpMomentForPoly
    :: CpFloat     -- ^ m
    -> #{type int} -- ^ count
    -> Ptr CpVect  -- ^ verts
    -> Ptr CpVect  -- ^ offset
    -> CpFloat     -- ^ radius
    -> IO CpFloat

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpAreaForPoly"
  cpAreaForPoly
    :: #{type int} -- ^ count
    -> Ptr CpVect  -- ^ verts
    -> CpFloat     -- ^ radius
    -> IO CpFloat

foreign import CALLCV unsafe "wrapper.h w_cpCentroidForPoly"
  cpCentroidForPoly
    :: #{type int} -- ^ count
    -> Ptr CpVect  -- ^ verts
    -> Ptr CpVect  -- ^ Output value
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpMomentForBox"
  cpMomentForBox
    :: CpFloat    -- ^ m
    -> CpFloat    -- ^ width
    -> CpFloat    -- ^ height
    -> IO CpFloat

foreign import CALLCV unsafe "wrapper.h w_cpMomentForBox2"
  cpMomentForBox2
    :: CpFloat    -- ^ m
    -> Ptr CpBB   -- ^ box
    -> IO CpFloat

foreign import CALLCV unsafe "chipmunk/chipmunk.h cpConvexHull"
  cpConvexHull
    :: #{type int}     -- ^ count
    -> Ptr CpVect      -- ^ verts
    -> Ptr CpVect      -- ^ result
    -> Ptr #{type int} -- ^ first
    -> CpFloat         -- ^ tol
    -> IO #type int


foreign import CALLCV unsafe "wrapper.h w_cpClosetPointOnSegment"
  cpClosetPointOnSegment
    :: Ptr CpVect -- ^ p
    -> Ptr CpVect -- ^ a
    -> Ptr CpVect -- ^ b
    -> Ptr CpVect -- ^ Output value
    -> IO ()

-- * chipmunk_structs.h

#include <chipmunk/chipmunk_structs.h>

data {-# CTYPE "chipmunk/chipmunk_structs.h" "cpArray" #-} CpArray =
       CpArray
         { num :: #type int
         , max :: #type int
         , arr :: Ptr (Ptr ())
         }

instance Offset "num" CpArray where rawOffset = #offset struct cpArray, num
instance Offset "max" CpArray where rawOffset = #offset struct cpArray, max
instance Offset "arr" CpArray where rawOffset = #offset struct cpArray, arr

instance Storable CpArray where
  sizeOf _ = #size struct cpArray
  alignment _ = #alignment struct cpArray

  peek ptr =
    CpArray
      <$> peek (offset @"num" ptr)
      <*> peek (offset @"max" ptr)
      <*> peek (offset @"arr" ptr)

  poke ptr val = do
    pokeField @"num" ptr val
    pokeField @"max" ptr val
    pokeField @"arr" ptr val

data {-# CTYPE "chipmunk/chipmunk.h" "cpBody" #-} CpBody =
       CpBody
         { velocity_func  :: FunPtr CpBodyVelocityFunc
         , position_func  :: FunPtr CpBodyPositionFunc
         , m              :: CpFloat
         , m_inv          :: CpFloat
         , i              :: CpFloat
         , i_inv          :: CpFloat
         , cog            :: CpVect
         , p              :: CpVect
         , v              :: CpVect
         , f              :: CpVect
         , a              :: CpFloat
         , w              :: CpFloat
         , t              :: CpFloat
         , transform      :: CpTransform
         , userData       :: CpDataPointer
         , v_bias         :: CpVect
         , w_bias         :: CpFloat
         , space          :: Ptr CpSpace
         , shapeList      :: Ptr CpShape
         , arbiterList    :: Ptr CpArbiter
         , constraintList :: Ptr CpConstraint
         , sleeping       :: CpBodySleeping
         }

instance Offset "velocity_func"  CpBody where rawOffset = #offset struct cpBody, velocity_func
instance Offset "position_func"  CpBody where rawOffset = #offset struct cpBody, position_func
instance Offset "m"              CpBody where rawOffset = #offset struct cpBody, m
instance Offset "m_inv"          CpBody where rawOffset = #offset struct cpBody, m_inv
instance Offset "i"              CpBody where rawOffset = #offset struct cpBody, i
instance Offset "i_inv"          CpBody where rawOffset = #offset struct cpBody, i_inv
instance Offset "cog"            CpBody where rawOffset = #offset struct cpBody, cog
instance Offset "p"              CpBody where rawOffset = #offset struct cpBody, p
instance Offset "v"              CpBody where rawOffset = #offset struct cpBody, v
instance Offset "f"              CpBody where rawOffset = #offset struct cpBody, f
instance Offset "a"              CpBody where rawOffset = #offset struct cpBody, a
instance Offset "w"              CpBody where rawOffset = #offset struct cpBody, w
instance Offset "t"              CpBody where rawOffset = #offset struct cpBody, t
instance Offset "transform"      CpBody where rawOffset = #offset struct cpBody, transform
instance Offset "userData"       CpBody where rawOffset = #offset struct cpBody, userData
instance Offset "v_bias"         CpBody where rawOffset = #offset struct cpBody, v_bias
instance Offset "w_bias"         CpBody where rawOffset = #offset struct cpBody, w_bias
instance Offset "space"          CpBody where rawOffset = #offset struct cpBody, space
instance Offset "shapeList"      CpBody where rawOffset = #offset struct cpBody, shapeList
instance Offset "arbiterList"    CpBody where rawOffset = #offset struct cpBody, arbiterList
instance Offset "constraintList" CpBody where rawOffset = #offset struct cpBody, constraintList
instance Offset "sleeping"       CpBody where rawOffset = #offset struct cpBody, sleeping

instance Storable CpBody where
  sizeOf _ = #size struct cpBody
  alignment _ = #alignment struct cpBody

  peek ptr =
    CpBody
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

data CpBodySleeping =
       CpBodySleeping
         { root     :: Ptr CpBody
         , next     :: Ptr CpBody
         , idleTime :: CpFloat
         }

instance Offset "root"     CpBodySleeping where rawOffset = #offset struct cpBodySleeping, root
instance Offset "next"     CpBodySleeping where rawOffset = #offset struct cpBodySleeping, next
instance Offset "idleTime" CpBodySleeping where rawOffset = #offset struct cpBodySleeping, idleTime

instance Storable CpBodySleeping where
  sizeOf _ = #size struct cpBodySleeping
  alignment _ = #alignment struct cpBodySleeping

  peek ptr =
    CpBodySleeping
      <$> peek (offset @"root"     ptr)
      <*> peek (offset @"next"     ptr)
      <*> peek (offset @"idleTime" ptr)

  poke ptr val = do
    pokeField @"root"     ptr val
    pokeField @"next"     ptr val
    pokeField @"idleTime" ptr val

type CpArbiterState = #type enum cpArbiterState

pattern CP_ARBITER_STATE_FIRST_COLLISION
      , CP_ARBITER_STATE_NORMAL
      , CP_ARBITER_STATE_IGNORE
      , CP_ARBITER_STATE_CACHED
      , CP_ARBITER_STATE_INVALIDATED
     :: (Eq a, Num a) => a
pattern CP_ARBITER_STATE_FIRST_COLLISION = #const CP_ARBITER_STATE_FIRST_COLLISION
pattern CP_ARBITER_STATE_NORMAL          = #const CP_ARBITER_STATE_NORMAL
pattern CP_ARBITER_STATE_IGNORE          = #const CP_ARBITER_STATE_IGNORE
pattern CP_ARBITER_STATE_CACHED          = #const CP_ARBITER_STATE_CACHED
pattern CP_ARBITER_STATE_INVALIDATED     = #const CP_ARBITER_STATE_INVALIDATED

data {-# CTYPE "chipmunk/chipmunk_structs.h" "cpArbiterThread" #-} CpArbiterThread =
       CpArbiterThread
         { next :: Ptr CpArbiter
         , prev :: Ptr CpArbiter
         }

instance Offset "next" CpArbiterThread where rawOffset = #offset struct cpArbiterThread, next
instance Offset "prev" CpArbiterThread where rawOffset = #offset struct cpArbiterThread, prev

instance Storable CpArbiterThread where
  sizeOf _ = #size struct cpArbiterThread
  alignment _ = #alignment struct cpArbiterThread

  peek ptr =
    CpArbiterThread
      <$> peek (offset @"next" ptr)
      <*> peek (offset @"prev" ptr)

  poke ptr val = do
    pokeField @"next" ptr val
    pokeField @"prev" ptr val

data {-# CTYPE "chipmunk/chipmunk_structs.h" "cpContact" #-} CpContact =
       CpContact
         { r1     :: CpVect
         , r2     :: CpVect
         , nMass  :: CpFloat
         , tMass  :: CpFloat
         , bounce :: CpFloat
         , jnAcc  :: CpFloat
         , jtAcc  :: CpFloat
         , jBias  :: CpFloat
         , bias   :: CpFloat
         , hash   :: CpHashValue
         }

instance Offset "r1"     CpContact where rawOffset = #offset struct cpContact, r1
instance Offset "r2"     CpContact where rawOffset = #offset struct cpContact, r2
instance Offset "nMass"  CpContact where rawOffset = #offset struct cpContact, nMass
instance Offset "tMass"  CpContact where rawOffset = #offset struct cpContact, tMass
instance Offset "bounce" CpContact where rawOffset = #offset struct cpContact, bounce
instance Offset "jnAcc"  CpContact where rawOffset = #offset struct cpContact, jnAcc
instance Offset "jtAcc"  CpContact where rawOffset = #offset struct cpContact, jtAcc
instance Offset "jBias"  CpContact where rawOffset = #offset struct cpContact, jBias
instance Offset "bias"   CpContact where rawOffset = #offset struct cpContact, bias
instance Offset "hash"   CpContact where rawOffset = #offset struct cpContact, hash

instance Storable CpContact where
  sizeOf _ = #size struct cpContact
  alignment _ = #alignment struct cpContact

  peek ptr =
    CpContact
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

data {-# CTYPE "chipmunk/chipmunk_structs.h" "cpCollisionInfo" #-} CpCollisionInfo =
       CpCollisionInfo
         { a     :: Ptr CpShape
         , b     :: Ptr CpShape
         , id    :: CpCollisionID
         , n     :: CpVect
         , count :: #type int
         , arr   :: Ptr CpContact
         }

instance Offset "a"     CpCollisionInfo where rawOffset = #offset struct cpCollisionInfo, a
instance Offset "b"     CpCollisionInfo where rawOffset = #offset struct cpCollisionInfo, b
instance Offset "id"    CpCollisionInfo where rawOffset = #offset struct cpCollisionInfo, id
instance Offset "n"     CpCollisionInfo where rawOffset = #offset struct cpCollisionInfo, n
instance Offset "count" CpCollisionInfo where rawOffset = #offset struct cpCollisionInfo, count
instance Offset "arr"   CpCollisionInfo where rawOffset = #offset struct cpCollisionInfo, arr

instance Storable CpCollisionInfo where
  sizeOf _ = #size struct cpCollisionInfo
  alignment _ = #alignment struct cpCollisionInfo

  peek ptr =
    CpCollisionInfo
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

data {-# CTYPE "chipmunk/chipmunk.h" "cpArbiter" #-} CpArbiter =
       CpArbiter
         { e          :: CpFloat
         , u          :: CpFloat
         , surface_vr :: CpVect
         , data_      :: CpDataPointer
         , a          :: Ptr CpShape
         , b          :: Ptr CpShape
         , body_a     :: Ptr CpBody
         , body_b     :: Ptr CpBody
         , thread_a   :: CpArbiterThread
         , thread_b   :: CpArbiterThread
         , count      :: #type int
         , contacts   :: Ptr CpContact
         , n          :: CpVect
         , handler    :: Ptr CpCollisionHandler
         , handlerA   :: Ptr CpCollisionHandler
         , handlerB   :: Ptr CpCollisionHandler
         , swapped    :: CpBool
         , stamp      :: CpTimestamp
         , state      :: CpArbiterState
         }

instance Offset "e"          CpArbiter where rawOffset = #offset struct cpArbiter, e
instance Offset "u"          CpArbiter where rawOffset = #offset struct cpArbiter, u
instance Offset "surface_vr" CpArbiter where rawOffset = #offset struct cpArbiter, surface_vr
instance Offset "data_"      CpArbiter where rawOffset = #offset struct cpArbiter, data
instance Offset "a"          CpArbiter where rawOffset = #offset struct cpArbiter, a
instance Offset "b"          CpArbiter where rawOffset = #offset struct cpArbiter, b
instance Offset "body_a"     CpArbiter where rawOffset = #offset struct cpArbiter, body_a
instance Offset "body_b"     CpArbiter where rawOffset = #offset struct cpArbiter, body_b
instance Offset "thread_a"   CpArbiter where rawOffset = #offset struct cpArbiter, thread_a
instance Offset "thread_b"   CpArbiter where rawOffset = #offset struct cpArbiter, thread_b
instance Offset "count"      CpArbiter where rawOffset = #offset struct cpArbiter, count
instance Offset "contacts"   CpArbiter where rawOffset = #offset struct cpArbiter, contacts
instance Offset "n"          CpArbiter where rawOffset = #offset struct cpArbiter, n
instance Offset "handler"    CpArbiter where rawOffset = #offset struct cpArbiter, handler
instance Offset "handlerA"   CpArbiter where rawOffset = #offset struct cpArbiter, handlerA
instance Offset "handlerB"   CpArbiter where rawOffset = #offset struct cpArbiter, handlerB
instance Offset "swapped"    CpArbiter where rawOffset = #offset struct cpArbiter, swapped
instance Offset "stamp"      CpArbiter where rawOffset = #offset struct cpArbiter, stamp
instance Offset "state"      CpArbiter where rawOffset = #offset struct cpArbiter, state

instance Offset "data" CpArbiter where
  rawOffset = rawOffset @"data_" @CpArbiter

instance HasField "data" CpArbiter CpDataPointer where
  getField = getField @"data_"

instance Storable CpArbiter where
  sizeOf _ = #size struct cpArbiter
  alignment _ = #alignment struct cpArbiter

  peek ptr =
    CpArbiter
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

data {-# CTYPE "chipmunk/chipmunk_structs.h" "cpShapeMassInfo" #-} CpShapeMassInfo =
       CpShapeMassInfo
         { m    :: CpFloat
         , i    :: CpFloat
         , cog  :: CpVect
         , area :: CpFloat
         }

instance Offset "m"    CpShapeMassInfo where rawOffset = #offset struct cpShapeMassInfo, m
instance Offset "i"    CpShapeMassInfo where rawOffset = #offset struct cpShapeMassInfo, i
instance Offset "cog"  CpShapeMassInfo where rawOffset = #offset struct cpShapeMassInfo, cog
instance Offset "area" CpShapeMassInfo where rawOffset = #offset struct cpShapeMassInfo, area

instance Storable CpShapeMassInfo where
  sizeOf _ = #size struct cpShapeMassInfo
  alignment _ = #alignment struct cpShapeMassInfo

  peek ptr =
    CpShapeMassInfo
      <$> peek (offset @"m"    ptr)
      <*> peek (offset @"i"    ptr)
      <*> peek (offset @"cog"  ptr)
      <*> peek (offset @"area" ptr)

  poke ptr val = do
    pokeField @"m"    ptr val
    pokeField @"i"    ptr val
    pokeField @"cog"  ptr val
    pokeField @"area" ptr val

type CpShapeType = #type cpShapeType

pattern CP_CIRCLE_SHAPE
      , CP_SEGMENT_SHAPE
      , CP_POLY_SHAPE
      , CP_NUM_SHAPES
     :: (Eq a, Num a) => a
pattern CP_CIRCLE_SHAPE  = #const CP_CIRCLE_SHAPE
pattern CP_SEGMENT_SHAPE = #const CP_SEGMENT_SHAPE
pattern CP_POLY_SHAPE    = #const CP_POLY_SHAPE
pattern CP_NUM_SHAPES    = #const CP_NUM_SHAPES

type CpShapeCacheDataImpl =
          Ptr CpShape -- ^ shape
       -> CpTransform -- ^ transform
       -> IO CpBB

type CpShapeDestroyImpl =
          Ptr CpShape -- ^shape
       -> IO ()

type CpShapePointQueryImpl =
          Ptr CpShape          -- ^ shape
       -> CpVect               -- ^ p
       -> Ptr CpPointQueryInfo -- ^ info
       -> IO ()

type CpShapeSegmentQueryImpl =
          Ptr CpShape            -- ^ shape
       -> CpVect                 -- ^ a
       -> CpVect                 -- ^ b
       -> CpFloat                -- ^ radius
       -> Ptr CpSegmentQueryInfo -- ^ info
       -> IO ()

data {-# CTYPE "chipmunk/chipmunk_structs.h" "cpShapeClass" #-} CpShapeClass =
       CpShapeClass
         { type_        :: CpShapeType
         , cacheData    :: FunPtr CpShapeCacheDataImpl
         , destroy      :: FunPtr CpShapeDestroyImpl
         , pointQuery   :: FunPtr CpShapePointQueryImpl
         , segmentQuery :: FunPtr CpShapeSegmentQueryImpl
         }

instance Offset "type_"        CpShapeClass where rawOffset = #offset struct cpShapeClass, type
instance Offset "cacheData"    CpShapeClass where rawOffset = #offset struct cpShapeClass, cacheData
instance Offset "destroy"      CpShapeClass where rawOffset = #offset struct cpShapeClass, destroy
instance Offset "pointQuery"   CpShapeClass where rawOffset = #offset struct cpShapeClass, pointQuery
instance Offset "segmentQuery" CpShapeClass where rawOffset = #offset struct cpShapeClass, segmentQuery

instance Offset "type" CpShapeClass where
  rawOffset = rawOffset @"type_" @CpShapeClass

instance HasField "type" CpShapeClass CpShapeType where
  getField = getField @"type_"

instance Storable CpShapeClass where
  sizeOf _ = #size struct cpShapeClass
  alignment _ = #alignment struct cpShapeClass

  peek ptr =
    CpShapeClass
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

data {-# CTYPE "chipmunk/chipmunk.h" "cpShape" #-} CpShape =
       CpShape
         { klass    :: Ptr CpShapeClass
         , space    :: Ptr CpSpace
         , body     :: Ptr CpBody
         , massInfo :: CpShapeMassInfo
         , bb       :: CpBB
         , sensor   :: CpBool
         , e        :: CpFloat
         , u        :: CpFloat
         , surfaceV :: CpVect
         , userData :: CpDataPointer
         , type_    :: CpCollisionType
         , filter   :: CpShapeFilter
         , next     :: Ptr CpShape
         , prev     :: Ptr CpShape
         , hashid   :: CpHashValue
         }

instance Offset "klass"    CpShape where rawOffset = #offset struct cpShape, klass
instance Offset "space"    CpShape where rawOffset = #offset struct cpShape, space
instance Offset "body"     CpShape where rawOffset = #offset struct cpShape, body
instance Offset "massInfo" CpShape where rawOffset = #offset struct cpShape, massInfo
instance Offset "bb"       CpShape where rawOffset = #offset struct cpShape, bb
instance Offset "sensor"   CpShape where rawOffset = #offset struct cpShape, sensor
instance Offset "e"        CpShape where rawOffset = #offset struct cpShape, e
instance Offset "u"        CpShape where rawOffset = #offset struct cpShape, u
instance Offset "surfaceV" CpShape where rawOffset = #offset struct cpShape, surfaceV
instance Offset "userData" CpShape where rawOffset = #offset struct cpShape, userData
instance Offset "type_"    CpShape where rawOffset = #offset struct cpShape, type
instance Offset "filter"   CpShape where rawOffset = #offset struct cpShape, filter
instance Offset "next"     CpShape where rawOffset = #offset struct cpShape, next
instance Offset "prev"     CpShape where rawOffset = #offset struct cpShape, prev
instance Offset "hashid"   CpShape where rawOffset = #offset struct cpShape, hashid

instance Offset "type" CpShape where
  rawOffset = rawOffset @"type_" @CpShape

instance HasField "type" CpShape CpCollisionType where
  getField = getField @"type_"

instance Storable CpShape where
  sizeOf _ = #size struct cpShape
  alignment _ = #alignment struct cpShape

  peek ptr =
    CpShape
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

data {-# CTYPE "chipmunk/chipmunk.h" "cpCircleShape" #-} CpCircleShape =
       CpCircleShape
         { shape :: CpShape
         , c     :: CpVect
         , tc    :: CpVect
         , r     :: CpFloat
         }

instance Offset "shape" CpCircleShape where rawOffset = #offset struct cpCircleShape, shape
instance Offset "c"     CpCircleShape where rawOffset = #offset struct cpCircleShape, c
instance Offset "tc"    CpCircleShape where rawOffset = #offset struct cpCircleShape, tc
instance Offset "r"     CpCircleShape where rawOffset = #offset struct cpCircleShape, r

instance Storable CpCircleShape where
  sizeOf _ = #size struct cpCircleShape
  alignment _ = #alignment struct cpCircleShape

  peek ptr =
    CpCircleShape
      <$> peek (offset @"shape" ptr)
      <*> peek (offset @"c"     ptr)
      <*> peek (offset @"tc"    ptr)
      <*> peek (offset @"r"     ptr)

  poke ptr val = do
    pokeField @"shape" ptr val
    pokeField @"c"     ptr val
    pokeField @"tc"    ptr val
    pokeField @"r"     ptr val

data {-# CTYPE "chipmunk/chipmunk.h" "cpSegmentShape" #-} CpSegmentShape =
       CpSegmentShape
         { shape     :: CpShape
         , a         :: CpVect
         , b         :: CpVect
         , n         :: CpVect
         , ta        :: CpVect
         , tb        :: CpVect
         , tn        :: CpVect
         , r         :: CpFloat
         , a_tangent :: CpVect
         , b_tangent :: CpVect
         }

instance Offset "shape"     CpSegmentShape where rawOffset = #offset struct cpSegmentShape, shape
instance Offset "a"         CpSegmentShape where rawOffset = #offset struct cpSegmentShape, a
instance Offset "b"         CpSegmentShape where rawOffset = #offset struct cpSegmentShape, b
instance Offset "n"         CpSegmentShape where rawOffset = #offset struct cpSegmentShape, n
instance Offset "ta"        CpSegmentShape where rawOffset = #offset struct cpSegmentShape, ta
instance Offset "tb"        CpSegmentShape where rawOffset = #offset struct cpSegmentShape, tb
instance Offset "tn"        CpSegmentShape where rawOffset = #offset struct cpSegmentShape, tn
instance Offset "r"         CpSegmentShape where rawOffset = #offset struct cpSegmentShape, r
instance Offset "a_tangent" CpSegmentShape where rawOffset = #offset struct cpSegmentShape, a_tangent
instance Offset "b_tangent" CpSegmentShape where rawOffset = #offset struct cpSegmentShape, b_tangent

instance Storable CpSegmentShape where
  sizeOf _ = #size struct cpSegmentShape
  alignment _ = #alignment struct cpSegmentShape

  peek ptr =
    CpSegmentShape
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

data {-# CTYPE "chipmunk/chipmunk_structs.h" "cpSplittingPlane" #-} CpSplittingPlane =
       CpSplittingPlane
         { v0 :: CpVect
         , n  :: CpVect
         }

instance Offset "v0"  CpSplittingPlane where rawOffset = #offset struct cpSplittingPlane, v0
instance Offset "n"   CpSplittingPlane where rawOffset = #offset struct cpSplittingPlane, n

instance Storable CpSplittingPlane where
  sizeOf _ = #size struct cpSplittingPlane
  alignment _ = #alignment struct cpSplittingPlane

  peek ptr =
    CpSplittingPlane
      <$> peek (offset @"v0" ptr)
      <*> peek (offset @"n"  ptr)

  poke ptr val = do
    pokeField @"v0" ptr val
    pokeField @"n"  ptr val

pattern CP_POLY_SHAPE_INLINE_ALLOC :: (Eq a, Num a) => a
pattern CP_POLY_SHAPE_INLINE_ALLOC = #const CP_POLY_SHAPE_INLINE_ALLOC

data {-# CTYPE "chipmunk/chipmunk.h" "cpPolyShape" #-} CpPolyShape =
       CpPolyShape
         { shape   :: CpShape
         , r       :: CpFloat
         , count   :: #type int
         , planes  :: Ptr CpSplittingPlane
         , _planes :: Ptr CpSplittingPlane
         }

instance Offset "shape"    CpPolyShape where rawOffset = #offset struct cpPolyShape, shape
instance Offset "r"        CpPolyShape where rawOffset = #offset struct cpPolyShape, r
instance Offset "count"    CpPolyShape where rawOffset = #offset struct cpPolyShape, count
instance Offset "planes"   CpPolyShape where rawOffset = #offset struct cpPolyShape, planes
instance Offset "_planes"  CpPolyShape where rawOffset = #offset struct cpPolyShape, _planes

instance Storable CpPolyShape where
  sizeOf _ = #size struct cpPolyShape
  alignment _ = #alignment struct cpPolyShape

  peek ptr =
    CpPolyShape
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

type CpConstraintPreStepImpl =
          Ptr CpConstraint -- ^ constraint
       -> CpFloat          -- ^ dt
       -> IO ()

type CpConstraintApplyCachedImpulseImpl =
          Ptr CpConstraint -- ^ constraint
       -> CpFloat          -- ^ dt_coef
       -> IO ()

type CpConstraintApplyImpulseImpl =
          Ptr CpConstraint -- ^ constraint
       -> CpFloat          -- ^ dt
       -> IO ()

type CpConstraintGetImpulseImpl =
          Ptr CpConstraint -- ^ constraint
       -> IO CpFloat

data {-# CTYPE "chipmunk/chipmunk_structs.h" "cpConstraintClass" #-} CpConstraintClass =
       CpConstraintClass
         { preStep            :: FunPtr CpConstraintPreStepImpl
         , applyCachedImpulse :: FunPtr CpConstraintApplyCachedImpulseImpl
         , applyImpulse       :: FunPtr CpConstraintApplyImpulseImpl
         , getImpulse         :: FunPtr CpConstraintGetImpulseImpl
         }

instance Offset "preStep"            CpConstraintClass where rawOffset = #offset struct cpConstraintClass, preStep
instance Offset "applyCachedImpulse" CpConstraintClass where rawOffset = #offset struct cpConstraintClass, applyCachedImpulse
instance Offset "applyImpulse"       CpConstraintClass where rawOffset = #offset struct cpConstraintClass, applyImpulse
instance Offset "getImpulse"         CpConstraintClass where rawOffset = #offset struct cpConstraintClass, getImpulse

instance Storable CpConstraintClass where
  sizeOf _ = #size struct cpConstraintClass
  alignment _ = #alignment struct cpConstraintClass

  peek ptr =
    CpConstraintClass
      <$> peek (offset @"preStep"            ptr)
      <*> peek (offset @"applyCachedImpulse" ptr)
      <*> peek (offset @"applyImpulse"       ptr)
      <*> peek (offset @"getImpulse"         ptr)

  poke ptr val = do
    pokeField @"preStep"            ptr val
    pokeField @"applyCachedImpulse" ptr val
    pokeField @"applyImpulse"       ptr val
    pokeField @"getImpulse"         ptr val

data {-# CTYPE "chipmunk/chipmunk.h" "cpConstraint" #-} CpConstraint =
       CpConstraint
         { klass         :: Ptr CpConstraintClass
         , space         :: Ptr CpSpace
         , a             :: Ptr CpBody
         , b             :: Ptr CpBody
         , next_a        :: Ptr CpConstraint
         , next_b        :: Ptr CpConstraint
         , maxForce      :: CpFloat
         , errorBias     :: CpFloat
         , maxBias       :: CpFloat
         , collideBodies :: CpBool
         , preSolve      :: FunPtr CpConstraintPreSolveFunc
         , postSolve     :: FunPtr CpConstraintPostSolveFunc
         , userData      :: CpDataPointer
         }

instance Offset "klass"         CpConstraint where rawOffset = #offset struct cpConstraint, klass
instance Offset "space"         CpConstraint where rawOffset = #offset struct cpConstraint, space
instance Offset "a"             CpConstraint where rawOffset = #offset struct cpConstraint, a
instance Offset "b"             CpConstraint where rawOffset = #offset struct cpConstraint, b
instance Offset "next_a"        CpConstraint where rawOffset = #offset struct cpConstraint, next_a
instance Offset "next_b"        CpConstraint where rawOffset = #offset struct cpConstraint, next_b
instance Offset "maxForce"      CpConstraint where rawOffset = #offset struct cpConstraint, maxForce
instance Offset "errorBias"     CpConstraint where rawOffset = #offset struct cpConstraint, errorBias
instance Offset "maxBias"       CpConstraint where rawOffset = #offset struct cpConstraint, maxBias
instance Offset "collideBodies" CpConstraint where rawOffset = #offset struct cpConstraint, collideBodies
instance Offset "preSolve"      CpConstraint where rawOffset = #offset struct cpConstraint, preSolve
instance Offset "postSolve"     CpConstraint where rawOffset = #offset struct cpConstraint, postSolve
instance Offset "userData"      CpConstraint where rawOffset = #offset struct cpConstraint, userData

instance Storable CpConstraint where
  sizeOf _ = #size struct cpConstraint
  alignment _ = #alignment struct cpConstraint

  peek ptr =
    CpConstraint
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

data {-# CTYPE "chipmunk/chipmunk.h" "cpPinJoint" #-} CpPinJoint =
       CpPinJoint
         { constraint :: CpConstraint
         , anchorA    :: CpVect
         , anchorB    :: CpVect
         , dist       :: CpFloat
         , r1         :: CpVect
         , r2         :: CpVect
         , n          :: CpVect
         , nMass      :: CpFloat
         , jnAcc      :: CpFloat
         , bias       :: CpFloat
         }

instance Offset "constraint" CpPinJoint where rawOffset = #offset struct cpPinJoint, constraint
instance Offset "anchorA"    CpPinJoint where rawOffset = #offset struct cpPinJoint, anchorA
instance Offset "anchorB"    CpPinJoint where rawOffset = #offset struct cpPinJoint, anchorB
instance Offset "dist"       CpPinJoint where rawOffset = #offset struct cpPinJoint, dist
instance Offset "r1"         CpPinJoint where rawOffset = #offset struct cpPinJoint, r1
instance Offset "r2"         CpPinJoint where rawOffset = #offset struct cpPinJoint, r2
instance Offset "n"          CpPinJoint where rawOffset = #offset struct cpPinJoint, n
instance Offset "nMass"      CpPinJoint where rawOffset = #offset struct cpPinJoint, nMass
instance Offset "jnAcc"      CpPinJoint where rawOffset = #offset struct cpPinJoint, jnAcc
instance Offset "bias"       CpPinJoint where rawOffset = #offset struct cpPinJoint, bias

instance Storable CpPinJoint where
  sizeOf _ = #size struct cpPinJoint
  alignment _ = #alignment struct cpPinJoint

  peek ptr =
    CpPinJoint
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

data {-# CTYPE "chipmunk/chipmunk.h" "cpSlideJoint" #-} CpSlideJoint =
       CpSlideJoint
         { constraint :: CpConstraint
         , anchorA    :: CpVect
         , anchorB    :: CpVect
         , min        :: CpFloat
         , max        :: CpFloat
         , r1         :: CpVect
         , r2         :: CpVect
         , n          :: CpVect
         , nMass      :: CpFloat
         , jnAcc      :: CpFloat
         , bias       :: CpFloat
         }

instance Offset "constraint" CpSlideJoint where rawOffset = #offset struct cpSlideJoint, constraint
instance Offset "anchorA"    CpSlideJoint where rawOffset = #offset struct cpSlideJoint, anchorA
instance Offset "anchorB"    CpSlideJoint where rawOffset = #offset struct cpSlideJoint, anchorB
instance Offset "min"        CpSlideJoint where rawOffset = #offset struct cpSlideJoint, min
instance Offset "max"        CpSlideJoint where rawOffset = #offset struct cpSlideJoint, max
instance Offset "r1"         CpSlideJoint where rawOffset = #offset struct cpSlideJoint, r1
instance Offset "r2"         CpSlideJoint where rawOffset = #offset struct cpSlideJoint, r2
instance Offset "n"          CpSlideJoint where rawOffset = #offset struct cpSlideJoint, n
instance Offset "nMass"      CpSlideJoint where rawOffset = #offset struct cpSlideJoint, nMass
instance Offset "jnAcc"      CpSlideJoint where rawOffset = #offset struct cpSlideJoint, jnAcc
instance Offset "bias"       CpSlideJoint where rawOffset = #offset struct cpSlideJoint, bias

instance Storable CpSlideJoint where
  sizeOf _ = #size struct cpSlideJoint
  alignment _ = #alignment struct cpSlideJoint

  peek ptr =
    CpSlideJoint
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

data {-# CTYPE "chipmunk/chipmunk.h" "cpPivotJoint" #-} CpPivotJoint =
       CpPivotJoint
         { constraint :: CpConstraint
         , anchorA    :: CpVect
         , anchorB    :: CpVect
         , r1         :: CpVect
         , r2         :: CpVect
         , k          :: CpMat2x2
         , jAcc       :: CpVect
         , bias       :: CpVect
         }

instance Offset "constraint" CpPivotJoint where rawOffset = #offset struct cpPivotJoint, constraint
instance Offset "anchorA"    CpPivotJoint where rawOffset = #offset struct cpPivotJoint, anchorA
instance Offset "anchorB"    CpPivotJoint where rawOffset = #offset struct cpPivotJoint, anchorB
instance Offset "r1"         CpPivotJoint where rawOffset = #offset struct cpPivotJoint, r1
instance Offset "r2"         CpPivotJoint where rawOffset = #offset struct cpPivotJoint, r2
instance Offset "k"          CpPivotJoint where rawOffset = #offset struct cpPivotJoint, k
instance Offset "jAcc"       CpPivotJoint where rawOffset = #offset struct cpPivotJoint, jAcc
instance Offset "bias"       CpPivotJoint where rawOffset = #offset struct cpPivotJoint, bias

instance Storable CpPivotJoint where
  sizeOf _ = #size struct cpPivotJoint
  alignment _ = #alignment struct cpPivotJoint

  peek ptr =
    CpPivotJoint
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

data {-# CTYPE "chipmunk/chipmunk.h" "cpGrooveJoint" #-} CpGrooveJoint =
       CpGrooveJoint
         { constraint :: CpConstraint
         , grv_n      :: CpVect
         , grv_a      :: CpVect
         , grv_b      :: CpVect
         , anchorB    :: CpVect
         , grv_tn     :: CpVect
         , clamp      :: CpFloat
         , r1         :: CpVect
         , r2         :: CpVect
         , k          :: CpMat2x2
         , jAcc       :: CpVect
         , bias       :: CpVect
         }

instance Offset "constraint" CpGrooveJoint where rawOffset = #offset struct cpGrooveJoint, constraint
instance Offset "grv_n"      CpGrooveJoint where rawOffset = #offset struct cpGrooveJoint, grv_n
instance Offset "grv_a"      CpGrooveJoint where rawOffset = #offset struct cpGrooveJoint, grv_a
instance Offset "grv_b"      CpGrooveJoint where rawOffset = #offset struct cpGrooveJoint, grv_b
instance Offset "anchorB"    CpGrooveJoint where rawOffset = #offset struct cpGrooveJoint, anchorB
instance Offset "grv_tn"     CpGrooveJoint where rawOffset = #offset struct cpGrooveJoint, grv_tn
instance Offset "clamp"      CpGrooveJoint where rawOffset = #offset struct cpGrooveJoint, clamp
instance Offset "r1"         CpGrooveJoint where rawOffset = #offset struct cpGrooveJoint, r1
instance Offset "r2"         CpGrooveJoint where rawOffset = #offset struct cpGrooveJoint, r2
instance Offset "k"          CpGrooveJoint where rawOffset = #offset struct cpGrooveJoint, k
instance Offset "jAcc"       CpGrooveJoint where rawOffset = #offset struct cpGrooveJoint, jAcc
instance Offset "bias"       CpGrooveJoint where rawOffset = #offset struct cpGrooveJoint, bias

instance Storable CpGrooveJoint where
  sizeOf _ = #size struct cpGrooveJoint
  alignment _ = #alignment struct cpGrooveJoint

  peek ptr =
    CpGrooveJoint
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

data {-# CTYPE "chipmunk/chipmunk.h" "cpDampedSpring" #-} CpDampedSpring =
       CpDampedSpring
         { constraint      :: CpConstraint
         , anchorA         :: CpVect
         , anchorB         :: CpVect
         , restLength      :: CpFloat
         , stiffness       :: CpFloat
         , damping         :: CpFloat
         , springForceFunc :: FunPtr CpDampedSpringForceFunc
         , target_vrn      :: CpFloat
         , v_coef          :: CpFloat
         , r1              :: CpVect
         , r2              :: CpVect
         , nMass           :: CpFloat
         , n               :: CpVect
         , jAcc            :: CpFloat
         }

instance Offset "constraint"      CpDampedSpring where rawOffset = #offset struct cpDampedSpring, constraint
instance Offset "anchorA"         CpDampedSpring where rawOffset = #offset struct cpDampedSpring, anchorA
instance Offset "anchorB"         CpDampedSpring where rawOffset = #offset struct cpDampedSpring, anchorB
instance Offset "restLength"      CpDampedSpring where rawOffset = #offset struct cpDampedSpring, restLength
instance Offset "stiffness"       CpDampedSpring where rawOffset = #offset struct cpDampedSpring, stiffness
instance Offset "damping"         CpDampedSpring where rawOffset = #offset struct cpDampedSpring, damping
instance Offset "springForceFunc" CpDampedSpring where rawOffset = #offset struct cpDampedSpring, springForceFunc
instance Offset "target_vrn"      CpDampedSpring where rawOffset = #offset struct cpDampedSpring, target_vrn
instance Offset "v_coef"          CpDampedSpring where rawOffset = #offset struct cpDampedSpring, v_coef
instance Offset "r1"              CpDampedSpring where rawOffset = #offset struct cpDampedSpring, r1
instance Offset "r2"              CpDampedSpring where rawOffset = #offset struct cpDampedSpring, r2
instance Offset "nMass"           CpDampedSpring where rawOffset = #offset struct cpDampedSpring, nMass
instance Offset "n"               CpDampedSpring where rawOffset = #offset struct cpDampedSpring, n
instance Offset "jAcc"            CpDampedSpring where rawOffset = #offset struct cpDampedSpring, jAcc

instance Storable CpDampedSpring where
  sizeOf _ = #size struct cpDampedSpring
  alignment _ = #alignment struct cpDampedSpring

  peek ptr =
    CpDampedSpring
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

data {-# CTYPE "chipmunk/chipmunk.h" "cpDampedRotarySpring" #-} CpDampedRotarySpring =
       CpDampedRotarySpring
         { constraint       :: CpConstraint
         , restAngle        :: CpFloat
         , stiffness        :: CpFloat
         , damping          :: CpFloat
         , springTorqueFunc :: FunPtr CpDampedRotarySpringTorqueFunc
         , target_wrn       :: CpFloat
         , w_coef           :: CpFloat
         , iSum             :: CpFloat
         , jAcc             :: CpFloat
         }

instance Offset "constraint"       CpDampedRotarySpring where rawOffset = #offset struct cpDampedRotarySpring, constraint
instance Offset "restAngle"        CpDampedRotarySpring where rawOffset = #offset struct cpDampedRotarySpring, restAngle
instance Offset "stiffness"        CpDampedRotarySpring where rawOffset = #offset struct cpDampedRotarySpring, stiffness
instance Offset "damping"          CpDampedRotarySpring where rawOffset = #offset struct cpDampedRotarySpring, damping
instance Offset "springTorqueFunc" CpDampedRotarySpring where rawOffset = #offset struct cpDampedRotarySpring, springTorqueFunc
instance Offset "target_wrn"       CpDampedRotarySpring where rawOffset = #offset struct cpDampedRotarySpring, target_wrn
instance Offset "w_coef"           CpDampedRotarySpring where rawOffset = #offset struct cpDampedRotarySpring, w_coef
instance Offset "iSum"             CpDampedRotarySpring where rawOffset = #offset struct cpDampedRotarySpring, iSum
instance Offset "jAcc"             CpDampedRotarySpring where rawOffset = #offset struct cpDampedRotarySpring, jAcc

instance Storable CpDampedRotarySpring where
  sizeOf _ = #size struct cpDampedRotarySpring
  alignment _ = #alignment struct cpDampedRotarySpring

  peek ptr =
    CpDampedRotarySpring
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

data {-# CTYPE "chipmunk/chipmunk.h" "cpRotaryLimitJoint" #-} CpRotaryLimitJoint =
       CpRotaryLimitJoint
         { constraint :: CpConstraint
         , min        :: CpFloat
         , max        :: CpFloat
         , iSum       :: CpFloat
         , bias       :: CpFloat
         , jAcc       :: CpFloat
         }

instance Offset "constraint" CpRotaryLimitJoint where rawOffset = #offset struct cpRotaryLimitJoint, constraint
instance Offset "min"        CpRotaryLimitJoint where rawOffset = #offset struct cpRotaryLimitJoint, min
instance Offset "max"        CpRotaryLimitJoint where rawOffset = #offset struct cpRotaryLimitJoint, max
instance Offset "iSum"       CpRotaryLimitJoint where rawOffset = #offset struct cpRotaryLimitJoint, iSum
instance Offset "bias"       CpRotaryLimitJoint where rawOffset = #offset struct cpRotaryLimitJoint, bias
instance Offset "jAcc"       CpRotaryLimitJoint where rawOffset = #offset struct cpRotaryLimitJoint, jAcc

instance Storable CpRotaryLimitJoint where
  sizeOf _ = #size struct cpRotaryLimitJoint
  alignment _ = #alignment struct cpRotaryLimitJoint

  peek ptr =
    CpRotaryLimitJoint
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

data {-# CTYPE "chipmunk/chipmunk.h" "cpRatchetJoint" #-} CpRatchetJoint =
       CpRatchetJoint
         { constraint :: CpConstraint
         , angle      :: CpFloat
         , phase      :: CpFloat
         , ratchet    :: CpFloat
         , iSum       :: CpFloat
         , bias       :: CpFloat
         , jAcc       :: CpFloat
         }

instance Offset "constraint" CpRatchetJoint where rawOffset = #offset struct cpRatchetJoint, constraint
instance Offset "angle"      CpRatchetJoint where rawOffset = #offset struct cpRatchetJoint, angle
instance Offset "phase"      CpRatchetJoint where rawOffset = #offset struct cpRatchetJoint, phase
instance Offset "ratchet"    CpRatchetJoint where rawOffset = #offset struct cpRatchetJoint, ratchet
instance Offset "iSum"       CpRatchetJoint where rawOffset = #offset struct cpRatchetJoint, iSum
instance Offset "bias"       CpRatchetJoint where rawOffset = #offset struct cpRatchetJoint, bias
instance Offset "jAcc"       CpRatchetJoint where rawOffset = #offset struct cpRatchetJoint, jAcc

instance Storable CpRatchetJoint where
  sizeOf _ = #size struct cpRatchetJoint
  alignment _ = #alignment struct cpRatchetJoint

  peek ptr =
    CpRatchetJoint
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

data {-# CTYPE "chipmunk/chipmunk.h" "cpGearJoint" #-} CpGearJoint =
       CpGearJoint
         { constraint :: CpConstraint
         , phase      :: CpFloat
         , ratio      :: CpFloat
         , ratio_inv  :: CpFloat
         , iSum       :: CpFloat
         , bias       :: CpFloat
         , jAcc       :: CpFloat
         }

instance Offset "constraint" CpGearJoint where rawOffset = #offset struct cpGearJoint, constraint
instance Offset "phase"      CpGearJoint where rawOffset = #offset struct cpGearJoint, phase
instance Offset "ratio"      CpGearJoint where rawOffset = #offset struct cpGearJoint, ratio
instance Offset "ratio_inv"  CpGearJoint where rawOffset = #offset struct cpGearJoint, ratio_inv
instance Offset "iSum"       CpGearJoint where rawOffset = #offset struct cpGearJoint, iSum
instance Offset "bias"       CpGearJoint where rawOffset = #offset struct cpGearJoint, bias
instance Offset "jAcc"       CpGearJoint where rawOffset = #offset struct cpGearJoint, jAcc

instance Storable CpGearJoint where
  sizeOf _ = #size struct cpGearJoint
  alignment _ = #alignment struct cpGearJoint

  peek ptr =
    CpGearJoint
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

data {-# CTYPE "chipmunk/chipmunk.h" "cpSimpleMotor" #-} CpSimpleMotor =
       CpSimpleMotor
         { constraint :: CpConstraint
         , rate       :: CpFloat
         , iSum       :: CpFloat
         , jAcc       :: CpFloat
         }

instance Offset "constraint" CpSimpleMotor where rawOffset = #offset struct cpSimpleMotor, constraint
instance Offset "rate"       CpSimpleMotor where rawOffset = #offset struct cpSimpleMotor, rate
instance Offset "iSum"       CpSimpleMotor where rawOffset = #offset struct cpSimpleMotor, iSum
instance Offset "jAcc"       CpSimpleMotor where rawOffset = #offset struct cpSimpleMotor, jAcc

instance Storable CpSimpleMotor where
  sizeOf _ = #size struct cpSimpleMotor
  alignment _ = #alignment struct cpSimpleMotor

  peek ptr =
    CpSimpleMotor
      <$> peek (offset @"constraint" ptr)
      <*> peek (offset @"rate"       ptr)
      <*> peek (offset @"iSum"       ptr)
      <*> peek (offset @"jAcc"       ptr)

  poke ptr val = do
    pokeField @"constraint" ptr val
    pokeField @"rate"       ptr val
    pokeField @"iSum"       ptr val
    pokeField @"jAcc"       ptr val

data {-# CTYPE "chipmunk/chipmunk_structs.h" "cpContactBufferHeader" #-} CpContactBufferHeader

type CpSpaceArbiterApplyImpulseFunc =
          Ptr CpArbiter -- ^ arb
       -> IO ()

data {-# CTYPE "chipmunk/chipmunk.h" "cpSpace" #-} CpSpace =
       CpSpace
         { iterations           :: #type int
         , gravity              :: CpVect
         , damping              :: CpFloat
         , idleSpeedThreshold   :: CpFloat
         , sleepTimeThreshold   :: CpFloat
         , collisionSlop        :: CpFloat
         , collisionBias        :: CpFloat
         , collisionPersistence :: CpTimestamp
         , userData             :: CpDataPointer
         , stamp                :: CpTimestamp
         , curr_dt              :: CpFloat
         , dynamicBodies        :: Ptr CpArray
         , staticBodies         :: Ptr CpArray
         , rousedBodies         :: Ptr CpArray
         , sleepingComponents   :: Ptr CpArray
         , shapeIDCounter       :: CpHashValue
         , staticShapes         :: Ptr CpSpatialIndex
         , dynamicShapes        :: Ptr CpSpatialIndex
         , constraints          :: Ptr CpArray
         , arbiters             :: Ptr CpArray
         , contactBuffersHead   :: Ptr CpContactBufferHeader
         , cachedArbiters       :: Ptr CpHashSet
         , pooledArbiters       :: Ptr CpArray
         , allocatedBuffers     :: Ptr CpArray
         , locked               :: #type unsigned int
         , usesWildcards        :: CpBool
         , collisionHandlers    :: Ptr CpHashSet
         , defaultHandler       :: CpCollisionHandler
         , skipPostStep         :: CpBool
         , postStepCallbacks    :: Ptr CpArray
         , staticBody           :: Ptr CpBody
         , _staticBody          :: CpBody
         }

instance Offset "iterations"           CpSpace where rawOffset = #offset struct cpSpace, iterations
instance Offset "gravity"              CpSpace where rawOffset = #offset struct cpSpace, gravity
instance Offset "damping"              CpSpace where rawOffset = #offset struct cpSpace, damping
instance Offset "idleSpeedThreshold"   CpSpace where rawOffset = #offset struct cpSpace, idleSpeedThreshold
instance Offset "sleepTimeThreshold"   CpSpace where rawOffset = #offset struct cpSpace, sleepTimeThreshold
instance Offset "collisionSlop"        CpSpace where rawOffset = #offset struct cpSpace, collisionSlop
instance Offset "collisionBias"        CpSpace where rawOffset = #offset struct cpSpace, collisionBias
instance Offset "collisionPersistence" CpSpace where rawOffset = #offset struct cpSpace, collisionPersistence
instance Offset "userData"             CpSpace where rawOffset = #offset struct cpSpace, userData
instance Offset "stamp"                CpSpace where rawOffset = #offset struct cpSpace, stamp
instance Offset "curr_dt"              CpSpace where rawOffset = #offset struct cpSpace, curr_dt
instance Offset "dynamicBodies"        CpSpace where rawOffset = #offset struct cpSpace, dynamicBodies
instance Offset "staticBodies"         CpSpace where rawOffset = #offset struct cpSpace, staticBodies
instance Offset "rousedBodies"         CpSpace where rawOffset = #offset struct cpSpace, rousedBodies
instance Offset "sleepingComponents"   CpSpace where rawOffset = #offset struct cpSpace, sleepingComponents
instance Offset "shapeIDCounter"       CpSpace where rawOffset = #offset struct cpSpace, shapeIDCounter
instance Offset "staticShapes"         CpSpace where rawOffset = #offset struct cpSpace, staticShapes
instance Offset "dynamicShapes"        CpSpace where rawOffset = #offset struct cpSpace, dynamicShapes
instance Offset "constraints"          CpSpace where rawOffset = #offset struct cpSpace, constraints
instance Offset "arbiters"             CpSpace where rawOffset = #offset struct cpSpace, arbiters
instance Offset "contactBuffersHead"   CpSpace where rawOffset = #offset struct cpSpace, contactBuffersHead
instance Offset "cachedArbiters"       CpSpace where rawOffset = #offset struct cpSpace, cachedArbiters
instance Offset "pooledArbiters"       CpSpace where rawOffset = #offset struct cpSpace, pooledArbiters
instance Offset "allocatedBuffers"     CpSpace where rawOffset = #offset struct cpSpace, allocatedBuffers
instance Offset "locked"               CpSpace where rawOffset = #offset struct cpSpace, locked
instance Offset "usesWildcards"        CpSpace where rawOffset = #offset struct cpSpace, usesWildcards
instance Offset "collisionHandlers"    CpSpace where rawOffset = #offset struct cpSpace, collisionHandlers
instance Offset "defaultHandler"       CpSpace where rawOffset = #offset struct cpSpace, defaultHandler
instance Offset "skipPostStep"         CpSpace where rawOffset = #offset struct cpSpace, skipPostStep
instance Offset "postStepCallbacks"    CpSpace where rawOffset = #offset struct cpSpace, postStepCallbacks
instance Offset "staticBody"           CpSpace where rawOffset = #offset struct cpSpace, staticBody
instance Offset "_staticBody"          CpSpace where rawOffset = #offset struct cpSpace, _staticBody

instance Storable CpSpace where
  sizeOf _ = #size struct cpSpace
  alignment _ = #alignment struct cpSpace

  peek ptr =
    CpSpace
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

data {-# CTYPE "chipmunk/chipmunk_unsafe.h" "cpPostStepCallback" #-} CpPostStepCallback =
       CpPostStepCallback
         { func  :: FunPtr CpPostStepFunc
         , key   :: Ptr ()
         , data_ :: Ptr ()
         }

instance Offset "func"  CpPostStepCallback where rawOffset = #offset struct cpPostStepCallback, func
instance Offset "key"   CpPostStepCallback where rawOffset = #offset struct cpPostStepCallback, key
instance Offset "data_" CpPostStepCallback where rawOffset = #offset struct cpPostStepCallback, data

instance Offset "data" CpPostStepCallback where
  rawOffset = rawOffset @"data_" @CpPostStepCallback

instance HasField "data" CpPostStepCallback (Ptr ()) where
  getField = getField @"data_"

instance Storable CpPostStepCallback where
  sizeOf _ = #size struct cpPostStepCallback
  alignment _ = #alignment struct cpPostStepCallback

  peek ptr =
    CpPostStepCallback
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
  cpCircleShapeSetRadius
    :: Ptr CpShape -- ^ shape
    -> CpFloat     -- ^ radius
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpCircleShapeSetOffset"
  cpCircleShapeSetOffset
    :: Ptr CpShape -- ^ shape
    -> Ptr CpVect  -- ^ offset
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpSegmentShapeSetEndpoints"
  cpSegmentShapeSetEndpoints
    :: Ptr CpShape -- ^ shape
    -> Ptr CpVect  -- ^ a
    -> Ptr CpVect  -- ^ b
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk_unsafe.h cpSegmentShapeSetRadius"
  cpSegmentShapeSetRadius
    :: Ptr CpShape -- ^ shape
    -> CpFloat     -- ^ radius
    -> IO ()

foreign import CALLCV unsafe "wrapper.h w_cpPolyShapeSetVerts"
  cpPolyShapeSetVerts
    :: Ptr CpShape     -- ^ shape
    -> #{type int}     -- ^ count
    -> Ptr CpVect      -- ^ verts
    -> Ptr CpTransform -- ^ transform
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk_unsafe.h cpPolyShapeSetVertsRaw"
  cpPolyShapeSetVertsRaw
    :: Ptr CpShape -- ^ shape
    -> #{type int} -- ^ count
    -> Ptr CpVect  -- ^ verts
    -> IO ()

foreign import CALLCV unsafe "chipmunk/chipmunk_unsafe.h cpPolyShapeSetRadius"
  cpPolyShapeSetRadius
    :: Ptr CpShape -- ^ shape
    -> CpFloat     -- ^ radius
    -> IO ()

-- end of chipmunk_unsafe.h
