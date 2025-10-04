{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}

-- |
-- Module: Serializotron
-- Description: Content-based deduplicating serialization library for Haskell
--
-- Serializotron is a serialization library:
--
--  - Content-based deduplicating serialization library for Haskell.
--  - Serializes data while automatically reusing repeated content to shrink file size and preserve structure.
--  - Stores values as Protocol Buffer messages and indexes shared chunks via a cryptographic content hash.
--
-- = Quick Start
--
-- For most use cases, you only need to derive 'ToSZT' and 'FromSZT' instances:
--
-- @
-- {-# LANGUAGE DeriveGeneric #-}
-- {-# LANGUAGE DeriveAnyClass #-}
-- import GHC.Generics
-- import Serializotron
--
-- data Person = Person Text Int
--   deriving (Generic, Show, Eq, ToSZT, FromSZT)
--
-- -- Save to file
-- savePerson :: Person -> IO ()
-- savePerson person = saveSzt "person.szt" person
--
-- -- Load from file
-- loadPerson :: IO (Either SerializotronError Person)
-- loadPerson = loadSzt "person.szt"
-- @
--
-- = Deduplication Strategies
--
-- Serializotron offers three built-in strategies for handling duplicate data:
--
-- @
-- -- No deduplication (fastest)
-- saveSzt "fast.szt" myData
--
-- -- Balanced deduplication (recommended)
-- saveSztCompressed "balanced.szt" myData
--
-- -- Maximum deduplication (smallest files)
-- saveSztCompressedAggressive "small.szt" myData
--
-- -- Custom strategy
-- customStrategy = DeduplicationStrategy
--   { _minSizeThreshold    = 100 -- Only dedupe values >= 100 bytes
--   , _maxDepthScan        = 50  -- Scan up to 50 levels deep
--   , _enableDeduplication = True
--   }
-- saveSztWithStrategy customStrategy "custom.szt" myData
-- @
--
-- = Complex Data Structures
--
-- The library handles nested data structures, collections, and custom types:
--
-- @
-- data Company = Company
--   { companyName  :: Text
--   , employees    :: [Person]
--   , departments  :: Map Text [Person]
--   , founded      :: UTCTime
--   }
--   deriving (Generic, ToSZT, FromSZT)
--
-- data Result a = Success a | Error Text
--   deriving (Generic, ToSZT, FromSZT)
--
-- -- All of these work automatically:
-- saveSzt "company.szt" myCompany
-- saveSzt "results.szt" (map Success [1,2,3] :: [Result Int])
-- saveSzt "lookup.szt" (Map.fromList [("key", Just 42)] :: Map Text (Maybe Int))
-- @
--
-- = Error Handling
--
-- Loading can fail for various reasons, all captured in 'SerializotronError':
--
-- @
-- handleLoad :: IO ()
-- handleLoad = do
--   result <- loadSzt "data.szt" :: IO (Either SerializotronError Person)
--   case result of
--     Left err -> putStrLn $ "Failed to load: " <> formatError err
--     Right person -> putStrLn $ "Loaded: " <> show person
-- @
--
-- = File Integrity Checking
--
-- Verify the integrity of .szt files:
--
-- @
-- checkFile :: FilePath -> IO ()
-- checkFile path = do
--   result <- fsckSzt path
--   if fsckPassed result
--     then putStrLn "File is valid"
--     else do
--       putStrLn "File has errors:"
--       mapM_ (putStrLn . formatError . ValidationError) (fsckErrors result)
-- @
--
-- = Performance Tips
--
-- * Use 'saveSzt' for fastest serialization when file size isn't important
-- * Use 'saveSztCompressed' for balanced performance in most applications
-- * Use 'saveSztCompressedAggressive' for archival storage or network transfer
-- * Consider custom strategies for specific performance requirements
-- * The library is most effective with data containing repeated structures
module Serializotron where

import Control.Exception (IOException, try)
import Control.Lens
import Control.Applicative ((<|>))
import Control.Monad (zipWithM, unless)
import Control.Monad.State.Strict
import Crypto.Hash (Blake2b_256, Digest, hash)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LBS
import Data.Int (Int32)
import Data.List (elemIndex)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isNothing)
import Data.ProtoLens (decodeMessage, defMessage, encodeMessage)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Typeable (Proxy (..), TyCon, TypeRep, Typeable, tyConModule, tyConName, typeRep, typeRepArgs, typeRepTyCon)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Generics
import Lens.Family2 qualified as Lens
import Codec.Compression.GZip qualified as GZip

import Proto.Serializotron qualified as Proto
import Proto.Serializotron_Fields qualified as Proto

--------------------------------------------------------------------------------
-- File Format Header
--------------------------------------------------------------------------------

-- | Compression method used in SZT file
data CompressionMethod
  = NoCompression      -- ^ Raw protobuf, no compression
  | GZipCompression    -- ^ GZip compression (good balance, ubiquitous)
  deriving stock (Show, Eq, Enum, Bounded)

-- | Convert compression method to byte representation
compressionToByte :: CompressionMethod -> Word8
compressionToByte NoCompression = 0x00
compressionToByte GZipCompression = 0x01

-- | Parse compression method from byte
compressionFromByte :: Word8 -> Maybe CompressionMethod
compressionFromByte 0x00 = Just NoCompression
compressionFromByte 0x01 = Just GZipCompression
compressionFromByte _ = Nothing

-- | SZT file header structure (8 bytes total)
data SZTHeader = SZTHeader
  { _headerMagic :: ByteString       -- 4 bytes: "SZT\0"
  , _headerVersion :: Word8          -- 1 byte: format version
  , _headerCompression :: CompressionMethod  -- 1 byte: compression type
  , _headerReserved :: Word16        -- 2 bytes: reserved for future use
  }
  deriving stock (Show, Eq)

-- | Magic bytes for SZT format identification
sztMagicBytes :: ByteString
sztMagicBytes = ByteString.pack [0x53, 0x5A, 0x54, 0x00] -- "SZT\0"

-- | Current header format version
headerFormatVersion :: Word8
headerFormatVersion = 1

-- | Create a header with specified compression
mkHeader :: CompressionMethod -> SZTHeader
mkHeader compression = SZTHeader
  { _headerMagic = sztMagicBytes
  , _headerVersion = headerFormatVersion
  , _headerCompression = compression
  , _headerReserved = 0
  }

-- | Serialize header to bytes (8 bytes total)
encodeHeader :: SZTHeader -> ByteString
encodeHeader (SZTHeader magic version compression reserved) =
  ByteString.concat
    [ magic                                    -- 4 bytes
    , ByteString.singleton version            -- 1 byte
    , ByteString.singleton (compressionToByte compression)  -- 1 byte
    , ByteString.pack [fromIntegral (reserved `div` 256), fromIntegral (reserved `mod` 256)]  -- 2 bytes
    ]

-- | Parse header from bytes
decodeHeader :: ByteString -> Either Text SZTHeader
decodeHeader bs
  | ByteString.length bs < 8 = Left "Header too short, expected 8 bytes"
  | otherwise = do
      let magic = ByteString.take 4 bs
      unless (magic == sztMagicBytes) $
        Left $ "Invalid magic bytes: expected SZT\\0, got " <> Text.pack (show magic)

      let version = ByteString.index bs 4
      unless (version == headerFormatVersion) $
        Left $ "Unsupported format version: " <> Text.pack (show version)

      let compressionByte = ByteString.index bs 5
      compression <- case compressionFromByte compressionByte of
        Just c -> Right c
        Nothing -> Left $ "Unknown compression method: " <> Text.pack (show compressionByte)

      let reserved = fromIntegral (ByteString.index bs 6) * 256 + fromIntegral (ByteString.index bs 7)

      Right $ SZTHeader magic version compression reserved

-- | Schema version for backwards compatibility
type SchemaVersion = Word32

currentSchemaVersion :: SchemaVersion
currentSchemaVersion = 1

-- | Primitive type classification for structural type information.
--
-- This enumeration corresponds exactly to the constructors in 'PrimitiveValue',
-- providing type-level information about primitive values.
--
-- Each variant maps to a specific Haskell type:
-- @
-- PTInt     -> Int
-- PTDouble  -> Double
-- PTText    -> Text
-- PTBool    -> Bool
-- PTWord64  -> Word64
-- PTInt32   -> Int32
-- PTWord32  -> Word32
-- PTInteger -> Integer
-- PBytes    -> ByteString
-- @
data PrimitiveType
  = PTInt
  | PTDouble
  | PTText
  | PTBool
  | PTWord64
  | PTInt32
  | PTWord32
  | PTInteger
  | PTBytes
  deriving stock (Generic, Show, Eq, Ord, Enum, Bounded)

-- | Structural type information describing the shape of types.
--
-- This provides detailed information about how types are constructed,
-- enabling better error messages and compatibility checking. It mirrors
-- the structure of 'DynamicCore' but at the type level.
--
-- Examples:
-- @
-- Int           -> TSPrimitive PTInt
-- (Text, Int)   -> TSProduct [textTypeInfo, intTypeInfo]
-- Maybe Int     -> TSSum [nothingTypeInfo, justTypeInfo]
-- [Int]         -> TSList intTypeInfo
-- ()            -> TSUnit
-- @
data TypeStructure
  = -- | Basic types: Int, Text, Bool, etc.
    TSPrimitive PrimitiveType
  | -- | Record fields and tuple components with names
    TSProduct [FieldInfo]
  | -- | Constructor alternatives in sum types
    TSSum [TypeInfo]
  | -- | List/array element type
    TSList TypeInfo
  | -- | Unit type or empty constructors
    TSUnit
  deriving stock (Generic, Show, Eq, Ord)

-- | Named field metadata for product types
data FieldInfo = FieldInfo
  { _fiFieldName :: Maybe Text
  , _fiFieldType :: TypeInfo
  }
  deriving stock (Generic, Show, Eq, Ord)

-- | Type metadata for compatibility checking and debugging.
--
-- This optional metadata helps detect when serialized data might be incompatible
-- with the current program's types. It's particularly valuable for:
--
-- * Schema evolution: detecting when field types have changed
-- * Debugging: providing meaningful error messages with type names
-- * Documentation: understanding what types were originally serialized
--
-- The information is extracted automatically using GHC Generics.
--
-- Example for @data Person = Person Text Int@:
-- @
-- TypeInfo
--   { _tiTypeName = Just "Person"
--   , _tiModule = Just "MyModule"
--   , _tiConstructors = ["Person"]
--   , _tiStructure = Just (TSProduct [textTypeInfo, intTypeInfo])
--   }
-- @
data TypeInfo = TypeInfo
  { -- | Type name: "Person", "Maybe", "[]"
    _tiTypeName :: Maybe Text

    -- | Module where type is defined: "MyModule", "GHC.Types"
  , _tiModule :: Maybe Text

    -- | Constructor names: ["Person"], ["Nothing", "Just"]
  , _tiConstructors :: [Text]

    -- | Record field labels in declaration order
  , _tiFieldLabels :: [Text]

    -- | Detailed structural information
  , _tiStructure :: Maybe TypeStructure
  }
  deriving stock (Generic, Show, Eq, Ord)

makeLenses ''TypeInfo
makeLenses ''FieldInfo

emptyTypeInfo :: TypeInfo
emptyTypeInfo = TypeInfo Nothing Nothing [] [] Nothing

assignFieldLabels :: [FieldInfo] -> [FieldInfo]
assignFieldLabels fields = zipWith assign [1 :: Int ..] fields
  where
    assign idx fi =
      let finalName =
            case fi ^. fiFieldName of
              Just n | not (Text.null n) -> Just n
              _ -> Just (Text.pack ("_" <> show idx))
       in fi & fiFieldName .~ finalName

-- | TyCon for the list type constructor ([])
listTyCon :: TyCon
listTyCon = typeRepTyCon (typeRep (Proxy @[()]))

maybeTyCon :: TyCon
maybeTyCon = typeRepTyCon (typeRep (Proxy @(Maybe Int)))

eitherTyCon :: TyCon
eitherTyCon = typeRepTyCon (typeRep (Proxy @(Either Int Int)))

listElementType :: TypeRep -> Maybe TypeRep
listElementType rep
  | typeRepTyCon rep == listTyCon
  , [elemRep] <- typeRepArgs rep
  = Just elemRep
  | otherwise
  = Nothing

maybeElementType :: TypeRep -> Maybe TypeRep
maybeElementType rep
  | typeRepTyCon rep == maybeTyCon
  , [elemRep] <- typeRepArgs rep
  = Just elemRep
  | otherwise = Nothing

eitherElementTypes :: TypeRep -> Maybe (TypeRep, TypeRep)
eitherElementTypes rep
  | typeRepTyCon rep == eitherTyCon
  , [leftRep, rightRep] <- typeRepArgs rep
  = Just (leftRep, rightRep)
  | otherwise = Nothing

typeInfoForRep :: TypeRep -> TypeInfo
typeInfoForRep rep =
  let tyCon = typeRepTyCon rep
      nameText = Text.pack (tyConName tyCon)
      moduleText = Text.pack (tyConModule tyCon)
   in emptyTypeInfo
        { _tiTypeName = Just nameText
        , _tiModule = Just moduleText
        , _tiStructure = structureForTypeRep rep
        }

structureForTypeRep :: TypeRep -> Maybe TypeStructure
structureForTypeRep rep
  | rep == typeRep (Proxy @Int) = Just (TSPrimitive PTInt)
  | rep == typeRep (Proxy @Double) = Just (TSPrimitive PTDouble)
  | rep == typeRep (Proxy @Text) = Just (TSPrimitive PTText)
  | rep == typeRep (Proxy @Bool) = Just (TSPrimitive PTBool)
  | rep == typeRep (Proxy @Word64) = Just (TSPrimitive PTWord64)
  | rep == typeRep (Proxy @Int32) = Just (TSPrimitive PTInt32)
  | rep == typeRep (Proxy @Word32) = Just (TSPrimitive PTWord32)
  | rep == typeRep (Proxy @Integer) = Just (TSPrimitive PTInteger)
  | rep == typeRep (Proxy @ByteString) = Just (TSPrimitive PTBytes)
  | Just elemRep <- listElementType rep = Just (TSList (typeInfoForRep elemRep))
  | Just elemRep <- maybeElementType rep =
      let elemInfo = typeInfoForRep elemRep
          nothingInfo = emptyTypeInfo & tiStructure ?~ TSUnit
       in Just (TSSum [nothingInfo, elemInfo])
  | Just (leftRep, rightRep) <- eitherElementTypes rep =
      let leftInfo = typeInfoForRep leftRep
          rightInfo = typeInfoForRep rightRep
       in Just (TSSum [leftInfo, rightInfo])
  | otherwise = Nothing


-- | Primitive value types
data PrimitiveValue
  = PInt      Int
  | PDouble   Double
  | PText     Text
  | PBool     Bool
  | PWord64   Word64
  | PInt32    Int32
  | PWord32   Word32
  | PInteger  Text -- Arbitrary precision stored as text
  | PBytes    ByteString
  deriving stock (Generic, Show, Eq)

-- | Core value representation for all Haskell data structures.
--
-- This algebraic data type captures the essential structure of any serializable
-- Haskell value. It follows the structure of Haskell's type system:
--
-- * Primitives: Basic types like Int, Text, Bool
-- * Products: Records, tuples (data Foo = Foo A B)
-- * Sums: Enums, tagged unions (data Foo = A Int | B Text)
-- * Lists: Homogeneous collections [a]
-- * Unit: Empty constructors (data Foo = EmptyConstructor)
-- * References: internal use for pointers to deduplicated shared values
--
-- Examples:
-- @
-- 42          -> DPrimitive (PInt 42)
-- ("Hi", 3)   -> DProduct [DPrimitive (PText "Hi"), DPrimitive (PInt 3)]
-- Just 5      -> DSum 1 (DPrimitive (PInt 5))  -- Nothing=0, Just=1
-- [1,2,3]     -> DList [DPrimitive (PInt 1), DPrimitive (PInt 2), DPrimitive (PInt 3)]
-- ()          -> DUnit
-- @
data DynamicCore
  = -- | Basic values: Int, Text, Bool, etc.
    DPrimitive PrimitiveValue

  | -- | Records and tuples: data Foo = Foo A B
    DProduct [DynamicValue]

  | -- | Tagged unions: data Foo = A | B Int (constructor index + value)
    DSum Word32 DynamicValue

  | -- | Homogeneous lists: [a]
    DList [DynamicValue]

  | -- | Unit type and empty constructors: () or EmptyConstructor
    DUnit

  | -- | Reference to shared value for deduplication
    DReference Word32

  deriving stock (Generic, Show, Eq)

-- | Core dynamic value with optional type metadata
data DynamicValue = DynamicValue
  { _dvCore          :: DynamicCore
  , _dvTypeInfo      :: Maybe TypeInfo
  , _dvSchemaVersion :: SchemaVersion
  }
  deriving stock (Generic, Show, Eq)

makeLenses ''DynamicValue

-- | Top-level SZT file structure
data SZTFile = SZTFile
  { _sztSchemaVersion :: SchemaVersion
  , _sztValue         :: DynamicValue
  , _sztSharedValues  :: Map.Map Word32 DynamicValue -- Deduplication table
  }
  deriving stock (Generic, Show, Eq)

makeLenses ''SZTFile

--------------------------------------------------------------------------------
-- Deduplication System
--------------------------------------------------------------------------------

-- | Configuration for deduplication behavior.
--
-- Deduplication reduces file size by identifying repeated values and storing
-- them only once, replacing duplicates with references. This strategy controls
-- the trade-offs between file size, serialization time, and memory usage.
--
-- Key parameters:
-- * Size threshold: Skip deduplicating small values (overhead isn't worth it)
-- * Depth limit: Prevent excessive recursion in deeply nested structures
-- * Enable flag: Master switch to disable deduplication entirely
--
-- Choosing the right strategy:
-- * 'noDeduplicationStrategy': Fastest serialization, larger files
-- * 'defaultDeduplicationStrategy': Good balance for most use cases
-- * 'aggressiveDeduplicationStrategy': Smallest files, slower serialization
--
-- Example:
-- @
-- customStrategy = DeduplicationStrategy
--   { _minSizeThreshold    = 50 -- Only dedupe values >= 50 bytes
--   , _maxDepthScan        = 20 -- Scan up to 20 levels deep
--   , _enableDeduplication = True
--   }
-- @
data DeduplicationStrategy = DeduplicationStrategy
  { -- | Only dedupe values with estimated size >= this (bytes)
    _minSizeThreshold :: Int

    -- | Maximum recursion depth for deduplication scanning
  , _maxDepthScan :: Int

    -- | Master switch for deduplication (disable for fastest serialization)
  , _enableDeduplication :: Bool
  }
  deriving stock (Show, Eq)

makeLenses ''DeduplicationStrategy

-- | Default deduplication strategy - conservative settings.
--
-- This provides a good balance between file size reduction and performance.
-- Suitable for most applications where you want some space savings without
-- significant serialization overhead.
--
-- Settings:
-- * Skips values smaller than 20 bytes (overhead not worth it)
-- * Limits recursion to 10 levels (prevents excessive scanning)
-- * Enabled by default
--
-- Use this when:
-- * You want smaller files but care about serialization speed
-- * Your data has moderate repetition
-- * You're unsure which strategy to choose
defaultDeduplicationStrategy :: DeduplicationStrategy
defaultDeduplicationStrategy =
  DeduplicationStrategy
    { _minSizeThreshold    = 20 -- Skip small values (< 20 bytes estimated)
    , _maxDepthScan        = 10 -- Don't recurse too deep
    , _enableDeduplication = True
    }

-- | Aggressive deduplication - deduplicate everything possible.
--
-- This strategy maximizes space savings by deduplicating even small values
-- and scanning deeply into nested structures. Best for archival storage
-- or when dealing with highly repetitive data.
--
-- Settings:
-- * Deduplicates values as small as 1 byte
-- * Scans up to 100 levels deep
-- * Maximum deduplication enabled
--
-- Use this when:
-- * File size is more important than serialization speed
-- * Your data contains lots of repetition
-- * You're archiving data for long-term storage
-- * Network bandwidth or storage costs are a concern
--
-- Warning: Can be significantly slower than other strategies.
aggressiveDeduplicationStrategy :: DeduplicationStrategy
aggressiveDeduplicationStrategy =
  DeduplicationStrategy
    { _minSizeThreshold    = 1   -- Deduplicate even tiny values
    , _maxDepthScan        = 100 -- Deep scanning
    , _enableDeduplication = True
    }

-- | Disable deduplication entirely.
--
-- This strategy turns off all deduplication, storing each value separately
-- even if they're identical. Results in the fastest serialization but
-- largest file sizes.
--
-- Settings:
-- * Size threshold set to maximum (never deduplicate)
-- * No depth scanning
-- * Deduplication disabled
--
-- Use this when:
-- * Serialization speed is critical
-- * Your data has little to no repetition
-- * Memory usage during serialization is a concern
-- * You're doing real-time serialization
-- * File size is not important
noDeduplicationStrategy :: DeduplicationStrategy
noDeduplicationStrategy =
  DeduplicationStrategy
    { _minSizeThreshold    = maxBound
    , _maxDepthScan        = 0
    , _enableDeduplication = False
    }

-- | Did our hash calculation use pure content values
-- or did we also hash a 'DReference'?
data HashScope = PureContent | WithReference
  deriving stock (Eq, Show)

-- | 'WithReference' taints any other value.
instance Semigroup HashScope where
  WithReference <> _             = WithReference
  _             <> WithReference = WithReference
  PureContent   <> PureContent   = PureContent

data Scoped a = Scoped HashScope a
  deriving stock (Eq)

instance Show a => Show (Scoped a) where
  show (Scoped s x) = show s <> ":" <> show x

instance Functor Scoped where
  fmap f (Scoped s x) = Scoped s (f x)

instance Applicative Scoped where
  pure = Scoped PureContent

  Scoped PureContent   f <*> Scoped PureContent   x = Scoped PureContent   (f x)
  Scoped PureContent   f <*> Scoped WithReference x = Scoped WithReference (f x)
  Scoped WithReference f <*> Scoped _             x = Scoped WithReference (f x)

instance Monad Scoped where
  Scoped PureContent x >>= f = f x
  Scoped WithReference x >>= f = case f x of
    Scoped _ x' -> Scoped WithReference x'

-- | Lens for accessing the HashScope component of Scoped
scopedScope :: Lens' (Scoped a) HashScope
scopedScope f (Scoped s a) = (`Scoped` a) <$> f s

-- | Lens for accessing the value component of Scoped
scopedValue :: Lens' (Scoped a) a
scopedValue f (Scoped s a) = Scoped s <$> f a

type ContentHash = Digest Blake2b_256

-- | State for deduplication process
data DeduplicationState = DeduplicationState
  { _nextReferenceId  :: Word32
  , _seenHashes       :: Map.Map ContentHash (Either DynamicValue Word32)  -- Left = first occurrence, Right = ref ID
  , _sharedTable      :: Map.Map Word32 DynamicValue
  , _currentDepth     :: Int
  , _strategy         :: DeduplicationStrategy
  }
  deriving stock (Show)

makeLenses ''DeduplicationState

-- | Monad for deduplication operations
type DeduplicationM = State DeduplicationState

-- | Initialize deduplication state
initDeduplicationState :: DeduplicationStrategy -> DeduplicationState
initDeduplicationState strat =
  DeduplicationState
    { _nextReferenceId  = 1 -- Start from 1, perhaps we will reserve 0 for something.
    , _seenHashes       = Map.empty
    , _sharedTable      = Map.empty
    , _currentDepth     = 0
    , _strategy         = strat
    }

-- | Estimate the serialized size of a DynamicValue (rough heuristic)
estimateSize :: DynamicValue -> Int
estimateSize (DynamicValue core _typeInfo _dvSchemaVersion) = coreSize + typeInfoSize
  where
    typeInfoSize = 50 -- Rough estimate for type info

    coreSize = case core of
      DPrimitive (PInt      _)  -> 8
      DPrimitive (PDouble   _)  -> 8
      DPrimitive (PBool     _)  -> 1
      DPrimitive (PText     t)  -> Text.length t * 2 -- Rough UTF-8 estimate
      DPrimitive (PWord64   _)  -> 8
      DPrimitive (PInt32    _)  -> 4
      DPrimitive (PWord32   _)  -> 4
      DPrimitive (PInteger  t)  -> Text.length t * 2 -- Text representation
      DPrimitive (PBytes    bs) -> ByteString.length bs -- Byte length
      DProduct    vals          -> 10 + sum (map estimateSize vals)
      DSum      _ val           -> 10 + estimateSize val
      DList       vals          -> 10 + sum (map estimateSize vals)
      DUnit                     -> 1
      DReference _              -> 4

-- | Compute content hash of a DynamicValue.
computeContentHash :: DynamicValue -> Scoped ContentHash
computeContentHash dynValue = hash . LBS.toStrict . Builder.toLazyByteString <$> contentHashBytes dynValue
  where
    contentHashBytes :: DynamicValue -> Scoped Builder.Builder
    contentHashBytes (DynamicValue core _ _) = coreHashBytes core

    coreHashBytes :: DynamicCore -> Scoped Builder.Builder
    coreHashBytes = \case
      DPrimitive pv -> Scoped PureContent $ Builder.word8 0x01 <> primitiveHashBytes pv
      DProduct vals -> do
        childHashes <- mapM contentHashBytes vals
        return $ Builder.word8 0x02 <> mconcat childHashes
      DSum index val -> do
        childHash <- contentHashBytes val
        return $ Builder.word8 0x03 <> Builder.word32BE index <> childHash
      DList vals -> do
        childHashes <- mapM contentHashBytes vals
        return $ Builder.word8 0x04 <> mconcat childHashes
      DUnit -> Scoped PureContent $ Builder.word8 0x05
      DReference refId -> Scoped WithReference $ Builder.word8 0x06 <> Builder.word32BE refId

    primitiveHashBytes :: PrimitiveValue -> Builder.Builder
    primitiveHashBytes = \case
      PInt i       -> Builder.word8 0x10 <> Builder.intDec i
      PDouble d    -> Builder.word8 0x11 <> Builder.doubleDec d
      PText t      -> Builder.word8 0x12 <> Builder.byteString (encodeUtf8 t)
      PBool b      -> Builder.word8 0x13 <> Builder.word8 (if b then 1 else 0)
      PWord64 w    -> Builder.word8 0x14 <> Builder.word64BE w
      PInt32 i     -> Builder.word8 0x15 <> Builder.int32BE i
      PWord32 w    -> Builder.word8 0x16 <> Builder.word32BE w
      PInteger t   -> Builder.word8 0x17 <> Builder.byteString (encodeUtf8 t)
      PBytes bs    -> Builder.word8 0x18 <> Builder.word32BE (fromIntegral $ ByteString.length bs) <> Builder.byteString bs

-- | Apply deduplication to a DynamicValue
deduplicateValue :: DeduplicationStrategy -> DynamicValue -> (DynamicValue, Map.Map Word32 DynamicValue)
deduplicateValue strat rootValue
  | _enableDeduplication strat
  = let (result, finalState) = runState (deduplicateValue' rootValue) (initDeduplicationState strat)
      in (result, _sharedTable finalState)

  | otherwise
  = (rootValue, Map.empty)

  where
    deduplicateValue' :: DynamicValue -> DeduplicationM DynamicValue
    deduplicateValue' dynVal = do
      depth <- use currentDepth
      strat <- use strategy

      if depth >= strat ^. maxDepthScan
        then return dynVal
        else do
          if estimateSize dynVal < strat ^. minSizeThreshold
            then return dynVal
            else do
              currentDepth += 1

              -- TOP-DOWN: Hash the value FIRST, before processing children
              let contentHash = computeContentHash dynVal ^. scopedValue
              seen <- use $ seenHashes . at contentHash

              result <- case seen of
                Just (Right refId) -> do
                  -- We've seen this content at least twice before - return existing reference
                  -- TOP-DOWN: Skip all child processing since we found a duplicate
                  return $ DynamicValue (DReference refId) Nothing currentSchemaVersion
                Just (Left firstOccurrence) -> do
                  -- This is the SECOND time we see this content
                  -- Now we need to create a shared entry for it
                  newId <- use nextReferenceId
                  nextReferenceId .= (newId + 1)

                  -- Move first occurrence to shared table
                  sharedTable . at newId ?= firstOccurrence

                  -- Update seen map to point to the reference
                  seenHashes . at contentHash ?= Right newId

                  -- Return reference (skip child processing for duplicate)
                  return $ DynamicValue (DReference newId) Nothing currentSchemaVersion
                Nothing -> do
                  -- First time seeing this content - record it but don't create reference yet
                  seenHashes . at contentHash ?= Left dynVal

                  -- Now recursively deduplicate children
                  dedupedVal <- deduplicateChildren dynVal

                  -- Return the deduplicated value (not a reference)
                  return dedupedVal

              -- Restore depth
              currentDepth .= depth
              return result

    deduplicateChildren :: DynamicValue -> DeduplicationM DynamicValue
    deduplicateChildren (DynamicValue core typeInfo version) = do
      dedupedCore <- deduplicateCore core
      return $ DynamicValue dedupedCore typeInfo version

    deduplicateCore :: DynamicCore -> DeduplicationM DynamicCore
    deduplicateCore = \case
      DPrimitive pv     -> return $ DPrimitive pv
      DProduct   vals   -> DProduct <$> mapM deduplicateValue' vals
      DSum     i val    -> DSum i   <$> deduplicateValue' val
      DList      vals   -> DList    <$> mapM deduplicateValue' vals
      DUnit             -> return DUnit
      DReference  refId -> return $ DReference refId

-- | Serialization type class for converting Haskell values to Serializotron format.
--
-- This is the main interface for serialization. Any type that can be serialized
-- must implement this class. For most types, you can derive it automatically:
--
-- @
-- data Person = Person Text Int deriving (Generic, ToSZT)
-- @
--
-- The class provides:
-- * Automatic derivation via GHC Generics for most types
-- * Manual instances for primitive types and special cases
-- * Type information extraction for compatibility checking
--
-- Custom instances are typically only needed for:
-- * Types with special serialization requirements
-- * Primitive types not covered by the standard instances
-- * Types requiring custom compatibility logic
--
-- Example of manual instance:
-- @
-- instance ToSZT MySpecialType where
--   toSzt (MySpecialType x y) = DynamicValue
--     { _dvCore = DProduct [toSzt x, toSzt y]
--     , _dvTypeInfo = Just $ TypeInfo ...
--     , _dvSchemaVersion = currentSchemaVersion
--     }
-- @
class ToSZT a where
  toSzt :: a -> DynamicValue
  default toSzt :: (Generic a, GToSZT (Rep a), GGetTypeInfo (Rep a), Typeable a) => a -> DynamicValue
  toSzt x =
    DynamicValue
      { _dvCore          = gToSZT (GHC.Generics.from x)
      , _dvTypeInfo      = Just $ gGetTypeInfo (GHC.Generics.from x) (typeRep (Proxy @a))
      , _dvSchemaVersion = currentSchemaVersion
      }

-- | Generic serialization type class
class GToSZT f where
  gToSZT :: f p -> DynamicCore

class GCollectProductValues f where
  gCollectProductValues :: f p -> [DynamicValue]

instance GCollectProductValues U1 where
  gCollectProductValues _ = []

instance (GCollectProductValues f, GCollectProductValues g) => GCollectProductValues (f :*: g) where
  gCollectProductValues (f :*: g) = gCollectProductValues f ++ gCollectProductValues g

instance (GCollectProductValues f) => GCollectProductValues (M1 i c f) where
  gCollectProductValues (M1 x) = gCollectProductValues x

instance (ToSZT a) => GCollectProductValues (K1 i a) where
  gCollectProductValues (K1 x) = [toSzt x]

-- | Generic type info extraction
class GGetTypeInfo f where
  gGetTypeInfo :: f p -> TypeRep -> TypeInfo
  -- | Get complete type info including all constructors for sum types
  gGetCompleteTypeInfo :: f p -> TypeRep -> TypeInfo
  gGetCompleteTypeInfo = gGetTypeInfo -- Default implementation

-- | Generic constructor names extraction
class GGetFieldInfos f where
  gGetFieldInfos :: f p -> [FieldInfo]

class GGetConstructorNames f where
  gGetConstructorNames :: f p -> [Text]

-- | Get ALL constructor names for a sum type, regardless of active constructor  
class GGetAllConstructorNames f where
  gGetAllConstructorNames :: Proxy (f p) -> [Text]

-- | Generic structure extraction
class GGetStructure f where
  gGetStructure :: f p -> TypeStructure

-- | Get complete structure for sum types, regardless of active constructor  
class GGetCompleteStructure f where
  gGetCompleteStructure :: Proxy (f p) -> TypeStructure

-- Generic instances for GToSZT
instance GToSZT U1 where
  gToSZT U1 = DUnit

instance (ToSZT a) => GToSZT (K1 i a) where
  gToSZT (K1 x) = case toSzt x of
    DynamicValue core _ _ -> core

instance (GCollectProductValues f, GCollectProductValues g) => GToSZT (f :*: g) where
  gToSZT prod = DProduct (gCollectProductValues prod)

instance (GToSZT f, GToSZT g) => GToSZT (f :+: g) where
  gToSZT (L1 x) = DSum 0 (DynamicValue (gToSZT x) Nothing currentSchemaVersion)
  gToSZT (R1 y) = DSum 1 (DynamicValue (gToSZT y) Nothing currentSchemaVersion)

instance (GToSZT f) => GToSZT (M1 i c f) where
  gToSZT (M1 x) = gToSZT x

-- Primitive instances
instance ToSZT Int where
  toSzt x =
    DynamicValue
      { _dvCore = DPrimitive (PInt x)
      , _dvTypeInfo =
          Just $
            TypeInfo
              { _tiTypeName = Just "Int"
              , _tiModule = Just "GHC.Types"
              , _tiConstructors = []
              , _tiFieldLabels = []
              , _tiStructure = Just (TSPrimitive PTInt)
              }
      , _dvSchemaVersion = currentSchemaVersion
      }

instance ToSZT Double where
  toSzt x =
    DynamicValue
      { _dvCore = DPrimitive (PDouble x)
      , _dvTypeInfo =
          Just $
            TypeInfo
              { _tiTypeName = Just "Double"
              , _tiModule = Just "GHC.Types"
              , _tiConstructors = []
              , _tiFieldLabels = []
              , _tiStructure = Just (TSPrimitive PTDouble)
              }
      , _dvSchemaVersion = currentSchemaVersion
      }

instance ToSZT Text where
  toSzt x =
    DynamicValue
      { _dvCore = DPrimitive (PText x)
      , _dvTypeInfo =
          Just $
            TypeInfo
              { _tiTypeName = Just "Text"
              , _tiModule = Just "Data.Text"
              , _tiConstructors = []
              , _tiFieldLabels = []
              , _tiStructure = Just (TSPrimitive PTText)
              }
      , _dvSchemaVersion = currentSchemaVersion
      }

instance ToSZT Bool where
  toSzt x =
    DynamicValue
      { _dvCore = DPrimitive (PBool x)
      , _dvTypeInfo =
          Just $
            TypeInfo
              { _tiTypeName = Just "Bool"
              , _tiModule = Just "GHC.Types"
              , _tiConstructors = ["False", "True"]
              , _tiFieldLabels = []
              , _tiStructure = Just (TSPrimitive PTBool)
              }
      , _dvSchemaVersion = currentSchemaVersion
      }

instance ToSZT ByteString where
  toSzt x =
    DynamicValue
      { _dvCore = DPrimitive (PBytes x)
      , _dvTypeInfo =
          Just $
            TypeInfo
              { _tiTypeName = Just "ByteString"
              , _tiModule = Just "Data.ByteString"
              , _tiConstructors = []
              , _tiFieldLabels = []
              , _tiStructure = Just (TSPrimitive PTBytes)
              }
      , _dvSchemaVersion = currentSchemaVersion
      }

instance (ToSZT a) => ToSZT [a] where
  toSzt xs =
    DynamicValue
      { _dvCore = DList (map toSzt xs)
      , _dvTypeInfo =
          Just $
            TypeInfo
              { _tiTypeName = Just "[]"
              , _tiModule = Just "GHC.Types"
              , _tiConstructors = ["[]", ":"]
              , _tiFieldLabels = []
              , _tiStructure = Just (TSList elementTypeInfo)
              }
      , _dvSchemaVersion = currentSchemaVersion
      }
    where
      elementTypeInfo = case xs of
        [] -> TypeInfo Nothing Nothing [] [] Nothing -- Unknown element type for empty list
        (x : _) -> case toSzt x of
          DynamicValue _ (Just ti) _ -> ti
          _ -> TypeInfo Nothing Nothing [] [] Nothing

-- Generic type info instances
instance (Datatype d, GGetConstructorNames f, GGetStructure f, GGetAllConstructorNames f, GGetCompleteStructure f, GGetFieldInfos f, GGetTypeInfo f) => GGetTypeInfo (M1 D d f) where
  gGetTypeInfo (M1 x) tr =
    let inner = gGetTypeInfo x tr
        constructors = gGetAllConstructorNames (Proxy :: Proxy (f p))
        isSumType = length constructors > 1
        fullStructure =
          if isSumType
            then Just (gGetCompleteStructure (Proxy :: Proxy (f p)))
            else inner ^. tiStructure
        fieldLabels = if isSumType then [] else inner ^. tiFieldLabels
     in inner
          { _tiTypeName     = Just $ Text.pack (datatypeName (undefined :: M1 D d f p))
          , _tiModule       = Just $ Text.pack (moduleName   (undefined :: M1 D d f p))
          , _tiConstructors = constructors
          , _tiFieldLabels  = fieldLabels
          , _tiStructure    = fullStructure
          }
instance (GGetTypeInfo f) => GGetTypeInfo (M1 C c f) where
  gGetTypeInfo (M1 x) = gGetTypeInfo x

instance (GGetTypeInfo f) => GGetTypeInfo (M1 S s f) where
  gGetTypeInfo (M1 x) = gGetTypeInfo x

instance GGetTypeInfo U1 where
  gGetTypeInfo U1 _tr = TypeInfo Nothing Nothing [] [] Nothing

instance GGetTypeInfo (K1 i a) where
  gGetTypeInfo (K1 _) _tr = TypeInfo Nothing Nothing [] [] Nothing

instance (GGetTypeInfo f, GGetTypeInfo g, GGetFieldInfos f, GGetFieldInfos g) => GGetTypeInfo (f :*: g) where
  gGetTypeInfo prod _ =
    let rawFields = gGetFieldInfos prod
        labeledFields = assignFieldLabels rawFields
        labels = map (fromMaybe Text.empty . (^. fiFieldName)) labeledFields
     in TypeInfo
          { _tiTypeName = Nothing      -- No specific type name for product
          , _tiModule = Nothing        -- No specific module for product
          , _tiConstructors = []       -- Product combines fields, not constructors
          , _tiFieldLabels = labels
          , _tiStructure = Just $ TSProduct labeledFields
          }
instance (GGetTypeInfo f, GGetTypeInfo g) => GGetTypeInfo (f :+: g) where
  gGetTypeInfo (L1 x) tr = gGetTypeInfo x tr
  gGetTypeInfo (R1 y) tr = gGetTypeInfo y tr

-- Constructor name extraction instances
instance (Constructor c) => GGetConstructorNames (M1 C c f) where
  gGetConstructorNames (M1 _) = [Text.pack (conName (undefined :: M1 C c f p))]

instance (GGetConstructorNames f, GGetConstructorNames g) => GGetConstructorNames (f :+: g) where
  gGetConstructorNames (L1 x) = gGetConstructorNames x
  gGetConstructorNames (R1 y) = gGetConstructorNames y

instance (GGetConstructorNames f) => GGetConstructorNames (M1 D d f) where
  gGetConstructorNames (M1 x) = gGetConstructorNames x

instance (GGetConstructorNames f) => GGetConstructorNames (M1 S s f) where
  gGetConstructorNames (M1 x) = gGetConstructorNames x

instance GGetConstructorNames (K1 i a) where
  gGetConstructorNames _ = []

instance GGetFieldInfos U1 where
  gGetFieldInfos _ = []

instance (ToSZT a) => GGetFieldInfos (K1 i a) where
  gGetFieldInfos (K1 x) =
    let fieldType = case toSzt x of
          DynamicValue _ (Just ti) _ -> ti
          _ -> emptyTypeInfo
     in [FieldInfo Nothing fieldType]

instance (GGetFieldInfos f, GGetFieldInfos g) => GGetFieldInfos (f :*: g) where
  gGetFieldInfos (f :*: g) = gGetFieldInfos f ++ gGetFieldInfos g

instance (GGetFieldInfos f) => GGetFieldInfos (M1 C c f) where
  gGetFieldInfos (M1 x) = gGetFieldInfos x

instance (GGetFieldInfos f) => GGetFieldInfos (M1 D d f) where
  gGetFieldInfos (M1 x) = gGetFieldInfos x

instance (Selector s, GGetFieldInfos f) => GGetFieldInfos (M1 S s f) where
  gGetFieldInfos m@(M1 x) =
    let nameStr = selName m
        name = if null nameStr then Nothing else Just (Text.pack nameStr)
        fields = gGetFieldInfos x
     in case fields of
          [] -> [FieldInfo name emptyTypeInfo]
          _ -> case name of
                 Nothing -> fields
                 Just n -> [ if isNothing (fi ^. fiFieldName) then fi & fiFieldName ?~ n else fi | fi <- fields ]

instance (GGetFieldInfos f, GGetFieldInfos g) => GGetFieldInfos (f :+: g) where
  gGetFieldInfos (L1 x) = gGetFieldInfos x
  gGetFieldInfos (R1 y) = gGetFieldInfos y

instance GGetConstructorNames U1 where
  gGetConstructorNames _ = []

-- All constructor names instances using Proxy
instance (GGetAllConstructorNames f, GGetAllConstructorNames g) => GGetAllConstructorNames (f :+: g) where
  gGetAllConstructorNames _ = gGetAllConstructorNames (Proxy :: Proxy (f p)) ++ gGetAllConstructorNames (Proxy :: Proxy (g p))

instance (Constructor c) => GGetAllConstructorNames (M1 C c f) where
  gGetAllConstructorNames _ = [Text.pack (conName (undefined :: M1 C c f p))]

instance (GGetAllConstructorNames f) => GGetAllConstructorNames (M1 D d f) where
  gGetAllConstructorNames _ = gGetAllConstructorNames (Proxy :: Proxy (f p))

instance (GGetAllConstructorNames f) => GGetAllConstructorNames (M1 S s f) where
  gGetAllConstructorNames _ = gGetAllConstructorNames (Proxy :: Proxy (f p))

instance GGetAllConstructorNames (K1 i a) where
  gGetAllConstructorNames _ = []

instance GGetAllConstructorNames U1 where
  gGetAllConstructorNames _ = []

-- Structure extraction instances
instance (GGetStructure f) => GGetStructure (M1 D d f) where
  gGetStructure (M1 x) = gGetStructure x

instance (GGetStructure f) => GGetStructure (M1 C c f) where
  gGetStructure (M1 x) = gGetStructure x

instance (GGetStructure f) => GGetStructure (M1 S s f) where
  gGetStructure (M1 x) = gGetStructure x

instance (GGetFieldInfos f, GGetFieldInfos g) => GGetStructure (f :*: g) where
  gGetStructure prod = TSProduct (assignFieldLabels (gGetFieldInfos prod))
instance (GGetStructure f, GGetStructure g) => GGetStructure (f :+: g) where
  gGetStructure (L1 x) = gGetStructure x
  gGetStructure (R1 y) = gGetStructure y

instance GGetStructure U1 where
  gGetStructure _ = TSUnit

-- Complete structure instances
instance (GGetCompleteStructure f, GGetCompleteStructure g) => GGetCompleteStructure (f :+: g) where
  gGetCompleteStructure _ = TSSum
    [ TypeInfo Nothing Nothing [] [] (Just (gGetCompleteStructure (Proxy :: Proxy (f p))))
    , TypeInfo Nothing Nothing [] [] (Just (gGetCompleteStructure (Proxy :: Proxy (g p))))
    ]

-- For constructor level, we need to create sample values to extract structure
-- This is complex because we can't create arbitrary values at the type level
-- Let's use a simplified approach that represents sum types properly

instance (GGetCompleteStructure f) => GGetCompleteStructure (M1 D d f) where
  gGetCompleteStructure _ = gGetCompleteStructure (Proxy :: Proxy (f p))

instance (GGetCompleteStructure f) => GGetCompleteStructure (M1 S s f) where
  gGetCompleteStructure _ = gGetCompleteStructure (Proxy :: Proxy (f p))

-- For constructor metadata, we just return TSUnit as a placeholder
-- The actual structure will be determined by the field types
instance (GGetCompleteStructure f) => GGetCompleteStructure (M1 C c f) where
  gGetCompleteStructure _ = gGetCompleteStructure (Proxy :: Proxy (f p))

instance Typeable a => GGetCompleteStructure (K1 i a) where
  gGetCompleteStructure _ =
    let rep = typeRep (Proxy @a)
     in fromMaybe TSUnit (structureForTypeRep rep)

instance GGetCompleteStructure U1 where
  gGetCompleteStructure _ = TSUnit

-- Product type instance
instance (GGetCompleteStructure f, GGetCompleteStructure g) => GGetCompleteStructure (f :*: g) where
  gGetCompleteStructure _ = TSProduct $ assignFieldLabels
    [ FieldInfo Nothing (TypeInfo Nothing Nothing [] [] (Just (gGetCompleteStructure (Proxy :: Proxy (f p)))))
    , FieldInfo Nothing (TypeInfo Nothing Nothing [] [] (Just (gGetCompleteStructure (Proxy :: Proxy (g p)))))
    ]
instance Typeable a => GGetStructure (K1 i a) where
  gGetStructure _ = case typeRep (Proxy @a) of
    tr | tr == typeRep (Proxy @Int)         -> TSPrimitive PTInt
       | tr == typeRep (Proxy @Double)      -> TSPrimitive PTDouble
       | tr == typeRep (Proxy @Text)        -> TSPrimitive PTText
       | tr == typeRep (Proxy @Bool)        -> TSPrimitive PTBool
       | tr == typeRep (Proxy @Word64)      -> TSPrimitive PTWord64
       | tr == typeRep (Proxy @Int32)       -> TSPrimitive PTInt32
       | tr == typeRep (Proxy @Word32)      -> TSPrimitive PTWord32
       | tr == typeRep (Proxy @Integer)     -> TSPrimitive PTInteger
       | tr == typeRep (Proxy @ByteString)  -> TSPrimitive PTBytes
       | otherwise -> error $ show tr

-- Helper function for type parameter extraction
extractTypeParams :: TypeRep -> [Text]
extractTypeParams tr = extractParams tr
  where
    -- Extract type parameters by looking at the type representation structure
    -- For example, Maybe Int -> ["Int"], Either String Bool -> ["String", "Bool"]
    extractParams :: TypeRep -> [Text]
    extractParams typ =
      case typeRepArgs typ of
        [] -> []  -- No type arguments
        args -> map showTypeParam args

    -- Convert a TypeRep to a readable parameter name
    showTypeParam :: TypeRep -> Text
    showTypeParam param = Text.pack (show param)

--------------------------------------------------------------------------------
-- Comprehensive Error Hierarchy
--------------------------------------------------------------------------------

-- | Legacy deserialization error type (now wrapped in SerializotronError).
--
-- This type represents the specific deserialization failures that can occur
-- when converting from 'DynamicValue' back to typed Haskell values. These
-- errors are automatically wrapped in 'DeserializationError' with context.
--
-- Error categories:
-- * 'StructuralMismatch': General structural problems (wrong data shape)
-- * 'PrimitiveMismatch': Type mismatch for basic values (Int vs Text)
-- * 'ConstructorMismatch': Wrong enum variant or constructor
-- * 'FieldCountMismatch': Record has wrong number of fields
-- * 'TypeIncompatible': Fundamental type incompatibility
-- * 'MetadataUnavailable': File missing type information
--
-- Example scenarios:
-- @
-- -- Trying to load Int when file contains Text
-- PrimitiveMismatch "Int" "Text"
--
-- -- Trying to load Maybe when file contains list
-- StructuralMismatch "Expected Maybe but found List"
--
-- -- Trying to load 3-tuple when file contains 2-tuple
-- FieldCountMismatch 3 2
--
-- -- Trying to load Nothing when file contains Just
-- ConstructorMismatch "Nothing" "Just"
-- @
data SZTError
  = -- | Generic structural mismatch (wrong data shape)
    StructuralMismatch Text
  | -- | Expected vs actual primitive type
    PrimitiveMismatch Text Text
  | -- | Expected vs actual constructor name
    ConstructorMismatch Text Text
  | -- | Expected vs actual field count
    FieldCountMismatch Int Int
  | -- | General type incompatibility
    TypeIncompatible Text
  | -- | File missing type metadata (old format)
    MetadataUnavailable SchemaVersion
  deriving stock (Show, Eq)

-- | Top-level error type for all Serializotron operations.
--
-- This is the main error type returned by all high-level Serializotron functions.
-- It provides a comprehensive classification of everything that can go wrong during
-- serialization, deserialization, and file operations.
--
-- The error hierarchy is designed to:
-- * Provide specific error types for different failure modes
-- * Include contextual information for debugging
-- * Offer helpful suggestions for common problems
-- * Support structured error handling in applications
--
-- Example usage:
-- @
-- handleError :: SerializotronError -> Text
-- handleError = \case
--   DeserializationError _ ctx -> "Data format issue at " <> formatPath ctx
--   FileSystemError (FileNotFound path) -> "Missing file: " <> Text.pack path
--   ProtocolBufferError _ -> "File corruption detected"
--   ValidationError (CyclicReferences refs) -> "Circular references: " <> Text.pack (show refs)
--   IOError _ -> "System I/O error"
-- @
data SerializotronError
  = -- | Type/structure mismatch during loading
    DeserializationError SZTError ErrorContext
  | -- | File I/O or format problems
    FileSystemError FileError
  | -- | Protocol buffer parsing errors
    ProtocolBufferError ProtocolError
  | -- | Data integrity violations
    ValidationError ValidationError
  | -- | Low-level I/O errors
    IOError IOException
  deriving stock (Show, Eq)

-- | File system related errors.
--
-- These errors occur when working with .szt files on disk. They cover
-- common file system issues as well as Serializotron-specific format problems.
--
-- Common scenarios:
-- * 'FileNotFound': The specified .szt file doesn't exist
-- * 'PermissionDenied': No read/write access to the file
-- * 'CorruptedFile': File exists but contains invalid data
-- * 'UnsupportedVersion': File was created with incompatible schema version
-- * 'InvalidFileFormat': File isn't a valid .szt file
--
-- Example handling:
-- @
-- handleFileError :: FileError -> IO ()
-- handleFileError = \case
--   FileNotFound path -> putStrLn $ "Please check that " <> path <> " exists"
--   PermissionDenied path -> putStrLn $ "Need read permission for " <> path
--   CorruptedFile path reason -> putStrLn $ "File " <> path <> " is damaged: " <> reason
--   UnsupportedVersion found expected -> putStrLn $ "File version " <> show found <> " not supported (need " <> show expected <> ")"
--   InvalidFileFormat path reason -> putStrLn $ "File " <> path <> " is not a valid .szt file: " <> reason
-- @
data FileError
  = -- | File doesn't exist at the specified path
    FileNotFound FilePath
  | -- | Insufficient permissions to read/write file
    PermissionDenied FilePath
  | -- | File exists but contains invalid/corrupted data
    CorruptedFile FilePath Text
  | -- | File created with incompatible schema version (found, expected)
    UnsupportedVersion SchemaVersion SchemaVersion
  | -- | File is not a valid .szt file format
    InvalidFileFormat FilePath Text
  deriving stock (Show, Eq)

-- | Protocol buffer related errors.
--
-- These errors occur when the Protocol Buffer layer fails to parse or validate
-- the binary data. They typically indicate file corruption or version incompatibility.
--
-- Common causes:
-- * File truncated or corrupted during write/transfer
-- * Attempting to read non-.szt files as .szt files
-- * Schema evolution issues between different Serializotron versions
-- * Network transfer corruption
--
-- Example handling:
-- @
-- handleProtocolError :: ProtocolError -> Text
-- handleProtocolError = \case
--   MissingField field -> "Required field missing: " <> field
--   InvalidEnumValue enum val -> "Invalid value " <> Text.pack (show val) <> " for enum " <> enum
--   UnexpectedMessageType expected -> "Wrong message type, expected: " <> expected
--   ProtocolDecodingError msg -> "Binary data corruption: " <> msg
-- @
data ProtocolError
  = -- | Required protocol buffer field is missing
    MissingField Text
  | -- | Enum field contains unrecognized value
    InvalidEnumValue Text Int
  | -- | Protocol buffer message has wrong type
    UnexpectedMessageType Text
  | -- | Binary data could not be parsed as protocol buffer
    ProtocolDecodingError Text
  deriving stock (Show, Eq)

-- | Data validation errors.
--
-- These errors occur when the data structure itself violates integrity constraints,
-- even if the file format is valid. They're detected during reference resolution
-- and integrity checking.
--
-- Common scenarios:
-- * 'CyclicReferences': Reference graph contains cycles (shouldn't happen in normal use)
-- * 'HashCollision': Cryptographic hash collision detected (extremely rare)
-- * 'DanglingReference': Reference points to non-existent shared value
-- * 'InvalidSchema': Schema information is malformed
-- * 'SchemaVersionMismatch': Version incompatibility detected
--
-- Most validation errors indicate either:
-- * File corruption
-- * Bugs in serialization code
-- * Attempting to load files with incompatible schema versions
--
-- Example handling:
-- @
-- handleValidationError :: ValidationError -> Text
-- handleValidationError = \case
--   CyclicReferences refs -> "Circular references detected in: " <> Text.pack (show refs)
--   HashCollision hash refs -> "Hash collision (very rare): " <> Text.pack (show hash)
--   DanglingReference ref -> "Missing shared value: " <> Text.pack (show ref)
--   InvalidSchema msg -> "Schema error: " <> msg
--   SchemaVersionMismatch found expected -> "Version mismatch: " <> Text.pack (show found) <> " vs " <> Text.pack (show expected)
-- @
data ValidationError
  = -- | Reference graph contains cycles
    CyclicReferences [Word32]
  | -- | Cryptographic hash collision detected
    HashCollision ContentHash [Word32]
  | -- | Reference to non-existent shared value
    DanglingReference Word32
  | -- | Schema information is malformed
    InvalidSchema Text
  | -- | Schema version incompatibility (found, expected)
    SchemaVersionMismatch SchemaVersion SchemaVersion
  deriving stock (Show, Eq)

-- | Context information for better error reporting.
--
-- This provides detailed context about where and why deserialization failed,
-- making it much easier to debug issues with complex data structures.
--
-- The context includes:
-- * Field path: Shows exactly where in the data structure the error occurred
-- * Expected type: What type the deserializer was looking for
-- * Actual value: What value was actually found in the data
-- * Suggestions: Helpful hints for fixing the problem
--
-- Example context for a nested error:
-- @
-- ErrorContext
--   { errorPath = ["company", "employees", "0", "age"]
--   , expectedType = Just "Int"
--   , actualValue = Just "\"twenty-five\""
--   , suggestions = ["Check that age field contains a number, not text"]
--   }
-- -- Results in error message:
-- -- "At field path 'company.employees.0.age': Expected Int but found \"twenty-five\" (actual value: \"twenty-five\")"
-- -- "Suggestions:"
-- -- "  - Check that age field contains a number, not text"
-- @
data ErrorContext = ErrorContext
  { -- | Field path like ["user", "address", "street"] (innermost first)
    _errorPath :: [Text]
    -- | What type was expected during deserialization
  , _expectedType :: Maybe Text
    -- | What value was actually found in the data
  , _actualValue :: Maybe Text
    -- | Helpful suggestions for fixing the problem
  , _suggestions :: [Text]
  }
  deriving stock (Show, Eq)

makeLenses ''ErrorContext

-- | Empty context for when no context is available
emptyContext :: ErrorContext
emptyContext = ErrorContext [] Nothing Nothing []

-- | Add a field to the error path
addPath :: Text -> ErrorContext -> ErrorContext
addPath field = errorPath %~ (field :)

-- | Set expected type in context
withExpected :: Text -> ErrorContext -> ErrorContext
withExpected expected = expectedType ?~ expected

-- | Set actual value in context
withActual :: Text -> ErrorContext -> ErrorContext
withActual actual = actualValue ?~ actual

-- | Add a suggestion to the context
withSuggestion :: Text -> ErrorContext -> ErrorContext
withSuggestion suggestion = suggestions %~ (suggestion :)

-- | Helper functions for creating contextual errors
mkDeserializationError :: SZTError -> ErrorContext -> SerializotronError
mkDeserializationError = DeserializationError

-- | Create a primitive mismatch error with context
mkPrimitiveMismatch :: Text -> Text -> ErrorContext -> SerializotronError
mkPrimitiveMismatch expected actual ctx =
  let ctxWithExpected = withExpected expected ctx
      ctxWithActual = withActual actual ctxWithExpected
   in DeserializationError (PrimitiveMismatch expected actual) ctxWithActual

-- | Create a structural mismatch error with context and suggestions
mkStructuralMismatch :: Text -> ErrorContext -> SerializotronError
mkStructuralMismatch msg ctx =
  DeserializationError (StructuralMismatch msg) ctx

-- | Pretty print an error with full context.
--
-- This function converts any 'SerializotronError' into a human-readable error
-- message with context information, suggestions, and helpful details.
--
-- The formatted output includes:
-- * Clear description of what went wrong
-- * Field path showing exactly where the error occurred
-- * Expected vs actual values when available
-- * Helpful suggestions for fixing the problem
--
-- Example outputs:
-- @
-- "At field path 'user.address.zipCode': Expected Int but found Text"
-- "File not found: data/myfile.szt"
-- "Protocol decoding error: Invalid message header"
-- "Cyclic references detected: [1, 2, 3]"
-- @
--
-- Use this function to display errors to users or write them to logs:
-- @
-- case loadSzt "data.szt" of
--   Left err -> putStrLn $ Text.unpack $ formatError err
--   Right value -> processValue value
-- @
formatError :: SerializotronError -> Text
formatError (DeserializationError err ctx) = formatDeserializationError err ctx
formatError (FileSystemError err) = formatFileError err
formatError (ProtocolBufferError err) = formatProtocolError err
formatError (ValidationError err) = formatValidationError err
formatError (IOError err) = "IO Error: " <> Text.pack (show err)

formatDeserializationError :: SZTError -> ErrorContext -> Text
formatDeserializationError err ctx =
  let pathStr =
        if null (ctx ^. errorPath)
          then ""
          else "At field path '" <> Text.intercalate "." (reverse (ctx ^. errorPath)) <> "': "
      baseMsg = case err of
        StructuralMismatch msg -> msg
        PrimitiveMismatch expected actual ->
          "Expected " <> expected <> " but found " <> actual
        ConstructorMismatch expected actual ->
          "Expected constructor " <> expected <> " but found " <> actual
        FieldCountMismatch expected actual ->
          "Expected " <> Text.pack (show expected) <> " fields but found " <> Text.pack (show actual)
        TypeIncompatible msg -> "Type incompatible: " <> msg
        MetadataUnavailable version -> "Metadata unavailable for schema version " <> Text.pack (show version)

      valueStr = case ctx ^. actualValue of
        Just val -> " (actual value: " <> val <> ")"
        Nothing -> ""

      suggestionStr = case ctx ^. suggestions of
        [] -> ""
        sgs -> "\nSuggestions:\n" <> Text.unlines (map ("  - " <>) sgs)
   in pathStr <> baseMsg <> valueStr <> suggestionStr

formatFileError :: FileError -> Text
formatFileError = \case
  FileNotFound path -> "File not found: " <> Text.pack path
  PermissionDenied path -> "Permission denied: " <> Text.pack path
  CorruptedFile path reason -> "Corrupted file " <> Text.pack path <> ": " <> reason
  UnsupportedVersion found expected ->
    "Unsupported version "
      <> Text.pack (show found)
      <> " (expected "
      <> Text.pack (show expected)
      <> ")"
  InvalidFileFormat path reason -> "Invalid file format " <> Text.pack path <> ": " <> reason

formatProtocolError :: ProtocolError -> Text
formatProtocolError = \case
  MissingField field -> "Missing required field: " <> field
  InvalidEnumValue enum val -> "Invalid enum value " <> Text.pack (show val) <> " for " <> enum
  UnexpectedMessageType expected -> "Unexpected message type, expected: " <> expected
  ProtocolDecodingError msg -> "Protocol decoding error: " <> msg

formatValidationError :: ValidationError -> Text
formatValidationError = \case
  CyclicReferences refs -> "Cyclic references detected: " <> Text.pack (show refs)
  HashCollision hash refs -> "Hash collision " <> Text.pack (show hash) <> " between: " <> Text.pack (show refs)
  DanglingReference ref -> "Dangling reference: " <> Text.pack (show ref)
  InvalidSchema msg -> "Invalid schema: " <> msg
  SchemaVersionMismatch found expected ->
    "Schema version mismatch: found "
      <> Text.pack (show found)
      <> " but expected "
      <> Text.pack (show expected)

-- | Deserialization type class for converting from Serializotron format to Haskell values.
--
-- This is the counterpart to 'ToSZT', handling the conversion from serialized
-- data back to typed Haskell values. Like 'ToSZT', it can be derived automatically:
--
-- @
-- data Person = Person Text Int deriving (Generic, FromSZT)
-- @
--
-- The class provides:
-- * Automatic derivation via GHC Generics for most types
-- * Detailed error reporting when deserialization fails
-- * Type safety through compile-time checking
--
-- Deserialization can fail for several reasons:
-- * Type mismatch: expecting Int but finding Text
-- * Structural mismatch: wrong number of fields
-- * Constructor mismatch: wrong enum variant
-- * Version incompatibility: schema evolution issues
--
-- Example usage:
-- @
-- loadPerson :: FilePath -> IO (Either SerializotronError Person)
-- loadPerson path = loadSzt path
-- @
--
-- Custom instances follow the same pattern as 'ToSZT':
-- @
-- instance FromSZT MySpecialType where
--   fromSzt (DynamicValue (DProduct [x, y]) _ _) = do
--     x' <- fromSzt x
--     y' <- fromSzt y
--     return $ MySpecialType x' y'
--   fromSzt _ = Left $ StructuralMismatch "Expected MySpecialType"
-- @
class FromSZT a where
  fromSzt :: DynamicValue -> Either SZTError a
  default fromSzt :: (Generic a, GFromSZT (Rep a)) => DynamicValue -> Either SZTError a
  fromSzt dynValue = GHC.Generics.to <$> gFromSZT (_dvCore dynValue)

-- | Generic deserialization type class
class GFromSZT f where
  gFromSZT :: DynamicCore -> Either SZTError (f p)

class GFromProductValues f where
  gFromProductValues :: [DynamicValue] -> Either SZTError (f p, [DynamicValue])

instance GFromProductValues U1 where
  gFromProductValues xs = Right (U1, xs)

instance (GFromProductValues f, GFromProductValues g) => GFromProductValues (f :*: g) where
  gFromProductValues xs = do
    (left, rest) <- gFromProductValues xs
    (right, rest') <- gFromProductValues rest
    pure (left :*: right, rest')

instance (GFromProductValues f) => GFromProductValues (M1 i c f) where
  gFromProductValues xs = do
    (inner, rest) <- gFromProductValues xs
    pure (M1 inner, rest)

instance (FromSZT a) => GFromProductValues (K1 i a) where
  gFromProductValues (value : rest) = do
    decoded <- fromSzt value
    pure (K1 decoded, rest)
  gFromProductValues [] = Left (FieldCountMismatch 1 0)

class GProductArity f where
  gProductArity :: Proxy f -> Int

instance GProductArity U1 where
  gProductArity _ = 0

instance GProductArity (K1 i a) where
  gProductArity _ = 1

instance (GProductArity f, GProductArity g) => GProductArity (f :*: g) where
  gProductArity _ = gProductArity (Proxy @f) + gProductArity (Proxy @g)

instance (GProductArity f) => GProductArity (M1 i c f) where
  gProductArity _ = gProductArity (Proxy @f)

-- Generic instances for deserialization
instance GFromSZT U1 where
  gFromSZT DUnit = Right U1
  gFromSZT _ = Left (StructuralMismatch "Expected unit type")

instance (FromSZT a) => GFromSZT (K1 i a) where
  gFromSZT core = case fromSZTCore core of
    Right value -> K1 <$> fromSzt value
    Left err -> Left err
    where
      fromSZTCore (DPrimitive pv) = Right (DynamicValue (DPrimitive pv) Nothing currentSchemaVersion)
      fromSZTCore c = Right (DynamicValue c Nothing currentSchemaVersion)

-- Note: :*: in GHC Generics always represents binary products (exactly 2 components).
-- Multi-field records use right-associative nesting: A :*: (B :*: C)
-- So each :*: node correctly handles exactly 2 fields.
-- Example: data Triple = Triple Int String Bool
-- Generic rep: Int :*: (String :*: Bool)
-- Serializes as: DProduct [Int_part, DProduct [String_part, Bool_part]]
instance (GFromProductValues f, GFromProductValues g, GProductArity f, GProductArity g) => GFromSZT (f :*: g) where
  gFromSZT (DProduct fields) = do
    (result, remaining) <- gFromProductValues fields
    if null remaining
      then pure result
      else Left (FieldCountMismatch expected actual)
    where
      expected = gProductArity (Proxy @(f :*: g))
      actual = length fields
  gFromSZT _ = Left (StructuralMismatch "Expected product type (:*:) ")

instance (GFromSZT f, GFromSZT g) => GFromSZT (f :+: g) where
  gFromSZT (DSum 0 dynValue) = L1 <$> gFromSZT (_dvCore dynValue)
  gFromSZT (DSum 1 dynValue) = R1 <$> gFromSZT (_dvCore dynValue)
  gFromSZT (DSum index _) = Left (ConstructorMismatch "valid index" (Text.pack (show index)))
  gFromSZT _ = Left (StructuralMismatch "Expected sum type")

instance (GFromSZT f) => GFromSZT (M1 i c f) where
  gFromSZT core = M1 <$> gFromSZT core

-- Primitive instances for deserialization
instance FromSZT Int where
  fromSzt (DynamicValue (DPrimitive (PInt x)) _ _) = Right x
  fromSzt _ = Left (PrimitiveMismatch "Int" "other")

instance FromSZT Double where
  fromSzt (DynamicValue (DPrimitive (PDouble x)) _ _) = Right x
  fromSzt _ = Left (PrimitiveMismatch "Double" "other")

instance FromSZT Text where
  fromSzt (DynamicValue (DPrimitive (PText x)) _ _) = Right x
  fromSzt _ = Left (PrimitiveMismatch "Text" "other")

instance FromSZT Bool where
  fromSzt (DynamicValue (DPrimitive (PBool x)) _ _) = Right x
  fromSzt _ = Left (PrimitiveMismatch "Bool" "other")

instance FromSZT ByteString where
  fromSzt (DynamicValue (DPrimitive (PBytes x)) _ _) = Right x
  fromSzt _ = Left (PrimitiveMismatch "ByteString" "other")

instance (FromSZT a) => FromSZT [a] where
  fromSzt (DynamicValue (DList elements) _ _) =
    traverse fromSzt elements
  fromSzt _ = Left (StructuralMismatch "Expected list")

-- Tuple instances
instance (ToSZT a, ToSZT b) => ToSZT (a, b) where
  toSzt (a, b) =
    DynamicValue
      { _dvCore = DProduct [toSzt a, toSzt b]
      , _dvTypeInfo =
          Just $
            TypeInfo
              { _tiTypeName = Just "(,)"
              , _tiModule = Just "GHC.Tuple"
              , _tiConstructors = ["(,)"]
              , _tiFieldLabels = []
              , _tiStructure = Nothing -- Simplified
              }
      , _dvSchemaVersion = currentSchemaVersion
      }

instance (FromSZT a, FromSZT b) => FromSZT (a, b) where
  fromSzt (DynamicValue (DProduct [dynA, dynB]) _ _) = do
    a <- fromSzt dynA
    b <- fromSzt dynB
    return (a, b)
  fromSzt (DynamicValue (DProduct fields) _ _) =
    Left (FieldCountMismatch 2 (length fields))
  fromSzt _ = Left (StructuralMismatch "Expected tuple")

instance (ToSZT a, ToSZT b, ToSZT c) => ToSZT (a, b, c) where
  toSzt (a, b, c) =
    DynamicValue
      { _dvCore = DProduct [toSzt a, toSzt b, toSzt c]
      , _dvTypeInfo =
          Just $
            TypeInfo
              { _tiTypeName = Just "(,,)"
              , _tiModule = Just "GHC.Tuple"
              , _tiConstructors = ["(,,)"]
              , _tiFieldLabels = []
              , _tiStructure = Nothing
              }
      , _dvSchemaVersion = currentSchemaVersion
      }

instance (FromSZT a, FromSZT b, FromSZT c) => FromSZT (a, b, c) where
  fromSzt (DynamicValue (DProduct [dynA, dynB, dynC]) _ _) = do
    a <- fromSzt dynA
    b <- fromSzt dynB
    c <- fromSzt dynC
    return (a, b, c)
  fromSzt (DynamicValue (DProduct fields) _ _) =
    Left (FieldCountMismatch 3 (length fields))
  fromSzt _ = Left (StructuralMismatch "Expected triple")

instance (ToSZT a, ToSZT b, ToSZT c, ToSZT d) => ToSZT (a, b, c, d) where
  toSzt (w, x, y, z) = DynamicValue
    { _dvCore = DProduct [toSzt w, toSzt x, toSzt y, toSzt z]
    , _dvTypeInfo = Just $ TypeInfo
        { _tiTypeName = Just "(,,,)"
        , _tiModule = Just "GHC.Tuple"
        , _tiConstructors = ["(,,,)"]
              , _tiFieldLabels = []
        , _tiStructure = Nothing
        }
    , _dvSchemaVersion = currentSchemaVersion
    }

instance (FromSZT a, FromSZT b, FromSZT c, FromSZT d) => FromSZT (a, b, c, d) where
  fromSzt (DynamicValue (DProduct [v1, v2, v3, v4]) _ _) = do
    w <- fromSzt v1
    x <- fromSzt v2
    y <- fromSzt v3
    z <- fromSzt v4
    return (w, x, y, z)
  fromSzt _ = Left (StructuralMismatch "Invalid quadruple structure")

--------------------------------------------------------------------------------
-- Protobuf Conversion Functions
--------------------------------------------------------------------------------

data TypeInfoPool = TypeInfoPool
  { _tipNextId :: Word32
  , _tipInterned :: Map.Map TypeInfo Word32
  , _tipShared :: Map.Map Word32 Proto.TypeInfo
  }

emptyTypeInfoPool :: TypeInfoPool
emptyTypeInfoPool = TypeInfoPool 1 Map.empty Map.empty

sharedTypeInfoMap :: TypeInfoPool -> Map.Map Word32 Proto.TypeInfo
sharedTypeInfoMap = _tipShared

type TypeInfoM = State TypeInfoPool

internTypeInfo :: Maybe TypeInfo -> TypeInfoM (Maybe Word32)
internTypeInfo Nothing = pure Nothing
internTypeInfo (Just ti) = do
  TypeInfoPool nextId interned shared <- get
  case Map.lookup ti interned of
    Just existing -> pure (Just existing)
    Nothing -> do
      let protoTI = toProtoTypeInfo ti
          assigned = nextId
          updatedPool = TypeInfoPool (nextId + 1) (Map.insert ti assigned interned) (Map.insert assigned protoTI shared)
      put updatedPool
      pure (Just assigned)

toProtoDynamicValue :: DynamicValue -> TypeInfoM Proto.DynamicValue
toProtoDynamicValue = toProtoDynamicValueWith Nothing

countConstructorsInTypeInfo :: TypeInfo -> Word32
countConstructorsInTypeInfo info =
  case info ^. tiStructure of
    Just (TSSum branches) -> sum (map countConstructorsInTypeInfo branches)
    _ -> 1

resolveConstructorIndex :: Maybe TypeInfo -> Word32 -> DynamicValue -> Word32
resolveConstructorIndex mInfo localIndex childValue =
  case mInfo >>= view tiStructure of
    Just (TSSum branches) ->
      let idx = fromIntegral localIndex
          (before, rest) = splitAt idx branches
          offset = sum (map countConstructorsInTypeInfo before)
       in case rest of
            (current : _) ->
              case (childValue ^. dvCore, current ^. tiStructure) of
                (DSum innerIdx innerValue, Just (TSSum _)) ->
                  offset + resolveConstructorIndex (Just current) innerIdx innerValue
                _ -> offset
            [] -> localIndex
    _ -> localIndex

selectSumBranch :: Maybe TypeInfo -> Word32 -> Either SerializotronError (Word32, Maybe TypeInfo)
selectSumBranch Nothing idx = Right (idx, Nothing)
selectSumBranch (Just info) idx =
  case info ^. tiStructure of
    Just (TSSum branches) ->
      let totalConstructors = sum (map countConstructorsInTypeInfo branches)
       in if totalConstructors == 0
            then mkInvalidSchema "Sum type has no constructors"
            else
              if idx < totalConstructors
                then go idx 0 branches
                else mkInvalidSchema "Constructor index out of range for sum type"
    _ -> Right (idx, Nothing)
  where
    go :: Word32 -> Word32 -> [TypeInfo] -> Either SerializotronError (Word32, Maybe TypeInfo)
    go remaining branchIdx = \case
      [] -> mkInvalidSchema "Constructor index out of range for sum type"
      branch : rest ->
        let branchSize = countConstructorsInTypeInfo branch
         in if remaining < branchSize
              then Right (branchIdx, Just branch)
              else go (remaining - branchSize) (branchIdx + 1) rest

toProtoDynamicValueWith :: Maybe TypeInfo -> DynamicValue -> TypeInfoM Proto.DynamicValue
toProtoDynamicValueWith parentTi (DynamicValue core thisTi _version) = do
  let effectiveTi = thisTi <|> parentTi
  coreProto <- toProtoDynamicCore effectiveTi core
  typeRef <- internTypeInfo effectiveTi
  let base = defMessage Lens.& Proto.core Lens..~ coreProto
  pure $ maybe base (\refId -> base Lens.& Proto.typeInfoRef Lens..~ refId) typeRef

toProtoDynamicCore :: Maybe TypeInfo -> DynamicCore -> TypeInfoM Proto.DynamicCore
toProtoDynamicCore typeInfo = \case
  DPrimitive pv -> pure $ defMessage Lens.& Proto.primitive Lens..~ toProtoPrimitiveValue pv
  DProduct values -> do
    let parents = case typeInfo >>= view tiStructure of
          Just (TSProduct infos) -> map (Just . view fiFieldType) infos ++ repeat Nothing
          _ -> repeat Nothing
    encoded <- zipWithM toProtoDynamicValueWith parents values
    pure $ defMessage Lens.& Proto.product Lens..~ (defMessage Lens.& Proto.fields Lens..~ encoded)
  DSum index value -> do
    let globalIndex = case typeInfo of
          Just info -> resolveConstructorIndex (Just info) index value
          Nothing -> index
        constructorName = maybe (Text.pack (show globalIndex)) (lookupConstructorName globalIndex) typeInfo
        branchTi = case typeInfo >>= view tiStructure of
          Just (TSSum branches) -> branches ^? ix (fromIntegral index)
          _ -> Nothing
    encodedValue <- toProtoDynamicValueWith branchTi value
    pure $
      defMessage
        Lens.& Proto.sum Lens..~
          ( defMessage
              Lens.& Proto.constructorIndex Lens..~ globalIndex
              Lens.& Proto.constructorName Lens..~ constructorName
              Lens.& Proto.value Lens..~ encodedValue
          )
  DList values -> do
    let itemTi = case typeInfo >>= view tiStructure of
          Just (TSList ti) -> Just ti
          _ -> Nothing
    encoded <- mapM (toProtoDynamicValueWith itemTi) values
    pure $ defMessage Lens.& Proto.list Lens..~ (defMessage Lens.& Proto.elements Lens..~ encoded)
  DUnit -> pure $ defMessage Lens.& Proto.unit Lens..~ defMessage
  DReference refId -> pure $ defMessage Lens.& Proto.reference Lens..~ (defMessage Lens.& Proto.referenceId Lens..~ refId)


lookupConstructorName :: Word32 -> TypeInfo -> Text
lookupConstructorName index info = fromMaybe "" $ info ^? tiConstructors . ix (fromIntegral index)

lookupConstructorIndex :: Text -> TypeInfo -> Maybe Word32
lookupConstructorIndex name info = fromIntegral <$> elemIndex name (info ^. tiConstructors)

toProtoPrimitiveValue :: PrimitiveValue -> Proto.PrimitiveValue
toProtoPrimitiveValue = \case
  PInt i -> defMessage Lens.& Proto.intVal Lens..~ fromIntegral i
  PDouble d -> defMessage Lens.& Proto.doubleVal Lens..~ d
  PText t -> defMessage Lens.& Proto.textVal Lens..~ t
  PBool b -> defMessage Lens.& Proto.boolVal Lens..~ b
  PWord64 w -> defMessage Lens.& Proto.word64Val Lens..~ w
  PInt32 i -> defMessage Lens.& Proto.int32Val Lens..~ i
  PWord32 w -> defMessage Lens.& Proto.word32Val Lens..~ w
  PInteger t -> defMessage Lens.& Proto.integerVal Lens..~ t
  PBytes bs -> defMessage Lens.& Proto.bytesVal Lens..~ bs

toProtoTypeInfo :: TypeInfo -> Proto.TypeInfo
toProtoTypeInfo (TypeInfo name mod cons fieldLabels struct) =
  defMessage
    Lens.& Proto.typeName Lens..~ fromMaybe "" name
    Lens.& Proto.moduleName Lens..~ fromMaybe "" mod
    Lens.& Proto.constructors Lens..~ cons
    Lens.& Proto.fieldLabels Lens..~ fieldLabels
    Lens.& Proto.structure Lens..~ maybe defMessage toProtoTypeStructure struct

toProtoTypeStructure :: TypeStructure -> Proto.TypeStructure
toProtoTypeStructure = \case
  TSPrimitive pt -> defMessage Lens.& Proto.primitive Lens..~ toProtoPrimitiveType pt
  TSProduct fields -> defMessage Lens.& Proto.product Lens..~ (defMessage Lens.& Proto.fields Lens..~ map toProtoFieldInfo fields)
  TSSum tis -> defMessage Lens.& Proto.sum Lens..~ (defMessage Lens.& Proto.constructorTypes Lens..~ map toProtoTypeInfo tis)
  TSList ti -> defMessage Lens.& Proto.list Lens..~ (defMessage Lens.& Proto.elementType Lens..~ toProtoTypeInfo ti)
  TSUnit -> defMessage Lens.& Proto.unit Lens..~ defMessage

toProtoFieldInfo :: FieldInfo -> Proto.ProductStructure'FieldInfo
toProtoFieldInfo (FieldInfo name ty) =
  defMessage
    Lens.& Proto.fieldName Lens..~ fromMaybe "" name
    Lens.& Proto.fieldType Lens..~ toProtoTypeInfo ty

toProtoPrimitiveType :: PrimitiveType -> Proto.PrimitiveType
toProtoPrimitiveType = \case
  PTInt -> Proto.PRIMITIVE_INT
  PTDouble -> Proto.PRIMITIVE_DOUBLE
  PTText -> Proto.PRIMITIVE_TEXT
  PTBool -> Proto.PRIMITIVE_BOOL
  PTWord64 -> Proto.PRIMITIVE_WORD64
  PTInt32 -> Proto.PRIMITIVE_INT32
  PTWord32 -> Proto.PRIMITIVE_WORD32
  PTInteger -> Proto.PRIMITIVE_INTEGER
  PTBytes -> Proto.PRIMITIVE_BYTES

fromProtoDynamicValue :: Word32 -> Map.Map Word32 TypeInfo -> Proto.DynamicValue -> Either SerializotronError DynamicValue
fromProtoDynamicValue schemaVersion typeTable protoDV = do
  let inlineTypeProto = protoDV Lens.^. Proto.typeInfo
      hasInline = inlineTypeProto /= defMessage
      refId = protoDV Lens.^. Proto.typeInfoRef
  resolvedTypeInfo <-
    if hasInline
      then do
        inlineTi <- fromProtoTypeInfo inlineTypeProto
        if refId /= 0
          then case Map.lookup refId typeTable of
            Just referencedTi
              | referencedTi == inlineTi -> pure (Just inlineTi)
              | otherwise ->
                  Left $ ValidationError $ InvalidSchema $
                    "Type info mismatch between inline metadata and reference id "
                      <> Text.pack (show refId)
            Nothing ->
              Left $ ValidationError $ InvalidSchema $
                "Unknown type_info_ref " <> Text.pack (show refId)
          else pure (Just inlineTi)
      else
        if refId /= 0
          then case Map.lookup refId typeTable of
            Just ti -> pure (Just ti)
            Nothing ->
              Left $ ValidationError $ InvalidSchema $
                "Unknown type_info_ref " <> Text.pack (show refId)
          else pure Nothing
  core <- fromProtoDynamicCore schemaVersion typeTable resolvedTypeInfo (protoDV Lens.^. Proto.core)
  return $ DynamicValue core resolvedTypeInfo schemaVersion

fromProtoDynamicCore :: Word32 -> Map.Map Word32 TypeInfo -> Maybe TypeInfo -> Proto.DynamicCore -> Either SerializotronError DynamicCore
fromProtoDynamicCore schemaVersion typeTable typeInfo protoCore =
  case protoCore Lens.^. Proto.maybe'core of
    Just (Proto.DynamicCore'Primitive pv) -> DPrimitive <$> fromProtoPrimitiveValue pv
    Just (Proto.DynamicCore'Product prod) -> DProduct <$> traverse (fromProtoDynamicValue schemaVersion typeTable) (prod Lens.^. Proto.fields)
    Just (Proto.DynamicCore'Sum sum') -> do
      let providedIndex = sum' Lens.^. Proto.constructorIndex
      (localIndex, _branchInfo) <- selectSumBranch typeInfo providedIndex
      value <- fromProtoDynamicValue schemaVersion typeTable (sum' Lens.^. Proto.value)
      return $ DSum localIndex value
    Just (Proto.DynamicCore'List list') -> DList <$> traverse (fromProtoDynamicValue schemaVersion typeTable) (list' Lens.^. Proto.elements)
    Just (Proto.DynamicCore'Unit _) -> return DUnit
    Just (Proto.DynamicCore'Reference ref) -> return $ DReference (ref Lens.^. Proto.referenceId)
    Nothing -> Left $ ProtocolBufferError (MissingField "core")

fromProtoPrimitiveValue :: Proto.PrimitiveValue -> Either SerializotronError PrimitiveValue
fromProtoPrimitiveValue protoPV =
  case protoPV Lens.^. Proto.maybe'primitive of
    Just (Proto.PrimitiveValue'IntVal i) -> return $ PInt (fromIntegral i)
    Just (Proto.PrimitiveValue'DoubleVal d) -> return $ PDouble d
    Just (Proto.PrimitiveValue'TextVal t) -> return $ PText t
    Just (Proto.PrimitiveValue'BoolVal b) -> return $ PBool b
    Just (Proto.PrimitiveValue'Word64Val w) -> return $ PWord64 w
    Just (Proto.PrimitiveValue'Int32Val i) -> return $ PInt32 i
    Just (Proto.PrimitiveValue'Word32Val w) -> return $ PWord32 w
    Just (Proto.PrimitiveValue'IntegerVal t) -> return $ PInteger t
    Just (Proto.PrimitiveValue'BytesVal bs) -> return $ PBytes bs
    Nothing -> Left $ ProtocolBufferError (MissingField "primitive")

fromProtoTypeInfo :: Proto.TypeInfo -> Either SerializotronError TypeInfo

mkInvalidSchema :: Text -> Either SerializotronError a
mkInvalidSchema msg = Left (ValidationError (InvalidSchema msg))

findDuplicateLabels :: [Text] -> [Text]
findDuplicateLabels labels =
  [ label
  | (label, count) <- Map.toList (Map.fromListWith (+) [(label, 1 :: Int) | label <- labels])
  , count > 1
  ]

formatLabels :: [Text] -> Text
formatLabels = Text.intercalate ", "

normalizeTypeInfo :: TypeInfo -> Either SerializotronError TypeInfo
normalizeTypeInfo info =
  case info ^. tiStructure of
    Just (TSProduct fields) ->
      let normalizedFields = assignFieldLabels fields
          labels = map (fromMaybe Text.empty . (^. fiFieldName)) normalizedFields
       in if any Text.null labels
            then mkInvalidSchema "Product field labels may not be empty"
            else
              let duplicates = findDuplicateLabels labels
               in if not (null duplicates)
                    then mkInvalidSchema $ "Duplicate field labels: " <> formatLabels duplicates
                    else
                      let provided = info ^. tiFieldLabels
                          info' = info & tiFieldLabels .~ labels & tiStructure ?~ TSProduct normalizedFields
                       in if null provided || provided == labels
                            then Right info'
                            else mkInvalidSchema $ "Field label mismatch: " <> formatLabels provided <> " vs " <> formatLabels labels
    _ ->
      if null (info ^. tiFieldLabels)
        then Right info
        else mkInvalidSchema "Field labels provided for non-product type"

fromProtoTypeInfo protoTI = do
  struct <-
    if protoTI Lens.^. Proto.structure == defMessage
      then return Nothing
      else Just <$> fromProtoTypeStructure (protoTI Lens.^. Proto.structure)
  let info =
        TypeInfo
          (if Text.null (protoTI Lens.^. Proto.typeName) then Nothing else Just (protoTI Lens.^. Proto.typeName))
          (if Text.null (protoTI Lens.^. Proto.moduleName) then Nothing else Just (protoTI Lens.^. Proto.moduleName))
          (protoTI Lens.^. Proto.constructors)
          (protoTI Lens.^. Proto.fieldLabels)
          struct
  normalizeTypeInfo info
fromProtoTypeStructure :: Proto.TypeStructure -> Either SerializotronError TypeStructure
fromProtoTypeStructure protoTS =
  case protoTS Lens.^. Proto.maybe'structure of
    Just (Proto.TypeStructure'Primitive pt) -> return $ TSPrimitive (fromProtoPrimitiveType pt)
    Just (Proto.TypeStructure'Product prod) -> TSProduct <$> traverse fromProtoFieldInfo (prod Lens.^. Proto.fields)
    Just (Proto.TypeStructure'Sum sum') -> TSSum <$> traverse fromProtoTypeInfo (sum' Lens.^. Proto.constructorTypes)
    Just (Proto.TypeStructure'List list') -> TSList <$> fromProtoTypeInfo (list' Lens.^. Proto.elementType)
    Just (Proto.TypeStructure'Unit _) -> return TSUnit
    Nothing -> Left $ ProtocolBufferError (MissingField "structure")

fromProtoFieldInfo :: Proto.ProductStructure'FieldInfo -> Either SerializotronError FieldInfo
fromProtoFieldInfo protoFI = do
  fieldType <- fromProtoTypeInfo (protoFI Lens.^. Proto.fieldType)
  let nameText = protoFI Lens.^. Proto.fieldName
      fieldName = if Text.null nameText then Nothing else Just nameText
  return $ FieldInfo fieldName fieldType

fromProtoPrimitiveType :: Proto.PrimitiveType -> PrimitiveType
fromProtoPrimitiveType = \case
  Proto.PRIMITIVE_INT -> PTInt
  Proto.PRIMITIVE_DOUBLE -> PTDouble
  Proto.PRIMITIVE_TEXT -> PTText
  Proto.PRIMITIVE_BOOL -> PTBool
  Proto.PRIMITIVE_WORD64 -> PTWord64
  Proto.PRIMITIVE_INT32 -> PTInt32
  Proto.PRIMITIVE_WORD32 -> PTWord32
  Proto.PRIMITIVE_INTEGER -> PTInteger
  Proto.PRIMITIVE_BYTES -> PTBytes
  -- Handle unrecognized values gracefully
  _ -> PTInt -- Default fallback

--------------------------------------------------------------------------------
-- File System Check (fsck) for .szt integrity verification
--------------------------------------------------------------------------------

-- | Integrity errors found during fsck
data FsckError
  = FsckDanglingReference Word32 -- Reference ID that doesn't exist in shared table
  | CyclicReference [Word32] -- List of reference IDs forming a cycle
  | CorruptedSharedValue Word32 Text -- Reference ID and error message
  deriving stock (Show, Eq)

-- | Non-fatal warnings found during fsck
data FsckWarning
  = UnusedSharedValue Word32 -- Reference ID that's never referenced
  | SuspiciousTypeInfo Text -- Potentially malformed type info
  deriving stock (Show, Eq)

-- | Statistics from integrity check
data FsckStats = FsckStats
  { _fsckTotalReferences :: Int
  , _fsckSharedValues :: Int
  , _fsckDanglingReferences :: Int
  }
  deriving stock (Show, Eq)

makeLenses ''FsckStats


-- | Results of integrity verification
data FsckResult = FsckResult
  { _fsckPassed :: Bool
  , _fsckErrors :: [FsckError]
  , _fsckWarnings :: [FsckWarning]
  , _fsckStats :: FsckStats
  }
  deriving stock (Show, Eq)

makeLenses ''FsckResult

-- | Verify integrity of a .szt file
fsckSzt :: FilePath -> IO FsckResult
fsckSzt path = do
  result <- try $ ByteString.readFile path
  case result of
    Left ioErr -> return $ FsckResult False [CorruptedSharedValue 0 (Text.pack (show (ioErr :: IOException)))] [] (FsckStats 0 0 1)
    Right fileBytes -> do
      -- Validate minimum size for header
      if ByteString.length fileBytes < 8
        then return $ FsckResult False [CorruptedSharedValue 0 "File too short to contain header"] [] (FsckStats 0 0 1)
        else do
          -- Parse header
          case decodeHeader (ByteString.take 8 fileBytes) of
            Left err -> return $ FsckResult False [CorruptedSharedValue 0 err] [] (FsckStats 0 0 1)
            Right header -> do
              let payloadBytes = ByteString.drop 8 fileBytes
              -- Decompress if needed
              protoBytes <- case _headerCompression header of
                NoCompression -> return $ Right payloadBytes
                GZipCompression ->
                  return $ Right $ LBS.toStrict $ GZip.decompress $ LBS.fromStrict payloadBytes

              case protoBytes of
                Left err -> return $ FsckResult False [CorruptedSharedValue 0 err] [] (FsckStats 0 0 1)
                Right bytes ->
                  case decodeMessage bytes :: Either String Proto.SZTFile of
                    Left err -> return $ FsckResult False [CorruptedSharedValue 0 (Text.pack err)] [] (FsckStats 0 0 1)
                    Right sztFile -> do
                      let schemaVersion = sztFile Lens.^. Proto.schemaVersion
                      let protoSharedTypes = sztFile Lens.^. Proto.sharedTypeInfo
                      case Map.traverseWithKey (const fromProtoTypeInfo) protoSharedTypes of
                        Left err -> return $ FsckResult False [CorruptedSharedValue 0 (formatError err)] [] (FsckStats 0 0 1)
                        Right sharedTypeMap -> do
                          let protoSharedValues = sztFile Lens.^. Proto.sharedValues
                          let sharedTable = Map.map (fromProtoDynamicValue schemaVersion sharedTypeMap) protoSharedValues

                          case fromProtoDynamicValue schemaVersion sharedTypeMap (sztFile Lens.^. Proto.value) of
                            Left err -> return $ FsckResult False [CorruptedSharedValue 0 (formatError err)] [] (FsckStats 0 0 1)
                            Right rootValue -> return $ performFsck rootValue sharedTable

-- | Internal implementation of integrity checking
performFsck :: DynamicValue -> Map.Map Word32 (Either SerializotronError DynamicValue) -> FsckResult
performFsck rootValue sharedTable =
  let -- Convert Either values to DynamicValue, collecting corruption errors
      (validSharedTable, corruptionErrors) = partitionSharedTable sharedTable

      -- Collect all reference IDs used in the value tree
      usedReferences = collectReferences rootValue validSharedTable

      -- Find dangling references (used but not in shared table)
      danglingRefs = [refId | refId <- Set.toList usedReferences, not (Map.member refId validSharedTable)]

      -- Find unused shared values (in table but never referenced)
      unusedValues = [refId | refId <- Map.keys validSharedTable, not (Set.member refId usedReferences)]

      -- Detect cycles in the reference graph
      cycles = detectCycles rootValue validSharedTable

      -- Build errors and warnings
      errors = map FsckDanglingReference danglingRefs
            ++ map CyclicReference cycles
            ++ corruptionErrors

      warnings = map UnusedSharedValue unusedValues

      stats =
        FsckStats
          { _fsckTotalReferences = length usedReferences
          , _fsckSharedValues = Map.size validSharedTable
          , _fsckDanglingReferences = length danglingRefs
          }
   in FsckResult (null errors) errors warnings stats

-- | Helper to partition shared table and collect corruption errors
partitionSharedTable :: Map.Map Word32 (Either SerializotronError DynamicValue) -> (Map.Map Word32 DynamicValue, [FsckError])
partitionSharedTable sharedTable =
  let (errors, valids) = Map.partition isLeft sharedTable
      corruptionErrors = [CorruptedSharedValue refId (formatError err) | (refId, Left err) <- Map.toList errors]
      validTable = Map.mapMaybe (\case Right val -> Just val; Left _ -> Nothing) valids
   in (validTable, corruptionErrors)
  where
    isLeft (Left _) = True
    isLeft (Right _) = False

-- | Collect all reference IDs used in a value tree
collectReferences :: DynamicValue -> Map.Map Word32 DynamicValue -> Set.Set Word32
collectReferences = collectReferencesWithVisited Set.empty
  where
    collectReferencesWithVisited :: Set.Set Word32 -> DynamicValue -> Map.Map Word32 DynamicValue -> Set.Set Word32
    collectReferencesWithVisited visited (DynamicValue core _ _) sharedTable =
      collectReferencesFromCore visited core sharedTable

    collectReferencesFromCore :: Set.Set Word32 -> DynamicCore -> Map.Map Word32 DynamicValue -> Set.Set Word32
    collectReferencesFromCore visited core sharedTable = case core of
      DPrimitive _ -> Set.empty
      DProduct vals -> Set.unions (map (\val -> collectReferencesWithVisited visited val sharedTable) vals)
      DSum _ val -> collectReferencesWithVisited visited val sharedTable
      DList vals -> Set.unions (map (\val -> collectReferencesWithVisited visited val sharedTable) vals)
      DUnit -> Set.empty
      DReference refId ->
        if Set.member refId visited
          then Set.singleton refId -- Include the reference itself even if cyclic
          else case Map.lookup refId sharedTable of
            Nothing -> Set.singleton refId -- Dangling reference
            Just sharedValue -> Set.insert refId (collectReferencesWithVisited (Set.insert refId visited) sharedValue sharedTable)

-- | Detect cycles in reference graph starting from the root value
detectCycles :: DynamicValue -> Map.Map Word32 DynamicValue -> [[Word32]]
detectCycles rootValue sharedTable =
  let rootRefs = getDirectReferences rootValue
   in concatMap (findCycleFromRef Set.empty) rootRefs
  where
    findCycleFromRef :: Set.Set Word32 -> Word32 -> [[Word32]]
    findCycleFromRef visiting refId
      | Set.member refId visiting = [[refId]] -- Found a cycle
      | otherwise = case Map.lookup refId sharedTable of
          Nothing -> [] -- Dangling reference, not a cycle
          Just dynVal ->
            let nextRefs = getDirectReferences dynVal
                newVisiting = Set.insert refId visiting
             in concatMap (findCycleFromRef newVisiting) nextRefs

    getDirectReferences :: DynamicValue -> [Word32]
    getDirectReferences (DynamicValue core _ _) = case core of
      DPrimitive _ -> []
      DProduct vals -> concatMap getDirectReferences vals
      DSum _ val -> getDirectReferences val
      DList vals -> concatMap getDirectReferences vals
      DUnit -> []
      DReference refId -> [refId]

-- | Helper function for triple destructuring
uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

encodeWithTypePool :: DynamicValue -> Map.Map Word32 DynamicValue -> (Proto.DynamicValue, Map.Map Word32 Proto.DynamicValue, Map.Map Word32 Proto.TypeInfo)
encodeWithTypePool root shared =
  let action :: TypeInfoM (Proto.DynamicValue, Map.Map Word32 Proto.DynamicValue)
      action = do
        rootProto <- toProtoDynamicValue root
        sharedProto <- Map.traverseWithKey (const toProtoDynamicValue) shared
        pure (rootProto, sharedProto)
      ((rootProto, sharedProto), pool) = runState action emptyTypeInfoPool
   in (rootProto, sharedProto, sharedTypeInfoMap pool)

--------------------------------------------------------------------------------
-- File I/O API with Protobuf Support (.szt format)
--------------------------------------------------------------------------------

-- | Save a value to a .szt file with no deduplication.
--
-- This is the simplest way to save data. Each repeated value is stored separately,
-- resulting in larger files but faster serialization.
--
-- Example:
-- @
-- data Person = Person Text Int deriving (Generic, ToSZT)
-- person = Person "Alice" 25
-- saveSzt "person.szt" person
-- @
saveSzt :: (ToSZT a) => FilePath -> a -> IO ()
saveSzt = saveSztWithStrategy noDeduplicationStrategy

-- | Save a value to a .szt file with a custom deduplication strategy.
--
-- This is the most flexible save function, allowing you to control how
-- duplicate values are detected and shared to reduce file size.
--
-- Deduplication strategies:
-- * 'noDeduplicationStrategy' - No sharing, fastest serialization
-- * 'defaultDeduplicationStrategy' - Conservative sharing, good balance
-- * 'aggressiveDeduplicationStrategy' - Maximum sharing, smallest files
--
-- Example:
-- @
-- -- Save with custom strategy for very large data
-- import qualified Data.Map as Map
-- largeData = Map.fromList [(i, "value" ++ show i) | i <- [1..10000]]
-- saveSztWithStrategy aggressiveDeduplicationStrategy "large.szt" largeData
-- @
saveSztWithStrategy :: (ToSZT a) => DeduplicationStrategy -> FilePath -> a -> IO ()
saveSztWithStrategy strategy path value =
  saveSztWithCompressionAndStrategy NoCompression strategy path value

-- | Internal function that handles compression
saveSztWithCompressionAndStrategy :: (ToSZT a) => CompressionMethod -> DeduplicationStrategy -> FilePath -> a -> IO ()
saveSztWithCompressionAndStrategy compression strategy path value = do
  let dynValue = toSzt value
  let (dedupedValue, sharedTable) = deduplicateValue strategy dynValue
  let (protoDynValue, protoSharedValues, protoSharedTypes) = encodeWithTypePool dedupedValue sharedTable
  let sztFile =
        defMessage
          Lens.& Proto.schemaVersion Lens..~ currentSchemaVersion
          Lens.& Proto.value Lens..~ protoDynValue
          Lens.& Proto.sharedValues Lens..~ protoSharedValues
          Lens.& Proto.sharedTypeInfo Lens..~ protoSharedTypes
  let protoBytes = encodeMessage (sztFile :: Proto.SZTFile)

  -- Apply compression if specified
  let payloadBytes = case compression of
        NoCompression -> protoBytes
        GZipCompression ->
          -- GZip compression with default settings
          LBS.toStrict $ GZip.compress $ LBS.fromStrict protoBytes

  -- Combine header and payload
  let header = mkHeader compression
  let headerBytes = encodeHeader header
  let finalBytes = ByteString.append headerBytes payloadBytes

  ByteString.writeFile path finalBytes

-- | Save a value with balanced deduplication (recommended for most use cases).
--
-- Uses 'defaultDeduplicationStrategy' which provides a good balance between
-- file size reduction and serialization performance. Deduplicates strings,
-- lists, and complex structures but avoids deduplicating small primitives.
--
-- Example:
-- @
-- data Config = Config [Text] (Map Text Int) deriving (Generic, ToSZT)
-- config = Config ["option1", "option2"] (Map.fromList [("key", 42)])
-- saveSztCompressed "config.szt" config
-- @
saveSztCompressed :: (ToSZT a) => FilePath -> a -> IO ()
saveSztCompressed = saveSztWithCompressionAndStrategy GZipCompression defaultDeduplicationStrategy

-- | Save a value with maximum deduplication (best for very large, repetitive data).
--
-- Uses 'aggressiveDeduplicationStrategy' which deduplicates everything possible,
-- including small primitives. This produces the smallest files but takes more
-- time and memory during serialization.
--
-- Best for: Large datasets, data with lots of repetition, archival storage.
--
-- Example:
-- @
-- -- Saves efficiently due to repeated "common" strings
-- data Record = Record Text Text Int deriving (Generic, ToSZT)
-- records = replicate 1000 (Record "common" "shared" 42)
-- saveSztCompressedAggressive "records.szt" records
-- @
saveSztCompressedAggressive :: (ToSZT a) => FilePath -> a -> IO ()
saveSztCompressedAggressive = saveSztWithCompressionAndStrategy GZipCompression aggressiveDeduplicationStrategy

-- | Load a value from a .szt file, deserializing it to the target type.
--
-- This function handles all the complexity of loading, deduplication resolution,
-- and type conversion. It provides detailed error information if anything goes wrong.
--
-- Returns 'Left SerializotronError' if:
-- * File doesn't exist or can't be read
-- * File is corrupted or has wrong format
-- * File was created with incompatible schema version
-- * Deserialization to target type fails
-- * File contains cyclic references or other integrity issues
--
-- Example:
-- @
-- data Person = Person Text Int deriving (Generic, FromSZT)
--
-- loadPerson :: IO (Either SerializotronError Person)
-- loadPerson = loadSzt "person.szt"
--
-- main = do
--   result <- loadPerson
--   case result of
--     Left err -> putStrLn $ "Error: " <> formatError err
--     Right person -> print person
-- @
loadSzt :: (FromSZT a) => FilePath -> IO (Either SerializotronError a)
loadSzt path = do
  result <- try $ ByteString.readFile path
  case result of
    Left ioErr -> return $ Left $ IOError ioErr
    Right fileBytes -> do
      -- Validate minimum size for header
      if ByteString.length fileBytes < 8
        then return $ Left $ FileSystemError $ InvalidFileFormat path "File too short to contain header"
        else do
          -- Parse header
          case decodeHeader (ByteString.take 8 fileBytes) of
            Left err -> return $ Left $ FileSystemError $ InvalidFileFormat path err
            Right header -> do
              let payloadBytes = ByteString.drop 8 fileBytes
              -- Decompress if needed
              protoBytes <- case _headerCompression header of
                NoCompression -> return $ Right payloadBytes
                GZipCompression ->
                  return $ Right $ LBS.toStrict $ GZip.decompress $ LBS.fromStrict payloadBytes

              case protoBytes of
                Left err -> return $ Left err
                Right bytes ->
                  case decodeMessage bytes :: Either String Proto.SZTFile of
                    Left err -> return $ Left $ ProtocolBufferError (ProtocolDecodingError (Text.pack err))
                    Right sztFile -> do
                      let schemaVersion = sztFile Lens.^. Proto.schemaVersion
                      let protoSharedTypes = sztFile Lens.^. Proto.sharedTypeInfo
                      case Map.traverseWithKey (\_ -> fromProtoTypeInfo) protoSharedTypes of
                        Left err -> return $ Left err
                        Right sharedTypeMap -> do
                          let protoSharedValues = sztFile Lens.^. Proto.sharedValues
                          let sharedTable = Map.map (fromProtoDynamicValue schemaVersion sharedTypeMap) protoSharedValues
                          case fromProtoDynamicValue schemaVersion sharedTypeMap (sztFile Lens.^. Proto.value) of
                            Left err -> return $ Left err
                            Right dynValue -> do
                              case resolveReferences sharedTable dynValue of
                                Left err -> return $ Left err
                                Right resolvedValue ->
                                  return $ first (\sztErr -> DeserializationError sztErr emptyContext) (fromSzt resolvedValue)

-- | Load the raw internal representation from a .szt file (for debugging).
--
-- This function loads the file and resolves references but doesn't attempt
-- to convert to a specific Haskell type. Useful for:
-- * Debugging serialization issues
-- * Inspecting file contents without knowing the original type
-- * Building tools that work with .szt files generically
--
-- Example:
-- @
-- main = do
--   result <- loadSztRaw "mystery.szt"
--   case result of
--     Left err -> putStrLn $ "Error: " <> formatError err
--     Right dynValue -> do
--       putStrLn $ "Loaded value with " <> show (estimateSize dynValue) <> " bytes"
--       print dynValue
-- @
loadSztRaw :: FilePath -> IO (Either SerializotronError DynamicValue)
loadSztRaw path = do
  result <- try $ LBS.readFile path
  case result of
    Left ioErr -> return $ Left $ IOError ioErr
    Right encodedBytes ->
      case decodeMessage (LBS.toStrict encodedBytes) :: Either String Proto.SZTFile of
        Left err -> return $ Left $ ProtocolBufferError (ProtocolDecodingError (Text.pack err))
        Right sztFile -> do
          let schemaVersion = sztFile Lens.^. Proto.schemaVersion
          let protoSharedTypes = sztFile Lens.^. Proto.sharedTypeInfo
          case Map.traverseWithKey (\_ -> fromProtoTypeInfo) protoSharedTypes of
            Left err -> return $ Left err
            Right sharedTypeMap -> do
              let protoSharedValues = sztFile Lens.^. Proto.sharedValues
              let sharedTable = Map.map (fromProtoDynamicValue schemaVersion sharedTypeMap) protoSharedValues
              case fromProtoDynamicValue schemaVersion sharedTypeMap (sztFile Lens.^. Proto.value) of
                Left err -> return $ Left err
                Right dynValue -> return $ resolveReferences sharedTable dynValue

-- Resolve references in a DynamicValue using the shared table
resolveReferences :: Map.Map Word32 (Either SerializotronError DynamicValue) -> DynamicValue -> Either SerializotronError DynamicValue
resolveReferences sharedTable = resolveValue Set.empty
  where
    resolveValue :: Set.Set Word32 -> DynamicValue -> Either SerializotronError DynamicValue
    resolveValue visiting (DynamicValue core typeInfo version) = do
      resolvedCore <- resolveCore visiting core
      return $ DynamicValue resolvedCore typeInfo version

    resolveCore :: Set.Set Word32 -> DynamicCore -> Either SerializotronError DynamicCore
    resolveCore visiting = \case
      DPrimitive pv -> return $ DPrimitive pv
      DProduct vals -> DProduct <$> traverse (resolveValue visiting) vals
      DSum i val -> DSum i <$> resolveValue visiting val
      DList vals -> DList <$> traverse (resolveValue visiting) vals
      DUnit -> return DUnit
      DReference refId ->
        if Set.member refId visiting
          then Left $ ValidationError (CyclicReferences [refId])
          else case Map.lookup refId sharedTable of
            Nothing -> Left $ ValidationError (DanglingReference refId)
            Just (Left err) -> Left err
            Just (Right sharedValue) -> do
              -- Add this reference to the visiting set to detect cycles
              resolvedShared <- resolveValue (Set.insert refId visiting) sharedValue
              return $ _dvCore resolvedShared
