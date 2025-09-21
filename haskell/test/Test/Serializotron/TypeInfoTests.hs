{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Tests for Generic TypeInfo extraction, particularly the product type instance
module Test.Serializotron.TypeInfoTests where

import GHC.Generics
import Data.Text (Text)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Control.Lens ((^.))

import Serializotron
import Serializotron.Instances () -- For Maybe and Either instances

--------------------------------------------------------------------------------
-- Test Data Types
--------------------------------------------------------------------------------

-- Simple product type (Text :*: Int)
data Person = Person Text Int 
  deriving stock (Generic, Eq, Show)

instance ToSZT Person
instance FromSZT Person

-- Product with three fields
data Employee = Employee
  { empName :: Text
  , empAge :: Int
  , empSalary :: Double
  } deriving stock (Generic, Eq, Show)

instance ToSZT Employee
instance FromSZT Employee

-- Single field record
newtype Solo = Solo Int
  deriving stock (Generic, Eq, Show)

instance ToSZT Solo
instance FromSZT Solo

-- Empty constructor
data Unit = Unit
  deriving stock (Generic, Eq, Show)

instance ToSZT Unit
instance FromSZT Unit

-- Sum type with products
data Shape
  = Circle Double
  | Rectangle Double Double
  | Triangle Double Double Double
  deriving stock (Generic, Eq, Show)

instance ToSZT Shape
instance FromSZT Shape

-- Simple sum type for testing constructor completeness
data Color = Red | Green | Blue
  deriving stock (Generic, Eq, Show)

instance ToSZT Color
instance FromSZT Color

--------------------------------------------------------------------------------
-- Generators
--------------------------------------------------------------------------------

genText :: Gen Text
genText = Gen.text (Range.linear 0 50) Gen.unicode

genPerson :: Gen Person
genPerson = Person <$> genText <*> Gen.int (Range.linear 0 120)

genEmployee :: Gen Employee
genEmployee = Employee 
  <$> genText 
  <*> Gen.int (Range.linear 18 100)
  <*> Gen.double (Range.exponentialFloat 0 1000000)

genSolo :: Gen Solo
genSolo = Solo <$> Gen.int (Range.linear minBound maxBound)

genUnit :: Gen Unit
genUnit = pure Unit

genShape :: Gen Shape
genShape = Gen.choice
  [ Circle <$> Gen.double (Range.exponentialFloat 0.1 100)
  , Rectangle <$> Gen.double (Range.exponentialFloat 0.1 100) 
               <*> Gen.double (Range.exponentialFloat 0.1 100)
  , Triangle <$> Gen.double (Range.exponentialFloat 0.1 100)
             <*> Gen.double (Range.exponentialFloat 0.1 100)
             <*> Gen.double (Range.exponentialFloat 0.1 100)
  ]

genColor :: Gen Color
genColor = Gen.choice [pure Red, pure Green, pure Blue]

--------------------------------------------------------------------------------
-- TypeInfo Extraction Tests
--------------------------------------------------------------------------------

-- | Test that Person (with Text :*: Int product) extracts correct type info
prop_person_typeinfo :: Property
prop_person_typeinfo = property $ do
  person <- forAll genPerson
  let dynVal = toSzt person
  case dynVal ^. dvTypeInfo of
    Just typeInfo -> do
      -- The top-level type should have the Person type name
      annotate $ "TypeInfo: " ++ show typeInfo
      typeInfo ^. tiTypeName === Just "Person"
      typeInfo ^. tiModule === Just "Test.Serializotron.TypeInfoTests"
      typeInfo ^. tiConstructors === ["Person"]
      
      -- Check that the structure shows a product
      case typeInfo ^. tiStructure of
        Just (TSProduct components) -> do
          annotate $ "Product has " ++ show (length components) ++ " components"
          -- Person has one constructor with a product of fields
          success
        Just other -> do
          annotate $ "Expected TSProduct, got: " ++ show other
          failure
        Nothing -> do
          annotate "No type structure found"
          failure
    Nothing -> do
      annotate "No type info found"
      failure

-- | Test Employee with three fields
prop_employee_typeinfo :: Property
prop_employee_typeinfo = property $ do
  employee <- forAll genEmployee
  let dynVal = toSzt employee
  case dynVal ^. dvTypeInfo of
    Just typeInfo -> do
      typeInfo ^. tiTypeName === Just "Employee"
      typeInfo ^. tiConstructors === ["Employee"]
      
      -- Verify the structure
      case typeInfo ^. tiStructure of
        Just (TSProduct _) -> success
        _ -> failure
    Nothing -> failure

-- | Test Solo with single field
prop_solo_typeinfo :: Property
prop_solo_typeinfo = property $ do
  solo <- forAll genSolo
  let dynVal = toSzt solo
  case dynVal ^. dvTypeInfo of
    Just typeInfo -> do
      typeInfo ^. tiTypeName === Just "Solo"
      typeInfo ^. tiConstructors === ["Solo"]
      success
    Nothing -> failure

-- | Test Unit with no fields
prop_unit_typeinfo :: Property
prop_unit_typeinfo = property $ do
  let unit = Unit
  let dynVal = toSzt unit
  case dynVal ^. dvTypeInfo of
    Just typeInfo -> do
      typeInfo ^. tiTypeName === Just "Unit"
      typeInfo ^. tiConstructors === ["Unit"]
      success
    Nothing -> failure

-- | Test Shape sum type with products - UPDATED to test complete constructor info
prop_shape_typeinfo :: Property
prop_shape_typeinfo = property $ do
  shape <- forAll genShape
  let dynVal = toSzt shape
  case dynVal ^. dvTypeInfo of
    Just typeInfo -> do
      annotate $ "TypeInfo: " ++ show typeInfo
      annotate $ "Shape value: " ++ show shape
      typeInfo ^. tiTypeName === Just "Shape"
      
      -- ALL constructors should be present regardless of which one is active
      typeInfo ^. tiConstructors === ["Circle", "Rectangle", "Triangle"]
      
      -- The structure should be a sum type with binary tree structure
      case typeInfo ^. tiStructure of
        Just (TSSum constructorTypes) -> do
          annotate $ "Sum type has " ++ show (length constructorTypes) ++ " constructor types"
          -- Generic representation uses binary trees, so we get 2 at the top level
          length constructorTypes === 2  -- Binary tree: Circle :+: (Rectangle :+: Triangle)
        Just other -> do
          annotate $ "Expected TSSum but got: " ++ show other
          failure
        Nothing -> do
          annotate "Expected type structure but got Nothing"
          failure
    Nothing -> do
      annotate "No type info found"
      failure

-- | Test that Color sum type captures ALL constructors regardless of which is serialized
prop_color_complete_constructors :: Property
prop_color_complete_constructors = property $ do
  color <- forAll genColor
  let dynVal = toSzt color
  case dynVal ^. dvTypeInfo of
    Just typeInfo -> do
      annotate $ "Color value: " ++ show color
      annotate $ "TypeInfo: " ++ show typeInfo
      
      -- Should have all constructors regardless of which color we serialized
      typeInfo ^. tiConstructors === ["Red", "Green", "Blue"]
      typeInfo ^. tiTypeName === Just "Color"
      
      -- Structure should be sum type with binary tree structure  
      case typeInfo ^. tiStructure of
        Just (TSSum constructorTypes) -> do
          length constructorTypes === 2  -- Binary tree: Red :+: (Green :+: Blue)
        Just other -> do
          annotate $ "Expected TSSum but got: " ++ show other
          failure
        Nothing -> failure
    Nothing -> failure

-- | Test that different Color values produce identical type info (constructor completeness)
prop_color_typeinfo_consistency :: Property
prop_color_typeinfo_consistency = property $ do
  let redInfo = toSzt Red ^. dvTypeInfo
  let greenInfo = toSzt Green ^. dvTypeInfo
  let blueInfo = toSzt Blue ^. dvTypeInfo
  
  case (redInfo, greenInfo, blueInfo) of
    (Just red, Just green, Just blue) -> do
      -- All three should have identical constructor lists
      red ^. tiConstructors === green ^. tiConstructors
      green ^. tiConstructors === blue ^. tiConstructors
      red ^. tiConstructors === ["Red", "Green", "Blue"]
      
      -- All should have same type name
      red ^. tiTypeName === green ^. tiTypeName
      green ^. tiTypeName === blue ^. tiTypeName
      red ^. tiTypeName === Just "Color"
    _ -> failure

--------------------------------------------------------------------------------
-- Round-trip Tests
--------------------------------------------------------------------------------

-- | Test that Person round-trips correctly
prop_person_roundtrip :: Property
prop_person_roundtrip = property $ do
  person <- forAll genPerson
  let encoded = toSzt person
  case fromSzt encoded of
    Left err -> do
      annotate $ "Failed to decode: " ++ show err
      failure
    Right decoded -> person === decoded

-- | Test that Employee round-trips correctly  
prop_employee_roundtrip :: Property
prop_employee_roundtrip = property $ do
  employee <- forAll genEmployee
  let encoded = toSzt employee
  case fromSzt encoded of
    Left err -> do
      annotate $ "Failed to decode: " ++ show err
      failure
    Right decoded -> employee === decoded

-- | Test that Shape round-trips correctly
prop_shape_roundtrip :: Property
prop_shape_roundtrip = property $ do
  shape <- forAll genShape
  let encoded = toSzt shape
  case fromSzt encoded of
    Left err -> do
      annotate $ "Failed to decode: " ++ show err
      failure
    Right decoded -> shape === decoded

-- | Test that Color round-trips correctly
prop_color_roundtrip :: Property
prop_color_roundtrip = property $ do
  color <- forAll genColor
  let encoded = toSzt color
  case fromSzt encoded of
    Left err -> do
      annotate $ "Failed to decode: " ++ show err
      failure
    Right decoded -> color === decoded
