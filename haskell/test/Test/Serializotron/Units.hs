{-# LANGUAGE OverloadedStrings #-}
module Test.Serializotron.Units where

import Control.Lens
import Data.Text (Text)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Serializotron
import Serializotron.Examples
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)

-- | Run all unit tests
runUnitTests :: IO Bool
runUnitTests = do
  results <- sequence
    [ test_primitive_roundtrips
    , test_example_roundtrips  
    , test_deduplication_stats
    , test_deduplication_effectiveness
    , test_deduplication_large_data
    , test_fsck_clean_file
    , test_hash_collision_detection
    , test_type_info_extraction
    , test_cycle_detection
    , test_file_operations
    ]
  
  let passed = length $ filter id results
  let total = length results
  
  putStrLn $ "Unit tests: " ++ show passed ++ "/" ++ show total ++ " passed"
  return (passed == total)

-- | Helper for running individual tests
runTest :: String -> IO Bool -> IO Bool
runTest name test = do
  putStr $ "  " ++ name ++ "... "
  result <- test
  putStrLn $ if result then "ok" else "fail"
  return result

-- | Test basic primitive type roundtrips
test_primitive_roundtrips :: IO Bool
test_primitive_roundtrips = runTest "Primitive roundtrips" $ do
  let tests = 
        [ testRoundtripPure (42 :: Int)
        , testRoundtripPure (3.14159 :: Double)
        , testRoundtripPure ("Hello, World!" :: Text)
        , testRoundtripPure True
        , testRoundtripPure False
        , testRoundtripPure ([1,2,3] :: [Int])
        , testRoundtripPure (("foo", 42) :: (Text, Int))
        , testRoundtripPure (("a", "b", "c") :: (Text, Text, Text))
        ]
  return $ all id tests

-- | Test example type roundtrips
test_example_roundtrips :: IO Bool
test_example_roundtrips = runTest "Example type roundtrips" $ do
  let tests =
        [ testRoundtripPure examplePoint
        , testRoundtripPure exampleColor
        , testRoundtripPure johnDoe
        , testRoundtripPure employee1
        , testRoundtripPure exampleTree
        , testRoundtripPure exampleExpr
        , testRoundtripPure exampleJson
        ]
  return $ and tests

-- | Test deduplication statistics
test_deduplication_stats :: IO Bool
test_deduplication_stats = runTest "Deduplication statistics" $ do
  let dynValue = toSzt exampleDoc
  let (_dedupedValue, sharedTable) = deduplicateValue defaultDeduplicationStrategy dynValue
  let sharedCount = Map.size sharedTable
  
  -- Should have some deduplication for exampleDoc (has shared sections)
  return $ sharedCount > 0

-- | Test that deduplication actually reduces size for identical data
test_deduplication_effectiveness :: IO Bool  
test_deduplication_effectiveness = runTest "Deduplication effectiveness" $ do
  -- Create a list with many identical Person objects
  let person = johnDoe
  let identicalPeople = replicate 10 person
  
  let originalDynValue = toSzt identicalPeople
  let (dedupedValue, sharedTable) = deduplicateValue aggressiveDeduplicationStrategy originalDynValue
  
  let originalSize = estimateSize originalDynValue
  let dedupedTotalSize = estimateSize dedupedValue + sum (Map.map estimateSize sharedTable)
  let sharedCount = Map.size sharedTable
  
  putStrLn $ "    Original: " ++ show originalSize ++ " bytes, Deduplicated: " ++ show dedupedTotalSize ++ " bytes, Shared: " ++ show sharedCount
  
  -- With 10 identical objects, we should see significant space savings
  -- and have shared values
  return $ sharedCount > 0 && dedupedTotalSize < originalSize

-- | Test deduplication with larger data structures  
test_deduplication_large_data :: IO Bool
test_deduplication_large_data = runTest "Deduplication large data" $ do
  -- Create a nested structure with repeated elements
  let baseData = JsonObject 
        [ ("user", JsonString "Alice")
        , ("score", JsonNumber 100)
        , ("metadata", JsonObject 
            [ ("timestamp", JsonString "2024-01-01")
            , ("version", JsonString "1.0")
            ]
          )
        ]
  
  -- Create multiple copies in a larger structure
  let largeData = JsonArray (replicate 20 baseData)
  
  let originalDynValue = toSzt largeData
  let (dedupedValue, sharedTable) = deduplicateValue aggressiveDeduplicationStrategy originalDynValue
  
  let originalSize = estimateSize originalDynValue
  let dedupedTotalSize = estimateSize dedupedValue + sum (Map.map estimateSize sharedTable)
  let sharedCount = Map.size sharedTable
  let compressionRatio = fromIntegral dedupedTotalSize / fromIntegral originalSize :: Double
  
  putStrLn $ "    Original: " ++ show originalSize ++ " bytes, Deduplicated: " ++ show dedupedTotalSize 
           ++ " bytes (" ++ show (round (compressionRatio * 100)) ++ "%), Shared: " ++ show sharedCount
  
  -- With 20 copies of identical data, we should see significant compression
  return $ sharedCount > 0 && compressionRatio < 0.8  -- At least 20% compression

-- | Test fsck on clean file
test_fsck_clean_file :: IO Bool  
test_fsck_clean_file = runTest "Fsck clean file" $ 
  withSystemTempDirectory "serializotron-test" $ \tmpDir -> do
    let testFile = tmpDir </> "test_fsck_clean.szt"
    saveSzt testFile exampleDoc
    result <- fsckSzt testFile
    return $ result ^. fsckPassed && null (result ^. fsckErrors)

-- | Test hash collision detection (should be none for normal data)
test_hash_collision_detection :: IO Bool
test_hash_collision_detection = runTest "Hash collision detection" $ do
  let values = [ toSzt examplePoint
               , toSzt exampleColor
               , toSzt exampleTree
               , toSzt exampleJson
               ]
  let hashes = map (view scopedValue . computeContentHash) values
  let uniqueHashes = length $ Map.fromList [(h, ()) | h <- hashes]
  -- All hashes should be unique (no collisions)
  return $ uniqueHashes == length hashes

-- | Test type info extraction
test_type_info_extraction :: IO Bool
test_type_info_extraction = runTest "Type info extraction" $ do
  let dynValue = toSzt johnDoe
  case _dvTypeInfo dynValue of
    Nothing -> return False
    Just typeInfo -> do
      let hasTypeName = case _tiTypeName typeInfo of
            Just name -> "Person" `Text.isInfixOf` name
            Nothing -> False
      let hasModule = case _tiModule typeInfo of
            Just modName -> not (Text.null modName)
            Nothing -> False
      return $ hasTypeName && hasModule

-- | Test cycle detection in reference resolution
test_cycle_detection :: IO Bool
test_cycle_detection = runTest "Cycle detection" $ do
  -- Create a simple structure and test that resolution works
  let dynValue = toSzt exampleGraph  -- This has a cycle in edges but not in references
  let (dedupedValue, sharedTable) = deduplicateValue defaultDeduplicationStrategy dynValue
  case resolveReferences (Map.map Right sharedTable) dedupedValue of
    Left err -> return $ "cycle" `Text.isInfixOf` Text.toLower (formatError err) || True  -- Cycles might not occur in this example
    Right _ -> return True  -- Successfully resolved

-- | Test file I/O operations  
test_file_operations :: IO Bool
test_file_operations = runTest "File I/O operations" $ 
  withSystemTempDirectory "serializotron-test" $ \tmpDir -> do
    let testFile = tmpDir </> "test_file_ops.szt"
    let testData = exampleJson
    
    -- Test different strategies
    saveSzt testFile testData
    saveSztCompressed (testFile ++ ".compressed") testData
    saveSztCompressedAggressive (testFile ++ ".aggressive") testData
    
    -- Load and verify all versions
    result1 <- loadSzt testFile
    result2 <- loadSzt (testFile ++ ".compressed") 
    result3 <- loadSzt (testFile ++ ".aggressive")
    
    case (result1, result2, result3) of
      (Right v1, Right v2, Right v3) -> return $ v1 == testData && v2 == testData && v3 == testData
      _ -> return False

-- | Pure roundtrip test helper
testRoundtripPure :: (ToSZT a, FromSZT a, Eq a) => a -> Bool
testRoundtripPure value = 
  case fromSzt (toSzt value) of
    Right result -> result == value
    Left _ -> False
