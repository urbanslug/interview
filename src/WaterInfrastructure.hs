{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module WaterInfrastructure 
( calculate
, sumFunctionalWaterPoints
, functionalWaterPointsPerVillage
, WaterPoint (..)
) where

-- Network imports
import Network.Wreq

-- Data marshalling handling imports.
import qualified Data.ByteString.Lazy as L
import Control.Lens

-- Exception handling
import Control.Exception (try)
import Network.HTTP.Client (HttpException(..))

-- JSON parsing
import Data.Aeson
import GHC.Generics

import Data.Aeson.Encode.Pretty (encodePretty)

-- For the actual data processing.
import qualified Data.Map as M

import Data.List (sortBy)
import Data.Ord (comparing)



calculate :: IO ()
calculate = do
  -- Fetch the JSON file via a url
  eitherFile <- getJSON jsonUrl

  -- Extract the actual JSON file as a Bytestring.
  eitherJSON <- case eitherFile of
                  Right jsonFile -> return jsonFile -- Get the JSON file.
                  Left exceptionMsg -> fail exceptionMsg -- Print the exception message

  -- Parse JSON to extract a list of Water Points. We use eitherDecode
  -- so that we can print out a proper error in case parsing the JSON fails.
  waterPoints <- case eitherDecode eitherJSON :: Either String [WaterPoint]  of
                   Right js -> return $ js
                   Left x -> fail $ show x

  let -- Compute the total number of water points in every village
      functionalWaterPoints = sumFunctionalWaterPoints waterPoints

      -- Compute the total number of water points per village
      waterPointsMap = functionalWaterPointsPerVillage waterPoints M.empty

      -- Convert the overall info needed (in the question) into a type.
      overall = OverallInfo functionalWaterPoints waterPointsMap (sortMap waterPointsMap)

      -- Marshall that type into JSON
      overallJSON = encodePretty overall

  -- Write the JSON to a file.
  L.writeFile "result.json" overallJSON


-- We hardcode the JSON url we will use.
-- No test, enforced by typechecker.
jsonUrl :: String
jsonUrl = "https://raw.githubusercontent.com/onaio/ona-tech/master/data/water_points.json"


-- Get the total number of functional water points.
-- | >>> sumFunctionalWaterPoints [ WaterPoint "Vil0" "yes" 
--                                , WaterPoint "Vil1" "no"
--                                , WaterPoint "Vil2" "yes" ]
-- 2 
-- >>> prop> sumFunctionalWaterPoints == foldr (\wp acc -> case water_functioning wp of
--                                                       "yes" -> 1 + acc
--                                                       _     -> acc) 0 
sumFunctionalWaterPoints :: [WaterPoint] -> Int
sumFunctionalWaterPoints [] = 0
sumFunctionalWaterPoints (x :xs) = 
  case water_functioning x of
    "yes" -> 1 + sumFunctionalWaterPoints xs
    _     -> 0 + sumFunctionalWaterPoints xs


-- Compute the total number of functional water points per village.
-- Create a map (village => numberOfWaterPoints) from [WaterPoint]
-- Each functional waterpoint is a +1 anything else is a +0
-- | >>> functionalWaterPointsPerVillage [ WaterPoint "Vil0" "yes" 
--                                , WaterPoint "Vil1" "no"
--                                , WaterPoint "Vil0" "yes" ]
-- M.fromList [("Vil0", 2), ("Vil1", 0)]
-- >>> prop> functionalWaterPointsPerVillage == foldr (\wp acc -> case water_functioning wp of
--                                                       "yes" -> M.insertWith 
--                                                                 (+) 
--                                                                 (communities_villages wp) 
--                                                                 1 
--                                                                 acc
--                                                       _     -> acc) M.empty
functionalWaterPointsPerVillage :: [WaterPoint] -> (M.Map String Int) -> (M.Map String Int)
functionalWaterPointsPerVillage [] mp = mp
functionalWaterPointsPerVillage (x:xs) mp = 
  let change wp = 
        case water_functioning wp of
          "yes" -> 1
          _     -> 0
  in functionalWaterPointsPerVillage xs $ M.insertWith (+) (communities_villages x) (change x) mp

-- Sort the map into an association list.
-- prop> sortmap == \x:xs -> 
sortMap :: (M.Map String Int) -> [(String, Int)]
sortMap mp = sortBy (comparing snd) $ M.toList mp



{-
   Network - in this case we just fetch JSON.
-}

-- Get the JSON file and handle any exceptions that may occur.
getJSON :: String -> IO (Either String L.ByteString) -- | Either "HttpError" Webpage 
getJSON url = do
  -- try :: IO L.ByteString -> IO (Either Exception L.ByteString)
  eitherResponse <- 
    try (get url) :: IO (Either HttpException (Response L.ByteString))
  case eitherResponse of
    Right response -> return $ Right $ response ^. responseBody
    Left ex -> return $ Left $ "HTTP Exception: " ++ show ex



{- 
   Types & their typeclass instances.
   No tests, these are enforced by the typechecker.
-}

-- A representation of the water point dataset as a type.
data WaterPoint = WaterPoint { communities_villages :: String
                             , water_functioning :: String 
                             }  deriving (Show, Generic)

-- The overall info needed as a type.
data OverallInfo = OverallInfo {number_functional :: Int
                               , number_water_points :: (M.Map String Int)
                               , community_ranking :: [(String, Int)]
                               } deriving (Show, Generic)

-- We create a Water Point instance of the FromJSON typeclass.                    
instance FromJSON WaterPoint

instance ToJSON OverallInfo

