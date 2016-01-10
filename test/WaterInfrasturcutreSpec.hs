module WaterInfrasturcutreSpec where

import Test.Hspec

-- Import the module we wish to test.
import WaterInfrastructure

-- For the map
import qualified Data.Map as M

-- import Test.QuickCheck

main :: IO ()
main = do 
  hspec spec
  -- hspec propertySpec

spec :: Spec
spec = describe "Unit tests for WaterInfrastrucutre module" $ do
          it "Computes the sum of functional water points." $
            sumFunctionalWaterPoints [ WaterPoint "Vil0" "yes" 
                                     , WaterPoint "Vil1" "no"
                                     , WaterPoint "Vil2" "yes" ] `shouldBe` 2
          it "We get a map of all the water points in a village." $
            functionalWaterPointsPerVillage [ WaterPoint "Vil0" "yes" 
                                            , WaterPoint "Vil1" "no"
                                            , WaterPoint "Vil0" "yes" ] 
                                            M.empty `shouldBe` M.fromList [("Vil0", 2), ("Vil1", 0)]
{-
Property based tests

propertySpec :: Spec
propertySpec = 
  describe "Propery tests for WaterInfrastrucutre module." $ do
    it "Property test for sum of functional water points." $ property $
      sumFunctionalWaterPoints == foldr (\wp acc -> case water_functioning wp of
                                                      "yes" -> 1 + acc
                                                      _     -> acc) 0 
    it "Property test for a map of all water points in a village." $ property $
      (\x -> functionalWaterPointsPerVillage x M.empty) == 
      foldr (\wp acc -> case water_functioning wp of
                          "yes" -> M.insertWith 
                                    (+) 
                                    (communities_villages wp) 
                                    1 
                                    acc
                          _     -> acc) M.empty

-}
